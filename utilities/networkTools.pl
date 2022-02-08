#!/usr/bin/perl -w

#######################################################################
#
#	This code does various (optional) pre-processing and manipulations with a river network
#	such as a Cell Table for the Water Balance Model (WBM), and many others.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	Created-	January 2011
#	Modified-
#	Feb 2019	More Network subsetting options. Change to using .init file for inputs.
#	Apr 2019	Minor improvements.
#	Nov 2020	Option to assume equal area projection.
#	Mar 2021	Option to exclude endorheic basin mouths near the network boundary/coast.
#	Mar 2021	Added section to copy basin attributes.
#	Apr 2021	Added section to fix endorheic basins; Segmentation method for endorheic mask.
#	May 2021	Added network stream order calculations; Output of shape and GeoJSON files.
#	Jun 2021	Added search for best match of upstream area between observed and input river network.
#	Jul 2021	Fixed bug in "upstr" PDL PP function
#	Sep 2021	Added RIMS::WBM
#
#	Version:	21.9.1
#
#######################################################################

use strict;
use Benchmark;
use Getopt::Long;
use Geo::GDAL;
use File::Basename;
use File::Path;
use File::Temp;
use FileHandle;
use List::Util;			# NB- min/max returns alias to a var in argument list
use Math::Trig qw/pi/;
use PDL;
use PDL::Image2D;
use PDL::IO::FlexRaw;
use PDL::NiceSlice;
use PDL::NetCDF;
use Inline qw/Pdlpp/;
use Fcntl;
use Storable qw/dclone/;
use RIMS;		### WSAG UNH module set
use RIMS::WBM;

#$PDL::BIGPDL = 1 if !$PDL::BIGPDL;	# Use BIGPDL if the network is really big (over 1GB)

STDOUT->autoflush(1);				# Disable buffering

set_autopthread_targ(4);	# Number of CPU threads
set_autopthread_size(1);	# Piddle size lower limit (in Meg) for multi-threading

my $time_start	= time();
my $separator	= "\n" . ('='x60) . "\n";
my $PATH	= get_file_path();

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose) = (0,0);
						# Get command line options
usage() if !GetOptions('h'=>\$help, 'v'=>\$verbose) or $help;

my $input_file		= shift() or usage();
my %runIO		= read_init($input_file);
   $runIO{Net_proj}	= 'epsg:4326'	unless $runIO{Net_proj};
   $runIO{Net_projForce}= 0		unless $runIO{Net_projForce};
   $runIO{basinID_file}	= ''		unless $runIO{basinID_file};
   $runIO{gdal_transl}	= get_file_path()->{gdal_translate};

my $file_ntwk		= $runIO{Net_file}	? $runIO{Net_file} : '';
my $file_bIDs		= $runIO{basinID_file}	? $runIO{basinID_file} : '';
my $file_upArea		= $runIO{upArea_file}	? $runIO{upArea_file}  : '';
my $flag_msk		= $runIO{stnMask}	? $runIO{stnMask}  : 0;
my $flag_preclip	= 0;
my $area_opt		= {FORCE => $runIO{Net_projForce}};

open STDOUT, '>/dev/null' unless $verbose;
my $CT_header	= ['ToCell','ToCellX','ToCellY','X','Y','StreamOrder','Lon','Lat','SubbasinArea'];

#######################################################################
#############   Job # 1 - Build Cell Table   ##########################

if ($runIO{cell_table}) {
  print	"\n\tCell Table job. The network to process:\n";
  die	"Network file is not given or does not exists. Aborting...\n\n" unless -e $file_ntwk;
  print	"$file_ntwk\n\n";

		### Setting defaults for output flags
  my $flag_csv	= set_default($runIO{cell_table}{CT_csv},		1);
  my $flag_dat	= set_default($runIO{cell_table}{CT_dat},		0);
  my $flag_IDs	= set_default($runIO{cell_table}{basinIDs},		1);
  my $flag_sOd	= set_default($runIO{cell_table}{streamOrder},		0);
  my $flag_upA	= set_default($runIO{cell_table}{upstrArea},		1);
  my $flag_shp	= set_default($runIO{cell_table}{Net_shape},		0);
  my $flag_EnR	= set_default($runIO{cell_table}{endorheic},		0);
  my $info_EnR	= set_default($runIO{cell_table}{endorheic_csv},	0);
  my $bffr_EnR	= set_default($runIO{cell_table}{endorheicBuffer},	0);
  my $segm_EnR	= set_default($runIO{cell_table}{segmentation},		0);

		### Making file names
  my $file_ctbl	= $runIO{cell_table}{output_file} || $file_ntwk;
     $file_ctbl			=~ s/\.\w+$/.dat/;
 (my $file_ctab	= $file_ctbl)	=~ s/\.\w+$/.csv/;
 (my $file_sOdr	= $file_ctbl)	=~ s/\.\w+$/_strmOrder.asc/;
 (my $file_upAr	= $file_ctbl)	=~ s/\.\w+$/_upstrArea.asc/;
 (my $file_IDs	= $file_ctbl)	=~ s/\.\w+$/_IDs.asc/;
 (my $file_EnR	= $file_ctbl)	=~ s/\.\w+$/_EnR.asc/;	# IDs  of endorheic basins
 (my $file_EnRs	= $file_ctbl)	=~ s/\.\w+$/_EnR.csv/;	# Info of endorheic basins
 (my $file_msk	= $file_ctbl)	=~ s/\.\w+$/_msk.asc/;	# Network mask file
  if($file_upArea) {
     die "\nUpstream area file does not exist :\n   $file_upArea\nAborthing...\n\n" unless -e $file_upArea;
     $file_upAr	= $file_upArea;
     $flag_upA	= 0; }
  my $shape_dir	= dirname($file_ctbl).'/Network_shape_files/';
 (my $file_shp	= $shape_dir .	basename(   $file_ctbl))=~s/\.\w+$//;
     prepare_dir( $file_ctbl);	prepare_dir($file_shp)	if $flag_shp;

		### Read Network data
  print "Reading flow directon data...\n";
  my @time		= (Benchmark->new);
  my $extent		= get_extent($file_ntwk, $runIO{Net_proj});
  my $flowDir		= $$extent{mask};
  my($colTo,$rowTo)	= flow_to($flowDir);		# Modifies $flowDir too
  my $basinID		= $file_bIDs ?
	read_GDAL($extent, {'Projection'=>$runIO{Net_proj}}, 0, $file_bIDs, 1, -9999) : 0;
  my $mask		= $flowDir->isgood	if $flag_msk;
  printf "   File \"%s\": %s\n", basename($file_ntwk), netwkString($flowDir);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Check curcularity of the Network
  printf "Checking curcularity of the Network (%d cells)...\n",$flowDir->ngood;
  check_circularityC($flowDir, $$extent{gTransform});			# C-coded version
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Calculate upstream area
  print((-e $file_upArea?'Reading':'Calculating')," upstream area...\n");
  my $cell_area	= cell_area(lonLat($extent),$extent,$area_opt);
  my $up_area	= -e $file_upArea ? read_raster($file_upAr) : byte($flowDir)->upstrAccumAll($cell_area);
  die "\nUpstream area data in the file provided:\n   $file_upArea\ndoes not match network grid. Aborting...\n\n"
	if $up_area->dim(0)!=$cell_area->dim(0) || $up_area->dim(1)!=$cell_area->dim(1);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Build Cell table
  print "Shaping Cell Table...\n";
  my ($area,$cell_pdl) = cell_table($up_area,$flowDir,$colTo,$rowTo);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));
  
 		### Calculate Stream Order
  print 'Calculating stream order... ';
  my  $strmOrder = $cell_pdl->streamOrder($colTo,$rowTo);
  printf "Max value = %d\n", $strmOrder->max;
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Build basin IDs
  unless (ref($basinID)) {
  print "Building basin IDs...\n";
    $basinID	= byte($flowDir)->basinIDs($cell_pdl);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2])); }

		### Build endorheic basin IDs
  my ($basinEnR, $hdr_EnR, $list_EnR);
  if ($flag_EnR || $info_EnR) {		# Skip, if not needed (may take a long time to compute)
    my $buffer	= floor($bffr_EnR/sqrt($cell_area))->lclip(1)->long;
    print "Building endorheic basin IDs...";
    $basinEnR	= endorheic_basins($basinID, $flowDir, $buffer, $segm_EnR);
    printf " %.2f %% of the Network cells are %d endorheic basins.\n",
		100*($basinEnR->ngood/$basinID->ngood), $basinEnR->uniq->dim(0);
    if ($info_EnR) {
      $hdr_EnR	= [qw(basin_ID mouth_Lon mouth_Lat mouth_Col mouth_Row n_pixels area)];
      $list_EnR	= info_EnR($extent, $basinEnR, $flowDir, $up_area); }
    push @time, Benchmark->new;
    printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));
  }

		### Save data to files
  print "Finishing and saving output files for this job...\n";
  write_CT_csv(	  $file_ctab,$CT_header,$extent, $area,$cell_pdl,$strmOrder)		if $flag_csv;
  write_shape(	  $file_shp, $CT_header,$extent, $area,$cell_pdl,$strmOrder,$basinID)	if $flag_shp;
  write_gridascii($file_sOdr,$strmOrder,$extent, {FORMAT => '%d'})			if $flag_sOd;
  write_gridascii($file_upAr,$up_area,	$extent, {FORMAT => '%.1f'})			if $flag_upA;
  write_gridascii($file_IDs, $basinID,	$extent, {FORMAT => '%d'})			if $flag_IDs;
  write_gridascii($file_EnR, $basinEnR,	$extent, {FORMAT => '%d'})			if $flag_EnR;
  write_gridascii($file_msk, $mask,	$extent, {FORMAT => '%d'})			if $flag_msk;
  writeflex(	  $file_ctbl,$up_area,	$area,    $cell_pdl)				if $flag_dat;
  write_csv(	  $file_EnRs,$hdr_EnR,  $list_EnR)					if $info_EnR;
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));
  print "\n\tCell Table job is Done! Output files:\n";
  map { print "   $$_[0]\n" if $$_[1]; } [$file_ctab,$flag_csv], [$file_ctbl,$flag_dat], [$file_sOdr, $flag_sOd],
		[$file_upAr,$flag_upA],  [$file_IDs,$flag_IDs ], [$file_EnR, $flag_EnR], [$file_EnRs, $info_EnR],
	map(["$file_shp.$_",$flag_shp],   qw(geojson shp kml));

  print $separator if $runIO{preclip} || $runIO{subset} || $runIO{combine} || $runIO{subtract};
}

#######################################################################
#############   Job # 2 - Build Network Preclip   #####################

if ($runIO{preclip}) {
  print	"\n\tNetwork pre-clipping job. Master network to process:\n";
  die	"Network file is not given or does not exists. Aborting...\n\n" unless -e $file_ntwk;
  print	"$file_ntwk\n\n";

		### Setting defaults for output flags
  my $flag_trim		= set_default($runIO{preclip}{trim_nodata},		1);
     $flag_preclip	= set_default($runIO{preclip}{truncation_check},	1);

		### Making file names
  my $file_clip	=  $runIO{preclip}{output_file} || $file_ntwk;
     $file_clip eq $file_ntwk ?
		  $file_clip	=~ s/\.\w+$/_preClip.asc/ :
		  $file_clip	=~ s/\.\w+$/.asc/;
 (my $file_msk	= $file_clip)	=~ s/\.\w+$/_msk.asc/;	# Network mask file
  prepare_dir(	  $file_clip);

		### Blind pre-clipping of the source Netowrk and IDs
  $file_ntwk	= preclip_rectangle($file_ntwk, $file_clip, \%runIO)	if $runIO{preclip}{rectangle};
  $file_ntwk	= preclip_polygon  ($file_ntwk, $file_clip, \%runIO)	if $runIO{preclip}{polygon};

		### Build mask file, if requested
  write_gridascii($file_msk, read_raster($file_ntwk)->isgood, get_extent($file_ntwk, $runIO{Net_proj}), {FORMAT=>'%d'})
	if $flag_msk;

  print "\tPre-Clipping job is Done! Output files:\n";
  map { print "   $$_[0]\n" if $$_[1]; } [$file_clip, 1], [$file_msk, $flag_msk];
  print $separator if $runIO{subset} || $runIO{combine} || $runIO{subtract};
}

#######################################################################
#############   Job # 3 - Build Network Subset   ######################
JOB_3: {					# This block is needed to use "last" function to quit out of it
if ($runIO{subset}) {
  print	"\n\tNetwork subsetting job. Master network to process:\n";
  die	"Network file is not given or does not exists. Aborting...\n\n" unless -e $file_ntwk;
  print	"$file_ntwk\n\n";

		### Setting defaults for output flags
  my $flag_trim	= set_default($runIO{subset}{trim_nodata},	1);
  my $flag_CTa	= set_default($runIO{subset}{CT_csv},		0);
  my $flag_CTb	= set_default($runIO{subset}{CT_dat},		0);
  my $flag_IDs	= set_default($runIO{subset}{basinIDs},		1);
  my $flag_sOd	= set_default($runIO{subset}{streamOrder},	0);
  my $flag_upA	= set_default($runIO{subset}{upstrArea},	1);
  my $flag_EnR	= set_default($runIO{subset}{endorheic},	0);
  my $bffr_EnR	= set_default($runIO{subset}{endorheicBuffer},	0);
  my $segm_EnR	= set_default($runIO{subset}{segmentation},	0);

		### Making file names
  my $file_Net	= $runIO{subset}{output_file} || $file_ntwk;
     $file_Net eq $file_ntwk ?
		  $file_Net	=~ s/\.\w+$/_Subset.asc/ :
		  $file_Net	=~ s/\.\w+$/.asc/;
 (my $file_ctab	= $file_Net)	=~ s/\.\w+$/.csv/;
 (my $file_ctbl	= $file_Net)	=~ s/\.\w+$/.dat/;
 (my $file_sOdr	= $file_ctbl)	=~ s/\.\w+$/_strmOrder.asc/;
 (my $file_upAr	= $file_Net)	=~ s/\.\w+$/_upstrArea.asc/;
 (my $file_IDs	= $file_Net)	=~ s/\.\w+$/_IDs.asc/;
 (my $file_EnR	= $file_Net)	=~ s/\.\w+$/_EnR.asc/;	# IDs  of internal basins
 (my $file_msk	= $file_Net)	=~ s/\.\w+$/_msk.asc/;	# Network mask file
  prepare_dir(	  $file_Net);

		### Read Network data
  print "Reading flow directon data...\n";
  my($basinNet, $bsnIDNet, $extNet);			# Resulting subsets for the Network and Basin IDs
  my @time		= (Benchmark->new);
  my $extent		= get_extent($file_ntwk, $runIO{Net_proj});
  my $flowDir		= $$extent{mask};
  my($colTo,$rowTo)	= flow_to($flowDir);		# Modifies $flowDir too
  my $basinID		= $file_bIDs ?
	read_GDAL($extent, {'Projection'=>$runIO{Net_proj}}, 0, $file_bIDs, 1, -9999) : 0;
  my($mask, $ID)	= (zeroes(byte,$flowDir->dims), undef);
  printf "   File \"%s\": %s\n", basename($file_ntwk), netwkString($flowDir);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Check curcularity of the Network
  printf "Checking curcularity of the Network (%d cells)...\n",$flowDir->ngood;
  check_circularityC($flowDir, $$extent{gTransform});			# C-coded version
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Calculate upstream area
  print((-e $file_upAr?'Reading':'Calculating')," upstream area...\n");
  my $cell_area	= cell_area(lonLat($extent),$extent,$area_opt);
  my $up_area	= -e $file_upAr ? read_raster($file_upAr) : byte($flowDir)->upstrAccumAll($cell_area);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Build Cell table
  print "Shaping Cell Table...\n";
  my ($area,$cell_pdl) = cell_table($up_area,$flowDir,$colTo,$rowTo);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

		### Build basin IDs
  unless (ref($basinID)) {
  print "Building basin IDs...\n";
    $basinID	= byte($flowDir)->basinIDs($cell_pdl);
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2])); }

	### Subsetting the Network by a given method (by priority order)
	###############################
	### Rectangle method

  if ($runIO{subset}{rectangle}) {
    print "\nSubsetting network by Rectangle method...\n";
    die "  Wrong input format for the rectangle subsetting option. Aborting...\n\n" unless
	ref($runIO{subset}{rectangle}) eq 'HASH' &&
	defined $runIO{subset}{rectangle}{lonMin} && defined $runIO{subset}{rectangle}{lonMax} &&
	defined $runIO{subset}{rectangle}{latMin} && defined $runIO{subset}{rectangle}{latMax};

    my @box	= (colRow($extent,$runIO{subset}{rectangle}{lonMin},$runIO{subset}{rectangle}{latMin}),
		   colRow($extent,$runIO{subset}{rectangle}{lonMax},$runIO{subset}{rectangle}{latMax}));
    my @dim	= (0, $mask->dim(0)-1, 0, $mask->dim(1)-1);

			# Check subsetting extents first
    die "\tThe subsetting regions is outside of the Network domain. Aborting...\n\n"
    if  $box[0] > $dim[1] || $box[2] < $dim[0] || $box[3] > $dim[3] || $box[1] < $dim[2];
    if ($box[0] < $dim[0]) { $box[0] = $dim[0]; print "\tSubsetting region min Longitute is clipped.\n"; }
    if ($box[2] > $dim[1]) { $box[2] = $dim[1]; print "\tSubsetting region max Longitute is clipped.\n"; }
    if ($box[3] < $dim[2]) { $box[3] = $dim[2]; print "\tSubsetting region max Latitude  is clipped.\n"; }
    if ($box[1] > $dim[3]) { $box[1] = $dim[3]; print "\tSubsetting region min Latitude  is clipped.\n"; }

    $mask($box[0]:$box[2],$box[3]:$box[1]) .= 1;

		### Inscribe
    my  $inscribe = set_default($runIO{subset}{polygon}{inscribe},	0);
    if (ref($inscribe) eq 'HASH') {
      ($mask, $basinNet, $bsnIDNet) = inscribe_upstream($mask, $flowDir, $basinID, $up_area, $extent, $inscribe);
      unless (defined $mask)	{ print "   No full basins found for this network subset...\n\n"; last; }
    }
    else {	### Superscribe or full basin inscribe
      printf "  Using %s method.\n\n", $inscribe ? 'inscribe' : 'superscribe';
      $ID	= find_IDs($mask, $basinID, $inscribe);
      unless ($ID->nelem)	{ print "   No full basins found for this network subset...\n\n"; last; }
    }
  }
	###############################
	### Polygon method

  elsif ($runIO{subset}{polygon}) {
    print "\nSubsetting network by Polygon method...\n";
    die "  Wrong input format for the polygon subsetting option. Aborting...\n\n"
	unless ref( $runIO{subset}{polygon} ) eq 'HASH';
    die "  Mask file for polygon subsetting does not exists. Aborting...\n\n"
	unless -e $runIO{subset}{polygon}{file};
    die "  Missing or wrong input format for polygon IDs- it must be a list (see manual). Aborting...\n\n"
	unless ref( $runIO{subset}{polygon}{polIDs} ) eq 'ARRAY';
    die "  Shape file in pyligon subsetting requires key for a masking variable name. Aborting...\n\n"
	if $runIO{subset}{polygon}{file} =~ m/\.shp$/i && !$runIO{subset}{polygon}{shp_var};

    my $proj	= $runIO{subset}{polygon}{proj} ? $runIO{subset}{polygon}{proj} : 'epsg:4326';

    my($var, $processing) = $runIO{subset}{polygon}{file} =~ m/\.shp$/i ?
	($runIO{subset}{polygon}{shp_var}, 'polygon') : ('', '');

    $mask	= read_GDAL($extent, {'Var_Name'=>$var, 'Processing'=>$processing, 'Projection'=>$proj},
	0, $runIO{subset}{polygon}{file}, 1, -9999)->in($runIO{subset}{polygon}{polIDs});

		### Inscribe
    my  $inscribe = set_default($runIO{subset}{polygon}{inscribe},	0);
    if (ref($inscribe) eq 'HASH') {
      ($mask, $basinNet, $bsnIDNet) = inscribe_upstream($mask, $flowDir, $basinID, $up_area, $extent, $inscribe);
      unless (defined $mask)	{ print "   No full basins found for this network subset...\n\n"; last; }
    }
    else {	### Superscribe or full basin inscribe
      printf "  Using %s method.\n\n", $inscribe ? 'inscribe' : 'superscribe';
      $ID	= find_IDs($mask, $basinID, $inscribe);
      unless ($ID->nelem)	{ print "   No full basins found for this network subset...\n\n"; last; }
    }
  }
	###############################
	### List of basin IDs method

  elsif ($runIO{subset}{basinID_List}) {
    print "\nSubsetting network by list of basin IDs method...\n";
    die "  Wrong input format for the basin ID subsetting option. Aborting...\n\n"
	unless ref( $runIO{subset}{basinID_List} ) eq 'ARRAY';	map {
    die "  Basin IDs must be numeric. Aborting...\n\n"
	unless isNumber($_) } @{$runIO{subset}{basinID_List}};
    print "   Requested IDs: ",join(' ', @{$runIO{subset}{basinID_List}}),"\n";

    $ID	= pdl $runIO{subset}{basinID_List};
    $ID	= setops($ID,'AND',$basinID->uniq);	# Filter IDs that exist in the Netowrk
    unless ($ID->nelem) {
      print "   No full basins found for this network subset...\n\n";
      last;
    }
    print "   Filtered  IDs: ",join(' ', $ID->list),"\n";
  }
	###############################
	### Upstream method

  elsif ($runIO{subset}{upstream}) {
    my $params = $runIO{subset}{upstream};
    print "\nSubsetting network by Upstream method...\n";
    die "  Wrong input format for the upstream subsetting option. Aborting...\n\n" unless ref($params) eq 'HASH';
    my $search	= set_default($$params{search},	0);

			# Read upstream point locations
    my (@LonLat, @name);
    if (exists($$params{lon}) && exists($$params{lat})) {
      @LonLat	= ([$$params{lon}, $$params{lat}]);
      @name	= ('N/A');
    }
    elsif ($$params{file} && $$params{lonCol} && $$params{latCol} && $$params{nameCol}) {
      my ($hdr,@dt)	= read_table($$params{file});
      @LonLat		= map [$$_[$$hdr{$$params{lonCol}}], $$_[$$hdr{$$params{latCol}}]],	@dt;
      @name		= map  $$_[$$hdr{$$params{nameCol}}],					@dt;
    }
    else { die "  Wrong input format for the upstream subsetting options. Aborting...\n\n"; }
    my @colRow	= map [colRow($extent, @$_)], @LonLat;

			# Check subsetting location first
    for (my $i=0; $i<=$#colRow; $i++) {
      if ($colRow[$i][0] < 0 || $colRow[$i][0] >= $$extent{ncols} ||
      	  $colRow[$i][1] < 0 || $colRow[$i][1] >= $$extent{nrows}) {
	print "  This subsetting upstream location is outside of the Network domain: \"$name[$i]\"...\n";
	splice @colRow,	$i, 1;	splice @colRow,	$i, 1;
	splice @name,	$i, 1;	$i--;
    } }
    die "  No upsteam location(s) found inside the Network domain. Aborting...\n\n" unless @colRow;
    print  "  Calculating upstream area...\n";
    print  "    Search is requested:\n" if $search;

		### Calculate upstream area
    for (my $i=0; $i<=$#colRow; $i++) {
      if ($search) {
	my $searchPix	= sprintf "%.0f", $search / sqrt($cell_area->at(@{$colRow[$i]}));   # Rounding to nearest integer
	my($colRow, $area)	= search_3x3($up_area, $colRow[$i], undef, $searchPix);
	printf "      Upstream location is (%s, %s) found within $search km from (%s, %s): \"$name[$i]\"\n",
	  Geo::GDAL::ApplyGeoTransform($$extent{gTransform}, $colRow[$i][0]+0.5, $colRow[$i][1]+0.5), @{$LonLat[$i]};
	$colRow[$i]	= $colRow;
      }
      $flowDir(@{$colRow[$i]}) .= 0;		# Set outlet point for the upstream subset network
      $mask += $flowDir->upstreamMask(@{$colRow[$i]})->copybad($flowDir);
    }

		### Subsetting using upstream mask
    $basinNet	= condition($mask, $flowDir, -9999)->setvaltobad(-9999);
    $bsnIDNet	= condition($mask, $basinID, -9999)->setvaltobad(-9999);

    push @time, Benchmark->new;
    printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));
  }
  else {
    die "Unknown or missing network subsetting method in the .init file. Aborting...\n\n";
  }
	###############################
	### Finishing subsetting job

  if (defined $ID) {		# Subset Network by basin IDs
    $basinNet	= condition($basinID->in($ID), $flowDir, -9999)->setvaltobad(-9999);
    $bsnIDNet	= condition($basinID->in($ID), $basinID, -9999)->setvaltobad(-9999);
  }
		### Trim subset grids, if requested
  delete $$extent{mask};	# clone does not work with PDL objects
  ($basinNet,$extNet) = trim($basinNet, dclone($extent), $flag_trim, $flag_preclip);
  ($bsnIDNet,$extNet) = trim($bsnIDNet, dclone($extent), $flag_trim, 0);		if ($flag_CTa || $flag_sOd) {
  ($colTo,   $rowTo ) = flow_to($basinNet);					}

		### Make optional output data
  print "Making optional output data:\n";
  my($basinEnR, $stnMask, $cell_csv, $strmOrder);
     $cell_area	= cell_area(lonLat($extNet),$extNet);			   if($flag_upA) { print "   Upstream area...\n";
     $up_area	= byte($basinNet)->upstrAccumAll($cell_area); }		   if($flag_EnR) { print "   Endorheic basins...\n";
  my $buffer	= floor($bffr_EnR/sqrt($cell_area))->lclip(1)->long;
     $basinEnR	= endorheic_basins($bsnIDNet,$basinNet,$buffer,$segm_EnR);}if($flag_msk) { print "   Network mask...\n";
     $stnMask	= $basinNet->isgood; }		if($flag_sOd ||$flag_CTa ||   $flag_CTb) { print "   Cell Table...\n";
    ($area,$cell_pdl) = cell_table($up_area,$basinNet,flow_to($basinNet));
     $strmOrder	= $cell_pdl->streamOrder($colTo,$rowTo); }

		### Save data to files
  print "   Saving output files...\n\n";
  write_CT_csv(	  $file_ctab,$CT_header,$extNet, $area,	$cell_pdl,$strmOrder)	if $flag_CTa;
  writeflex(	  $file_ctbl,$up_area,	$area,		$cell_pdl)		if $flag_CTb;
  write_gridascii($file_Net, $basinNet, $extNet, {FORMAT => '%d'  });
  write_gridascii($file_sOdr,$strmOrder,$extNet, {FORMAT => '%d'  })		if $flag_sOd;
  write_gridascii($file_IDs, $bsnIDNet, $extNet, {FORMAT => '%d'  })		if $flag_IDs;
  write_gridascii($file_upAr,$up_area,  $extNet, {FORMAT => '%.1f'})		if $flag_upA;
  write_gridascii($file_EnR, $basinEnR, $extNet, {FORMAT => '%d'  })		if $flag_EnR;
  write_gridascii($file_msk, $stnMask,  $extNet, {FORMAT => '%d'  })		if $flag_msk;
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));
  print "\n\tSubsetting job is Done! Output files:\n";
  map { print "   $$_[0]\n" if $$_[1]; } ["$file_Net :\t".netwkString($basinNet), 1],
	[$file_ctab, $flag_CTa], [$file_ctbl, $flag_CTb], [$file_sOdr, $flag_sOd], [$file_IDs, $flag_IDs],
	[$file_upAr, $flag_upA], [$file_EnR,  $flag_EnR], [$file_msk,  $flag_msk];

  print $separator if $runIO{combine} || $runIO{subtract};
}}

#######################################################################
#############   Job # 4 - Combining list of Networks   ################

if ($runIO{combine}) {
  print	"\n\tNetwork combine job. Sub-networks to combine:\n";
  die	"Empty or wrong input format for the file list to combine. Aborting...\n\n"
		unless ref($runIO{combine}{Net_files}) eq 'ARRAY' &&  scalar @{$runIO{combine}{Net_files}};
  die	"File path for output Network must be given. Aborting...\n\n"	unless $runIO{combine}{output_file};
  map print("$runIO{combine}{Net_files}[$_]\n"), 0 .. $#{$runIO{combine}{Net_files}};

		### Setting defaults for output flags
  my $flag_overlap = set_default($runIO{combine}{allow_overlap}, 0);

	### Read and check Networks to combine
  my  (%extent, @extent);
  for (my $i=0; $i<scalar(@{$runIO{combine}{Net_files}}); $i++) {
    my $file	= $runIO{combine}{Net_files}[$i];
    push   @extent, get_extent($file, $runIO{Net_proj});
    printf "   $file:\t%s\n", netwkString($extent[-1]{mask});
    if ($i) {						# Check cell size match and find combined extent
      die "\tMissmatch of cell size in the last file. Aborting...\n\n"
	if abs($extent[$i]{cellsizeX} - $extent[0]{cellsizeX}) > 0.0001 * $extent[0]{cellsizeX} ||
	   abs($extent[$i]{cellsizeY} - $extent[0]{cellsizeY}) > 0.0001 * $extent[0]{cellsizeY};
      map $extent{$_} = List::Util::min($extent{$_}, $extent[$i]{$_}), qw(xllcorner yllcorner);
      map $extent{$_} = List::Util::max($extent{$_}, $extent[$i]{$_}), qw(xurcorner yurcorner);
    } else {
      map $extent{$_} = $extent[$i]{$_}, qw(xllcorner yllcorner xurcorner yurcorner);
  } }
  map $extent{$_}= $extent[0]{$_}, qw(cellsize cellsizeX cellsizeY);
  $extent{ncols} = sprintf "%.0f", ($extent{xurcorner} - $extent{xllcorner}) / $extent{cellsizeX};
  $extent{nrows} = sprintf "%.0f", ($extent{yurcorner} - $extent{yllcorner}) / $extent{cellsizeY};

	### Combine networks
  my $comboMSK	= zeroes(short, $extent{ncols}, $extent{nrows});
  my $comboNet	= $comboMSK - 9999;
  foreach my $extent (@extent) {
    my $mask	= $$extent{mask}->isgood;
    my $idx	= whichND($mask);
    my $IDX	= $idx->copy;
       $IDX(0,)+= sprintf "%.0f", ($$extent{xllcorner} -  $extent{xllcorner}) / $extent{cellsizeX};
       $IDX(1,)+= sprintf "%.0f", ( $extent{yurcorner} - $$extent{yurcorner}) / $extent{cellsizeY};

    $comboMSK->indexND($IDX) += $mask		->indexND($idx);
    $comboNet->indexND($IDX) .= $$extent{mask}	->indexND($idx);
  }
  die "\tOverlapping sub-networks. Aborting...\n\n" if !$flag_overlap && ($comboMSK > 1)->sum;	# Check combined network

  prepare_dir(	  $runIO{combine}{output_file});
  my $file_out	= $runIO{combine}{output_file};
 (my $file_msk	= $file_out)	=~ s/\.\w+$/_msk.asc/;	# Network mask file
  write_gridascii($file_out, $comboNet,   \%extent, {FORMAT => '%d'});
  write_gridascii($file_msk, $comboMSK>0, \%extent, {FORMAT => '%d'}) if $flag_msk;
  print  "\n\tCombine job is Done!  Output files:\n";
  map {  print "   $$_[0]\n" if $$_[1]; } ["$file_out :\t".netwkString($comboNet->setvaltobad(-9999)), 1],
	[$file_msk,$flag_msk];

  print $separator if $runIO{subtract};
}

#######################################################################
#############   Job # 5 - Subtracting list of Networks   ##############

if ($runIO{subtract}) {
  print	"\n\tNetwork subtract job. Sub-networks to subtract:\n";
  die	"Empty or wrong input format for the file list to subtract. Aborting...\n\n"
		unless ref($runIO{subtract}{Net_files}) eq 'ARRAY' && scalar @{$runIO{subtract}{Net_files}};
  die	"File path for output Network must be given. Aborting...\n\n"	unless $runIO{subtract}{output_file};
  map print("$runIO{subtract}{Net_files}[$_]\n"), 0 .. $#{$runIO{subtract}{Net_files}};

		### Setting defaults for output flags
  my $flag_trim	= set_default($runIO{subtract}{trim_nodata},	1);

	### Read and check Networks to subtract
  my @extent;
  for (my $i=0; $i<scalar(@{$runIO{subtract}{Net_files}})+1; $i++) {
    my $file	= $i ? $runIO{subtract}{Net_files}[$i-1] : $file_ntwk;
    push   @extent, get_extent($file, $runIO{Net_proj});
    printf "   $file:\t%s\n", netwkString($extent[-1]{mask});
    if ($i) {						# Check cell size match and find subtracted extent
      die "\tMissmatch of cell size in the last file. Aborting...\n\n"
	if abs($extent[$i]{cellsizeX} - $extent[0]{cellsizeX}) > 0.0001 * $extent[0]{cellsizeX} ||
	   abs($extent[$i]{cellsizeY} - $extent[0]{cellsizeY}) > 0.0001 * $extent[0]{cellsizeY};

    my @half	= (0.5*$extent[0]{cellsizeX}, 0.5*$extent[0]{cellsizeY});
    my @box	= (colRow($extent[0],$extent[$i]{xllcorner}+$half[0],$extent[$i]{yllcorner}+$half[1]),
		   colRow($extent[0],$extent[$i]{xurcorner}-$half[0],$extent[$i]{yurcorner}-$half[1]));
    my @dim	= (0, $extent[0]{ncols}-1, 0, $extent[0]{nrows}-1);

			# Check subsetting extents first
    die "\tThe subtractinging region $i is outside of the Network domain. Aborting...\n\n"
    if  $box[0] > $dim[1] || $box[2] < $dim[0] || $box[3] > $dim[3] || $box[1] < $dim[2];
    if ($box[0] < $dim[0]) { $box[0] = $dim[0]; print "\tSubsetting region $i min Longitute is clipped.\n"; }
    if ($box[2] > $dim[1]) { $box[2] = $dim[1]; print "\tSubsetting region $i max Longitute is clipped.\n"; }
    if ($box[3] < $dim[2]) { $box[3] = $dim[2]; print "\tSubsetting region $i max Latitude  is clipped.\n"; }
    if ($box[1] > $dim[3]) { $box[1] = $dim[3]; print "\tSubsetting region $i min Latitude  is clipped.\n"; }

	### Subtract networks
    $extent[0]{mask}->($box[0]:$box[2],$box[3]:$box[1])->where($extent[$i]{mask}->isgood) .= -9999;
    $extent[0]{mask}->setvaltobad(-9999);
  }}
	### Save subtracted network
  my  $NETWORK	= delete $extent[0]{mask};
  my ($network,$extent) = trim($NETWORK, dclone($extent[0]), $flag_trim, 0);

  prepare_dir(	  $runIO{subtract}{output_file});
  my $file_out	= $runIO{subtract}{output_file};
 (my $file_msk	= $file_out)	=~ s/\.\w+$/_msk.asc/;	# Network mask file
  write_gridascii($file_out, $network,		$extent, {FORMAT => '%d'});
  write_gridascii($file_msk, $network->isgood,	$extent, {FORMAT => '%d'}) if $flag_msk;
  print  "\n\tSubtract job is Done!  Output files:\n";
  map {  print "   $$_[0]\n" if $$_[1]; } ["$file_out :\t".netwkString($network->setvaltobad(-9999)), 1],
	[$file_msk,$flag_msk];
}

#######################################################################
#############   Job # 6 - Copy basin attributes      ##################

if ($runIO{copy_attrib}) {
  print	"\n\tCopy basin ID attributes job:\n";

  my $file_out	= $runIO{copy_attrib}{output_file} ? $runIO{copy_attrib}{output_file} : $runIO{copy_attrib}{basin_ID};
     $file_out	=~ s/\.\w+$/_attr.csv/        unless $runIO{copy_attrib}{output_file};
  prepare_dir($file_out);

	### Read data and set output file header
  print "   Reading the input data: ";
  my $extent	= get_extent($runIO{copy_attrib}{basin_ID}, $runIO{Net_proj});
  my $cell_area	= cell_area(lonLat($extent),$extent);
  my $basinID	= $$extent{mask};
  my $up_area	= read_raster($runIO{copy_attrib}{basin_upArea});
		### Grids
  my $basin_ID	= read_GDAL($extent, {'Projection'=>$runIO{Net_proj}}, 0, $runIO{copy_attrib}{src_basin_ID}, 1, -9999);
  my $sea_ID	= read_GDAL($extent, {'Projection'=>$runIO{Net_proj}}, 0, $runIO{copy_attrib}{seabasin_ID},  1, -9999);
  my $cont_ID	= read_GDAL($extent, {'Projection'=>$runIO{Net_proj}}, 0, $runIO{copy_attrib}{subcont_ID},   1, -9999);
		### Attributes
  my @attrib;
  my %basinAttr	= read_table_hash($runIO{copy_attrib}{src_basin_attr},	'ID');
  my %seaAttr	= read_table_hash($runIO{copy_attrib}{seabasin_attr},	'SeaCode');
  my %contAttr	= read_table_hash($runIO{copy_attrib}{subcont_attr},	'SubContinentCode');
  print "Done\n";

	### Matching basins by the overlap method
  my @basin_list= $basinID->uniq->list;
  foreach my $ID (@basin_list) {
    last if $ID > $runIO{copy_attrib}{max_n_basins};
    printf "\r   Processing basin ID: $ID of %d", scalar(@basin_list);

    my $id	= 'N/A';
    my $name	= 'N/A';
    my $idx	= whichND($basinID == $ID);
    my $idx_mth	= $up_area  ->indexND($idx)->maximum_ind;	# Index of river mouth pixel
    my $area	= $cell_area->indexND($idx)->sum;		
    my $seaID	= $sea_ID   ->indexND($idx)->index($idx_mth);
    my $contID	= $cont_ID  ->indexND($idx)->index($idx_mth);
		### Check minimum area
    if($area   >= $runIO{copy_attrib}{min_check_area}) {
		### Check overlap area
      my $b_ID	= $basin_ID->indexND($idx)->mode;
     ($id,$name)= ($b_ID, $basinAttr{$b_ID}{Name})
	if (1 - abs($cell_area->where($basin_ID == $b_ID)->sum - $area)/$area >= $runIO{copy_attrib}{min_overlap});
    }		### Put together this basin attributes
    push @attrib, [ map(defined($_)?$_:'N/A', $ID, $name, sprintf('%.2f',$area), $id,  $seaID, $seaAttr{$seaID}{SeaName},
	 $seaAttr{$seaID}{OceanCode},$seaAttr{$seaID}{OceanName}, $contID, $contAttr{$contID}{SubContinentName}) ];
  }
	### Save output
  print "\n   Saving basin attributes to a file\n";
  my @header	= qw(ID Name Area src_ID SeaCode SeaName OceanCode OceanName SubContinentCode SubContinentName);
  write_csv($file_out, \@header, \@attrib);
  print  "\n\tBasin attributes job is Done!  Output file:\n   $file_out\n";
}


#######################################################################
#############   Job # 7 - Merge endorheic basins    ###################

if ($runIO{merge_endorheic}) {
  print	"\n\tMerge endorheic basins job:\n";

	### Set defaults
  my $method	= set_default($runIO{merge_endorheic}{search_method},	0);
  my $max_dist	= set_default($runIO{merge_endorheic}{max_distance},	1e6);	# No limit
  my $max_delta	= set_default($runIO{merge_endorheic}{max_elev_delta},	1e6);	# No limit
  my $max_area	= set_default($runIO{merge_endorheic}{max_area},	1e9);	# No limit
  my $mthd_out	= set_default($runIO{merge_endorheic}{dir_out_method},	0);
  my @max_str	= (($max_dist==1e6?'NoLimit':$max_dist), ($max_delta==1e6?'NoLimit':$max_delta));
  print  "   Max search distance = $max_str[0] cells\n";
  print  "   Max elevation delta = $max_str[1] m\n";
  printf "   Breakout cell search by - %s method\n", ($method   ? 'Min Elevation'     : 'Min Distance');
  printf "   Breakout direction   by - %s method\n", ($mthd_out ? 'Max Upstream Area' : 'Min Elevation');

	### Read data
  print "   Reading the input data: ";
  my $extent	= get_extent($runIO{Net_file}, $runIO{Net_proj});
  my $flowDir	= $$extent{mask};
  my $cell_area	= cell_area(lonLat($extent),$extent); 
  my $basinID	= read_raster($runIO{merge_endorheic}{basin_ID_file});
  my $endorhID	= read_raster($runIO{merge_endorheic}{endorh_ID_file});	# Endorheic basin IDs only in this file
  my $up_area	=-read_raster($runIO{merge_endorheic}{uparea_file});	# Must negate to use min method
  my $stnElev	= read_raster($runIO{merge_endorheic}{elev_file});
  my $subElev	= $mthd_out ? $up_area : $stnElev;
  print "Done...\n";

	### Initializations
  my $dir	= byte([32,64,128],[16,0,1],[8,4,2]);
  my @direction	= ([0,1,1,0,-1,-1,-1,0,1],[0,0,1,1,1,0,-1,-1,-1]);
  my %log2	= (0=>0,1=>1,2=>2,4=>3,8=>4,16=>5,32=>6,64=>7,128=>8);		# Flow to values
  my %reverse	= (0=>0,1=>16,2=>32,4=>64,8=>128,16=>1,32=>2,64=>4,128=>8);	# Reverse flow directions

	### Process each endorheic basins
  my $maskBig	= zeroes(long, $$extent{ncols}, $$extent{nrows});
  my @basins_in	= $endorhID->uniq->list;
  my @basins_out;
  my($count,$N)	= (0, scalar(@basins_in));
  my @result	=((0) x 5);
# my @colRow=colRow($extent,0,70);		# Ad hoc filtering of basins above 70 deg North
# open (FILE, ">endorheic_basin_area.csv") or die "Couldn't open  endorheic_basin_area.csv file, $!";
# print FILE "Basin_ID\tLon\tLat\tArea\n";
  foreach my $ID (reverse @basins_in) {
    push @basins_out, $ID;
    printf "\r   Processing basin ID: $ID - %d of %d".(' 'x10), ++$count, $N;
		### Clip out the basin and find min distance from it mouth to outside
    $maskBig   .= ($basinID == $ID)->setbadtoval(0);
    my $idx	= whichND($maskBig);
    die "\n\nBasin ID $ID does not exists in the basin ID mask. Aborting...\n\n" unless $idx->dim(1);
    my($xmin,$xmax,$ymin,$ymax) = (
		pdl(short,$idx((0),)->min-1,$idx((0),)->max+1)->clip(0,$$extent{ncols}-1)->list,
		pdl(short,$idx((1),)->min-1,$idx((1),)->max+1)->clip(0,$$extent{nrows}-1)->list);
# next if $ymax > $colRow[1];
    my $basin	= $maskBig($xmin:$xmax, $ymin:$ymax);
    my $flow	= $flowDir($xmin:$xmax, $ymin:$ymax);
    my $elev	= $stnElev($xmin:$xmax, $ymin:$ymax);
    my $bElev	= $subElev($xmin:$xmax, $ymin:$ymax);	# Outside boundary metric for breakout direction
    my $upArea	= $up_area($xmin:$xmax, $ymin:$ymax);
    my @mouth	=(whichND($basin & ($flow < 1)))->list;
    my $area	=-$upArea->at(@mouth);
# printf FILE "$ID\t%f\t%f\t$area\n",Geo::GDAL::ApplyGeoTransform($$extent{gTransform},$mouth[0]+$xmin,$mouth[1]+$ymin); next;
    my $distance= $basin->rvals({Center => \@mouth});
    my $dist	= $distance->where(!$basin)->min - 1;
    if($area	> $max_area) { $result[1]++; next; }	# Skip: exceeds max area
    if($dist	> $max_dist) { $result[2]++; next; }	# Skip: exceeds max distance
    if($#mouth	< 0	   ) { $result[3]++; next; }	# Skip: no mouth point (wrap around problem)

		### Find basin boundaries
    my $conv	= $basin->conv2d(ones(3,3),{Boundary => 'Truncate'});
       $basin->where(($conv > 0) & ($conv < 9) &  $basin) .= 2;	# Inner boundary has mask values = 2
       $basin->where(($conv > 0) & ($conv < 9) & !$basin) .= 3;	# Outer boundary has mask values = 3
    my($bIndx, $idxRim, $dirOut, $val) = (undef, [0,0], -2, 1e6);

		### Find breakout cell(s) - Chose one in the next step, e.g. lowest elev outside
    if ($method == 0) {		### Shortest distance method
      $bIndx	= whichND(($distance == $dist) & ($basin == 2));
    }
    elsif ($method == 1) {	### Lowerst elevation within max distance method
      my $minEl	= $elev->where(($distance <= $dist ) & ($basin == 2))->min;
	 $bIndx	=      whichND(($elev     == $minEl) & ($basin == 2));
    } else { die "\n\nNot coded for other methods yet. Aborting...\n\n"; }

		### Find breakout direction
    foreach my $i (0 .. $bIndx->dim(2)-1) {
      my($xmin,$xmax,$ymin,$ymax) = (		# Do not use PDL range function since region can be not square(!)
		pdl(short,$bIndx(0,$i)-1, $bIndx(0,$i)+1)->clip(0,$basin->dim(0)-1)->list,
		pdl(short,$bIndx(1,$i)-1, $bIndx(1,$i)+1)->clip(0,$basin->dim(1)-1)->list);
      my $basin_clip	= $basin($xmin:$xmax, $ymin:$ymax);
      my $elev_clip 	= $bElev($xmin:$xmax, $ymin:$ymax)->setbadtoval(9999)->setbadif($basin_clip != 3); # 9999 for coast
      my @indx		= whichND($elev_clip == $elev_clip->min)->(,0)->list;
      my $elev_val	= $elev_clip->at(@indx);
	($idxRim, $dirOut, $val) = ([$bIndx(,($i))->list], $dir->at(@indx), $elev_val)	if $elev_val < $val;
    }
    $dirOut = 0 if $val == 9999;	# Case of endorheic basin on the ocean coast
    die "This should not happen. Aborting...\n\n" if $dirOut == -2;
    if ($dist && ($elev->at(@$idxRim)-$elev->at(@mouth)) > $max_delta) { $result[4]++; next; } # Skip: exceeds max elev delta

		### Set recipient basin ID to the removed basin cells
    my @indexTo	= ($xmin+$$idxRim[0]+$direction[0][$log2{$dirOut}],	# Recipient cell location in the full grid
		   $ymin+$$idxRim[1]+$direction[1][$log2{$dirOut}]);
    $basinID->indexND($idx) .= $basinID->at(@indexTo)	if $basinID(@indexTo)->isgood;	# Basin     ID layer

		### Reverse flow direction between endorheic mouth and the breakout point
    my $indDown	=     dclone( $idxRim);
    my $dirDown	= $flow->at(@$idxRim);
       $flow(@$idxRim) .= $dirOut;				# Set the flow direction at the breakout cell
    while ($dirDown > 0) {					# NB- Endorheic mouth can be 0 or -1
      my $dirUp	= $reverse{$dirDown};
      $indDown	=[$$indDown[0]+$direction[0][$log2{$dirDown}],	# Updating location of next cell down
		  $$indDown[1]+$direction[1][$log2{$dirDown}]];
      $dirDown	= $flow->at(@$indDown);				# Updaing direction at next cell down
      $flow(@$indDown) .= $dirUp;				# Reversing the flow direction from next cell
    }
    pop @basins_out;	$result[0]++;
  }
# close FILE; die "\nFile = endorheic_basin_area.csv";
	### Build new endorheic basin mask
  print "\n   Building new endorheic basin ID mask...";
  $endorhID	= $basinID->basins_from_list(pdl(long, @basins_out))->setvaltobad(0);
  $up_area	= byte($flowDir)->upstrAccumAll($cell_area);

	### Save resulted network
  prepare_dir(	  $runIO{merge_endorheic}{output_file});
  my $file_out	= $runIO{merge_endorheic}{output_file};
 (my $file_upAr	= $file_out)	=~ s/\.\w+$/_upstrArea.asc/;
 (my $file_IDs	= $file_out)	=~ s/\.\w+$/_IDs.asc/;
 (my $file_EnR	= $file_out)	=~ s/\.\w+$/_EnR.asc/;
  write_gridascii($file_out, $flowDir->short,	$extent, {FORMAT => '%d'  });
  write_gridascii($file_upAr,$up_area,		$extent, {FORMAT => '%.1f'});
  write_gridascii($file_IDs, $basinID,		$extent, {FORMAT => '%d'  });
  write_gridascii($file_EnR, $endorhID,		$extent, {FORMAT => '%d'  });
  print  "\n\tMerge endorheic basins job is Done!\n";
  printf "   Merged %d endorheic basins. Skipped (area/dist/wrap/elev): (%d/%d/%d/%d)\n", @result;
  printf "   Result summary:   %.2f %% of the Network cells are %d endorheic basins.\n",
		100*($endorhID->ngood/$basinID->ngood), $endorhID->uniq->dim(0);
  print  "   Output files (Note- Basin IDs are retained):\n";
  map {  print "     $$_[0]\n" if $$_[1]; } [$file_out, 1], [$file_upAr, 1], [$file_IDs, 1], [$file_EnR, 1];
}

#######################################################################
#############   Job # 8 - Upstream area match       ###################

if ($runIO{upstream_match}) {
  print	"\n\tUpstream area match job:\n";

	### Set defaults
  my $units	= set_default($runIO{upstream_match}{search_units},	'km');
  my $c_round	= set_default($runIO{upstream_match}{coord_round},	'%.2f');
  my $method	= set_default($runIO{upstream_match}{search_method},	1);
  die "Search units can only be 'km' or 'pix'. Aborting...\n\n" if $units !~ m/^km|pix$/i;

	### Read data
  print "   Reading the input data: ";
  my ($hdr,@data) = read_table($runIO{upstream_match}{sites_file});
  my  @ID	  = map $$_[$$hdr{$runIO{upstream_match}{header_ID}}],  @data;
  my  @lon	  = map $$_[$$hdr{$runIO{upstream_match}{header_lon}}], @data;
  my  @lat	  = map $$_[$$hdr{$runIO{upstream_match}{header_lat}}], @data;
  my  @area	  = map $$_[$$hdr{$runIO{upstream_match}{header_area}}],@data;
  my  @header	  = join "\t", qw(ID Lon Lat Obs_area Snap_lon Snap_lat Snap_area Snap_dist);
  print $#data," sites\n";

		### Calculate upstream area
  print((-e $file_upArea?'   Reading':'   Calculating')," upstream area...\n");
  my @time	= (Benchmark->new);
  my $extent	= get_extent($file_ntwk, $runIO{Net_proj});
  my $flowDir	= $$extent{mask};
  my $cell_area	= cell_area(lonLat($extent),$extent); 
  my $up_area	= unpdl_data((-e $file_upArea ? read_raster($file_upArea) : byte($flowDir)->upstrAccumAll($cell_area))
		   ->setbadtoval(-9999));
  my $distance	= $units=~m/pix/i  ? $runIO{upstream_match}{search_distance} * ones($$extent{ncols}, $$extent{nrows}) :
			       floor($runIO{upstream_match}{search_distance} / sqrt($cell_area))->long;
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

	### Searching
  printf "   Searching for best match by %s method : ", $method ? 'Log' : 'Linear';
  my @colRow	= map [colRow($extent,$lon[$_],$lat[$_])], 0 .. $#data;
  my @search	= map [search_3x3($up_area, $colRow[$_], $area[$_], $distance->at(@{$colRow[$_]}),$method)], 0 .. $#data;
  print "Done\n";
  push @time, Benchmark->new;
  printf "\t%s\n",timestr(timediff($time[-1], $time[-2]));

	### Save resulted data
  my @result	= map [ $ID[$_], $lon[$_], $lat[$_], $area[$_],
    map(sprintf($c_round,$_+0.5*$$extent{cellsize}), Geo::GDAL::ApplyGeoTransform($$extent{gTransform},@{$search[$_][0]})),
	sprintf("%.2f", $search[$_][1]),
	sprintf("%.2f", sqrt(($colRow[$_][0]-$search[$_][0][0])**2 + ($colRow[$_][1]-$search[$_][0][1])**2))], 0 .. $#data;
  write_csv($runIO{upstream_match}{output_file}, \@header, \@result);
  print  "\n\tUpstream area match job is Done!  Output file:\n   $runIO{upstream_match}{output_file}\n";
}

#######################################################################
#######################################################################
						# Report Total Time
printf "\nTime used - %d hours, %d minutes, and %d seconds\n\n",
	time_used($time_start,time());
exit;

#######################################################################
######################  Functions  ####################################

sub preclip_rectangle
{
  my ($file_in, $file_out, $runIO) = @_;
  die "Wrong input format for the preclip option. Aborting...\n\n" unless ref($$runIO{preclip}{rectangle}) eq 'HASH' &&
	defined $$runIO{preclip}{rectangle}{lonMin} && defined $$runIO{preclip}{rectangle}{lonMax} &&
	defined $$runIO{preclip}{rectangle}{latMin} && defined $$runIO{preclip}{rectangle}{latMax};

  my $extent	= get_extent($file_in, $$runIO{Net_proj});
  my @box	=(colRow($extent, $$runIO{preclip}{rectangle}{lonMin}, $$runIO{preclip}{rectangle}{latMin}),
		  colRow($extent, $$runIO{preclip}{rectangle}{lonMax}, $$runIO{preclip}{rectangle}{latMax}));
  my @dim	=(0, $$extent{ncols}-1, 0, $$extent{nrows}-1);

			### Check subsetting extents
  die "The subsetting regions is outside of the Network domain. Aborting...\n\n"
  if  $box[0] > $dim[1] || $box[2] < $dim[0] || $box[3] > $dim[3] || $box[1] < $dim[2];
  if ($box[0] < $dim[0]) { $box[0] = $dim[0]; print "\tPreclipping region min Longitute is clipped.\n"; }
  if ($box[2] > $dim[1]) { $box[2] = $dim[1]; print "\tPreclipping region min Longitute is clipped.\n"; }
  if ($box[3] < $dim[2]) { $box[3] = $dim[2]; print "\tPreclipping region max Latitude  is clipped.\n"; }
  if ($box[1] > $dim[3]) { $box[2] = $dim[1]; print "\tPreclipping region max Latitude  is clipped.\n"; }

			### Perform clipping using "gdal_translate"
  my $preclip	= sprintf "$$runIO{gdal_transl} -of AAIGrid -srcwin $box[0] $box[3] %d %d $file_in $file_out",
	$box[2]-$box[0], $box[1]-$box[3];
  my $junk = `$preclip`;
  die "Failed to preclip. Aborting...\n\n" unless -e $file_out;

  return $file_out;
}

#######################################################################

sub preclip_polygon
{
  my ($file_in, $file_out, $runIO) = @_;
  die "Wrong input format for the preclip polygon option. Aborting...\n\n"
	unless ref( $$runIO{preclip}{polygon} ) eq 'HASH';
  die "Mask file for preclip polygon does not exists. Aborting...\n\n"
	unless -e $$runIO{preclip}{polygon}{file};
  die "Missing or wrong input format for preclip polygon ID- it must be numeric. Aborting...\n\n"
	unless ref( $$runIO{preclip}{polygon}{polIDs} ) eq 'ARRAY';
  die "Shape file in preclip_pyligon requires key for a masking variable name. Aborting...\n\n"
	if $$runIO{preclip}{polygon}{file} =~ m/\.shp$/i && !$$runIO{preclip}{polygon}{shp_var};

  my  $proj	= $$runIO{preclip}{polygon}{proj} ? $$runIO{preclip}{polygon}{proj} : 'epsg:4326';
  my  $trim	= $$runIO{preclip}{trim_nodata}   ? $$runIO{preclip}{trim_nodata}   : 1;

		### Clip Network by polygon
  my  $extent	= get_extent($file_in, $$runIO{Net_proj});
  my ($var, $processing) = $$runIO{preclip}{polygon}{file} =~ m/\.shp$/i ?
	($$runIO{preclip}{polygon}{shp_var}, 'polygon') : ('', '');

  my  $mask	= read_GDAL($extent, {'Var_Name'=>$var, 'Processing'=>$processing, 'Projection'=>$proj},
	0, $$runIO{preclip}{polygon}{file}, 1, -9999)->in($$runIO{preclip}{polygon}{polIDs});
  my  $network	= condition($mask, $$extent{mask}, -9999)->setvaltobad(-9999);
  die "Failed to do preclip by polygon- No valid pixels.  Aborting...\n\n" unless $network->ngood;

		### Trim clipped Network
  delete $$extent{mask};	# clone does not work with PDL objects
  my ($poligonNet,$extNet) = trim($network, dclone($extent), $trim, 0);

		### Save clipped Network
  write_gridascii($file_out, $poligonNet, $extNet, {FORMAT => '%d'});
  die "Failed to do preclip by polygon. Aborting...\n\n" unless -e $file_out;

  return $file_out;
}

#######################################################################

sub inscribe_upstream
{
  my ($msk, $network, $basinID, $upArea, $ext, $option) = @_;
	### Check $option input
  $$option{spillOver}	= set_default($$option{spillOver},	0  );
  $$option{minUpsteam}	= set_default($$option{minUpsteam},	0  );
  $$option{minTributary}= set_default($$option{minTributary},	1e6);
  $$option{continuous}	= set_default($$option{continuous},	0  );	my $conStr = $$option{continuous}?'Yes':'No';

  print  "  Inscribe by upstream options:\n    spillOver = $$option{spillOver}; continuous = $conStr";
  printf "; minUpsteam = %.1f; minTributary = %.0f",	$$option{minUpsteam}, $$option{minTributary}
	unless $$option{spillOver};			print  "\n\n";

	### Initialization- Prepare basic variables
  my $msk_bnd	= zeroes($network->dims);	# Mask boundary
 map $msk_bnd+=$_,mask_boundary($msk);		# sub "mask_boundary" returns array
     $msk_bnd->where($msk & ($network == 0)) .= 1;	# Add endorheic mouth points
  my $ID_in	= find_IDs( $msk, $basinID, 1);
  my $mask	= $basinID->in($ID_in);

  my @direction	= ([0,1,1,0,-1,-1,-1,0,1],[0,0,1,1,1,0,-1,-1,-1]);
  my %log2	= (0=>0,1=>1,2=>2,4=>3,8=>4,16=>5,32=>6,64=>7,128=>8);	# Flow to values

	### Go around polygon mask boundary and find upsteam sections inside the polygon
  my $bnd_idx	= whichND($msk_bnd > 0);	# Must make $bnd_idx otherwhise it will cause whichND list warning
  my @msk_bnd	= unpdl_data( $bnd_idx );
  foreach my $rec (@msk_bnd) {
    if ($mask(@$rec) == 0) {
      my @colRow_to  = ($$rec[0]+$direction[0][$log2{$network->at(@$rec)}],
			$$rec[1]+$direction[1][$log2{$network->at(@$rec)}]);
		### Check upsteam section, if flow at the boundary goes outside of the polygon
      if ($mask(@colRow_to) == 0 && $upArea->at(@$rec) >= $$option{minUpsteam}) {

		#######################
		### Spillover case
	if ($$option{spillOver}) {
	    my $upstr	= $network->upstreamMask(@$rec);
	    my $sumMsk	= $msk->where($upstr)->sum;
	    my $sumUps	= $upstr->sum;

	    $mask->where($upstr) .= 1	if abs($sumUps - $sumMsk)/($sumUps + $sumMsk) < $$option{spillOver};
	}
		#######################
	else {	### No spillover case
	  my @stack	= ($rec);
	  while (@stack) {
	    my @pix	= @{shift(@stack)};
	    my $inside	= 1;
	    my $upstr	= $network->upstreamMask(@pix);
	    my $sumMsk	= $msk->where($upstr)->sum;
	    my $sumUps	= $upstr->sum;

	    while ($inside && $sumUps != $sumMsk) {
			# Find next upstream pixel of the river main stem
	      my $area	= 0; my (@PIX, @pixStack);
	      foreach my $i (1 .. 8) {
		my @colRow_from	= ($pix[0]-$direction[0][$i],
				   $pix[1]-$direction[1][$i]);
		next unless $network(@colRow_from)->isgood;
		next unless $log2{$network->at(@colRow_from)} == $i;
			# Processing of contributing upstream pixel(s)
		my  $pixArea = $upArea->at(@colRow_from);
		push @pixStack,\@colRow_from if $pixArea >= $$option{minTributary};	# Route upsteam later

		if ($pixArea > $area) {
		  $area	 = $pixArea;
		  @PIX	 = @colRow_from;
		  $inside= $area >= $$option{minUpsteam} ? $msk->at(@PIX) : 0;
	      } }
			# Filter pixel stack from the next main-stem upstream pixel
	      if (scalar(@pixStack) > 1) { foreach my $pix (@pixStack) {
		push @stack,$pix unless $$pix[0] == $PIX[0] && $$pix[1] == $PIX[1];
	      } }

	      $inside = 0 unless @PIX;		# Case of pixel with no upsteam
	      if ($inside) {
		@pix    = @PIX if @PIX;
		$upstr  = $network->upstreamMask(@pix);
		$sumMsk = $msk->where($upstr)->sum;
		$sumUps = $upstr->sum;
	    } }
	    $mask->where($upstr) .= 1 if $inside;
  } } } } }
  return undef unless $mask->sum;	# Check that the mask is not empty

	### Select largest continuous area in the subset
  if ($$option{continuous}) {
    my  $segment	= $mask->setbadtoval(0)->cc4compt;	# Connected 4-component labeling (segmentation)
    my ($maxSeg, $N, @segID)	= (0, 0, $segment->where($segment > 0)->uniq->list);
    if (scalar(@segID) > 1) {
		# Find index of the largest segment
      foreach my $id (@segID) {
	my $sum	=  ($segment == $id)->sum;
	($maxSeg, $N)	= ($id, $sum) if $sum > $N;
      }
		# Check that there is no flow between segments (can be a diagonal flow)
		#	by comparing original and derived upstream areas
      my $flowDir = condition($mask, $network, -9999)->setvaltobad(-9999);
      my $U_Area  = condition($mask, $upArea,  -9999)->setvaltobad(-9999);		  # original upsteam area
      my $u_Area  = byte($flowDir)->upstrAccumAll(cell_area(lonLat($ext),$ext,$area_opt));# derived  upsteam area
      die "  The segmentation for the \"contineous\" option resulted in error. Aborting...\n\n"
	if abs($U_Area - $u_Area)->where($segment == $maxSeg)->sum > 1e-6;

      $mask *= $segment == $maxSeg;
  } }
	### Finilize using calculated network mask
  my $basinNet	= condition($mask, $network, -9999)->setvaltobad(-9999);
  my $bsnIDNet	= condition($mask, $basinID, -9999)->setvaltobad(-9999);

  return $mask, $basinNet, $bsnIDNet;
}

#######################################################################

sub find_IDs
{
  my ($mask, $basins, $flag) = @_;

  my  $IDs	= condition($mask, $basins, -9999)->setvaltobad(-9999)->uniq;
  my  $IDs_out	= condition($mask, -9999, $basins)->setvaltobad(-9999)->uniq;
      $IDs	= setops($IDs,'XOR',setops($IDs,'AND',$IDs_out)) if $flag;

  return $IDs;
}

#######################################################################

sub search_3x3		### TODO- rewrite it in PDL
			### Taken from "upstream_masks.pl" script
{
  my ($DATA, $pix, $match, $n_pix, $method) = @_;
  my  $isNumber	= isNumber($match);
      $method	= $method && $isNumber;
      $match	= log(List::Util::max($match,1e-6)) if $method;

  		### Convert PDL object to Perl array
  my $data	= ref($DATA) eq 'PDL' ? unpdl_data($DATA->setbadtoval(-9999)) : $DATA;
  return $pix, $$data[$$pix[1]][$$pix[0]] unless $n_pix;

		### Check if the pixel is out of the data bounds
  my ($nCols,$nRows) = (scalar(@{$$data[0]}), scalar(@$data));
  return ([0,0],-9999) if $$pix[1] < 0 || $$pix[1] >= $nRows || $$pix[0] < 0 || $$pix[0] >= $nCols;

  my ($bestMatch,$bestCol,$bestRow) = ($$data[$$pix[1]][$$pix[0]], @$pix);
      $bestMatch = log(List::Util::max($bestMatch,1e-6)) if $method;

  for (my $row=$$pix[1]-$n_pix; $row<=$$pix[1]+$n_pix; $row++) {
    for (my $col=$$pix[0]-$n_pix; $col<=$$pix[0]+$n_pix; $col++) {
      next if $row<0 || $row>=$nRows || $col<0 || $col>=$nCols || $$data[$row][$col]==-9999;

      if ($isNumber) {	# Search by match
	my $pix_data = $method ? log(List::Util::max($$data[$row][$col],1e-6)) : $$data[$row][$col];
	($bestMatch,$bestCol,$bestRow) = ($pix_data, $col, $row)
	  if abs($pix_data-$match) < abs($bestMatch-$match);
      }
      else {		# Search for max area
	($bestMatch,$bestCol,$bestRow) = ($$data[$row][$col], $col, $row)
	  if $$data[$row][$col] > $bestMatch;
      }
    }
  }
  $bestMatch = exp($bestMatch) if $method;

  return [$bestCol,$bestRow],$bestMatch;
}

#######################################################################

sub trim
{
  my($net,$ext,$trm,$pcl) = @_;

  if ($trm || $pcl) {
    my @directon;
    my @dir	= qw(North South West East);
    my @trim	= (0,0,0,0);		### (top, bottom, left, right)
    my @check	= ($net->sumover, $net(:,-1:0)->sumover,
		   $net->transpose->sumover, $net(-1:0,:)->transpose->sumover);

		### Check pre-clipped network
    if ($pcl) {
      foreach my $i (0..3) { push(@directon, $dir[$i]) unless $check[$i]->(0)->isbad; }
      die sprintf("\nPre-clipping has truncated the subset Network at the (%s) boundar%s. Aborting...\n\n",
	join(',',@directon), (scalar(@directon)>1?'ies':'y')) if @directon;
    }
		### Trim
    if ($trm) {
      foreach my $i (0..3) { $trim[$i]++ while ($check[$i]->($trim[$i])->isbad) }
      $$ext{ncols}    -= $trim[2]+$trim[3];
      $$ext{nrows}    -= $trim[0]+$trim[1];
      $$ext{xllcorner}+= $trim[2]*$$ext{cellsize};
      $$ext{yllcorner}+= $trim[1]*$$ext{cellsize};

      $net	= $net($trim[2]:-$trim[3]-1,$trim[0]:-$trim[1]-1);
    }
  }

  return $net, $ext;
}

#######################################################################

sub endorheic_basins
{
  my ($basinID, $flowDir, $buffer, $segment) = @_;
  my  $basinInt	= zeroes($basinID->dims)->copybad($flowDir);
  my  $segmMask	= zeroes($basinID->dims)->copybad($flowDir);

	### Coastal segmentaiton mask of outlets
  if ($segment) {
    $segmMask  .= $flowDir < 1;				# This is mask of river outlets (mouths)
    my $rim	= $segmMask->isgood->conv2d(ones(3,3),{Boundary => 'Truncate'})->copybad($segmMask) < 9;
    my $segment	= $segmMask->setbadtoval(0)->cc8compt;	# Connected 8-component labeling (segmentation)
    my $rimPnts	= whichND($rim);			# Must make $rimPnts otherwhise it will cause whichND list warning
    $segmMask  .= $segment ->copybad($flowDir)->mask_union($rimPnts); # WARNING: ccNcompt does not handle bad values
  }
	### Distance to coast filtering
  my $mouthPnts	= whichND(($flowDir < 1) & (!$segmMask));# Must make $mouthPnts otherwhise it will cause whichND list warning
  $basinInt	= $basinID->mask_endorheic($buffer, $mouthPnts)->setvaltobad(0);

  return $basinInt;
}

#######################################################################

sub info_EnR
{
  my ($extent, $basinEnR, $flowDir, $up_area)	= @_;
  my  @list_EnR;

  my @basins_in	= $basinEnR->uniq->list;
  my($count,$N)	= (0, scalar(@basins_in));
  foreach my $id (@basins_in) {
    printf "\r\tProcessing basin ID: $id - %d of %d".(' 'x10), ++$count, $N;
    my $mask	=  ($basinEnR == $id);
    my $idx	= whichND($mask);
    my($xmin,$xmax,$ymin,$ymax) = (
	pdl(short,$idx((0),)->min,$idx((0),)->max)->list,
	pdl(short,$idx((1),)->min,$idx((1),)->max)->list);
    my $basin	= $mask(   $xmin:$xmax, $ymin:$ymax);
    my $flow	= $flowDir($xmin:$xmax, $ymin:$ymax);
    my @mouth	=(whichND( $basin & ($flow < 1)))->list;
    my @colRow	=($mouth[0]+$xmin, $mouth[1]+$ymin);
    my @lonLat	= map sprintf("%.4f",$_), Geo::GDAL::ApplyGeoTransform($$extent{gTransform}, $colRow[0]+0.5, $colRow[1]+0.5);
    push @list_EnR, [$id, @lonLat, @colRow, $basin->sum, sprintf("%.2f", $up_area->at(@colRow))];
  }
  print "\n";
  return \@list_EnR;
}

#######################################################################

sub write_CT_csv
{
  my ($file, $header, $extent, $area, $cell_pdl, $strmOrder) = @_;
  my ($count,$size) = (0,$area->dim(0));
  print "\r\tSaving cell table: ";

  open (FILE,">$file") or die "Couldn't open $file, $!";
    print FILE join("\t",@$header),"\n";	foreach my $row (0 .. $size-1) {
    print FILE join("\t",
	$cell_pdl(,$row)->list, $strmOrder->at($cell_pdl(3:4,$row)->list),
	Geo::GDAL::ApplyGeoTransform($$extent{gTransform},($cell_pdl(3:4,$row)+0.5)->list), $area->at($row)),"\n"; }
    print "\r\tSaving cell table: row $count of $size\n" unless $count++ %1000;
  close FILE;
  print "\r\tSaving cell table: row $size of $size\n";
}

#######################################################################

sub write_shape
{
  my ($file, $header, $extent, $area, $cell_pdl, $strmOrder,$basinID) = @_;
  my ($count,$size,   $cName) = (0,$area->dim(0), basename($file));
  print "\r   Saving Network ESRI shape files: ";

  open (FILE,">$file.geojson") or die "Couldn't open $file.geojson, $!";
    print FILE <<EOF;		### Header
{
"type": "FeatureCollection",
"name": "$cName",
"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },
"features": [
EOF
    foreach my $row (reverse(0 .. $size-1)) {
      my @colRow = $cell_pdl(3:4,$row)->list;
die sprintf("(%d,%d)\n%s\n%s\n%s\n%s\n",@colRow,$basinID->at(@colRow),$strmOrder->at(@colRow),$cell_pdl->at(0,$row),$area->at($row)) if $basinID->at(@colRow) eq 'BAD' || $strmOrder->at(@colRow) eq 'BAD' || $cell_pdl->at(0,$row) eq 'BAD' || $area->at($row) eq 'BAD';
      printf FILE "{ \"type\": \"Feature\", \"properties\": { \"NAME\": \"Linestring\", \"ID\": %d, \"BASINID\": %d, \"STORDER\": %d, \"FLOWDIR\": %d, \"UPAREA\": %s }, \"geometry\": { \"type\": \"LineString\", \"coordinates\": [ [ %s, %s ], [ %s, %s ] ] } }%s",
	++$count, $basinID->at(@colRow), $strmOrder->at(@colRow), $cell_pdl->at(0,$row), $area->at($row),
    Geo::GDAL::ApplyGeoTransform($$extent{gTransform},($cell_pdl(3:4,$row)+0.5)->list),
    Geo::GDAL::ApplyGeoTransform($$extent{gTransform},($cell_pdl(1:2,$row)+0.5)->list), ($count==$size?"\n]\n}\n":",\n");
      print "\r   Saving GeoJSON: row $count of $size" unless $count % 10000;
    }
  close FILE;
  print "\r   Saving Network GeoJSON: row $size of $size - Done!\n";
  print   "   Saving Network ESRI Shape File...";
    system "ogr2ogr -f 'ESRI Shapefile' $file.shp $file.geojson";
  print " Done!\n";
  print   "   Saving Network KML File...";
    system "ogr2ogr -f 'KML' $file.kml $file.geojson";
  print " Done!\n";
}

#######################################################################

sub netwkString
{
  my  $network	= shift();
  my ($nCells, $nElem)	= ($network->ngood, $network->nelem);
  return sprintf "%d network cells, %.1f %% in (%dx%d) layer", $nCells, 100*$nCells/$nElem, $network->dims;
}

#######################################################################

sub usage
{
  my $app_name = basename($0);
  print <<EOF;

Usage:
	$app_name [-h] [-v] INIT_FILE

This code generates Cell Table and/or Network subset with rules/options given in the init file.

Options:

h	Display this help.
v	Verbose mode.

EOF
  exit;
}

#######################################################################
###################  PDL::PP Functions  ###############################

__DATA__
__Pdlpp__

#######################################################################

pp_addhdr('
  #include <unistd.h>       /* we need defs of XXXX */
  #include <stdio.h>

  static void upstr(PDL_Byte * flowDir, PDL_Long * stack, PDL_Long n_size, PDL_Long m_size, PDL_Long N, PDL_Long M)
  {
    long from[2][8] = { {1,1,0,-1,-1,-1,0,1} , {0,1,1,1,0,-1,-1,-1} };
    long dir, ind, xx, yy;
    long NN = N;	long count = 1;
    long MM = M;	long pos   = 0;
    stack[0] = N + M*n_size;

    while (pos < count) {
      MM = stack[pos] / n_size;
      NN = stack[pos] - n_size*MM;
      pos++;
      for (dir=0; dir<8; dir++) {
        xx  = NN - from[0][dir];
        yy  = MM - from[1][dir];
        if (xx<0 || yy<0 || xx==n_size || yy==m_size) continue;
        ind = xx + yy*n_size;
        if (flowDir[ind] == (0x01<<dir)) stack[count++] = ind;
      }
    }
  }
');

#######################################################################

pp_def('upstreamMask', HandleBad => 1,
  Pars => 'byte flowDir(n,m);
    int N(); int M();
    byte [o] mask(n,m);',
  Code => '
    int ind;
    int n_size = $SIZE(n);    int NN = $N();
    int m_size = $SIZE(m);    int MM = $M();
    int *myStack;	myStack = malloc(n_size*m_size*sizeof *myStack);

    loop(n,m) %{ $mask() = 0; %}  	//	Initialization of the output arrays
    for (ind=0; ind < n_size*m_size; ind++) {
      myStack[ind] = -1;
    }

    upstr($P(flowDir),myStack,n_size,m_size,NN,MM);
    ind = 0;
    while (myStack[ind] != -1 && ind < n_size*m_size) {
      MM = myStack[ind] / n_size;
      NN = myStack[ind] - n_size*MM;
      ind++;
      $mask(n=>NN,m=>MM) = 1;
    }
	// Free OS memory
    free(myStack);
');

#######################################################################

pp_def('upstrAccumAll', HandleBad => 1,
  Pars => 'byte flowDir(n,m); double data(n,m);
    double [o] upstrAccData(n,m);',
  Code => '
    int i, j, ind;
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    double *myData;	myData  = malloc(n_size*m_size*sizeof *myData);
    int    *myStack;	myStack = malloc(n_size*m_size*sizeof *myStack);

	//	Initialization of the output arrays
    loop(n,m) %{ $upstrAccData() = 0; %}
    for (j=0; j<m_size; j++) {
      for (i=0; i<n_size; i++) {
	ind = i + j*n_size;
	myData[ind]  = ( $ISBAD($data(n=>i,m=>j)) ) ? 0 : $data(n=>i,m=>j);
	myStack[ind] = -1;
      }
    }

    for (j=0; j<m_size; j++) {
      for (i=0; i<n_size; i++) {
	if ( $ISBAD($flowDir(n=>i,m=>j)) ) {
	  $SETBAD($upstrAccData(n=>i,m=>j));
	}
	else {
	  upstr($P(flowDir),myStack,n_size,m_size,i,j);
	  ind = 0;
	  while (myStack[ind] != -1 && ind < n_size*m_size) {
	    $upstrAccData(n=>i,m=>j) += myData[myStack[ind]];
	    myStack[ind++] = -1;				// Reset myStack
	  }
	}
      }
    }
	// Free OS memory
    free(myData);
    free(myStack);
');

#######################################################################

pp_def('basinIDs', HandleBad => 1,
  Pars => 'byte flowDir(n,m);
    int table(k,l);
    int [o] basinID(n,m);',
  Code => '
    int ID     = 0;
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    int l_size = $SIZE(l);
    int ind, i, j, i_cell;
    int *myStack;	myStack = malloc(n_size*m_size*sizeof *myStack);

	//	Initialization of the output arrays
    loop(n,m) %{ $SETBAD($basinID()); %}
    for (j=0; j<m_size; j++) {
      for (i=0; i<n_size; i++) {
	ind = i + j*n_size;
	myStack[ind] = -1;
      }
    }

	//	Building basin IDs
    for(i_cell=l_size-1; i_cell>=0; i_cell--) {		// Reverse order
      if ($table(k=>0,l=>i_cell) != 0) continue;
      i = $table(k=>3,l=>i_cell);
      j = $table(k=>4,l=>i_cell);

      upstr($P(flowDir),myStack,n_size,m_size,i,j);
      ind = 0;	ID++;
      while (myStack[ind] != -1 && ind < n_size*m_size) {
	j = myStack[ind] / n_size;
	i = myStack[ind] - n_size*j;
	myStack[ind++] = -1;				// Reset myStack
	$basinID(n=>i,m=>j) = ID;
      }
    }
	// Free OS memory
    free(myStack);
');

#######################################################################

pp_def('checkCircularity', HandleBad => 1,
  Pars => 'byte flowDir(n,m);
    int [o] indexX();	int [o] indexY();',
  Code => '
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);

	//	Initialization of "downstream" variables

    int i, j, xx, yy, dir, pix;
    int from[2][255];
    int From[2][9]	= { {0,1,1,0,-1,-1,-1,0,1} , {0,0,1,1,1,0,-1,-1,-1} };
    int dInd[9]		= {0,1,2,4,8,16,32,64,128};
    for (i=0; i<9; i++) {
      from[0][dInd[i]]	= From[0][i];
      from[1][dInd[i]]	= From[1][i];
    }
    $indexX() = -1;	$indexY() = -1;

	//	Initialization of myMask array

    int *myMask[n_size];		// Dynamically allocate memory
    for (i=0; i<n_size; i++)
      myMask[i] = (int *)malloc(m_size * sizeof(int));

    for (j=0; j<m_size; j++)		// Initialize with values
      for (i=0; i<n_size; i++)
	myMask[i][j] = -1;

	//	Calculations

    for (j=0; j<m_size; j++) {
      for (i=0; i<n_size; i++) {
	if ( $ISBAD($flowDir(n=>i,m=>j)) ) continue;

	pix = i + j * n_size;
	xx  = i;
	yy  = j;

	do {
	  if (myMask[xx][yy] > 0) break;		// Already checked pixel and flow downstream

	  myMask[xx][yy] = pix;			// Mark as checked pixel
	  dir = $flowDir(n=>xx,m=>yy);
	  xx += from[0][dir];
	  yy += from[1][dir];

	  if (dir && myMask[xx][yy] == pix) {	// Circularity found!!!
	    $indexX() = xx;	$indexY() = yy;
	    j = m_size;	i = n_size;	break;
	  }
	} while (myMask[xx][yy] == -1);
      }
    }
	// Free OS memory
    for (i=0; i<n_size; i++)
      free(myMask[i]);
');

#######################################################################

pp_def('streamOrder', HandleBad => 1,
  Pars => 'int table(k,l); int colTo(n,m); int rowTo(n,m);
    int [o] sOrder(n,m);',
  Code => '
    int l_size = $SIZE(l);
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    int i, ip, jp, in, jn, add, iii, jjj, iMax;

	////	Initializations
    loop(n,m) %{ $sOrder() = 0; %}

	////	Calculations
    for (i=0; i<l_size; i++) {
      ip = $table(k=>3,l=>i);
      jp = $table(k=>4,l=>i);
      add = $sOrder(n=>ip, m=>jp) ? 0 : 1;
      while ( add ) {
	$sOrder(n=>ip, m=>jp) = add;
	in = $colTo(n=>ip,m=>jp);
	jn = $rowTo(n=>ip,m=>jp);
	if ((ip==in && jp==jn) || ($sOrder(n=>in, m=>jn) > $sOrder(n=>ip, m=>jp))) { add = 0; }
	else {
		// Check max stream order of the incoming streams
	  iMax = 0;
	  for (iii=in-1; iii<=in+1; iii++) {
	    if (iii<0 || iii>=n_size) continue;
	    for (jjj=jn-1; jjj<=jn+1; jjj++) {
	      if (jjj<0 || jjj>=m_size) continue;
	      if ((iii==in && jjj==jn) || (iii==ip && jjj==jp)) continue;
	      if ($colTo(n=>iii,m=>jjj)==in && $rowTo(n=>iii,m=>jjj)==jn) {
		if ($sOrder(n=>iii, m=>jjj) > iMax) iMax = $sOrder(n=>iii, m=>jjj);
	  } } }
	  if ((iMax > 0) && (iMax == $sOrder(n=>ip, m=>jp))) add++;
	}
	ip = in;
	jp = jn;
    } }
	////	Set bad values
    loop(n,m) %{ if ($sOrder()==0) $SETBAD($sOrder()); %}
');

#######################################################################

pp_def('mask_union', HandleBad => 1,
  Pars => 'int mask_in(n,m);	int rim(k,l);
    int [o] mask_out(n,m);',
  Code => '
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    int l_size = $SIZE(l);
    int i, j, ii, jj, idx;
    int *myKeys;	myKeys  = malloc(n_size*m_size*sizeof *myKeys);	// Dynamically allocate memory
    for (i=0; i<l_size; i++) { myKeys[i] = 0; }				// Initialize with values

	//	Initialization of the output arrays
    loop(n,m) %{ if ($ISBAD($mask_in())) { $SETBAD($mask_out()); }
		 else { $mask_out() = 0; } %}

	//	Build list of mask IDs at the coast
    for (i=0; i<l_size; i++) {
      ii	= $rim(k=>0,l=>i);
      jj	= $rim(k=>1,l=>i);
      idx	= $mask_in(n=>ii,m=>jj);
      myKeys[idx] = 1;
    }
	//	Build output mask
    for (j=0; j<m_size; j++) {
      for (i=0; i<n_size; i++) {
	if ( $ISBAD($mask_in(n=>i,m=>j)) ) continue;

        idx	= $mask_in(n=>i,m=>j);
	if (myKeys[idx]) { $mask_out(n=>i,m=>j) = 1; }
      }
    }
	// Free OS memory
    free(myKeys);
');

#######################################################################

pp_def('mask_endorheic', HandleBad => 1,
  Pars => 'int basinID(n,m);	int buffer(n,m);	int mouth(k,l);
    int [o] mask_out(n,m);',
  Code => '
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    int l_size = $SIZE(l);
    int i, j, ii, jj, ll, bb, ibmin, ibmax, jbmin, jbmax, idx;
    bool skip;

	/////	Initialization of the output arrays
    loop(n,m) %{ if ($ISBAD($basinID())) { $SETBAD($mask_out()); }
		 else { $mask_out() = 0; } %}

	/////	Process outlets
    for (ll=0; ll<l_size; ll++) {
      ii	= $mouth(k=>0,l=>ll);
      jj	= $mouth(k=>1,l=>ll);
      idx	= $basinID(n=>ii,m=>jj);
      skip	= false;

	//	Check the outlet to be endorheic
      bb	= $buffer(n=>ii,m=>jj);
      ibmin	= ii - bb;	if (ibmin <  0)      ibmin = 0;
      jbmin	= jj - bb;	if (jbmin <  0)	     jbmin = 0;
      ibmax	= ii + bb;	if (ibmax >= n_size) ibmax = n_size - 1;
      jbmax	= jj + bb;	if (jbmax >= m_size) jbmax = m_size - 1;
      for (j=jbmin; j<=jbmax && !skip; j++) {
	for (i=ibmin; i<=ibmax; i++) {
	  if ($ISBAD($basinID(n=>i,m=>j))) { skip = true; break; }
	}
      }
	//	Set endorheic mask for this basin 
      for (j=0; j<m_size && !skip; j++) {
	for (i=0; i<n_size; i++) {
	  if ($basinID(n=>i,m=>j) == idx) $mask_out(n=>i,m=>j) = idx;
	}
      }
    }
');

#######################################################################

pp_def('basins_from_list', HandleBad => 1,
  Pars => 'int basinID(n,m);	int list(k);
    int [o] mask_out(n,m);',
  Code => '
    int n_size = $SIZE(n);
    int m_size = $SIZE(m);
    int k_size = $SIZE(k);
    int i, j, kk, idx;

	/////	Initialization of the output arrays
    loop(n,m) %{ if ($ISBAD($basinID())) { $SETBAD($mask_out()); }
		 else { $mask_out() = 0; } %}

	/////	Process list of basins
    for (kk=0; kk<k_size; kk++) {
      idx = $list(k=>kk);

      for (j=0; j<m_size; j++) {
	for (i=0; i<n_size; i++) {
	  if ($basinID(n=>i,m=>j) == idx) $mask_out(n=>i,m=>j) = idx;
	}
      }
    }
');

#######################################################################

pp_done();
