#!/usr/bin/perl

#######################################################################
#
#       All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      (C) University of New Hampshire and Water Systems Analysis Group 2008-2021
#
#######################################################################

#######################################################################
#
#	This code makes a spatial aggregation of data.
#
#	Source and destination dataset information is taken from the Magic Table.
#
#	Presently aggregation data is sampled from coordinates of the
#	mask pixels.
#	This is good if the data pixel resolution is less or same as
#	the mask pixel resolution.
#	TODO- Reverse sampling... Do we need it ???
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#	Copyright- No distribution without permission of the author.
#
#	March 2010
#		History-
#	Feb 2011	Sigma caculations are added
#	May 2011	Added- 1. Start/end dates; 2. Zero ID;
#			  3. Shape file polygon masks; 4. Others.
#	Oct 2012	X at zero prime meridian (pm0) case of
#			  source/mask dataset has been addressed.
#	Nov 2012	Polygon area is added to the output NetCDF file
#	Mar 2013	bug in translation keys when dataset extent is less than mask extent
#	Dec 2013	No area calculation option is added.
#	Apr 2013	Added processing of multiple masks. Some other changes.
#	May 2014	Credits to output files
#	Mar 2016	Some bugs fixed.
#	Feb 2017	Some small fixes for compatibility with new version of perl
#			Added option to work with masks without defined Attributes
#	Jun 2017	Removing old NetCDF module
#	Nov 2017	Adding non-NetCDF file formats for the input dataset
#	Feb 2019	Added use of "*.init" files for dataset IDs aling with Magic Table
#	Oct 2019	Added "scale" and "offset" for aggregated data unit conversion
#	Aug 2021	Removed Proj4 module dependencies.
#	Aug 2022	Transition to GDAL FFI interface.
#
#	Version- 23.4.0	(YY.M.#) - Small change if not listed above
#
#######################################################################

use strict;
use Getopt::Long;
use File::Basename;
use File::Path;
use File::Temp;
use FileHandle;
use Geo::GDAL::FFI;
use List::Util;
use Math::Trig qw/pi/;
use PDL;
use PDL::Char;
use PDL::IO::FlexRaw;
use PDL::NetCDF;
use Time::JulianDay;
use Time::DaysInMonth;
use warnings;
use RIMS;		### WSAG UNH module

my $PATH		= get_file_path();
my $gdal_translate	= $$PATH{gdal_translate};
my $gdal_rasterize	= $$PATH{gdal_rasterize};
my $gdalwarp		= $$PATH{gdalwarp};
my $flip_nc		= $$PATH{flip_nc};
my $shift_pm		= $$PATH{shift_pm};

use vars qw(*NEWERR *OLDERR);	# To avoid silly warning messages from GDAL, such as
open NEWERR, ">/dev/null";	# "No UNIDATA NC_GLOBAL:Conventions attribute"
open OLDERR, ">&STDERR";

STDOUT->autoflush(1);		# Disable buffering
my $credits = get_credits();	# Credits to output file metadata
$PDL::BIGPDL= 1 if !$PDL::BIGPDL;

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose,$zID,$remove,$remove_mask,$rm_rec,$s_date,$e_date,$tr,$na,$scale,$offset,
	$cumulative, $mdt,$type,$m_file,$MT_file) =
(     0,      0,    0,     0,         0,        0,      '',     '',   0,  0,    1,     0,
	     0,       '',   '',    '',     ''   );
						# Get command line options
usage() if !GetOptions('h'=>\$help, 'v'=>\$verbose, 'z'=>\$zID, 'rm'=>\$remove, 'rmsk'=>\$remove_mask, 'rr=i'=>\$rm_rec, 'sd=s'=>\$s_date, 'ed=s'=>\$e_date, 'tr=f'=>\$tr, 'na=f'=>\$na, 'c'=>\$cumulative, 'mdt=s'=>\$mdt, 'ot=s'=>\$type, 'f=s'=>\$m_file, 'mt=s'=>\$MT_file, 'scl=f'=>\$scale, 'off=f'=>\$offset) or $help;

my $d_code = shift() or usage();
my $m_code = shift() or usage();
my $nc_file= shift();

if ($type) {
  my %data_type = (
	"byte"		=> \&byte,
	"short"		=> \&short,
	"long"		=> \&long,
	"float"		=> \&float,
	"double"	=> \&double);
  die "Unknown output data type\n\n" unless defined $data_type{lc($type)};
  $type = $data_type{lc($type)};
}

#######################################################################
#######################################################################

my $time_start = time();
my @loc_time_0 = localtime($time_start);		# Report Time
$loc_time_0[5] += 1900; $loc_time_0[4]++;
printf "\nThe job started on %04d-%02d-%02d at %02d:%02d:%02d.\n\n",
	reverse(@loc_time_0[0..5]) if $verbose;

print "Running: $0\n\n" if $verbose;
#######################################################################
			### Other Initializations

my $names_file	= $MT_file ? $MT_file : $$PATH{names_file};
my $pol_key_dir	= $$PATH{pol_key_dir};
my %names_d	=      attrib($d_code, $names_file);
my %names_m	= read_ATTRIB($m_code, $names_file);

$names_d{Start_Date}	= $s_date if $s_date;
$names_d{End_Date}	= $e_date if $e_date;
$names_d{Param_Name}	= 'N/A'		unless defined $names_d{Param_Name};
$names_d{Orig_Units}	= 'N/A'		unless defined $names_d{Orig_Units};
$names_d{Round}		= '%.2f'	unless defined $names_d{Round};

# die "NetCDF Variable Name is not defined in the Magic Table. Aborting...\n"
# 	unless $names_d{Var_Name};
die "Source dataset has \"flip\" Processing flag. Code needs to be updated...\n"
	if $names_d{Processing} =~ m/flip/i || $names_m{Processing} =~ m/flip/i;

my @metadata	= split m/:/,$mdt;	map {
$metadata[$_]	= "SAME"    unless defined $metadata[$_]; } 0..2;
$metadata[0]	= $names_d{Param_Name}	if $metadata[0] =~ m/^SAME$/i;
$metadata[1]	= $names_d{Orig_Units}	if $metadata[0] =~ m/^SAME$/i;
$metadata[2]	= $names_d{Round}	if $metadata[0] =~ m/^SAME$/i;
$metadata[3]	= $cumulative ? 'Cumulative' : 'Average';

#######################################################################

unless (defined $nc_file) {
  my $pol_files	= polygon_files(\%names_d);
  $nc_file	= $$pol_files{$m_code} if defined $$pol_files{$m_code};
}
die("NetCDF_OUT is not provided...\n") unless $nc_file;

				### Prepare output directory
my     $dir_out = dirname($nc_file);
mkpath($dir_out,0,0775) or die "Cannot make directory...\n$dir_out\n" unless -e $dir_out;
unlink $nc_file if $remove;

if ($verbose) {
  printf  "Data Code   = %s\n",  $d_code;
  printf  "Mask Code   = %s\n",  $m_code;
  printf  "Mask File   = %s\n", ($m_file?$m_file:'By RIMS rule');
  printf  "Output File = %s\n\n",$nc_file;
}

#######################################################################
#################     Build date and file lists    ####################

my  $jd_1900	= julian_day(1900,1,1);
my ($j_date_list,$date_list) = make_date_list($names_d{Start_Date},$names_d{End_Date},\%names_d);
my  @nc_time	= map {$_ - $jd_1900} @$j_date_list;
my  $file_list	= make_file_list($j_date_list,\%names_d,1e10,'0_0');

		### Get data compression parameters
(my $test_file = $$file_list[0][0]) =~ s/NETCDF:(.+):$names_d{Var_Name}/$1/;
my ($netcdf_scale,$netcdf_offset,$netcdf_nodata) =
	get_netcdf_compression($test_file,$names_d{Var_Name},$names_d{Processing});
my $compression = [$netcdf_scale,$netcdf_offset];

	######   Builing Lists of Polygon IDs and names   ######

my ($id,$name);
my ($attr_id,$attr_name,$attr_file) = split m/:/,$names_m{Data_Attrib};
if (defined($attr_file) && -e $attr_file) {
  my ($field,@attr_data) = -e $attr_file ? read_table($attr_file) : (undef,undef);
     ($id,$name) = NO_GHAAS(
	[map $$_[$$field{$attr_id}],  @attr_data[0..$#attr_data]],
	[map $$_[$$field{$attr_name}],@attr_data[0..$#attr_data]]);
} else {
  print "Attributes for spatial polygons (ID= $names_m{Code_Name}) are not provided...\n\n" if $verbose;
}

#######################################################################
##########     Check existing NetCDF file for being updated   #########

if (-e $nc_file)
{
  my @nc_date = get_NetCDF_var($nc_file,'time')->list;

  if (scalar(@nc_date) <= $rm_rec) {
    unlink $nc_file;
  }
  else {
    splice @nc_date,-$rm_rec if $rm_rec;

    foreach my $date (@nc_date) {
      my   $d;
      do { $d = shift @nc_time; shift @$j_date_list; shift @$date_list; }    until !defined($d) || $d == $date;
		### It happens when time variable in the NetCDF file are not sequential, corrupted
      die "Bad NetCDF file: $nc_file\n\tDates do not match. Aborting...\n\n" unless defined($d) && $d == $date;
    }
    unless (@$date_list) {
      print "Destination dataset is up to date! Nothing to do...\n\n" if $verbose;
      exit;
    }

		### Updating file list to process
    $file_list = make_file_list($j_date_list,\%names_d,1e10);
  }
		### Check that the update layers will not cut in the middle of existing data
  if ($rm_rec > scalar(@nc_time)) {
    die "\"-rr $rm_rec\" is greater than # of update layers.\nAborting...\n",
	"It is recommended to use \"-rm\" option instead.\n";
  }
}

#######################################################################
##########	Rule to build polygon key (mask) file name	#######

unless ($m_file) {
  mkpath($pol_key_dir,0,0775) or die "Cannot make directory...\n$pol_key_dir\n" unless -e $pol_key_dir;
  my ($sz,$gT)	= get_geo_transform($$file_list[0][0]);
  my $sffx	= ($names_m{Processing}=~m/polygon/i && $tr) ? ".$tr" : '';
 ($m_file	= $pol_key_dir . join('_',@$gT) . "$sffx.$m_code.key") =~ s/(\.\d{5})\d+/$1/g;
}
unlink $m_file if $remove_mask;

#######################################################################
##############     Convert vector polygons to grid     ################

my $rst_file;		# Rasterized vector/shape file
if ($names_m{Processing}=~s/polygon//i && !(-e $m_file)) {
  my  $file		= file_pyramid($names_m{File_Path},1e10);
  $rst_file		= rasterize_shp_file($file, $tr);
  $names_m{File_Path}	=~ s/$file/$rst_file/;

  print "Done- converting polygons to grid.\n" if $verbose;
}

#######################################################################
###################   Building masks   ################################

my ($mask,$d_size) = (-e $m_file) ? read_mask($m_file,$$file_list[0][0]) :
	build_mask($m_file,$$file_list[0][0],\%names_d,\%names_m,$id);
($id,$name) = NO_GHAAS([keys %$mask], [map "Undefined-$_", keys %$mask]) unless defined $id;
unlink $rst_file if defined $rst_file;

die "No mask polygons exist in the masking dataset (ID= $names_m{Code_Name}). Aborting...\n\n"
	unless scalar(@$id);

#######################################################################
###################   Data aggregation   ##############################

		### Create job groups
my @job_group = make_groups($names_d{Time_Series},$date_list,$file_list,\@nc_time);

		### Run aggregation
for (my $i=0; $i<=$#job_group; $i++)
{
  my $n_layers = scalar @{$job_group[$i][2]};
#   printf "\n\nNew Group (%d)-\n",$i+1;
#   printf "N dates    = %d\n",scalar(@{$job_group[$i][0]});
#   printf "N nc dates = %d\n",scalar(@{$job_group[$i][2]});
#   foreach my $file (@{$job_group[$i][1]}) {
#     printf "%s [%s]\n",$$file[0],join(',',@{$$file[1]});
#   }
  my ($aggr_data, $aggr_sigma, $area, $nodata) =
    aggregate($job_group[$i][0],$job_group[$i][1],$mask,$d_size,$compression,$cumulative,$na,$scale,$offset);

  if (-e $nc_file) {
	update_nc($nc_file,$names_d{Var_Name},$type,$aggr_data,$aggr_sigma,$area,
		  $job_group[$i][2],$rm_rec);
	$rm_rec = List::Util::max(0, $rm_rec-$n_layers);
	printf "\tFile: updated (%d: $n_layers layers)\n",$i if $verbose;
  } else {
	write_nc ($nc_file,$aggr_data,$aggr_sigma,$area,$nodata,$type,
		  $job_group[$i][2],$name,\%names_d,\@metadata,$credits);
	$rm_rec = 0;
	printf "\tFile: written (%d: $n_layers layers)\n",$i if $verbose;
  }
}

#######################################################################
						# Report Total Time
printf "\nTime used for the job - %d hours, %d minutes, and %d seconds\n\n",
	time_used($time_start,time()) if $verbose;

#######################################################################

print "\nAll Done!\n\n" if $verbose;

close NEWERR; close OLDERR;
exit;

#######################################################################
######################  Functions  ####################################

sub aggregate

{
  my ($date_list,$file_list,$mask,$size,$compr,$cumul,$na,$scale,$offset) = @_;
  my $date_count = 0;
  my $nodata;
  my %aggr; my %aggr_sigma; my %area;
  map {$aggr{$_}=[$_]; $aggr_sigma{$_}=[$_]; $area{$_}=[$_];} keys(%$mask);
       $aggr{0} =[0];  $aggr_sigma{0} =[0];  $area{0} =[0];

  foreach my $file (@$file_list)
  {
    (my $path = $$file[0]) =~ s/.+:(.+):.+/$1/;	### Strip NetCDF extras

    unless (-e $path)	### In case file does not exist...
    {
      map push(@{$aggr{$_}},      (($nodata) x scalar(@{$$file[1]}))),keys(%$mask);
      map push(@{$aggr_sigma{$_}},(($nodata) x scalar(@{$$file[1]}))),keys(%$mask);
      map push(@{$area{$_}},      (($nodata) x scalar(@{$$file[1]}))),keys(%$mask);
      next;
    }

    foreach my $band (@{$$file[1]})
    {
      my ($global_n,$global_d,$global_d_sq,$global_a) = (0,0,0,0);	# Global
      my  $band_data = read_raster($$file[0],$band,$compr)->flat*$scale + $offset;

      if ($band_data->nelem != $size) {			### In case of bad band
	$band_data = zeroes($size);
	$band_data = $band_data->setbadif($band_data==0);
      }
      if ($names_d{Processing}=~m/nodata=([-+e\.\d]+)/i) {	# $names_d is global
	$nodata    = $1;					# Add to sub args?
	$band_data = $band_data->setbadif(abs($band_data-$nodata) < 1e-12);
      }
      $nodata = get_bad_value($$file[0]) unless defined $nodata;

      foreach my $id (keys %$mask)
      {
	my $data = $band_data->dice($$mask{$id}[1][0])->sever;
	my $area = $na ? ones($data->dims)*$na : pdl $$mask{$id}[1][1];
	   $area->inplace->copybad($data);

			### Calculate weighted average
	my $n_cells = $data->ngoodover;
	if ($n_cells) {
	  my ($sum_d,$sum_d_sq,$sum_a) = (($area*$data)->sum,($area*$data**2)->sum,$area->sum);
	  push @{$aggr{$id}},	($cumul?$sum_d:$sum_d/$sum_a);
	### "lclip" is just in case since very rarely it makes very small (-1e-6) negative numbers
	  push @{$aggr_sigma{$id}},
		sqrt(List::Util::max(0,$sum_a*$sum_d_sq-$sum_d**2))/$sum_a;
	  push @{$area{$id}},	$sum_a;
	  $global_n	+= $n_cells;
	  $global_d	+= $sum_d;
	  $global_d_sq	+= $sum_d_sq;
	  $global_a	+= $sum_a;
	}
	else {	push @{$aggr{$id}},      $nodata;
		push @{$aggr_sigma{$id}},$nodata;
		push @{$area{$id}},      $nodata; }
      }
		### Calculate global weighted average
      unless ($zID) {
	push @{$aggr{0}},      ($global_n ? ($cumul?$global_d:$global_d/$global_a) : $nodata);
	push @{$aggr_sigma{0}},($global_n ?
		sqrt(List::Util::max(0,$global_a*$global_d_sq-$global_d**2))/$global_a :
		$nodata);
	push @{$area{0}},      ($global_n ? $global_a : $nodata);
      }
      printf "\tLayer- %s\n",$$date_list[$date_count++] if $#$date_list && $verbose;
    }
  }
#   print "Done- All data aggregation!\nAggregation-\n" if $verbose;
#   map {print join(' ',@{$aggr{$_}}),"\n"} sort {$a<=>$b} keys(%aggr);
#   print "Sigma-\n";
#   map {print join(' ',@{$aggr_sigma{$_}}),"\n"} sort {$a<=>$b} keys(%aggr);

  return \%aggr,\%aggr_sigma,\%area,$nodata;
}

#######################################################################

sub build_mask

{
  my ($m_file,$file_d,$names_d,$names_m,$pol_id) = @_;

  (my $path = $file_d) =~ s/.+:(.+):.+/$1/;	### Strip NetCDF extras
  die "First data file must exist-\n\t$path\n\n" unless -e $path;

  my $file_m		= file_pyramid($$names_m{File_Path},1e10);
# if $file_m is a NetCDF file then "cell_area" has to be rewritten since
# rows are in reverse and $info_m cannot be used
# TODO- use $gT_m instead of $info_m in "cell_area"
  my $extent_m	= get_extent($file_m,$$names_m{Projection});
  my $gT_m	= $$extent_m{gTransform};
  my $size_m	=[$$extent_m{ncols},$$extent_m{nrows}];
  my $nD_m	= get_bad_value($file_m);
  my $data_m	=[$$extent_m{mask}->setbadtoval($nD_m)->list];
     $pol_id	=[$$extent_m{mask}->uniq->list] unless defined $pol_id;
  print "Done- read Mask Dataset.\n" if $verbose;

  my @area_m	= cell_area($gT_m,$size_m,$names_m);
  print "Done- build Cell Area data.\n" if $verbose;

	########################################################
	######   Translating mask pixels to data pixels   ######

  my @pixel_d;
  my ($size_d,$gT_d) = get_geo_transform($file_d);
  my $dSize = $$size_d[0]*$$size_d[1];
  my $order = ($$gT_d[5]>0) ? 1 : -1;
  my $igT_d = InvGeoTransform($gT_d);
  my $pm0_d = $$names_d{Processing} =~ m/pm0/;
  my $pm0_m = $$names_m{Processing} =~ m/pm0/;

  my $proj_from	= $$names_m{Projection};		# New RIMS with Geo::GDAL::FFI
  my $proj_to	= $$names_d{Projection};		# New RIMS with Geo::GDAL::FFI
  # my $proj_from	= crs_to_Obj($$names_m{Projection});	# Old RIMS with GDAL SWIG
  # my $proj_to	= crs_to_Obj($$names_d{Projection});		# Old RIMS with GDAL SWIG

  for (my $row=0; $row<$$size_m[1]; $row++) {
    for (my $col=0; $col<$$size_m[0]; $col++) {
      my $index_d = undef;
      if ($$data_m[$row*$$size_m[0]+$col] != $nD_m) {	# transform_point() changed to Geo::GDAL::FFI API
	my $coord_d = transform_point($proj_from, $proj_to, ApplyGeoTransform($gT_m,$col+0.5,$row+0.5));
	  $$coord_d[0] -= 360 if $pm0_m && $$coord_d[0] > 180;	# Wrap around 180 deg Lon
	  $$coord_d[0] += 360 if $pm0_d && $$coord_d[0] < 0;	# if needed
	my @colRow  = map POSIX::floor($_), ApplyGeoTransform($igT_d,$$coord_d[0],$$coord_d[1]);
	$index_d    = $colRow[1]*$$size_d[0]+$colRow[0];
	if ($colRow[0] < 0 || $colRow[1] < 0 || $colRow[0] >= $$size_d[0] || $colRow[1] >= $$size_d[1]) {
	  $index_d = undef;
	  $$data_m[$row*$$size_m[0]+$col] = $nD_m;
	}
     }
      push @pixel_d,$index_d;
    }
  }
  print "Done- Translating mask pixels.\n" if $verbose;

	###########################################
	######   Building Polygon Mask keys   #####

  my %mask;
  map {$mask{$_}=[0]} @$pol_id;

  for (my $i=0; $i<=$#$data_m; $i++)
  {
    push @{$mask{$$data_m[$i]}},[$pixel_d[$i],$area_m[$i/$$size_m[0]]]
	if defined($mask{$$data_m[$i]});
  }
  unless ($zID) {
    die "No polygons with ID=0 are allowed in the mask. Aborting...\n" if defined $mask{0}[1];
    delete $mask{0};
  }

		### Calculating total polygon area
  foreach my $id (sort {$a<=>$b} keys(%mask))
  {
    if (defined $mask{$id}[1]) {
      $mask{$id}[0] = List::Util::sum(map($mask{$id}[$_][1],1..$#{$mask{$id}}));
#       printf "\tPolygon ID %d - %d\tpixels\t%.3f\n",$id,$#{$mask{$id}},$mask{$id}[0];
    }
    else { delete $mask{$id}; }
  }

		### Write mask to file
  write_mask($m_file,\%mask,$dSize,$order) if $m_file;

		### Convert to PDL
  foreach my $id (keys(%mask))
  {
    my $pix  = pdl(map($mask{$id}[$_][0],1..$#{$mask{$id}}));
    my $area = pdl(map($mask{$id}[$_][1],1..$#{$mask{$id}}));
    $mask{$id} = [$mask{$id}[0],[$pix,$area]];
  }

  printf("Done- Polygon masks (N=%d)!\n\n",scalar(keys %mask)) if $verbose;

  return \%mask,$dSize;
}

#######################################################################

sub write_mask

{
  my ($file,$mask,$size,$order) = @_;

  open (FILE,">$file") or die "Couldn't open $file, $!";
  print FILE pack("l1",$size);					### Write data size
  print FILE pack("c1",$order);					### Write latitude order
    foreach my $id (sort {$a<=>$b} keys(%$mask)) {
      my $n_rec = scalar(@{$$mask{$id}});
      print FILE pack("l1l1d1",$n_rec,$id,$$mask{$id}[0]);	### Array length, ID, Total area
      print FILE pack("l*",map($$mask{$id}[$_][0],1..$#{$$mask{$id}}));	# Pix
      print FILE pack("d*",map($$mask{$id}[$_][1],1..$#{$$mask{$id}}));	# Area
    }
  close FILE;
}

#######################################################################

sub read_mask

{
  my ($file,$data_file) = @_;
  my %mask;
  my $int_len = length(pack("c1"));
  my $rec_len = length(pack("l1l1d1"));
  my $r_len_l = length(pack("l1"));
  my $r_len_d = length(pack("d1"));
  my $rec;

		### Get data GeoTransform and latitude order
  my ($size_d,$gT_d) = get_geo_transform($data_file);
  my  $order_d = ($$gT_d[5]>0) ? 1 : -1;

		################################################
		#####      Read Mask Translation Keys      #####

  open (FILE,"<$file") or die "Couldn't open $file, $!";
    read FILE,$rec,$r_len_l;
    my $dSize   = unpack("l1",$rec);			### Read data size

    read FILE,$rec,$int_len;
    my $order_m = unpack("c1",$rec);			### Read latitude order

    while (read FILE,$rec,$rec_len) {
      my ($len,$id,$val) = unpack("l1l1d1",$rec);	### Array length, ID, Total area
      $mask{$id} = [$val];
					### Read pixels
      read FILE,$rec,$r_len_l*($len-1);
      open(my $FH1,"<",\$rec) or die "Couldn't open filehabdle to binary data stream, $!";
        my $pix  = readflex($FH1, [{Dims=>[$len-1], Type=>'long'}]);
      close $FH1;
					### Read pix area
      read FILE,$rec,$r_len_d*($len-1);
      open(my $FH2,"<",\$rec) or die "Couldn't open filehabdle to binary data stream, $!";
        my $area = readflex($FH2, [{Dims=>[$len-1], Type=>'double'}]);
      close $FH2;

      push @{$mask{$id}},[$pix,$area];
    }
  close FILE;

  		### Transpose keys if needed
  if ($order_d != $order_m) {
    foreach my $id (keys %mask) {
      my $row = floor($mask{$id}[1][0]/$$size_d[0]);
      my $col = $mask{$id}[1][0]-$row*$$size_d[0];
      $mask{$id}[1][0] = $$size_d[0]*($$size_d[1]-1-$row) + $col;
    }
  }

  return \%mask,$dSize;
}

#######################################################################

sub cell_area
			### Area in km^2
{
  my ($gT,$dS,$names) = @_;
  die "Non-square pixels are not implemented for cell area calculations...\n"
	if abs($$gT[1]) != abs($$gT[5]);

  my @data =
	($$names{Projection} =~ m/epsg:(4269|4326|4284)/i) ?
    map(12387.69*$$gT[1]**2*cos(($$gT[3]+$$gT[5]*($_-0.5))/180*pi),1..$$dS[1]) :
	($$names{Projection} =~ m/(3408|3174|3309|proj=aea|proj=tmerc)/i) ?	# "tmerc" is NOT good here
    ((1e-6*$$gT[1]**2) x $$dS[1]) :
	 die "\nUnknown method to calculate cell area for mask data...\n\n";
  return @data;
}

#######################################################################

sub NO_GHAAS

{
  my ($id,$name) = @_;
  my (@id,%name);

  for (my $i=0; $i<=$#$name; $i++)
  {
    if ($$name[$i] !~ m/GHAASBasin/) { push @id,$$id[$i]; $name{$$id[$i]} = $$name[$i]; }
  }

  $name{0} = 'Global' unless $zID;
  return \@id,\%name;
}

#######################################################################

sub get_NetCDF_var

{
  my ($file,$var_name) = @_;

  my $ncobj = PDL::NetCDF->new ($file, {MODE => O_RDONLY});
    my $var = $ncobj->get($var_name);
  $ncobj->close();

  return $var;
}

#######################################################################

sub write_nc
{
  my ($file,$data,$sigma,$area,$nodata,$type,$time,$name,$names_d,$metadata,$credits) = @_;

			### Check credits
  $credits = [(getpwuid($<))[0,6],'email unknown','unknown'] unless $credits;

			###  Prepare variables

  my $var_data	= [map $$data{$_},sort {$a<=>$b} keys(%$data)];
  my $IDs	= [map shift(@$_),@$var_data];
     $var_data	= transpose_nc($var_data);

  my $sig_data	= [map $$sigma{$_},sort {$a<=>$b} keys(%$sigma)];
     $IDs	= [map shift(@$_),@$sig_data];
     $sig_data	= transpose_nc($sig_data);

  my $area_data	= [map $$area{$_},sort {$a<=>$b} keys(%$area)];
     $IDs	= [map shift(@$_),@$area_data];
     $area_data	= transpose_nc($area_data);

  my @name	= map $$name{$_},@$IDs;
  my $n_chars	= List::Util::max(map(length,@name));  ### Pad name strings with spaces
     @name	= map $_.((' ')x($n_chars-length($_))),@name;

  my $n_time	= scalar(@$time);
  my $n_IDs	= scalar(@$IDs);
  my $var_name	= $$names_d{Var_Name} ? $$names_d{Var_Name} : 'AggrVar'; # 'AggrVar' in case of non_NetCDF data files
  my $sig_name	= $var_name.'_sigma';
  my $area_name	= 'area';
     $type	= \&float unless $type;
  my $fill_val	= &{$type}([$nodata]);

			###  Create NetCDF attributes
  my %var_att	= (
	'missing_value'	=> $fill_val,
	'long_name'	=> $$metadata[0],
	'units'		=> $$metadata[1],
	'format'	=> $$metadata[2],
	'aggregation'	=> $$metadata[3],
  );

  my %sig_att	= (
	'missing_value'	=> $fill_val,
	'long_name'	=> "Sigma of $$metadata[0]",
	'units'		=> $$metadata[1],
	'format'	=> $$metadata[2]
  );

  my %area_att	= (
	'missing_value'	=> $fill_val,
	'long_name'	=> 'Polygon Area',
	'units'		=> 'km^2',
	'format'	=> '%.2f'
  );

  my %time_att	= (
	'units'		=> 'days since 1900-1-1',
	'long_name'	=> 'Time',
	'resolution'	=> $$names_d{Time_Series}
  );

  my %id_att	= (
	'long_name'	=> 'ID',
	'units'		=> 'none',
	'description'	=> 'polygon IDs'
  );

  my %name_att	= (
	'long_name'	=> 'Polygon Name',
	'units'		=> 'none',
	'Description'	=> 'Polygon name from attribute table'
  );

  my %global_att= (
	'title'		=> 'Spatial aggregation of data',
	'history'	=> 'Created on '.date_now()." by $$credits[1] ($$credits[2])",
	'NetCDF_version'=> 'netCDF.3.5.1',
	'source'	=> 'Unreferenced Data',
	'institution'	=> $$credits[3],
	'references'	=> 'http://www.wsag.unh.edu',
	'FilePath'	=> $file
  );

			###  NetCDF File definitions
  my $nc = new PDL::NetCDF($file, {MODE => O_CREAT, REVERSE_DIMS => 1});

			###  Write Dimensions
  $nc->putslice('time',['time'], [PDL::NetCDF::NC_UNLIMITED()], [0],[$n_time],	long($time));
  $nc->putslice('id',  ['id'],		[$n_IDs],		[0],[$n_IDs],	long($IDs ));
  $nc->putslice('name',['chars','id'],	[$n_chars,$n_IDs],    [0,0],[$n_chars,$n_IDs],PDL::Char->new(\@name));

			###  Write Data
  $nc->putslice($var_name, ['id','time'],[$n_IDs,PDL::NetCDF::NC_UNLIMITED()],[0,0],[$n_IDs,$n_time],
	&{$type}($var_data), {_FillValue => $fill_val});
  $nc->putslice($sig_name, ['id','time'],[$n_IDs,PDL::NetCDF::NC_UNLIMITED()],[0,0],[$n_IDs,$n_time],
	&{$type}($sig_data), {_FillValue => $fill_val});
  $nc->putslice($area_name,['id','time'],[$n_IDs,PDL::NetCDF::NC_UNLIMITED()],[0,0],[$n_IDs,$n_time],
	&{$type}($area_data),{_FillValue => $fill_val});

			###  Write Attributes
  foreach my $key (keys %global_att) { $nc->putatt ($global_att{$key}, $key); }
  foreach my $key (keys %time_att)   { $nc->putatt ($time_att{$key},   $key, 'time');}
  foreach my $key (keys %id_att)     { $nc->putatt ($id_att{$key},     $key, 'id'); }
  foreach my $key (keys %var_att)    { $nc->putatt ($var_att{$key},    $key, $var_name); }
  foreach my $key (keys %sig_att)    { $nc->putatt ($sig_att{$key},    $key, $sig_name); }
  foreach my $key (keys %area_att)   { $nc->putatt ($area_att{$key},   $key, $area_name);}
  foreach my $key (keys %name_att)   { $nc->putatt ($name_att{$key},   $key, 'name'); }

  $nc->close();
}

#######################################################################

sub update_nc
{
  my ($file,$var_name,$type,$data,$sigma,$area,$time,$rec_back) = @_;

			###  Prepare variables
  my $var_data	= [map $$data{$_},sort {$a<=>$b} keys(%$data)];
  my $IDs	= [map shift(@$_),@$var_data];
     $var_data	= transpose_nc($var_data);

  my $sig_name	= $var_name.'_sigma';
  my $sig_data	= [map $$sigma{$_},sort {$a<=>$b} keys(%$sigma)];
     $sig_data	= transpose_nc($sig_data);

  my $area_name	= 'area';
  my $area_data	= [map $$area{$_},sort {$a<=>$b} keys(%$area)];
     $area_data	= transpose_nc($area_data);

  my $n_IDs	= scalar(@$IDs);
  my $n_time	= scalar(@$time);
     $type	= \&float unless $type;

			### Open NetCDF file and write data
  my $ncobj = new PDL::NetCDF($file, {MODE => O_RDWR, REVERSE_DIMS => 1});
  my $band  = $ncobj->dimsize('time');

  $ncobj->putslice('time',    [], [], [  $band-$rec_back], [$n_time],        long($time));
  $ncobj->putslice($var_name, [], [], [0,$band-$rec_back], [$n_IDs,$n_time], &{$type}($var_data));
  $ncobj->putslice($sig_name, [], [], [0,$band-$rec_back], [$n_IDs,$n_time], &{$type}($sig_data));
  $ncobj->putslice($area_name,[], [], [0,$band-$rec_back], [$n_IDs,$n_time], &{$type}($area_data));
  $ncobj->close();
}

#######################################################################

sub transpose_nc
{
  my $in=shift;
  my $cols=scalar(@{$in->[0]}) || 0;
  my @out=();
  foreach my $col (0 .. $cols-1) {
    push @out, [map {$_->[$col]} @$in];
  }
  return wantarray ? @out : \@out
}

#######################################################################

sub make_groups

{
  my ($ts,$date_list,$file_list,$nc_time) = @_;
  my (@group,@date,@file,@time);

  my $year = substr $$date_list[0],0,4;
  my $file = -1;
  my $band;
     $ts   =~ s/\W.+//;			### Get core time series definition

  if ($ts eq 'daily' || $ts eq 'monthly') {
    for (my $i=0; $i<=$#$date_list; $i++)
    {
      if (		### Group by year (daily data) or by decade (monthly)
	($ts eq 'daily'   &&        $year      eq substr($$date_list[$i],0,4)) ||
	($ts eq 'monthly' && substr($year,0,3) eq substr($$date_list[$i],0,3)))
      {
	push @date,$$date_list[$i];
	push @time,$$nc_time[$i];

	if (defined $file[0] && defined $$file_list[$file][1][$band]) {
	  push @{$file[-1][1]},$$file_list[$file][1][$band++];
	}
	else {
	  $file++;
	  $band = 0;
	  push @file,[$$file_list[$file][0],[$$file_list[$file][1][$band++]]];
	}
      }
      else
      {
	push @group,[[@date],[@file],[@time]];

	$year = substr $$date_list[$i],0,4;
	@date = ($$date_list[$i]);
	@time = ($$nc_time[$i]);

	unless (defined $$file_list[$file][1][$band]) {
	  $file++;
	  $band = 0;
	}
	@file = ([$$file_list[$file][0],[$$file_list[$file][1][$band++]]]);
      }
    }
    push @group,[[@date],[@file],[@time]];
  }
  else {
    @group = ([$date_list,$file_list,$nc_time]);
  }

  return @group;
}

#######################################################################

sub read_ATTRIB
	# Processing of multiple masks- Builds "combo" mask/attributes
{
  my ($code, $mt_file) = @_;

  my @code   = split m/\+/, $code;
  my @attrib = map{{attrib($_,$mt_file)}} @code;# array of hash refs
  my %attrib = %{$attrib[0]};
  return %attrib unless $#code;			### Case of one mask

	######   Make multi-mask attributes   ######
  my $dir	= $$PATH{polygon_dir}.'multi_mask_data';
  my $file_m	= "$dir/$code.tif";
 (my $file_a	= $file_m) =~ s/tif$/csv/;
  $attrib{Code_Name}	= $code;
  $attrib{File_Path}	= "(1e20,1e6):$file_m;";
  $attrib{Data_Attrib}	= "ID:Name:$file_a";
  return %attrib if -e $file_m && -e $file_a;	### Case of multi-mask exists

	######   Reading multi-masks   ######
  my @file	= map file_pyramid($$_{File_Path},1e10), @attrib;
     $file[0]	= rasterize_shp_file($file[0],$tr) if $attrib{Processing} =~ m/polygon/i;
  my $extent	= get_extent($file[0],$attrib[0]{Projection});
  my @data	= map read_GDAL($extent,$attrib[$_],0,$file[$_],1,0),0..$#file;
  my $mask	= 0 * $data[0];
  unlink $file[0] if $attrib{Processing} =~ s/polygon//i;

	######   Reading multi-mask attributes   ######
  my (@id,@name);
  for (my $i=0; $i<= $#file; $i++) {
    my ($id,$name);
    my ($attr_id,$attr_name,$attr_file) = split m/:/,$attrib[$i]{Data_Attrib};
    die "Missing attributes for the multi-mask case is not coded yet...\n" unless -e $attr_file;
    if (defined($attr_file) && -e $attr_file) {
      my ($field,@attr_data) = read_table($attr_file);
         ($id,$name) = NO_GHAAS(
	[map $$_[$$field{$attr_id}],  @attr_data[0..$#attr_data]],
	[map $$_[$$field{$attr_name}],@attr_data[0..$#attr_data]]);
    } else {
      print "Attributes for spatial polygons (ID= $attrib[$i]{Code_Name}) are not provided...\n\n" if $verbose;
      $id	= [$data[0]->uniq->list];
      $name	= [map("Undefined-$_", @$id)];
    }
    push @id,	$id;
    push @name,	$name;
  }

	######   Builing combination mask   ######
  my %name;
  my ($id,$count) = 0;
  my @combo = combo(@id);
  print "Building combination mask-\n" if $verbose;

  for (my $j=0; $j<=$#combo; $j++) {
    my $m = ones($mask->dims);
    for (my $i=0; $i<= $#data; $i++) {
      $m *= $data[$i] == $combo[$j][$i];	### Checks overlap
    }
    if ($m->sum) {
      $mask += $m * (++$id);
      $name{$id} = join ' : ',map($name[$_]{$combo[$j][$_]}." ($combo[$j][$_])",0..$#data);
      printf "\r\tFound $id from $j (%d)",scalar(@combo) if $verbose;
    }
  }
  print "\n\n" if $verbose;

	######   Saving the combination mask and attributes  ######
  mkpath($dir,0,0775) or die "Cannot make directory...\n$dir\n" unless -e $dir;
  write_tif($extent, 'Int16', $file_m, $mask);

  open (FILE,">$file_a") or die "Couldn't open $file_a, $!";
	 print FILE "ID\tName\n";
    map {print FILE "$_\t$name{$_}\n"} 1..$id;
  close FILE;

  return %attrib;
}

sub combo
{
  my @stack = @_;
  my @combo = map [$_],@{shift @stack};

  while (my $arr = shift @stack) {
    my @c;
    foreach my $cmb (@combo) {
      foreach my $mem (@$arr) {
	push @c,[map $_,@$cmb,$mem];
      }
    }
    @combo = @c;
  }
  return @combo
}

#######################################################################

sub rasterize_shp_file
{
  my ($file,$tRes) = @_;

  die "Pixel resolution for conversion of vector polygons to grids\n".
	"must be given with -tr flag (in source units)...\n" unless $tRes;
  my $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm');
#   my $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm', SUFFIX => '.tif');	# Makes problems
  $rst_file	= $tmp_obj->filename . '.tif';

 (my $l_name	= basename($file)) =~ s/\.\w+$//;
  my $junk	=  `$gdal_rasterize -of gtiff -a_nodata -9999 -q -tr $tRes $tRes -a $names_m{Var_Name} -l $l_name $file $rst_file`;
  unless (-s $rst_file) {
    unlink   $rst_file;
    die "Failed to convert vector polygons to grid...\n";
  }

  return $rst_file;
}

#######################################################################

sub read_GDAL
	### TBD- This plus 6 functions below are from pdl_wbm_io.pl
{
  my ($extent,$meta,$resample,$file,$b,$patch_value) = @_;
 (my  $file_s = $file) =~ s/.+:(.+):.+/$1/;
  die "Requested file (in read_GDAL)-\n$file_s\ndoes not exist...\n" unless -e $file_s;
 (my $tmp_file	= tmpnam()) =~ s#^/tmp#/dev/shm#;

		### Build extent string
  my  $extents	= sprintf "%f %f %f %f", $$extent{xllcorner}, $$extent{yllcorner},
	$$extent{xllcorner}+$$extent{cellsizeX}*$$extent{ncols},
	$$extent{yllcorner}+$$extent{cellsizeY}*$$extent{nrows};

		### Build resample string
		#    0      1       2        3         4      5      6
  my @resample = qw/near bilinear cubic cubicspline lanczos average mode/;

		### Populate metadata if not provided in the Magic Table
  $$meta{Var_Scale}	= 1  if $$meta{Var_Scale}  eq '';
  $$meta{Var_Offset}	= 0  if $$meta{Var_Offset} eq '';
  $$meta{Var_Name}	= $1 if $file =~ m/.+:.+:(.+)/;		# non-MagicTable files
  $$meta{Processing}   .=($$meta{Processing}?'':':').$1 if $file =~ m/(.+):.+:.+/;

  if ($file =~ m/(.+):.+:.+/) {
    $$meta{Processing} .= $$meta{Processing} ? $1 : ":$1";	# non-MagicTable files
  }

		### Get NetCDF data compression and NODATA
  my ($netcdf_scale,$netcdf_offset,$netcdf_nodata) =
	get_NetCDF_compression($file_s,$$meta{Var_Name});
  $$meta{compression} = [$netcdf_scale,$netcdf_offset];
  my $nodata = ($$meta{Processing}=~m/nodata=([-+e\.\d]+)/i) ? $1 : $netcdf_nodata;

		### Get Polygon Attribute Name (shape file dataset)
  my $pol_var = $$meta{Var_Name} =~ m/_YEAR_|_MONTH_|_DAY_|_HOUR_|_MIN_/ ? $b : $$meta{Var_Name}
	if $$meta{Processing}=~m/polygon/i;

		### Perform GDAL warping
  my $bounds	= "-q -ts $$extent{ncols} $$extent{nrows} -te $extents";
 (my $l_name	= basename($file_s)) =~s/\.shp$//i if $$meta{Processing}=~m/polygon/i;
  my $gdal_warp	= ($$meta{Processing}=~m/polygon/i) ?
			### Shape  Files
	"$gdal_rasterize -of gtiff $bounds -a_nodata -9999 -a $pol_var -l $l_name $file_s $tmp_file.tif" :
			### Raster Files
	"$gdal_translate -of VRT -b $b $file $tmp_file.vrt;".
	(($$meta{Processing}=~m/flip/i) ? "$flip_nc $tmp_file.vrt;" : '').
	(($$meta{Processing}=~m/pm0/i) ? "$shift_pm $tmp_file.vrt;" : '').
	"$gdalwarp -of gtiff -multi -s_srs \"$$meta{Projection}\" -t_srs \"$$extent{projection}\" $bounds -r $resample[$resample] -srcnodata $nodata -dstnodata -9999 $tmp_file.vrt $tmp_file.tif";

#   warn $gdal_warp;				### In case it fails...
  open STDERR, ">$tmp_file.err";
    for (my $i=0; $i<5; $i++) {	### Makes up to 5 attempts to warp the source file
      sleep 1 if $i;
      my $junk = `$gdal_warp`;		### It does it all
      if (-e "$tmp_file.tif") { last }
      else { unlink <$tmp_file.*> }
    }
  open STDERR, ">&OLDERR";
  die "Failed to warp $file\nFork code   = $?\nFork string = $gdal_warp\n" unless -e "$tmp_file.tif";

		### Check for GDAL errors that still result in output file with all zeros (default patch values)
  open ERRFILE, "$tmp_file.err";
    my @err_lines = grep  m/ERROR/i, <ERRFILE>;
       @err_lines = grep !m/writing to NetCDF/, @err_lines;	# Avoid bug in GDAL v1.92
  close ERRFILE;
  if (@err_lines) {
    unlink <$tmp_file.*>;
    die "ERROR  in warp $file\nFork code   = $?\nFork string = $gdal_warp\nFork errors =\n@err_lines\n" if @err_lines ;
  }

		### Read Raster and change units
  my $pdl = read_raster("$tmp_file.tif",1,$$meta{compression});
     $pdl = $pdl*$$meta{Var_Scale} + $$meta{Var_Offset};	### Change Units
  unlink <$tmp_file.*>;
  $pdl->inplace->setvaltobad(-9999)  if $file_s =~ m/udel_precip_m/;	### Delete when UDEL data is fixed.
  $pdl->inplace->setvaltobad(-999.9) if $file_s =~ m/udel_airtemp_m/;	### Delete when UDEL data is fixed.

		### Patch Bad values by given patch value and apply Good mask
  if (ref($patch_value) =~ m/PDL/) {
    $pdl = condition_slice($pdl->isgood, $pdl, $patch_value);
  }
  elsif (defined $patch_value) {
    $pdl->inplace->setbadtoval($patch_value);
  }
		### Patch bad pixels by the average of their non-bad neighbours
		###	if patch value is not provided
  my ($nPDL,$nMask) = ($pdl->inplace->copybad($$extent{mask})->ngood, $$extent{mask}->ngood);
  while ($nPDL != $nMask) {
    $pdl = $pdl->patchbad2d->copybad($$extent{mask});
    if ($nPDL == $pdl->ngood) {		### Patch islands with Global average
      $pdl->where(($$extent{mask}->isgood - $pdl->isgood)==1) .= $pdl->avg;
    }
    $nPDL = $pdl->ngood;
  }

  return $pdl;
}

#######################################################################

sub read_raster

{
  my ($file,$b,$compr) = @_;
      $b	=  1	unless $b;
      $compr	= [1,0]	unless $compr;
 (my  $file_s	= $file) =~ s/.+:(.+):.+/$1/;
  die "Requested file (in read_raster)-\n$file_s\ndoes not exist...\n" unless -e $file_s;

  open STDERR, ">&NEWERR";
    my $band	= Geo::GDAL::FFI::Open( $file, {Flags => ['READONLY']} )->GetBand($b);
    my $pdl	= $band->GetPiddle;		# Note, GetPiddle(...) can do resample too
    my $nodata	= $band->GetNoDataValue;
       $nodata//= -9999;
  open STDERR, ">&OLDERR";

		### Set bad and uncompress data
  $pdl = $pdl->setvaltobad($nodata)*$$compr[0]+$$compr[1];

  return $pdl;
}

#######################################################################

sub get_extent

{
  my($file,$projection) = @_;
  my($size,$gT)	= get_geo_transform($file);
  my $igT	= InvGeoTransform($gT);
  my @llcorner	= ApplyGeoTransform($gT, 0, $$size[1]);
  my %extent	= (	'file'		=> $file,
			'ncols'		=> $$size[0],
			'nrows'		=> $$size[1],
			'xllcorner'	=> $llcorner[0],
			'yllcorner'	=> $llcorner[1],
			'cellsize'	=> $$gT[1],
			'projection'	=> $projection,
			'gTransform'	=> $gT,
			'igTransform'	=> $igT,
			'mask'		=> read_raster($file,1,[1,0]));

  die "Non-Square pixels in $file..." if $$gT[1] != -$$gT[5];
  return \%extent;
}

#######################################################################

sub get_geo_transform

{
  my $file = shift;
  open STDERR, ">&NEWERR";
    my $options	= $file =~ m/.+:.+:.+/i ? ['IGNORE_XY_AXIS_NAME_CHECKS=YES'] : [];	# NetCDF specific iptions
    my $geotiff_data = Geo::GDAL::FFI::Open($file, {Flags => ['READONLY'], Options => $options});
  open STDERR, ">&OLDERR";
  my $gT = $geotiff_data->GetGeoTransform;
     $gT = parse_gdalinfo($file) if join('',@$gT) eq '010001';
		### Check the geotransform to be valid
  if (join('',@$gT) eq '010001') {
   (my  $file_s = $file) =~ s/.+:(.+):.+/$1/;
    die "\nGDAL problem: Failed to read coordinates/geotransform in\n   $file_s\nAborting...\n\n";
  }

  return [$geotiff_data->GetBand(1)->GetSize], $gT;
}
sub parse_gdalinfo	### Just in case the Geo::GDAL::FFI does not work on WRF output files...
{
  my $file = shift();
  my $str  = &{sub{$_=get_file_path->{gdalinfo}." --config GDAL_NETCDF_IGNORE_XY_AXIS_NAME_CHECKS YES $file";
		   $_=qx($_); chomp; return($_)}};

  my @pix_size	= ($1,$2) if $str =~ m/Pixel Size = \((\S+),(\S+)\)/;
  my @ul_corner	= ($1,$2) if $str =~ m/Upper Left\s+\(\s+(\S+),\s+(\S+)\)/;

  return @pix_size && @ul_corner ? [$ul_corner[0],$pix_size[0],0,$ul_corner[1],0,$pix_size[1]] : [0,1,0,0,0,1];
}

sub InvGeoTransform
{
  my $gT  = shift();
  my $igT = [0,0,0,0,0,0];
	die "\nFailed to get InvGeoTransform. Aborting...\n\n" unless
  Geo::GDAL::FFI::GDALInvGeoTransform($gT, $igT);
  return $igT;
}

sub ApplyGeoTransform
{
  my ($X, $Y) = (0,0);
  Geo::GDAL::FFI::GDALApplyGeoTransform(@_, \$X, \$Y);
  return $X, $Y;
}

#######################################################################

sub get_bad_value

{
  my $file = shift;
  open STDERR, ">&NEWERR";
    my $geotiff_data = Geo::GDAL::FFI::Open("$file", {Flags => ['READONLY']});
  open STDERR, ">&OLDERR";

  my $band	= $geotiff_data->GetBand(1);
  my $nodata	= $band->GetNoDataValue;
     $nodata  //= -9999;

  return $nodata;
}

#######################################################################

sub get_NetCDF_compression

{
  my ($file,$var_name) = @_;
  my ($scale,$offset,$nodata) = (1,0,-9999);
  return ($scale,$offset,$nodata) unless $file =~ m/\.nc$/i && $var_name;

  my $ncobj = PDL::NetCDF->new ($file, {MODE => O_RDONLY});
    my %att = map(($_ => $ncobj->getatt($_,$var_name)), @{$ncobj->getattributenames($var_name)});
  $ncobj->close();

  $scale  = $att{scale_factor}->at(0)	if defined $att{scale_factor};
  $offset = $att{add_offset}->at(0)	if defined $att{add_offset};
  $nodata = $att{_FillValue}->at(0)	if defined $att{_FillValue};
  $nodata = $att{missing_value}->at(0)	if defined $att{missing_value} && !defined($att{_FillValue});

  return ($scale,$offset,$nodata);
}

#######################################################################

sub write_tif
{
  my ($extent, $ot, $file, $data) = @_;
  my %pack = (	'Float32'	=> 'f*',
		'Int16'		=> 's*',
		'Int32'		=> 'i*');
  die "Unknown pack format in \"write_tif\" function. Aborting...\n" unless defined $pack{$ot};

  system "$gdal_translate -q -ot $ot -a_srs $$extent{projection} $$extent{file} $file";

  my $geotiff_data = Geo::GDAL::Open($file, 'Update');
  my $band	= $geotiff_data->GetRasterBand(1);
  my $nodata	= $band->GetNoDataValue;
     $nodata	= -9999 unless defined $nodata;
  $band->WriteRaster(0,0,$$extent{ncols},$$extent{nrows},pack($pack{$ot},$data->setbadtoval($nodata)->list));
}

#######################################################################

sub attrib
{
  my($id, $MT)	=  @_;

		### Read MT attributes
  my $file_flag	= $id=~m/\.init$/i  || -e $id;
  my %attrib	= $file_flag ? read_init($id) : read_attrib($MT,$id,'Code_Name');
		### Read dates from file, if needed
  check_MT_date($attrib{Start_Date});
  check_MT_date($attrib{End_Date});
		### Fill up blanks
  $attrib{Var_Scale}  = 1 unless $attrib{Var_Scale};
  $attrib{Var_Offset} = 0 unless $attrib{Var_Offset};
		### Other cleanups
  $attrib{Processing} =~ s/\s//g;		# Remove blank spaces in processing

  get_calendar(   \%attrib);			# Find the dataset calendar format

  return %attrib;
}

#######################################################################

sub read_init
	### Read IO options from a separate file
{
  my ($file, $options) = @_;
  die "Init file does not exist:\n   $file\n" unless -e $file;
  			### Options
  my $check	= set_default( $$options{CHECK_ARR},	undef);
  my $make	= set_default( $$options{MAKE_ARR},	undef);

  #####################################################################
  ############    Read parameter = value pairs   ######################

  my $str =  htm_template($file);
     $str =~ s/=>\s*,/=> '',/g;		# Fix some erroneous hash values
  my $att =  eval $str;
  die "Error in read_init 'eval': $@\nFile = $file\n" if $@;
  my %attrib = %{ $att };

  #####################################################################
  ############    Read additional parameters. Options.   ##############

	### Check required parameters-
  if ($check) {
    map { die("Parameter \"$_\" is required in File:\n   $file\n") unless $attrib{$_} } @$check;
  }
	### Make optional parameters-
  if ($make) {
    map { $attrib{$_} = '' unless defined $attrib{$_} } @$make;
  }

  return %attrib;
}

#######################################################################

sub time_used

{
  my ($t_start,$t_end) = @_;

  my @used = ($t_end-$t_start,0,0,0);
  $used[1] = int($used[0]/3600);			# Hours
  $used[2] = int(($used[0]-$used[1]*3600)/60);		# Minutes
  $used[3] = $used[0]-$used[1]*3600-$used[2]*60;	# Seconds

  return @used[1..3];
}

#######################################################################

sub usage

{
  my $app_name = basename($0);
  print <<EOF;

Usage:
	$app_name [-h] [-v] [-rm] [-rr NUMBER] [-ot TYPE] [-c] [-z] [-na FACTOR] [-scl SCALE] [-off OFFSET] [-sd YYYY-MM-DD] [-ed YYYY-MM-DD] [-tr RES] [-mdt NAME:UNITS:FORMAT] [-f MASK_FILE] [-mt MT_FILE] DATA_CODE MASK_CODE[+MASK_2[+etc]] [NetCDF_OUT]

This code makes new or updates existing spatial aggregation of the MagicTable/*.init DATA_CODE dataset over MASK_CODE mask. File NetCDF_OUT can be given or taken from the Magic Table.

Options:

h	Display this help.
v	Verbose mode.
z	Allow polygon ID with zero value (no global summary will be done).
rm	Remove and do not update existing NetCDF_OUT file.
rmsk	Remove mask mapping file.
rr	Remove end NUMBER of records before updating NetCDF_OUT file.
sd	Start date for the aggregation.
ed	End   date for the aggregation.
tr	Pixel resolution RES for masking off vector polygons (in source units).
ot	Output data type (byte, short, long, float, double). Default is float.
c	Cumulative area aggregation: sum(data*area). Otherwise area weighted average (default).
na	No area or constant weight aggregations-
	Use FACTOR=1 if resolution of data and mask are the same.
mdt	Metadata to overwrite Magic Table values for NAME, UNITS, and FORMAT. Use "SAME" for no change.
mt	File path to an alternative Magic Table file.
f	Mask key file (*.msk). If this option present and
	the file does not exist it will be created for future use.
	If this option is not present then the mask key file path
	will be created by RIMS rule.
scl	Scale  for units conversion of the output data (use -mtd to change metadata).
off	Offset for units conversion of the output data (use -mtd to change metadata).

EOF
  exit;
}

#######################################################################

sub get_credits
{
  my %attrib  = (
	'institution'	=> 'Water Systems Analysis Group (WSAG), the University of New Hampshire (UNH)',

	'alexp'		=> 'alex.proussevitch@unh.edu',
	'dgrogan'	=> 'Danielle.Grogan@unh.edu',
	'dwisser'	=> 'dwisser@uni-bonn.de',
	'lammers'	=> 'Richard.Lammers@unh.edu',
	'sasha'		=> 'alex.shiklomanov@unh.edu',
	'shan'		=> 'Shan.Zuidema@unh.edu',
	'stanley'	=> 'Stanley.Glidden@unh.edu'
	);

  my ($user,$name) =  (getpwuid($<))[0,6];
            $name  =~ s/,.*//;
  return [$user, $name,	$attrib{$user}		|| 'email unknown',
			$attrib{institution}	|| 'unknown'];
}

#######################################################################
