#!/usr/bin/perl

#######################################################################
#
#       All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      (C) University of New Hampshire and Water Systems Analysis Group 2008-2019
#
#######################################################################

#######################################################################
#
#	This code makes a temporal aggregation of data.
#	This code uses PDL (Perl Data Language) that saves CPU time
#		(>10 times faster).
#
#	Source dataset is taken from the Magic Table.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	October 2010
#		History-
#	Feb 2011	Sigma caculations are added
#	Apr 2014	Changing data type to double for lon and lat
#	May 2014	Added credits to output files, NetCDF v4 support
#	Dec 2014	Added georeferencing to output NetCDF files; some bugs fixed
#	Oct 2015	"decadal" destination time series is added
#	Jul 2016	Adding "flip" in Processing (Magic Table)
#	Nov 2016	Adding flag to alternative Magic Table file, and
#			Aggregation range to NetCDF Global attributes
#	Dec 2016	Scale and offset for the output data
#	Jan 2017	Creating output files with data band > 1
#	Jun 2017	Removing old NetCDF module
#	Oct 2017	Making correct lon/lat vars/units based on projection type- NetCDF CF-1.6
#	Jan 2019	Option to patch nodata in the input layers with a given value
#	Feb 2019	Added use of "*.init" files for dataset IDs aling with Magic Table
#	Apr 2021	Added option to output files in nc3 for classic (v.3) NetCDF format
#	Aug 2021	Removed Proj4 module dependencies.
#	Aug 2022	Transition to GDAL FFI interface.
#	Oct 2023	Added "no aggregation" option
#
#	Version- 23.10.0	(YY.M.#) - Small change if not listed above
#
#######################################################################

use strict;
use Getopt::Long;
use File::Basename;
use File::Path;
use FileHandle;
use Geo::GDAL::FFI;
use PDL;
use PDL::Char;
use PDL::IO::FlexRaw;
use PDL::NetCDF;
use Time::JulianDay;
use Time::DaysInMonth;
use warnings;
use RIMS;		### WSAG UNH module

use vars qw(*NEWERR *OLDERR);	# To avoid silly warning messages from GDAL, such as
open NEWERR, ">/dev/null";	# "No UNIDATA NC_GLOBAL:Conventions attribute"
open OLDERR, ">&STDERR";

STDOUT->autoflush(1);		# Disable buffering
my $credits = get_credits();	# Credits to output file metadata

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose,$remove,$rm_rec,$agg_type,$sclOffs,$patch,$trim,$push,$dcCheck,$mdt,$type,$s_date,$e_date,$MT_file,$nc3) =
   (  0,     0,       0,      0,       0,      undef,  'N/A',   0,   0,     0,     '',   '',    '',     ''  ,   '',    0  );
# print "$sclOffs[0] $sclOffs[1] $agg_type\n";
						# Get command line options
usage() if !GetOptions('h'=>\$help, 'v'=>\$verbose, 'rm'=>\$remove, 'rr=i'=>\$rm_rec, 'a=i'=>\$agg_type, 'so=i{2}'=>\@$sclOffs, 'trim'=>\$trim, 'pd'=>\$push, 'ndc'=>\$dcCheck, 'mdt=s'=>\$mdt, 'ot=s'=>\$type, 'sd=s'=>\$s_date, 'ed=s'=>\$e_date, 'mt=s'=>\$MT_file, 'p=f'=>\$patch, 'nc3'=>\$nc3) or $help;

my $d_code  = shift() or usage();	# Source
my $m_code  = shift() or usage();	# Destination
   $sclOffs = [1,0] unless scalar @$sclOffs;

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

my @agg_type = qw/Average Cumulative None/;
die "\"-rr NUMBER\"- NUMBER must be positive...\n" if $rm_rec < 0;

#######################################################################
#######################################################################

my $time_start = time();
my @loc_time_0 = localtime($time_start);		# Report Time
$loc_time_0[5] += 1900; $loc_time_0[4]++;
printf "\nThe job started on %04d-%02d-%02d at %02d:%02d:%02d.\n\n",
	reverse(@loc_time_0[0..5]) if $verbose;

#######################################################################
			### Other Initializations

my $names_file	= $MT_file ? $MT_file : get_file_path()->{names_file};
my %names_d	= attrib($d_code,$names_file);		# Source
my %names_m	= attrib($m_code,$names_file);		# Destination

	### Willmott-Matsuura test dates-
# $names_d{Start_Date} = '1960-01-00';
# $names_d{End_Date}   = '1989-12-00';
# $names_m{Data_Cube}  = $names_d{Data_Cube};

				#### Overwrite start and end dates if requested
if ($s_date) {
  my ($yr,$mo,$dy)	= $s_date=~m/(\d{4})-(\d{2})-(\d{2})/ ? ($1,$2,$3) :
	die "Start date is not in YYYY-MM-DD format\n";
  substr($names_m{Start_Date}, 0, 4, substr($s_date,0,4));
  substr($names_m{Start_Date}, 5, 2, substr($s_date,5,2)) if substr($names_m{Start_Date}, 5, 2) ne '00';
  substr($names_m{Start_Date}, 8, 2, substr($s_date,8,2)) if substr($names_m{Start_Date}, 8, 2) ne '00';
  $names_d{Start_Date}	= $s_date;
}
if ($e_date) {
  my ($yr,$mo,$dy)	= $e_date=~m/(\d{4})-(\d{2})-(\d{2})/ ? ($1,$2,$3) :
	die "End date is not in YYYY-MM-DD format\n";
  substr($names_m{End_Date}, 0, 4, substr($e_date,0,4));
  substr($names_m{End_Date}, 5, 2, substr($e_date,5,2)) if substr($names_m{End_Date}, 5, 2) ne '00';
  substr($names_m{End_Date}, 8, 2, substr($e_date,8,2)) if substr($names_m{End_Date}, 8, 2) ne '00';
  $names_d{End_Date}	= $e_date;
}
				### It also checks allowed aggregations
my $search = make_search_par($names_d{Time_Series},$names_m{Time_Series});
$agg_type  = 0 if $names_m{Time_Series} =~ m/_clim/;

die "NetCDF Variable Name is not defined in the Magic Table. Aborting...\n"
	unless $names_m{Var_Name};	unless ($dcCheck)  {
die "Source and Destination datasets are not in the same DataCube. Aborting...\n"
	unless $names_d{Data_Cube} eq $names_m{Data_Cube}; }

my @metadata =	split m/:/,$mdt;
$metadata[0] =	$names_d{Param_Name}	unless defined $metadata[0];
$metadata[1] =	$names_d{Orig_Units}	unless defined $metadata[1];
$metadata[2] =	$names_d{Round}		unless defined $metadata[2];
$metadata[3] =	$agg_type[$agg_type];
$metadata[4] =	$names_d{Time_Series} =~ m/daily\{\d+\}/ ? 'hourly' :
		$names_d{Time_Series} =~ m/([a-z]+)/ ? $1 : 'N/A';

#######################################################################

if ($verbose)
{
  print "Running: $0\n\n";

  printf  "Src. Data Code   = %s\n",$d_code;
  printf  "Dst. Data Code   = %s\n",$m_code;
  printf  "Aggregation Type = %s\n\n",$agg_type[$agg_type];
}

#######################################################################
#######     Build date and file lists for the source dataset    #######

trimDates($names_d{Start_Date},$names_d{End_Date})	# Trim dates
	if $trim && $names_m{Time_Series}!~m/_clim/;

my ($j_date_list,$date_list) = make_date_list($names_d{Start_Date},$names_d{End_Date},\%names_d);
my $file_list	= make_file_list($j_date_list,\%names_d,1e10,'0_0');

		### Get data compression parameters
(my $test_file	= $$file_list[0][0]) =~ s/NETCDF:(.+):$names_d{Var_Name}/$1/;
my ($netcdf_scale,$netcdf_offset,$netcdf_nodata) =
	get_netcdf_compression($test_file,$names_d{Var_Name},$names_d{Processing});
my $compression	= [$netcdf_scale,$netcdf_offset,$patch];

#######################################################################
#####     Build date and file lists for the destination dataset    ####

						# Push destination dates
pushDates($names_d{Start_Date},$names_d{End_Date},$names_d{Time_Series},
	  $names_m{Start_Date},$names_m{End_Date},$names_m{Time_Series})
	if $push;

my ($j_Date_List,$date_List) = make_date_list($names_m{Start_Date},$names_m{End_Date},\%names_m);
my @nc_Time   = map {$_-julian_day(1900,1,1)} @$j_Date_List;
my $file_List = make_file_list($j_Date_List,\%names_m,1e10,'0_0');

		### Remove destination files, if requested
if ($remove) {
  map { (my $path = $$_[0]) =~ s/.+:(.+):.+/$1/; unlink($path); } @$file_List;
}

		### Get list of existing dates in the destination dataset
my ($j_dates,$dates) =
	chk_date_list($j_Date_List,$date_List,\%names_m,1e10,'0_0');

		### Roll Back records
($j_dates,$dates) = (undef,undef) if $rm_rec >= scalar(@$j_dates);
(splice(@$j_dates,-$rm_rec),splice(@$dates,-$rm_rec)) if $rm_rec;

#######################################################################
###################   Data aggregation   ##############################

if ($#$date_List == $#$dates) {
  print "Destination dataset is up to date! Nothing to do...\n\n" if $verbose;
  exit;
}

my $count = -1;

foreach my $file (@$file_List)
{
  (my $path = $$file[0]) =~ s/.+:(.+):.+/$1/;	### Strip NetCDF extras

  foreach my $band (@{$$file[1]})
  {
    if (++$count <= $#$j_dates) {
      die sprintf("Missing aggregate record for %s in %s\n",$$dates[$count],$path)
		if $$j_Date_List[$count] != $$j_dates[$count];
      next;
    }
    else {
      my  $time_value	= $names_m{Time_Series} =~ m/daily[\w]*\{\d+\}/ ?
			  sprintf("%.0f",$nc_Time[$count]*24) : $nc_Time[$count];
      my  $f_list	= trim_file_list($file_list,$date_list,$$date_List[$count],$search);
      my ($info,$gT,$data,$sigma) = aggregate($f_list,$agg_type,$compression);

		### Scale/Offset the output data if requested
      if (scalar @$sclOffs) {
	$data  = $$sclOffs[0]*$data  + $$sclOffs[1];
	$sigma = $$sclOffs[0]*$sigma + $$sclOffs[1];
      }
		### Flip data row order in case of "flip" in MT "Processing"
      if ($names_d{Processing} =~ m/flip/i) {
	$data	= $data ->slice(':,-1:0');
	$sigma	= $sigma->slice(':,-1:0');
      }
		### Case of no aggregation
	$sigma	= 0 if $agg_type[$agg_type] eq 'None';

		### Save aggregation data
      if (-e $path) {
	update_nc($path,$names_m{Var_Name},$data,$sigma,$time_value,$band);
	printf "\tFile- %s: updated (%s)\n",basename($path),$$date_List[$count] if $verbose;
      }
      else {
	my ($lon,$lat) = make_Lon_Lat($info,$gT);
			### Pad missing bands with Nodata
	if ($band > 1) {
	  my $nodata = $data*0 + $$info[2];
	  write_nc($path,$$info[2],$lat,$lon,$nodata,$nodata,$type,$info,\%names_d,\%names_m,\@metadata,$credits,$gT,$nc3);
	  printf "\tFile- %s: written (%s)\n",basename($path),'Nodata, band=1' if $verbose;
	  foreach my $b (2 .. $band-1) {
	    update_nc($path,$names_m{Var_Name},$nodata,$nodata,$$info[2],$b);
	    printf "\tFile- %s: updated (%s)\n",basename($path),"Nodata, band=$b" if $verbose;
	  }
			### Save good data
	  update_nc($path,$names_m{Var_Name},$data,$sigma,$time_value,$band);
	  printf "\tFile- %s: updated (%s)\n",basename($path),$$date_List[$count] if $verbose;
	}
	else {
# 	die "Not coded to create NetCDF file with BAND # gt 1..." if $band != 1;
	  write_nc($path,$time_value,$lat,$lon,$data,$sigma,$type,$info,\%names_d,\%names_m,\@metadata,$credits,$gT,$nc3);
	  printf "\tFile- %s: written (%s)\n",basename($path),$$date_List[$count] if $verbose;
	}
      }
    }
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

sub make_search_par
{
  my ($ts_d,$ts_m) = @_;

  my %search = (		#####   Date Search Part of YYYY-MM-DD :
	'hourly'	=> [0,15],	### YYYY-MM-DDTHHMM
	'hourly_clim'	=> [5,15],	### MM-DDTHHMM
	'daily'		=> [0,10],	### YYYY-MM-DD
	'daily_clim'	=> [5,5],	### MM-DD
	'monthly'	=> [0,7],	### YYYY-MM
	'monthly_clim'	=> [5,2],	### MM
	'yearly'	=> [0,4],	### YYYY
	'yearly_clim'	=> [4,1],	### -
	'decadal'	=> [0,3]	### YYY
  );

  my $tsD = $1 if $ts_d =~ m/^(\w+)/;
  my $tsM = $1 if $ts_m =~ m/^(\w+)/;
  $tsD = 'hourly'      if $ts_d =~ m/daily\{/;
  $tsM = 'hourly_clim' if $ts_m =~ m/daily_clim\{/;
  die "Unknown time series- \"$ts_d\"\n" unless defined $tsD;
  die "Unknown time series- \"$ts_m\"\n" unless defined $tsM;
  die "Unknown time series- \"$tsD\"\n"  unless defined $search{$tsD};
  die "Unknown time series- \"$tsM\"\n"  unless defined $search{$tsM};

		### Check allowed aggregations
  die "Cannot aggregate \"$tsM\" from \"$tsD\"...\n"
	if !(	($tsM eq 'hourly_clim'	&& $tsD=~m/hourly$/)		||
		($tsM eq 'daily'	&& $tsD=~m/hourly$/)		||
		($tsM eq 'daily_clim'	&& $tsD=~m/daily$/)		||
		($tsM eq 'monthly'	&& $tsD=~m/daily$/)		||
		($tsM eq 'monthly_clim'	&& $tsD=~m/monthly$/)		||
		($tsM eq 'yearly'	&& $tsD=~m/(daily|monthly)$/)	||
		($tsM eq 'yearly_clim'	&& $tsD!~m/clim$/)		||
		($tsM eq 'decadal'	&& $tsD=~m/(daily|monthly|yearly)$/));

  return $search{$tsM};
}

#######################################################################

sub aggregate
{
  my ($list,$agg_type,$compr) = @_;
  my ($agg_info,$agg_gT,$agg_data,$agg_data_sq);
  my $count = 0;

  foreach my $file (@$list)
  {
    foreach my $band (@{$$file[1]})
    {
# print "($$file[0],$band)\n";
      my ($info,$gT,$data) = read_GDAL($$file[0],$band,$compr);

      if ($count++) {
	$agg_data    += $data;
	$agg_data_sq += $data**2;
      }
      else { ($agg_info,$agg_gT,$agg_data,$agg_data_sq) = ($info,$gT,$data,$data**2); }
    }
  }	### "lclip" is just in case since veryrarely it makes very small (-1e-6) negative numbers
  my $agg_sgma = 1/$count * sqrt(($count*$agg_data_sq-$agg_data**2)->lclip(0));

			### Average
  if ($agg_type == 0) {
    $agg_data /= $count;
  }			### Cumulative
  elsif ($agg_type == 1 || $agg_type == 2) {}
			### Unknown
  else { die "Unknown aggregation method...\n"; }

  $agg_data->inplace->setbadtoval($$agg_info[2]);
  $agg_sgma->copybad($agg_data)->setbadtoval($$agg_info[2]);
  $agg_sgma=$agg_sgma->convert($agg_data->type);

  return $agg_info,$agg_gT,$agg_data,$agg_sgma;
}

#######################################################################

sub read_GDAL
{
  my ($file,$b,$compr) = @_;

 (my  $file_s = $file) =~ s/.+:(.+):.+/$1/;
  die "Requested file (in read_GDAL)-\n$file_s\ndoes not exist...\n" unless -e $file_s;

  open STDERR, ">/dev/null";
    my $geotiff_data = Geo::GDAL::FFI::Open( $file, {Flags => ['READONLY']} );
  open STDERR, ">&OLDERR";

  my $geoTransform = $geotiff_data->GetGeoTransform;
  my $band         = $geotiff_data->GetBand($b);
  die "Failed to get geoTransform. Uneven grid is likely the problem. Aborting...\n"
	if join('', @$geoTransform) eq '010001';

  my($x_size, $y_size) = $band->GetSize;
  my $pdl	= $band->GetPiddle;		# Note, GetPiddle(...) can do resample too
  my $nodata	= $band->GetNoDataValue;
     $nodata	= $netcdf_nodata unless defined $nodata;

		### Uncompress data and nodata
     $nodata	= $nodata*$$compr[0]+$$compr[1];
     $pdl	= $pdl*$$compr[0]+$$compr[1];
     $pdl	= $pdl->setbadif(abs($pdl-$nodata) < 1e-12);
     $pdl	= $pdl->setbadtoval($$compr[2]) if $$compr[2] ne 'N/A';

  return [$x_size,$y_size,$nodata],$geoTransform,$pdl;
}

#######################################################################

sub make_Lon_Lat
		### This sub makes coords for CENTER of grid cell
{
  my ($info,$gT) = @_;

  my @lon = map {$$gT[0]+$gT->[1]*($_+0.5)} 0..$$info[0]-1;
  my @lat = map {$$gT[3]+$gT->[5]*($_+0.5)} 0..$$info[1]-1;
     @lat = reverse @lat;

  return \@lon,\@lat;
}

#######################################################################

sub write_nc
{
  my ($file,$time,$lat,$lon,$data,$sigma,$type,$info,$names_d,$names_m,$metadata,$credits,$gT,$nc3) = @_;

			### Check credits
  $credits = [(getpwuid($<))[0,6],'email unknown','unknown'] unless $credits;

			### Check calendar
  my $calendar	= $$names_m{Processing} =~ m/calendar=(\d+)/i ? $1 : 366;
  my %calendar_str = (366 => 'standard', 365 => '365_day', 360 => '360_day');

			### Prepare output directory
  my $dir_out = dirname($file);
  unless (-e $dir_out) {
    mkpath($dir_out,0,0775) or die "Cannot make directory...\n$dir_out\n";
  }

			###  Prepare variables
  my $var_name	= $$names_m{Var_Name};
  my $sig_name	= $var_name.'_sigma';
  my $fill_val	= eval(sprintf("%s([%s])",$data->type,$$info[2]));
  my $proj_crs	= crs_to_Obj($$names_m{Projection});
  my $wkt	= $proj_crs->Export('Wkt');		### Convert to Wkt fromat
  my($xVr,$yVr)	= Geo::GDAL::FFI::OSRIsGeographic($$proj_crs) ? qw(lon lat) : qw(x y);
  my $TS_str	= parse_TS($$names_m{Time_Series});

			###  Create NetCDF attributes
  my %var_att	= (
	'missing_value'	=> $fill_val,
	'long_name'	=> $$metadata[0],
	'units'		=> $$metadata[1],
	'query_format'	=> $$metadata[2],
	'aggregation'	=> $$metadata[3],
	'source_TS'	=> $$metadata[4],
	'grid_mapping'	=> 'crs'
  );

  my %sig_att	= (
	'missing_value'	=> $fill_val,
	'long_name'	=> "Sigma of $$metadata[0]",
	'units'		=> $$metadata[1],
	'query_format'	=> $$metadata[2],
	'source_TS'	=> $$metadata[4],
	'grid_mapping'	=> 'crs'
  );
  my %crs_att	= (
	'srs_ref'	=> $$names_m{Projection},
	'srs_wkt'	=> $wkt,		### Attribute name convention used by CF
	'spatial_ref'	=> $wkt,		### Attribute name convention used by GDAL
	'GeoTransform'	=> join(' ',@$gT)
  );

  my %time_att	= (
	'units'		=> $TS_str =~ m/hour/ ? 'hours since 1900-01-01T00:00' : 'days since 1900-01-01',
	'long_name'	=> 'Time',
	'resolution'	=> $TS_str,
	'calendar'	=> $calendar_str{$calendar}
  );

  my %lon_att	= Geo::GDAL::FFI::OSRIsGeographic($$proj_crs) ? (
	'units'		=> 'degrees_east',	'long_name'	=> 'longitude',	'standard_name' => 'longitude') : (
	'units'		=> 'meters',		'long_name'	=> 'x coordinate of projection',
	'standard_name' => 'projection_x_coordinate' );

  my %lat_att	= Geo::GDAL::FFI::OSRIsGeographic($$proj_crs) ? (
	'units'		=> 'degrees_north',	'long_name'	=> 'latitude',	'standard_name' => 'latitude' ) : (
	'units'		=> 'meters',		'long_name'	=> 'y coordinate of projection',
	'standard_name' => 'projection_y_coordinate' );

  my %global_att= (
	'Conventions'	=> 'CF-1.6',
	'title'		=> 'Temporal aggregation of data',
	'history'	=> 'Created on '.date_now()." by $$credits[1] ($$credits[2])",
	'projection'	=>  $$names_m{Projection},
	'Temporal_Res.'	=>  $TS_str,
	'NetCDF_driver'	=> 'NetCDF.' . $PDL::NetCDF::VERSION,
	'source'	=> 'Unreferenced Data',
	'institution'	=>  $$credits[3],
	'references'	=> 'https://www.wsag.unh.edu',
	'FilePath'	=>  $file,
	'System'	=>  get_file_path()->{SYSTEM},
		$TS_str =~ m/_clim/  ? (
	'AggrRangeStart'=> $$names_d{Start_Date},
	'AggrRangeEnd'	=> $$names_d{End_Date}) : ()
  );

			###  NetCDF File definitions
  my $nc = new PDL::NetCDF($file, {MODE => O_CREAT, REVERSE_DIMS => 1,
	NC_FORMAT => !$nc3 ? PDL::NetCDF::NC_FORMAT_NETCDF4 : PDL::NetCDF::NC_FORMAT_CLASSIC});
  my $slice_opt = {_FillValue => $fill_val};
    $$slice_opt{SHUFFLE} = 0 if !$nc3;
    $$slice_opt{DEFLATE} = 1 if !$nc3;

			###  Write Dimensions
  $nc->putslice('time',['time'],[PDL::NetCDF::NC_UNLIMITED()],[0],[1],long($time));
  $nc->putslice( $yVr, [$yVr],  [$$info[1]], [0], [$$info[1]], double($lat));
  $nc->putslice( $xVr, [$xVr],  [$$info[0]], [0], [$$info[0]], double($lon));
  $nc->putslice('crs', [],      [],          [0], [1],         PDL::Char->new(''));

			###  Write Data
  $nc->putslice($var_name,[$xVr,$yVr,'time'],[$$info[0],$$info[1],PDL::NetCDF::NC_UNLIMITED()],
	[0,0,0], [$data->dims,1], ($type ? $data->$type : $data)->slice(':,-1:0'), $slice_opt);
  $nc->putslice($sig_name,[$xVr,$yVr,'time'],[$$info[0],$$info[1],PDL::NetCDF::NC_UNLIMITED()],
	[0,0,0], [$data->dims,1], ($type ? $sigma->$type: $sigma)->slice(':,-1:0'),$slice_opt)
	if ref($sigma) eq 'PDL';

			###  Write Attributes
  foreach my $key (keys %global_att) { $nc->putatt ($global_att{$key}, $key); }
  foreach my $key (keys %time_att)   { $nc->putatt ($time_att{$key},   $key, 'time'); }
  foreach my $key (keys %lat_att)    { $nc->putatt ($lat_att{$key},    $key, $yVr); }
  foreach my $key (keys %lon_att)    { $nc->putatt ($lon_att{$key},    $key, $xVr); }
  foreach my $key (keys %var_att)    { $nc->putatt ($var_att{$key},    $key, $var_name); } if (ref($sigma) eq 'PDL') {
  foreach my $key (keys %sig_att)    { $nc->putatt ($sig_att{$key},    $key, $sig_name); } }
  foreach my $key (keys %crs_att)    { $nc->putatt ($crs_att{$key},    $key, 'crs'); }

  $nc->close();
}

#######################################################################

sub update_nc
{
  my ($file,$var_name,$data,$sigma,$time,$band) = @_;
  my $sig_name	= $var_name.'_sigma';
			### Open NetCDF file and write data
  my $ncobj = new PDL::NetCDF($file, {MODE => O_RDWR, REVERSE_DIMS => 1});
  $ncobj->putslice('time',   [], [], [$band-1],    [1],             long($time));
  $ncobj->putslice($var_name,[], [], [0,0,$band-1],[$data->dims,1], $data->slice(':,-1:0'), {SHUFFLE => 0, DEFLATE => 1});
  $ncobj->putslice($sig_name,[], [], [0,0,$band-1],[$data->dims,1],$sigma->slice(':,-1:0'), {SHUFFLE => 0, DEFLATE => 1})
	if ref($sigma) eq 'PDL';
  $ncobj->close();
}

#######################################################################

sub parse_TS
{
  my $MT_TS =  shift() . '';	# clone just in case
     $MT_TS =  sprintf "%d-hourly",24/$1 if $MT_TS =~ m/daily[\w]*\{(\d+)\}/;
     $MT_TS =~ s/^1-//;

  return $MT_TS;
}

#######################################################################

sub trim_file_list
{
  my ($list,$dates,$date,$search) = @_;

  my @dates	= map substr($_,$$search[0],$$search[1]),@$dates;
  my $str	= substr($date, $$search[0],$$search[1]);
  my @file_list	= (['',[0]]);
  my $count	= 0;

  foreach my $file (@$list)
  {
    foreach my $band (@{$$file[1]})
    {
      if ($dates[$count++] eq $str)		### Check the date match
      {
	if ($$file[0] eq $file_list[-1][0]) { push @{$file_list[-1][1]},  $band;   }
	else				    { push @file_list,[$$file[0],[$band]]; }
      }
    }
  }
  shift @file_list;
  die "No data to aggregate was found...\n" unless @file_list;

  return \@file_list;
}

#######################################################################

sub trimDates
{
  my @dateS = split m/-/,$_[0];
  my @dateE = split m/-/,$_[1];
  my $days  = days_in(@dateE[0,1]);

  if ($names_m{Time_Series} =~ m/monthly/) {
						### Start Date check
    $_[0] = sprintf("%04d-%02d-%02d",inverse_julian_day(julian_day(@dateS[0,1],1)+days_in(@dateS[0,1])))
	if $dateS[2] != 1;
						### End Date check
    $_[1] = sprintf("%04d-%02d-%02d",inverse_julian_day(julian_day(@dateE[0,1],1)-1))
	if $dateE[2] != $days;
  }

  if ($names_m{Time_Series} =~ m/yearly/) {
    my $dayS = ($names_d{Time_Series} =~ m/daily/) ?  1 : 0;
    my $dayE = ($names_d{Time_Series} =~ m/daily/) ? 31 : 0;
						### Start Date check
    $_[0] = sprintf("%04d-%02d-%02d",$dateS[0]+1,1,$dayS)
	if $_[0] !~ m/(01-01|01-00)$/;
						### End Date check
    $_[1] = sprintf("%04d-%02d-%02d",$dateE[0]-1,12,$dayE)
	if $_[1] !~ m/(12-31|12-00)$/;
  }

  die "Cannot trim dates...\n" if julian_day(split m/-/,$_[0]) >= julian_day(split m/-/,$_[1]);
}

#######################################################################

sub pushDates
{
  unless ($_[5] =~ m/_clim/) {
    if ($_[5] =~ m/monthly/) {
      ($_[3] = $_[0]) =~ s/\d{2}$/00/;
      ($_[4] = $_[1]) =~ s/\d{2}$/00/;
    }
    if ($_[5] =~ m/yearly/) {
      ($_[3] = $_[0]) =~ s/\d{2}-\d{2}$/00-00/;
      ($_[4] = $_[1]) =~ s/\d{2}-\d{2}$/00-00/;
    }
  }
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
	$app_name [-h] [-v] [-ot TYPE] [-a AGG_TYPE] [-so SCALE OFFSET] [-rm] [-nc3] [-rr NUMBER] [-p PATCH] [-trim] [-pd] [-mdt NAME:UNITS:FORMAT] [-sd YYYY-MM-DD] [-ed YYYY-MM-DD] [-mt MT_FILE] SRC_DATA_CODE DST_DATA_CODE

This code makes new or updates existing temporal aggregation of the MagicTable/*.init SRC_DATA_CODE dataset to DST_DATA_CODE dataset. DST_DATA_CODE files can be only in NetCDF format.

Options:

h	Display this help.
v	Verbose mode.
rm	Remove and do not update existing DST_DATA_CODE files.
rr	Remove end NUMBER of records before updating DST_DATA_CODE files.
sd	Start date (YYYY-MM-DD) for the aggregation.
ed	End   date (YYYY-MM-DD) for the aggregation.
ot	Output data type (byte, short, long, float, double). Default is float.
a	Aggregation type. TYPE is 0 for average (default), 1 for cumulative,
	  2 for "no aggregation" (e.g. adding CF-1.6 convention to NetCDF files).
so	Scale and offset the output data. Consider changing units with -mtd flag:
	  -mtd ":NEW_UNITS:"
p	Patch input datasets nodata with value PATCH.
trim	Discard incomplete start/end records in monthly/yearly aggregations.
pd	Push/overwrite destination dates to maximize aggregation range.
mdt	Metadata to overwrite Magic Table values for NAME, UNITS, and FORMAT.
mt	File path to an alternative Magic Table file.
ndc	Do not check DataCube field in the Magic Table.
nc3	Output files in classic (v.3) NetCDF format.

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
