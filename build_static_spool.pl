#!/usr/bin/perl -w

#######################################################################
#
#	This code builds GDAL warped static (not a time series) data files
#	for a given wbm run ID in the WBM spool area.
#	Output files are written in NetCDF or GeoTiff formats.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	August 2016
#	Modified-
#
#	Compliant with WBM version '16.8.1'
#
#######################################################################

use strict;
use File::Basename;
use File::Path;
use FileHandle;
use Getopt::Long;
use Math::Trig qw/pi/;
use Parallel::ForkManager;
use PDL;
use PDL::Image2D;
use PDL::IO::FlexRaw;
use PDL::NetCDF;	### Must come before Geo::GDAL (?)
use PDL::NiceSlice;
use Geo::GDAL;
use Fcntl;
use POSIX qw/tmpnam/;
use Time::JulianDay;
use Time::DaysInMonth;
use RIMS;		### WSAG UNH module
use RIMS::DataSet;	### WSAG UNH module

my ($init_file, $io_file)	= (script_dir().'pdl_wbm_path.init', script_dir().'pdl_wbm_io.pl');
{			### pdl_wbm_io.pl must be in the same directory
  local @ARGV	= ($init_file);
  require $io_file;
}			### pdl_wbm_path.init must be in the same directory
my %path	  = read_init($init_file);

use vars qw(*OLDERR);		# To avoid silly message-
open OLDERR, ">&STDERR";	# "No UNIDATA NC_GLOBAL:Conventions attribute"

STDOUT->autoflush(1);		# Disable buffering and some warnings
no if $] >= 5.017011, warnings => 'experimental::smartmatch';

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose,$remove) = (0, 0, 0);
					# Get command line options
usage() if !GetOptions(
	'h'=>\$help, 'v'=>\$verbose, 'rm'=>\$remove) or $help;

my $runID		= shift() or usage();
my %runIO		= read_attrib($path{runIO_list},$runID,'ID');
   $runIO{spool}	= $path{spool_dir}.fileBaseName($runIO{Network}).'/static/';
   $runIO{MagicT}	= $path{MT_file};

rmtree (   $runIO{spool}) if $remove;	### Remove destination files, if requested
unless (-e $runIO{spool}) { mkpath($runIO{spool},0,0775) or die "Cannot create-\n$runIO{spool}\n"; }
			### Generic dataset metadata
my %meta = ('Var_Scale' => 1, 'Var_Offset' => 0, 'Processing' => '', 'Projection' => 'epsg:4326');

if ($verbose) {
  print "\nBuilding static spool files-\n";
  print   "Run ID:		$runID\n";
  print   "Spool directory:	$runIO{spool}\n\n";
}

#######################################################################
##################     Read Input Data        #########################

my $extent	= get_extent($runIO{Network}, $runIO{Projection});
my($lon,$lat)	= lonLat($extent);
my $data_list	= get_data_list(\%runIO);	# List of data to process

#######################################################################
#############   Build NetCDF Spool Files  #############################

foreach my $key (@{$$data_list{Static}{array}}) {
  next unless $$data_list{Static}{$key};

  print "   Processing $key\n" if $verbose;
  my $dir = $runIO{spool}."$key/";
  unless (-e $dir) { mkpath($dir,0,0775) or die "Cannot create-\n$dir\n" }

	#######################################################
	#####	Special case of LandCover	###############

  if ($key eq 'LandCover') {
    my $landCover = get_landCover($runIO{$key});
    foreach my $lc (keys %{$landCover}) {
     (my $file_out  = basename(trim_filename($$landCover{FractionFile}))) =~ s/\.\w+$/.tif/;
      write_tif($extent, 'Int16', $dir.$file_out, $$landCover{data});
      print "\tLandCover = $lc\n" if $verbose;
  }}

	#######################################################
	#####	Special case of LivestockParam	###############

  if ($key eq 'LivestockParam') {
    my $liveStock = stock_init($$data_list{Static}{$key});
    foreach my $ls (keys %{$liveStock}) {
     (my $file_out  = basename(trim_filename($$liveStock{$ls}{DensityFile}))) =~ s/\.\w+$/.tif/;
      write_tif($extent, 'Float32', $dir.$file_out, $$liveStock{$ls}{data});
      print "\tLivestock = $ls\n" if $verbose;
  }}

	#######################################################
	#####	Special case of lapseDownscale	###############

  elsif ($key eq 'lapseDownscale') {
    my $airTLapse	= -6.4;
    my $elDataset	= new RIMS::DataSet(attrib($runIO{lapseDownscale},$runIO{MagicT}));
	# Check existance of the source file
   (my  $file_dat = $$extent{file}) =~ s/\.\w+$/.elvAverg.dat/;
   (my  $file_tif = $$extent{file}) =~ s/\.\w+$/.elvAverg.tif/;
    unless (-e $file_dat) {
	die "Elevation Average file not found..." unless -e $file_tif;
      writeflex($file_dat, read_raster($file_tif));
    }
		### Read data
    my $elevPixel	= readflex($file_dat);		# m
    my $elevClimt	= read_GDAL($extent,$elDataset->{MT_attrib},1,$elDataset->{fileList}[0][0],1,0);
    my $elDelta		= $airTLapse/1000 * ($elevPixel - $elevClimt);
		### Write data
    write_tif($extent, 'Float32', $dir.'elDelta.tif', $elDelta);
  }

	#######################################################
	#####	General case	###############################

  else {
    my($resample, $path_value, $type) = (1, undef, 'Float32');
      ($resample, $path_value, $type) = (0, 1, 'Int16') if m/$key/ ~~ qw(openWater);	# Exceptions
    my $file_in	= $$data_list{Static}{$key};
   (my $file_out	= basename(trim_filename($file_in))) =~ s/\.\w+$/.tif/;
		### Read/Write data
    my $data	= read_GDAL($extent,\%meta,$resample,$file_in,1,$path_value);
    write_tif($extent, $type, $dir.$file_out, $data);
  }
}

if ($verbose) {
  print "\nList of time series datasets for reference only (not processed):\n";
  foreach my $key (@{$$data_list{TimeSeries}{array}}) {
    print "   $key\n" if $$data_list{TimeSeries}{$key};
  }
}

#######################################################################
print "\nAll Done!\n" if $verbose;

close OLDERR;
exit;

#######################################################################
######################  Functions  ####################################

sub get_data_list
{
  my $runIO = shift;
  my %list;

	###############################################################
	### Collect data list by the Run Table sections

  $list{TimeSeries}{array} =
	###############################################################
	###	Section			Subsection	###############
  [qw(		MT_Precip
		precipFraction
		MT_airT
					IrrFrequency
		FreeWaterEvap
		glacierMelt
		glacierArea
					cloudFr windU windV humidity
		MT_LAI
					PopDensity DomDemandPP IndDemandPP
		CropAreaFrac
		CropParFile
		WM_mod_precip
		WM_obs_precip
		WM_mod_airT
		WM_obs_airT
  )];

  $list{Static}{array} =
	###############################################################
	###	Section			Subsection	###############
  [qw(					smReservoirFile
		soilAWCapacity
		FieldCap
		WiltingPoint
		rootingDepth
		PET_Factor
		lapseDownscale
					canopy gw_temp
					maskFile basinIDFile maskList
		Impervious
		openWater
		LandCover
					LivestockParam
		IrrEfficiency
		SW_GW_ratio
  )];
	###############################################################

  map $list{TimeSeries}{$_}	= $$runIO{$_}, @{$list{TimeSeries}{array}};
  map $list{Static}{$_}		= $$runIO{$_}, @{$list{Static}    {array}};

	### Reservoirs
  if (my %param = read_param_str($$runIO{Reservoirs})) {
    map $list{TimeSeries}{$_}	= $param{$_}, qw(IrrFrequency);
    map $list{Static}{$_}	= $param{$_}, qw(smReservoirFile);
  }
	### Water Temperature
  if (my %param = read_param_str($$runIO{waterTemperature})) {
    map $list{TimeSeries}{$_}	= $param{$_}, qw(cloudFr windU windV humidity);
    map $list{Static}{$_}	= $param{$_}, qw(canopy gw_temp);
  }
	### Runoff Mask
  if (my %param = read_param_str($$runIO{Runoff_mask})) {
    map $list{Static}{$_}	= $param{$_}, qw(maskFile basinIDFile maskList);
  }
	### Water Demand
  if (my %param = read_param_str($$runIO{WaterDemand})) {
    map $list{TimeSeries}{$_}	= $param{$_}, qw(PopDensity DomDemandPP IndDemandPP);
    map $list{Static}{$_}	= $param{$_}, qw(LivestockParam);
  }

  return \%list;
}

#######################################################################

sub get_landCover
{			# This sub uses global vars ($extent, %meta)
  my %landCover;
		### Read Land Cover parameter inputs
  my($hdr,@data)= read_table(shift());
  my $cCol	= delete $$hdr{Description};
  foreach my $row (@data) {
    $landCover{$$row[$cCol]}	= {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
  }
		### Read Land Cover data (LC pixel fraction)
  foreach my $lc (map $$_[$cCol],@data) {
    $landCover{$lc}{data}	= read_GDAL($extent,\%meta,0,$landCover{$lc}{FractionFile},1,0);
  }
  return \%landCover;
}

#######################################################################

sub stock_init
{			# This sub uses global vars ($extent, %meta)
  my ($hdr,@data) = read_table(shift());
  my %LiveStock;

  my $cCol = delete $$hdr{Stock};
  foreach my $row (@data) {
    $LiveStock{$$row[$cCol]}	= {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
  }
		### Read Livestock data
  map	$LiveStock{$_}{data} =
	read_GDAL($extent,\%meta,1,$LiveStock{$_}{DensityFile},1,0.0 ), keys(%LiveStock);

  return \%LiveStock;
}

#######################################################################

sub trim_filename
{
  my $str = shift;
  return $str =~ m/.+:(.+):.+/ ? $1 : $str;
}

#######################################################################

sub script_dir
{
  $0=~/^(.+[\\\/])[^\\\/]+[\\\/]*$/;
  return $1 || "./";
}

#######################################################################

sub usage

{
  my $app_name = basename($0);
  print <<EOF;

Usage:
	$app_name [-h] [-v] [-rm] RUN_ID

This code builds GDAL warped static (not a time series) data files
for a given wbm run ID in the WBM spool area.

Options:

h	Display this help.
v	Verbose mode.
rm	Remove existing spool files.

Example:
$app_name -v merra_real

EOF
  exit;
}

#######################################################################
