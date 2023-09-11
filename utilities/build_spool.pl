#!/usr/bin/perl

#######################################################################
#
#	This code pre-builds GDAL warped PDL data files in the WBM spool area.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	October 2014
#	Modified-
#		August 2016	Added an option to build NetCDF files
#		December 2016	Added an option for patch dataset in addition to patch number
#		June/Nov 2017	Minor improvements/changes:
#				1) new format for "write_nc()" function argument list
#				2) additional option "-pp" for primary patch value
#		Jul 2021	Adopted to run from any location, i.e. system wide path to required files
#		Sep 2021	Added RIMS::WBM
#		Nov 2022	Added multi-day option. NB- It is not needed for the non-redundancy spool
#
#	Version:	23.8.0
#
#######################################################################

use strict;
use File::Basename;
use File::Path;
use File::Temp;
use FileHandle;
use Getopt::Long;
use Math::Trig qw/pi/;
use Parallel::ForkManager;
use PDL;
use PDL::Char;
use PDL::Image2D;
use PDL::IO::FlexRaw;
use PDL::NetCDF;	$ENV{GDAL_NETCDF_IGNORE_XY_AXIS_NAME_CHECKS} = "YES";	# required for GDAL > 3.0
use PDL::NiceSlice;
use Fcntl;
use Storable qw/dclone/;
use Time::JulianDay;
use Time::DaysInMonth;
use warnings;
use RIMS;		### WSAG UNH module set
use RIMS::DataSet;
use RIMS::WBM;

my $PATH = get_file_path();
my %path = get_conf();

use vars qw(*OLDERR);		# To avoid silly message-
open OLDERR, ">&STDERR";	# "No UNIDATA NC_GLOBAL:Conventions attribute"

STDOUT->autoflush(1);		# Disable buffering

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose,$remove,$forks,$resample,$patch,$PATCH,$nc,$proj,$s_date,$e_date,$mDay,$spool_dir,$MT_file) = (
      0,      0,       0,    12,      1,     undef, undef,0,'epsg:4326','',  ''    ,  '' ,     ''   ,   ''   );
my ($spDelta) = (
      0     );

					# Get command line options
usage() if !GetOptions(
	'h'=>\$help, 'v'=>\$verbose, 'rm'=>\$remove, 'f=i'=>\$forks, 'r=i'=>\$resample, 'p=s'=>\$patch,
	'pp=s'=>\$PATCH, 'nc'=>\$nc, 'proj=s'=>\$proj, 'sd=s'=>\$s_date, 'ed=s'=>\$e_date, 'md=s'=>\$mDay,
	'spDir=s'=>\$spool_dir, 'mt=s'=>\$MT_file, 'spDelta'=>\$spDelta ) or $help;

my $networkFile	= shift() or usage();
my $dataID	= shift() or usage();

my $patchFlag	= defined($patch) && !isNumber($patch);	# Flag to patch with secondary dataset
   $patch	= $PATCH if defined($PATCH) && !defined($patch);
my $patchAtt;
   $MT_file	= $path{MT_file} || $$PATH{names_file} unless $MT_file;	# Custom_MT otherwise Global_MT

my $pm		= new Parallel::ForkManager($forks);	# Parallel Processing object
die "Start date is not in YYYY-MM-DD format\n"	if $s_date && $s_date !~ m/^\d{4}-\d{2}-\d{2}$/;
die "End   date is not in YYYY-MM-DD format\n"	if $e_date && $e_date !~ m/^\d{4}-\d{2}-\d{2}$/;
die "Multi-day string format is wrong\n"	if $mDay   && $mDay   !~ m/^\[[\d,]+lastDay\]$/;

#######################################################################
##################     Read Input Data        #########################

my $extent	= get_extent($networkFile, $proj, {INDEX=>1});
my $grid	= [lonLat($extent), $$extent{gTransform}, $$extent{projection}];

		### Primary dataset
my $dataAtt	= attrib($dataID, $MT_file);
  $$dataAtt{End_Date} = $e_date if $e_date && $e_date le $$dataAtt{End_Date};
my $option	= {START_YEAR_CLIP =>  substr($s_date,0,4)} if $s_date;
  $$option{SP_DELTA}  = 1	if $spDelta;
my $dataSet	= new RIMS::DataSet($dataAtt, $option);		# Create the dataSet object
   $spool_dir	= $path{spool_dir} unless $spool_dir;
   $spool_dir  .= '/' if $spool_dir && $spool_dir !~ m/\/$/;	# Add closing slash to the path string
   $spool_dir  .= fileBaseName($networkFile) . "/$$dataAtt{Code_Name}";
my $DtSrchOpt	= {YR_UP=>1} if $$dataAtt{Name} =~ m/Glacier (area|volume)/i;
		### Dataset dates
my  $dates;
if ($mDay && $$dataAtt{Time_Series} =~ m/^daily$/i) {	# Note: -md ($mDay) is not needed for the non-redundancy spool
  my $dataAttMd	= dclone($dataAtt);
    $$dataAttMd{Time_Series} .= $mDay;
  my $dataSetMd	= new RIMS::DataSet($dataAttMd,{START_YEAR_CLIP=>substr($$dataAtt{Start_Date},0,4)});
  $dates	= $dataSetMd->{dateList}; } else {
  $dates	= $dataSet  ->{dateList};
}
my @date_groups	= mDate_groups($dates);

		### Secondary (patch) dataset
if ($patchFlag) {
   $patch	= new RIMS::DataSet(attrib($patch, $MT_file));
   $patchAtt	= $patch->get('MT_attrib');
   $spool_dir  .= '_'.$$patchAtt{Code_Name};
}

if ($verbose) {
  print "\nBuilding spool files-\n";
  print   "Data ID Input:\t	$dataID\n";
  print   "Data ID:		$$dataAtt{Code_Name}\n";
  print   "Network:		$networkFile\n";
  print   "Spool directory:	$spool_dir\n\n";
}

#######################################################################
#############   Build Spool   #########################################

unless (-e $spool_dir) { mkpath($spool_dir,0,0775) or die "Cannot make directory...\n$spool_dir\n"; }
unlink <$spool_dir/*> if $remove;	### Remove destination files, if requested

foreach my $mGroup (@date_groups) {
  $pm->start and next;			### Run fork processes
  foreach my $date (@$mGroup) {
    next if ($s_date &&  $date  lt $s_date) || ($e_date && $date gt $e_date);
    print "\r\tProgress: $date" if $verbose;

		### Build the warped PDL data to write to spool
    my $file = sprintf "$spool_dir/%s.dat",$dataSet->searchDate($date,3);
    unless (-e $file) {
      my $data;

      if (!$dataSet->{spDelta} || $date =~ m/-01$/) { 
	$data =	read_GDAL($extent,$dataAtt, $resample,$dataSet->dateLayer($date,$DtSrchOpt), $patchFlag ?
		read_GDAL($extent,$patchAtt,$resample,$patch  ->dateLayer($date,$DtSrchOpt), $PATCH)    : $patch);
	writeflex($file, $data->flat->index($$extent{indx}));
      } else {
		######################################
		### Case of Delta spool write
	my @date_prev	= $dataSet->{date} ?	split(m/-/, $dataSet->{date}) : (0,0,0);
	my @date	=			split m/-/, $date;
			### First day of month no-Delta spool write
	if ($date[1] != $date_prev[1]) {
	  (my $DATE = $date)	=~ s/\d{2}$/01/;
	  $file			=~ s/\d{2}\.dat$/01.dat/;
          $data=read_GDAL($extent,$dataAtt, $resample,$dataSet->dateLayer($DATE,$DtSrchOpt), $patchFlag ?
		read_GDAL($extent,$patchAtt,$resample,$patch  ->dateLayer($DATE,$DtSrchOpt), $PATCH)    : $patch);
	  writeflex($file, $data->flat->index($$extent{indx}));
	  @date_prev		= (@date[0,1],1);
	  $dataSet->{data}	=  $data;
	}
			### Other days of month Delta spool write
	for (my $day=$date_prev[2]+1; $day<=$date[2]; $day++) {
	  (my $DATE = $date)	=~ s/\d{2}$/sprintf("%02d",$day)/e;
	  $file			=~ s/\d{2}\.dat$/sprintf("%02d",$day).".dat"/e;
	  $data=read_GDAL($extent,$dataAtt, $resample,$dataSet->dateLayer($DATE,$DtSrchOpt), $patchFlag ?
		read_GDAL($extent,$patchAtt,$resample,$patch  ->dateLayer($DATE,$DtSrchOpt), $PATCH)    : $patch);
	  my $delta		= $data->flat - $dataSet->{data}->flat;
	  my $idx		= which($delta != 0);
	  writeflex($file, $delta->index($idx), $idx);
	  $dataSet->{data}	=  $data;
        }
      }
      $dataSet->{data} = $data;
      $dataSet->{date} = $date;
    }
  }
  $pm->finish;
}
$pm->wait_all_children;

#######################################################################
#############   Build NetCDF files	###############################

sub mDate_groups	# Groups of dates by each monthly TS
{
  my $dates = shift();
  my $mDate = '9999-99';
  my @groups;

  foreach my $date (@$dates) {
    my  $yyyy_mm = substr($date,0,7);
    if ($yyyy_mm ne $mDate) {
		push   @groups,     [$date];	$mDate = $yyyy_mm;
    } else {	push @{$groups[-1]}, $date; }
  }
  return @groups;
}

#######################################################################

if ($nc) {
  print "\n\nBuilding NetCDF files-\n" if $verbose;

  opendir DIR, $spool_dir;
    my @FILES   = readdir(DIR);
  closedir DIR;
  my @files   = sort grep(m/\.dat$/,@FILES);
  my @ncFiles = sort grep(m/\.nc$/, @FILES);
  map unlink("$spool_dir/$_"), @ncFiles;	# Remove all NetCDF files
  map { $_ = "$spool_dir/$_" } @files;		# Make full file path
  my $band;
  delete $dataSet->{data};
  delete $dataSet->{date};

  foreach my $file (@files) {
		### Read data
    my $data	= spoolFile_read($file, $extent, $dataSet);
		### Set NetCDF time variable
    my $date	= $file	=~ m/(\d{4}-\d{2}-\d{2})\.dat$/ ? $1 : die("Cannot create NetCDF file...\n");
   (my $ncDate	= $date)=~ s/^0000/2001/;
    my $ncTime	= julian_day(split(m/-/,$ncDate)) - julian_day(1900,1,1);
       $band	= 0 if $date =~ m/-01-01$/;	# daily
       $band	= 0 if $date =~ m/-01-00$/;	# monthly
       $band	= 0 if $date =~ m/-00-00$/;	# yearly
		### Make NetCDF data structure
    my %dataToWrite	= ($$dataAtt{Var_Name} => [$data, $$dataAtt{Name}, $$dataAtt{Units}]);
	$file		=~ s/dat$/nc/;
	$file		=~ s/-\d{2}-\d{2}(\.nc)$/$1/;
		### Write NetCDF file
    write_nc($file, ++$band, $ncTime, $grid, \%dataToWrite,
	{CREDITS => $path{credits}, CALENDAR => 366, TS_RESOLUTION => $$dataAtt{Time_Series}});

    print "\r\tProgress: $date" if $verbose;
  }
}

#######################################################################
print "\n\nAll Done!\n" if $verbose;

close OLDERR;
exit;

#######################################################################
######################  Functions  ####################################

sub usage

{
  my $app_name = basename($0);
  print <<EOF;

Usage:
	$app_name [-h] [-v] [-rm] [-sd YYYY-MM-DD] [-ed YYYY-MM-DD] [-md MD_STRING] [-spDelta] [-nc] [-f FORKS] [-r RESAMPLE_INT] [-p PATCH_VALUE] [-pp PPATCH_VALUE] [-spDir SPOOL_DIR] [-proj NET_PROJ] NETWORK_PATH DATASET_ID

This code pre-builds GDAL warped PDL data files in the WBM spool area.

Options:

h	Display this help
v	Verbose mode
rm	Remove existing spool files
f	Number of forks to use. Default is $forks
sd	Start date for the time series
ed	End   date for the time series
md	Multi-day time series string, e.g. "[5,10,15,20,25,lastDay]"
spDelta	Use spool Delta method
proj	River Network projection. Default is "$proj"
r	Resample method as Integer number. See GDAL docs. Default is 1 (bilinear)
	('near'=>0,'bilinear'=>1,'cubic'=>2,'cubicspline'=>3,'lanczos'=>4,'average'=>5,
	 'mode'=>6,'max'=>7,'min'=>8,'med'=>9,'Q1'=>10,'Q3'=>11)
p	Apply PATCH_VALUE for bad values over the network grid -
		It can be a number or secondary dataset ID. Default is undef
pp	Primary PPATCH_VALUE for bad values over the network grid
spDir	Spool directory for the output files
mt	File path to an alternative Magic Table file
nc	Build additional NetCDF copy of spool binary data

Example:
$app_name -v -nc /net/nfs/zero/home/WBM_TrANS/data/flowdirection602.ascii merra_evwater_dc

EOF
  exit;
}

#######################################################################
