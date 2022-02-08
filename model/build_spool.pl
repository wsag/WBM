#!/usr/bin/perl -w

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
#		July 2021	Adopted to run from any location, i.e. system wide path to required files
#		Sep 2021	Added RIMS::WBM
#
#	Version:	21.9.0
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
use PDL::NetCDF;
use PDL::NiceSlice;
use Geo::GDAL;
use Fcntl;
use Time::JulianDay;
use Time::DaysInMonth;
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

my ($help,$verbose,$remove,$forks,$resample,$patch,$PATCH,$nc, $proj,  $s_date,$e_date,$spool_dir,$MT_file) = (
      0,      0,       0,     8,      1,     undef, undef, 0,'epsg:4326', '',     '',      '',       ''   );

					# Get command line options
usage() if !GetOptions(
	'h'=>\$help, 'v'=>\$verbose, 'rm'=>\$remove, 'f=i'=>\$forks, 'r=i'=>\$resample, 'p=s'=>\$patch,
	'pp=s'=>\$PATCH, 'nc'=>\$nc, 'proj=s'=>\$proj, 'sd=s'=>\$s_date, 'ed=s'=>\$e_date,
	'spDir=s'=>\$spool_dir, 'mt=s'=>\$MT_file ) or $help;

my $networkFile	= shift() or usage();
my $dataID	= shift() or usage();

my $patchFlag	= defined($patch) && !isNumber($patch);	# Flag to patch with secondary dataset
   $patch	= $PATCH if defined($PATCH) && !defined($patch);
my $patchAtt;
   $MT_file	= $path{MT_file} || $$PATH{names_file} unless $MT_file;	# Custom_MT otherwise Global_MT

my $pm		= new Parallel::ForkManager($forks);	# Parallel Processing object
die "Start date is not in YYYY-MM-DD format\n" if $s_date && $s_date !~ m/^\d{4}-\d{2}-\d{2}$/;
die "End   date is not in YYYY-MM-DD format\n" if $e_date && $e_date !~ m/^\d{4}-\d{2}-\d{2}$/;

#######################################################################
##################     Read Input Data        #########################

my $extent	= get_extent($networkFile, $proj);
my $grid	= [lonLat($extent), $$extent{gTransform}, $$extent{projection}];

		### Primary dataset
my $dataSet	= new RIMS::DataSet(attrib($dataID,$MT_file));
my $dataAtt	= $dataSet->get('MT_attrib');
   $spool_dir	= $path{spool_dir} unless $spool_dir;
   $spool_dir  .= '/' if $spool_dir && $spool_dir !~ m/\/$/;	# Add closing slash to the path string
   $spool_dir  .= fileBaseName($networkFile) . "/$$dataAtt{Code_Name}";

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

foreach my $date (@{$dataSet->{dateList}}) {
  next if ($s_date && $date lt $s_date) || ($e_date && $date gt $e_date);
  $pm->start and next;			### Run fork processes
  print "\r\tProgress: $date" if $verbose;

		### Build the warped PDL data to write to spool
  my $file = sprintf "$spool_dir/%s.dat",$dataSet->searchDate($date,3);
  unless (-e $file) {
    my $data =	read_GDAL($extent,$dataAtt, $resample,$dataSet->dateLayer($date,3), $patchFlag ?
		read_GDAL($extent,$patchAtt,$resample,$patch  ->dateLayer($date,3), $PATCH)    : $patch);
    writeflex($file, $data);
  }

  $pm->finish;
}
$pm->wait_all_children;

#######################################################################
#############   Build NetCDF files	###############################

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

  foreach my $file (@files) {
		### Read data
    my $data	= readflex($file);
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
	$app_name [-h] [-v] [-rm] [-sd YYYY-MM-DD] [-ed YYYY-MM-DD] [-nc] [-f FORKS] [-r RESAMPLE_INT] [-p PATCH_VALUE] [-pp PPATCH_VALUE] [-spDir SPOOL_DIR] [-proj NET_PROJ] NETWORK_PATH DATASET_ID

This code pre-builds GDAL warped PDL data files in the WBM spool area.

Options:

h	Display this help.
v	Verbose mode.
rm	Remove existing spool files.
f	Number of forks to use. Default is $forks.
sd	Start date for the time series.
ed	End   date for the time series.
proj	River Network projection. Default is "$proj".
r	Resample method as Integer number. See GDAL docs. Default is 1 (bilinear).
	('near'=>0,'bilinear'=>1,'cubic'=>2,'cubicspline'=>3,'lanczos'=>4,'average'=>5,
	 'mode'=>6,'max'=>7,'min'=>8,'med'=>9,'Q1'=>10,'Q3'=>11)
p	Apply PATCH_VALUE for bad values over the network grid.
		It can be a number or secondary dataset ID. Default is undef.
pp	Primary PPATCH_VALUE for bad values over the network grid.
spDir	Spool directory for the output files.
mt	File path to an alternative Magic Table file.
nc	Build additional NetCDF copy of spool binary data.

Example:
$app_name -v -nc /net/nfs/zero/home/WBM_TrANS/data/flowdirection602.ascii merra_evwater_dc

EOF
  exit;
}

#######################################################################
