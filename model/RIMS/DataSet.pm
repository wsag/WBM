
#######################################################################
#
#      All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      C University of New Hampshire and Water Systems Analysis Group 2008-2021
#
#######################################################################
#######################################################################
#
#	DataSet object for RIMS.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	October, 2012
#
#		Last Modified:
#	Oct 2016	Spool removed
#	Mar 2017	Convertion of non-timeseries dataset to fake 'yearly_clim'
#	Oct 2018	Added "UNITS" option to check/verify units of the dataset data
#	Jul 2019	Hourly
#	Sep 2019	Year up search and changes in date search function
#	Nov 2022	Option for forsed spool/sparse Delta
#
#	Version-	(YY.M.#)	# Number is zero-based
my $version =		'22.12.0';	# Version. Note- update version variable below
#
#######################################################################

package RIMS::DataSet;

use strict;
use Time::JulianDay;
use Time::DaysInMonth;
use RIMS qw/check_MT_date make_date_list make_file_list make_date_layers set_default/;

our $VERSION	= '22.12.0';

no if $] >= 5.017011, warnings => 'experimental::smartmatch';

#######################################################################

sub new
		### Creates an Object
{
  my $proto = shift;
  my $class = ref($proto) || $proto;	### An object can be used as a prototype

  die "Cannot create $class without proper arguments" unless @_ > 0;
  my($metadata, $options) = @_;
  $options = undef unless ref($options) eq 'HASH';
			### Options
  my $start_year_clip	= $$options{START_YEAR_CLIP}	|| undef;
  my $units		= $$options{UNITS}		|| undef;	# Option to check units
  my $TS_res_min	= $$options{MIN_TS_RES}		|| undef;	# Option to check minimum temporal resolution
  my $TS_res_max	= $$options{MAX_TS_RES}		|| undef;	# Option to check maximum temporal resolution
  my $spDelta		= $$options{SP_DELTA}		|| undef;	# Option to use sparse delta method for spool

	### Attributes from the Magic Table or init file
  my $check_MT_date  = check_MT_date($$metadata{Start_Date});
     $check_MT_date += check_MT_date($$metadata{End_Date});
  die "Wrong format of Start_Date or End_Date in the metadata for the dataset ID: $$metadata{Code_Name}\nAborting...\n"
	if $check_MT_date;

	### Check/apply START_YEAR_CLIP option
  if (defined($start_year_clip)) {
    die "Wrong format of START_YEAR_CLIP option for the dataset ID: $$metadata{Code_Name}\nAborting...\n"
	if $start_year_clip !~ m/^\d{4}$/;
    substr($$metadata{Start_Date},0,4,$start_year_clip)		  if $$metadata{Time_Series} &&
	  ($start_year_clip > substr($$metadata{Start_Date},0,4)) && $$metadata{File_Path} =~ /_YEAR_/;
  }
	### Check/verify dataset units
  die sprintf("Dataset ID: \"%s\". Requested units do not match (%s vs. %s). Aborting...\n",
    $$metadata{Code_Name}, $units, $$metadata{Units}) if defined($units) && $$metadata{Units} !~ m/^$units/i;

	### Check/verify if "Processing" has "polygon" instruction for ESRI shape files
  die "Dataset ID: \"$$metadata{Code_Name}\". " .
      "It appears to use shape file, but \"Processing\" does not have \"polygon\" instruction. Aborting...\n"
	if  $$metadata{File_Path}  !~ m/;\s*\S+/ && $$metadata{Processing} !~ m/polygon/i &&
	   ($$metadata{File_Path}  =~ m/\.shp$/i || $$metadata{File_Path} =~ m/\.shp\W/i);

	### Check/verify dataset minimum temporal resolution
  (my $TS = $$metadata{Time_Series}) =~ s/([a-z]+).*/$1/i;	# Use only principle TS resolution
  if ($TS_res_min) {	my @TS_list;
    foreach my $ts (qw(daily monthly yearly)) { push @TS_list,$ts; last if $TS_res_min =~ m/$ts/i; }
    die "Unknown TS resolution \"$TS_res_min\" in MIN_TS_RES option for the DataSet object. Aborting...\n"
	unless m/$TS_res_min/i ~~ @TS_list;
    die sprintf("Dataset ID: \"%s\". TS resolution is \"%s\" while requested minimum is \"%s\"). Aborting...\n",
      $$metadata{Code_Name}, $$metadata{Time_Series}, $TS_res_min) unless m/$TS/ ~~ @TS_list;
  }
	### Check/verify dataset maximum temporal resolution
  if ($TS_res_max) {	my @TS_list;
   (my $TS = $$metadata{Time_Series}) =~ s/_clim$//i;
    foreach my $ts (qw(yearly monthly daily)) { push @TS_list,$ts; last if $TS_res_max =~ m/$ts/i; }
    die "Unknown TS resolution \"$TS_res_max\" in MAX_TS_RES option for the DataSet object. Aborting...\n"
	unless m/$TS_res_max/i ~~ @TS_list;
    die sprintf("Dataset ID: \"%s\". TS resolution is \"%s\" while requested maximum is \"%s\"). Aborting...\n",
      $$metadata{Code_Name}, $$metadata{Time_Series}, $TS_res_max) unless m/$TS/ ~~ @TS_list;
  }
	### Check forsed spool/sparse Delta option
  $spDelta	= 1	if $$metadata{Time_Series} =~ m/^daily$/i && $$metadata{Processing} =~ m/SpDelta/i;
  $spDelta	= 0	if $$metadata{Processing}  =~ m/noSpDelta/i;	# Highest priority option

	### Convert non-timeseries dataset to fake 'yearly_clim'
  ($$metadata{Time_Series}, $$metadata{Start_Date}, $$metadata{End_Date}) =
	('yearly_clim', '0000-00-00', '0000-00-00') unless $$metadata{Time_Series};

	### Build date lists
  my ($j_dateList,$dateList) = make_date_list($$metadata{Start_Date},$$metadata{End_Date},$metadata);

	### Build file list by date
  my $fileList	= make_file_list($j_dateList, $metadata, 1e10, '0_0');
  my $dateLayer	= make_date_layers($fileList,$dateList);

	### Construct the DataSet object
  my $self  = {
	'ID'		=> $$metadata{Code_Name},
	'MT_attrib'	=>  $metadata,
	'j_dateList'	=>  $j_dateList,
	'dateList'	=>  $dateList,
	'fileList'	=>  $fileList,
	'dateLayer'	=>  $dateLayer,
	'hourly'	=> $$metadata{Time_Series} =~ m/daily\{/i	? 1 : 0,
	'daily'		=> $$metadata{Time_Series} =~ m/daily(?!\{)/i	? 1 : 0,
	'monthly'	=> $$metadata{Time_Series} =~ m/monthly/i	? 1 : 0,
	'yearly'	=> $$metadata{Time_Series} =~ m/yearly/i	? 1 : 0,
	'decadal'	=> $$metadata{Time_Series} =~ m/decadal/i	? 1 : 0,
	'climatology'	=> $$metadata{Time_Series} =~ m/_clim/i		? 1 : 0,
	'units'		=> $$metadata{Units},
	'spDelta'	=>  $spDelta
  };

  my $object = bless $self,$class;
  return $object;
}

#######################################################################

sub dateLayer
	### Find file and band number for the requested date
{
  my $self	= shift;
  my $date	= shift;
  my $options	= shift;

  my $DATE	= $self->searchDate($date, $options);
  my $file_band = $self->{'dateLayer'}{$DATE};

  return wantarray ? @$file_band : $file_band;
}

#######################################################################

sub searchDate
	### Find file and band number for the requested date
{
  my $self	= shift;
  my $DATE	= shift;
  my $options	= shift;

  my $search	= 3;			### Downscale search level (daily->monthly->yearly->decadal)
  my $yr_up	= 0;			### Added years up to search
  if (defined $options) {
    if (isNumber($options)) {
      $search	= $options; }
    elsif (ref($options) eq 'HASH') {
      $search	= set_default($$options{DATASET_OPT},	3);
      $yr_up	= set_default($$options{YR_UP},		0);
    }
    else { die "Dataset date search options must be a number of hash reference. Aborting... in \"" .
	$self->{'ID'} . '" dataset' }
  }
  $search	= 3	if $search > 3;
  my %meta	= map (($_ => $self->{$_}), qw(ID hourly daily climatology));

		### Standard (default) search
  my $date	= findDate($DATE, $search, $self->{'dateLayer'}, \%meta);
  return $date if exists $self->{'dateLayer'}{$date};

		### Optional (years up) search
  if ($yr_up && !$self->{climatology} && !$self->{'hourly'}) {
    foreach my $add (1 .. $yr_up) {
     ($date	= $DATE) =~ s/^\d{4}/substr($DATE,0,4)+$add/e;
      $date	= findDate($date, $search, $self->{'dateLayer'}, \%meta);
  } }
  return $date if exists $self->{'dateLayer'}{$date};

		### Report failed search
  die "Date $DATE (searched to $date) does not exist in \"".$self->{'ID'}.'" dataset'
	unless exists $self->{'dateLayer'}{$date};
}

sub findDate
{
  my($date, $search, $dateLayer, $meta) = @_;
  my @pos	= (3,5,8);
  my @len	= (1,2,2);

		### Check for Time Series of the search date to be hourly?
  my($hour, $hourly)	= $date =~ m/T(\d{2})/ ? (int($1), 1) : (0, 0);
  die "Cannot use hourly dataset for non-hourly search date (Dataset ID: \"$$meta{ID}\"; Search date: $date)." .
	" Aborting...\n"	if $$meta{hourly} && !$hourly;

  		### Remove year and leap year for Climatology datasets
  if ($$meta{climatology}) {
    $date =~ s/^\d{4}/0000/;		### There is no actual year in a climatology dates
    $date =~ s/-02-(29|30)/-02-28/;	### There is no leap   year in a climatology dates
  }

		### Fix leap year if the dataset does not have it
  if ($date =~ m/-02-(29|30)$/ && $$meta{'daily'} && !exists($$dateLayer{$date}))
    { $date =~ s/(29|30)$/28/; }	### It could be day_360 calendar...

		### Search for hourly date
  if ($$meta{hourly}) {		### Search
    $date	=~ s/T\d{2}/'T'.sprintf("%02d",--$hour)/e while $hour > 0 && !exists($$dateLayer{$date});
  } else {
    $date	=~ s/T.+//;	# Strip hourly portion of the search date, if dataset is not hourly

		### Search for closest downscaled date
    while ($search-- && !exists($$dateLayer{$date})) {
      my $len    = pop(@len);
      substr($date,pop(@pos),$len) = '0' x $len;
  } }

  return $date;
}

#######################################################################

sub get
{
  my $self	= shift;
  my $key	= shift;
  die "DataSet property \"$key\" does not exist in \"".$self->{'ID'}.'" dataset'
	unless exists $self->{$key};
  return $self->{$key};
}

sub hourly
{
  my $self	= shift;
  return $self->{'hourly'};
}

sub daily
{
  my $self	= shift;
  return $self->{'daily'};
}

sub monthly
{
  my $self	= shift;
  return $self->{'monthly'};
}

sub yearly
{
  my $self	= shift;
  return $self->{'yearly'};
}

sub decadal
{
  my $self	= shift;
  return $self->{'decadal'};
}

sub climatology
{
  my $self	= shift;
  return $self->{'climatology'};
}

#######################################################################

sub rmFiles
{
  my $self	= shift;
  map { (my $path = $$_[0]) =~ s/.+:(.+):.+/$1/; unlink($path); } @{$self->{'fileList'}};
}

#######################################################################
################	Priviate Functions	#######################

sub isNumber
{
  my $num = shift();
  return defined($num) ? $num =~ m/^[-+]?\d+(?:\.\d*)?(?:[eE][-+]?\d+(?:\.\d*)?)?$/ : 0;
}

#######################################################################

return 1;

#######################################################################
