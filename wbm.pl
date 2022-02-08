#!/usr/bin/perl -w

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
#	PDL implementation for the the Water Balance Model (WBM).
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	January 2011
#		Development-
#	May 2012	Added	Connectivity/Interbasin transfer.
#	October 2012	Added	DataSet objects; spooling; etc.
#	November 2012	Added	sub-pixel crops.
#	January 2013	Added	Dynamic reservoirs.
#	February 2013	Added	Evaporation from open water.
#	September 2013	Added	Land Cover types; intercept; glacial melt; etc.
#	December 2013	Added	Routing of water components; combined routing functions.
#	March 2014	Added	Domestic and Stock water demand.
#	May 2014	Added	Slice processing option was added.
#	May 2014	Added	Snowbands and Lapse downscaling; credits; bug fixes.
#	June 2014	Added	Evaporation from interbasin transfer canals; init file format.
#	August 2014	Added	Instantaneous stream width/depth; water temperature.
#	September 2014	Added	Spillway/other dam rules; surface runoff detention pool;
#				impervious surface; lake pixels model; connectivity stack;
#				save spinup state; some bug fixes.
#	October 2014	Added	NetCDF v.4 support; 360-day calendar; monthly irrigation avg, etc.
#				new model for stream water use; shape file inputs; cropland TS;
#	November 2014	Revised	land priority to irrigation equipped area over landcover croplands;
#	April 2015		New MIRCA full irrigation model; A number of minor improvements.
#			Added	Tracking of flow components by runoff mask origin.
#				Interbasin transfer from reservoirs.
#	May 2015	Revised	Baseflow water temperature; Inter-basing transfer from reservoirs.
#			Added	Option for output dataset to be defined without Magic table entry.
#				PET scaling factor for Hamon method; WBM Parmeter options in input.
#			Fixed	Bug in network subset dayLength function. Few other bugs.
#	June 2015	Added	Runoff mask filtering by basin IDs.
#	July 2015	Fixed	Fallow land rotation bug; irrigation/demand from flow balance.
#			Added	Irrigation dam control by irrigation demand; Rice water percolation;
#				Irrigation intake by Surface/Groundwater ratio.
#	August 2015	Added	Small Irrigation Reservoirs (SIR).
#			Fixed	Bugs in routing causing small missbalance with irrgation & diversion.
#	September 2015	Revised	Circularity check (C-code).
#	October 2015	Revised	Domestic, Industrial, and Livestock water demand.
#	February 2016	Revised	Application of rice added water (first applied to soil then percolated).
#	May 2016	Revised	Some tracking water routines.
#	August 2016	Fixed	Output consistency problem- sorting of hash keys in sub-pixel variables.
#			Revised	Major water component tracking to address water use from flow problem.
#			Added	Full model for Spatial mask tracking.
#	October 2016	Revised	New WBM I/O format (Major incompatibility with the previous versions).
#	November 2016	Added	Rain contribution to rise paddy flooding/percolation.
#	December 2016	Added	Primary and Secondary (Patch) datasets for climate drivers: Temp & Precip.
#
#		New Master Branch- wbm.pl
#
#	January 2017	Added	GDAL test function to check NetCDF files are read correctly.
#	May 2017	Added	Hydrobiochemistry: Nitrogen (DIN + WWTP); variable velocity routing.
#	June 2017	Added	Unsustainable groundwater pool (variable UGW). Open water indexing.
#			Added	Irrigation technologies for water delivery and application; daily irr flag.
#	July 2017	Revised	Process and efficiency based irrigation technologies.
#	August 2017	Added	Cell fractional open water (affects storm runoff).
#			Change	Water release from managed reservoir changed from daily to hourly steps.
#	September 2017	Added	VIS (Virtual Irrigation Storage) to enable daily irrigation water withdrawals.
#	October 2017	Change	Reservoir input and initialization. Spider's algebra for managed reservoirs.
#			Added	Dam removal option. Other small improvements and changes (e.g. NetCDF CF-1.6).
#			Change	Cropland and irrigated land growth inputs/calcualtions.
#	November 2017	Added	Penman-Monteith PET. Number of new output variables, other improvements.
#			Added	Secondary datasets for "CropParFile" in "Irrigation" input.
#	December 2017	Added	Output file options, other improvements.
#			Added	Springs/Karst process modeling. Potential Irrigation demand. Yearly summaries.
#	January 2018	Added	Lumped aquifers for Springs/Karst modeling (unstable version).
#	February 2018	Added	Irr/Other withdrawals from Lumped aquifers; GW exclusion from delivery losses.
#			Change	Irr. reservoirs operation. Annual summaries. Fixed potential irrigation.
#	May 2018	Added	Sinks to virtual/lumped aquifers; Observed reservoir discharge option.
#	June 2018	Change	Glacier area representation, Fixed snow accumulation problem.
#       			Process and stability improvements to aquifer sinks.
#			Added	DIN balance, and observed reservoir discharge. Endorheic lakes.
#				Bogacki-Shampine solution to lumped aquifer simulation, DIN balance calculations.
#				Aquifer summary output to spreadsheets. Component tracking for Irr with relict.
#	July 2018	Added	Number of improvements/changes.
#	August 2018	Added	Coupling with reproduced USGS groundwater models MODFLOW and RIV. Many other fixes.
#	September 2018	Added	Cell area reference and scaling of output vars. Fixes relevant to the reference area.
#				Important fix of crop area fraction accounting in the cell partitioning.
#				Relict/unsustainable water in aqifers for Primary components tracking.
#	October 2018	Added	Benchmark mapping. Options for dom/ind/stk surface water search distance.
#	December 2018	Added	Test option. Revised snow accumulation.
#       January 2019    Change  Update lumped aquifer volume calculation.  Add lumped aquifer volume to output table.
#			Fixed	Tracking of irrigation water (primarily in WMA branch). Bug in monthly precip inputs.
#	March 2019	Change	Split of ExtraCoeff to non/aquifer areas. Fix to potenential irr. WMA table updates.
#	April 2019	Added	Endorheic buffer/mask. Elevation layer for T lapse rate. Benchmarking pause.
#	June 2019	Added	USGS data assimilation for WBM forecast mode. FAO-56 PET. End-of-year state file.
#			Change	Refined/Extended Penman-Monteith PET. Balance totals are clarified.
#	July 2019	Added	Option to reset tracking in groups [2,3] after spinup. Change: Canopy intercept.
#	August 2019	Change	Rice paddy system upgrade. Added- Endorheic lakes component tracking. GeoTIFF upgrade.
#			Added	Alternative climate drivers- Tmin/Tmax; Derived- Spec. humidity, Open water evaporation.
#	September 2019	Added	Green/Blue water in soil and evaporation. Deep upgrade of tracking with adding balance.
#			Change	Many small fixes and improvements.
#	October 2019	Added	Component tracking balance for (a) Primary sources; and (b) Irrigation/DIL usages.
#	March 2020	Added	Sub-daily (hourly) simulation capacity.
#	April 2020	Fixed	Bugs relevant to hourly code; fixed valid range of air humidity, etc.
#	July 2020	Fixed	Interpolation method in temperature lapse downscale function. Glacier runoff scaling fix.
#	August 2020	Added	Processing metadata in the output MT. Support for tileindex shape files.
#	September 2020	Added	EarthAtlas URL for output vars; Option for no compression of output.
#	January 2021	Fixed	Soil moisture function in sub-daily runs. Conservative DIN. Some small improvements.
#	March 2021	Added	Water reuse cycles in Irrigation tracking. Number of small additions.
#	April 2021	Added	Coastal segmentaiton mask of exorheic outlets. Some non-critical bug fixes.
#				Runtime temporal aggregaton.
#	May 2021	Change	Format of spinup input. Added "Force_ID_Date" parameter to "Spinup" block.
#	July 2021	Added	Utility "build_spool_batch.pl" to create spool files. Improves performance of WBM.
#	August 2021	Added	Initial head depth in ModFlow aquifer inputs. Removed Proj4 module dependencies.
#			Change	Aquifer water abstractions moved to a binary function "demandFromStreamGwater".
#	September 2021	Added	RIMS::WBM
#	December 2021	Added	Option to determine endorheic basins by Network flow dir of -1 at their mouth.
#	January 2022	Change	Transition to a new version of PDL. Fixed a bug in TWS calculation.
#
#######################################################################
#
#	NB-	This is Public domain version that does not contain any unpublished or development features
#		The full version is WSAG UNH domain has the following additional fuctionality/features:
#			1. Lumped/Virtual deep groundwater aquifers
#			2. Replicated MODFLOW deep groundwater aquifers dynamics
#			3. Springs and Sinks linked to deep groundwater aquifers
#			4. Endorheic lakes
#			5. USGS data assimilation
#
#		Users who are interested in the WSAG UNH domain version with full set of features should contact
#		Dr. R. Lammers (WSAG director).
#
#	Documentation-		https://www.watsys.sr.unh.edu/WBM
#
#######################################################################
#
#	Version-		(YY.M.#)	# Number is zero-based
use constant WBM_VERSION =>	'22.1.1';	# If this month is not listed in the notes above-
#						#	this version has small bug fixes or minor changes.
#
# Debugging errors: >perl -Mdiagnostics wbm.pl -v ...
#######################################################################

BEGIN {				# Load optional user defined module directory path
 (my $conf_file	= $0) =~ s/[\w\.]+$/WBM.conf/;		# Look it up in ./WBM.conf
  if (-e $conf_file) {
    open(F,$conf_file); my @f=grep(!/^\s*#/,<F>); close F; @f=grep(/^\s*lib_dir/,@f);
    my $dir = @f ? {eval("($f[0])")}->{lib_dir} : '';
    die "\nError in reading lib_dir: $@\n\n" if $@;
    unshift @INC,$dir if $dir;
} }
my $module_v = 'WSAG/RIMS modules:';

use strict;
use Benchmark;
use Cwd qw/abs_path/;
use Data::Dumper;
use Geo::GDAL;
use Getopt::Long;
use FileHandle;
use File::Basename;
use File::Path;
use File::Tee qw(tee);
use File::Temp;
use List::Util;
use Math::Trig qw/pi/;
use PDL;
use PDL::Char;
use PDL::GSL::MROOT;
use PDL::Image2D;
use PDL::IO::FlexRaw;
use PDL::Math;
use PDL::NetCDF;
use PDL::NiceSlice;
use Fcntl;
use Inline qw/Pdlpp/;
use Time::JulianDay;
use Time::DaysInMonth;
use version;				### WSAG UNH module set
use RIMS;		$module_v .= ' RIMS v.'		.$RIMS::VERSION.	 ';';
use RIMS::DataSet;	$module_v .= ' RIMS::DataSet v.'.$RIMS::DataSet::VERSION.';';
use RIMS::WBM;		$module_v .= ' RIMS::WBM v.'	.$RIMS::WBM::VERSION.	 ';';

no if $] >= 5.017011, warnings => 'experimental::smartmatch';
# use PDL::Graphics::PGPLOT;	### PGPLOT graphics for debugging (<imag>)
# dev '/XWINDOW';	# Graphics output devise for PDL::Graphics::PGPLOT

	### Check compartible PDL version
die "\nPDL version (v.".$PDL::VERSION.") is not compatible with this code. Aborting...\n\n"
	unless $PDL::VERSION >= 2.063;

#######################################################################
#############   Process and check command line inputs   ###############

my ($help,$verbose,$bMarkD,$bMarkY,$bMarkM,$remove,$rmSpool,$noOutput,$noState,$threads,$thrSize,$slice,$saveDams) =
   (  0,      0,      0,      0,      0,      0,      0,       0,        0,       4,       1,       0,      0    );
my ($idump,$errors,$dState,$test,$noRun,$spool_dir,$run_state_dir) =
   (  1,      0,      0,     0,     0,       0,           0      );
my ($MT_file, $credits);
						# Get command line options
GetOptions(
	'h'=>\$help, 'v'=>\$verbose, 'bd'=>\$bMarkD, 'by'=>\$bMarkY, 'bm'=>\$bMarkM, 'rm'=>\$remove,
	'rmSpool'=>\$rmSpool, 'noOutput'=>\$noOutput, 'noState'=>\$noState, 't=i'=>\$threads, 'tz=i'=>\$thrSize,
	'sl'=>\$slice, 'saveDams'=>\$saveDams, 'err'=>\$errors, 'dState'=>\$dState, 'test'=>\$test, 'noRun'=>\$noRun,
	'spoolDir=s'=>\$spool_dir, 'stateDir=s'=>\$run_state_dir) or exit();	usage() if $help;

#######################################################################
#############   Read this run settings and parameters   ###############

my $runID  = shift() or usage();
my %runIO  = isInitFile($runID) ? read_init(  $runID,{CHECK_ARR=>wbm_reqParams(), MAKE_ARR=>wbm_params()}) :
				  read_attrib({get_conf}->{runIO_list},$runID,'ID',{EVAL_KEYS=>1});
   $runIO{Spinup}	= spinup_param_init( \%runIO, $dState );	# Read and check spinup inputs
my %path   = get_paths();	# Get WBM dir/file paths from (1) conf; (2) WBM init file; (3) command line inputs

	### Make MT metadata for this WBM run
my $runAttr		= make_runAttr(\%runIO, $MT_file);
my $idPrfx		= $$runAttr{Code_Name}; $idPrfx =~ s/runoff_(h|d|m)$//;		# ID prefix

	### Add other required WBM run I/O parrameters
   $runIO{spool}	= $spool_dir.fileBaseName($runIO{Network}).'/';
   $runIO{Cell_Table}	= $runIO{spool}.'Cell_Table.dat';
   $runIO{Output_TS}	= $$runAttr{Time_Series};
  ($runIO{Output_dir}	= dirname(file_pyramid($$runAttr{File_Path}))) =~ s/\/((hourly|daily|monthly)(\/_YEAR_)*)*$//;
   $runIO{Run_Start}	= $$runAttr{Start_Date};
   $runIO{Run_End}	= $$runAttr{End_Date};
   $runIO{calendar}	= $$runAttr{Processing} =~ m/calendar=(\d+)/i ? $1 : 366;
   $runIO{Input_MT}	= $MT_file;
   $runIO{Output_MT}	= $runIO{Output_dir}."/$runIO{ID}.MT.csv";
   $runIO{Output_log}	= $runIO{Output_dir}."/$runIO{ID}.log";
   $runIO{bMarkM_log}	= $runIO{Output_dir}."/$runIO{ID}.BM.log" if $bMarkM;
   $runIO{PP_script}	= $runIO{Output_dir}.'/PostProcessing.pl' if $runIO{PostProcessing};
   $runIO{SB_script}	= $runIO{Output_dir}.'/build_spool_batch.pl';
   $runIO{Area_dir}	= $runIO{Output_dir}.'/cell_area/';
   $runIO{Output_vars}	= join ' ', uniq_list(split(m/\s+/,$runIO{Output_vars}));   # Remove duplicates in "Output_vars"
   $runIO{spool_list}	= [];					# List of spool items for build_spool_batch.pl utility

my $MagicT		=[$runIO{Input_MT}, $runIO{Output_MT}]; # [Global,Local] Magic Table files
my $run_state_file	= $run_state_dir.$runIO{ID};
my @runIO_Exra_Params	= qw(spool Output_TS Run_Start Run_End Output_dir Input_MT Output_MT);
my @list_out		= make_list_out(\%runIO);		# List of WBM variables to output
   $noOutput		= 1	if	!@list_out || $test;	# No output to files, if the output list is empty
   $runIO{noOutput}	= $noOutput;				#	or test mode is turned on
   $bMarkM		= 0	if	 $noOutput;		# Benchmark mapping hash
my %wbmParam		= read_param_str(\%runIO, 'wbmParam');	# Custom WBM parameters
   $wbmParam{saveEndorhMask}= 0	if	 $test;			# Disable saving files in test mode
   $runIO{NC4}		= set_default($wbmParam{NC4}, 1);	# Use NetCDF v.4 for output files (compressed)
my $days_in		= $runIO{calendar}==365 ?		# Calendar functions
			  \&calendar365_DaysInMonth : \&Time::DaysInMonth::days_in;
die "\nWBM is not coded for 360 calendar. Aborting...\n\n" if $runIO{calendar}==360;

	### Check input compatibility with older versions
die "\nObsolete \"MT_relHumidity\" input. Use \"MT_humidity\" instead. Aborting...\n\n"   if exists $runIO{MT_relHumidity};
die "\nObsolete \"FreeWaterEvap\"  input. Use \"openWaterEvap\" instead. Aborting...\n\n" if exists $runIO{FreeWaterEvap};

#######################################################################
# Command line check:
# perl -we 'use PDL; use version; $a = (version->parse($PDL::VERSION) >= version->parse("2.4.11")) ? "yes\n" : "no\n"; print $a;'

die "Wrong muli-threading options...\n" if $threads < 1 && $thrSize < 0;
$threads = 8 if $threads > 8;

set_autopthread_targ($threads);	# Number of CPU threads
set_autopthread_size($thrSize);	# Piddle size lower limit (in Meg) for multi-threading
set_err() if $errors;

#######################################################################
#######################################################################

my $time_start     = time();
my @loc_time_0     = localtime($time_start);		# Report Time
   $loc_time_0[5] += 1900; $loc_time_0[4]++;
my $time_start_str = sprintf "%04d-%02d-%02d at %02d:%02d:%02d", reverse(@loc_time_0[0..5]);

#######################################################################
			### Other Initializations

	### Remove Local MT, spool, and output files, if requested
unlink  $runIO{Output_MT}	unless	$noOutput;
rmtree  $runIO{spool}	 	if	$rmSpool;
rmtree  $runIO{Output_dir}	if 	$remove;
unlink <$run_state_file.*>	if 	$remove;

	### Make changes for "no output" case
if ($noOutput) {
  $runIO{Output_MT} = '';			# This will prevent writing input dataset metadata to local MT
  $MagicT	    = $runIO{Input_MT};		# This will prevent writing input dataset metadata to local MT
  $saveDams	    = 0;			# This will prevent writing output for reservoir filters
  $runIO{SB_script} = 'None';			# This will prevent writing the script file
  delete $runIO{PostProcessing};		# This will prevent post-processing
} else {
	### Make output, spool, and run state directories
unless (-e $runIO{Output_dir}) { mkpath($runIO{Output_dir},0,0775) or die "Cannot create-\n$runIO{Output_dir}\n"; }}
unless (-e $runIO{spool})      { mkpath($runIO{spool},     0,0775) or die "Cannot create-\n$runIO{spool}\n"; }
unless (-e $run_state_dir)     { mkpath($run_state_dir,    0,0775) or die "Cannot create-\n$run_state_dir\n"; }

	### Setup printing log to file and STDOUT if requested
     STDOUT->autoflush(1);		# Disable buffering
open STDOUT, '>/dev/null'		unless	$verbose;
tee  STDOUT, '>>', $runIO{Output_log}	unless	$noOutput;
	### Setup benchmark mapping
if ($bMarkM) {
  open BMFILE,'>', $runIO{bMarkM_log};	# Benchmark mapping log file
  $bMarkM = { FH => \*BMFILE, pause => 0, dir => $runIO{Output_dir} };
# $$bMarkM{FH}->autoflush(1);		# Disable buffering
}

print  "\nThe job started on $time_start_str.\n$module_v\n";
printf "WBM-TrANS version: %s\n", WBM_VERSION;
printf "WBM-TrANS  run ID: %s\n", $runIO{ID}. ($test ? '  (Test Mode)' : '');
printf "WBM-TrANS  run MT: %s\n", $noOutput ? 'No Output' : $runIO{Output_MT};

	### Make Global attributes for the output NetCDF files
my $global_attrib = [
	['title',		'WBM-TrANS data'],
	['Projection',		$runIO{Projection}],
	['WBM_VERSION',		WBM_VERSION],
	['PDL_VERSION',		$PDL::VERSION],
	['PERL_VERSION',	$]],
	['GDAL_VERSION',	&{sub{$_=get_file_path->{gdalinfo}.' --version';$_=qx($_);chomp;return($_)}}],
	['WBM_RUN_ID',		$runIO{ID}],
	['WBM_RUN_Init',	$runID],
	['WBM_Start_time',	$time_start_str],
	['SYSTEM_NAME',		$path{SYSTEM}],
	['Script_dir',		abs_path(dirname($0)).'/'],
	map(["runIO_$_",$runIO{$_}], @{wbm_params()}),		# Core WBM settings
	map(["runIO_$_",$runIO{$_}], @runIO_Exra_Params)	# Additional settings
];
	### Remove unwated Global attributes
foreach my $unwated (qw(runIO_ID runIO_Projection runIO_NC4 runIO_spool_list)) {
  $global_attrib = [map $$_[0] =~ m/$unwated/ ? () : $_, @$global_attrib];
}
	### Global attributes: Convert hash parameters to strings
foreach my $attrib (@$global_attrib) {
  $$attrib[1] = '' unless defined($$attrib[1]);
  if (ref($$attrib[1])) {
    my $dumper = Data::Dumper->new( [$$attrib[1]] );
    $dumper->Purity(1)->Deepcopy(1)->Quotekeys(0)->Sortkeys(1)->Indent(0);
    $$attrib[1] = $dumper->Dump;	$$attrib[1] =~ s/.+?\{/{/;	$$attrib[1] =~ s/;$//;
}}
	### Make Global attributes for the output NetCDF files of runtime aggregations
my $RT_global_attrib = [
	['title',		'WBM-TrANS data of runtime aggregations'],
	['Projection',		$runIO{Projection}],
	['WBM_VERSION',		WBM_VERSION],
	['references',		'https://www.wsag.unh.edu']
];
	### GDAL test to check NetCDF files are read correctly
my $GDAL_test	= GDAL_test($path{gdal_test_dir});
printf "GDAL NetCDF test : %s\n", $GDAL_test ? 'Good' : 'Not Performed';
print  "System           : $path{SYSTEM}\n\n";

#######################################################################
#########     Process/Prepare Run & Output metadata        ############
#######################################################################

	### Make DataCube entries for all output variables
make_dataCube(\%runIO, $runAttr) unless $noOutput;	#  and save them to local MT

	### Replace input "DEFAULT" keys with actual datasets
check_defaults(\%runIO, $MagicT);

my %dataSet;			# Hash to hold DataSet objects
   $dataSet{run}= new RIMS::DataSet($runAttr);
my $run_daily	= $dataSet{run}->hourly || $dataSet{run}->daily;
my $run_TS	= $dataSet{run}->hourly? 'hourly' : $dataSet{run}->daily? 'daily' : $dataSet{run}->monthly? 'monthly' :
		die "\nInvalid simulation Time Series type. Aborting...\n\n";
				# WBM files output directory
my $output_dir	= substr(dirname($dataSet{run}->{fileList}[0][0]), 7).'/';

	### Make lists of daily dates
my $metaDaily	= change_meta(0,$runAttr);
my($j_date_list, $date_list) = make_date_list($$metaDaily{Start_Date},$$metaDaily{End_Date},$metaDaily);
my $jDate_1900	= julian_day(1900,1,1);

	### Spinup Initialization (dates)
my $n_SP	= add_spinup_dates($j_date_list,$date_list,\%runIO,$metaDaily);
my $n_spinup	= $runIO{Spinup}{Loops} * $n_SP;

#######################################################################
	### Input- WBM Geographical Extent (taken from Network file)

my $extent	= get_extent($runIO{Network}, $runIO{Projection});
my $network	= $$extent{mask};	# This is not really needed.
my($lon, $lat)	= lonLat($extent);
my $grid	= [$lon, $lat, $$extent{gTransform}, $$extent{projection}];
my $nCells	= $network->ngood;

			### Check spool area to match network file.
die "\nNetwork file has changed but old spool area is not removed. ",
    "\nPlease, use '-rmSpool' flag to fix the problem. Aborting...\n\n"
	if -e $runIO{Cell_Table} && ((readflex($runIO{Cell_Table}))[0]->shape - $network->shape)->sum != 0;

			### Generic dataset metadata
my %meta	= ('Var_Scale' => 1, 'Var_Offset' => 0, 'Processing' => '', 'Projection' => 'epsg:4326');
my $runSet	= [\%runIO, \%meta, $extent, $MagicT];	### Set of commonly used variables in subroutines

#######################################################################
	### Input- Other miscellaneous initializations

my $zeroes	= zeroes(double,$$extent{ncols},$$extent{nrows})->copybad($$extent{mask});
my $ones	= ones  (double,$$extent{ncols},$$extent{nrows})->copybad($$extent{mask});
my $CELL_AREA	= cell_area($lon, $lat, $extent);	### Full cell area in km^2

			### Sub-daily variables
		# Note- notation "sub-daily interval" or "dt" is referred as the model time step (e.g. flux in mm/dt units)
my $subDi	= $$runAttr{Time_Series} =~ m/daily\{(\d+)}/ ? $1 : 1;	# Sub-daily intervals in a day
my $subDh	= 24/$subDi;						# Sub-daily interval, hours
my $subDf	=  1/$subDi;						# Sub-daily interval, day
my $hr_flag	=    $subDi > 1;					# Flag for sub-daily run
my $last_hr	=  $hr_flag ? 24-$subDh : 0;				# Last hour of sub-daily cycle
die "\nBad number of sub-daily intervals. Aborting...\n\n" unless grep $subDi==$_, (1,2,3,4,6,8,12,24);

			### Time step, etc.
my $dt		= 3600*$subDh;			### Time step, sec.
my $dL		= sqrt($CELL_AREA) *		### Cell river reach length, km
	condition(($network==1)|($network==4)|($network==16)|($network==64), 1, 1/sin(pi/4));

			### Start/End of the year conditions
my $yearBeg	= '01-01'					. ($hr_flag ? 'T00'		: '');
my $yearEnd	=($runIO{calendar} == 360 ? '12-30' : '12-31')	. ($hr_flag ? 'T'.(24-$subDh)	: '');

			### Active and soil areas (km2), and conversion coefficients
my $M3_2_MM	= 1e-3  /  $CELL_AREA;			# conversion: m3 -> mm  over full cell area
my($cell_area, $soil_area, $mm_2_KM3, $mm_2_km3, $mm_2_m3, $m3_2_mm) = area_layers( $CELL_AREA, 0, 0, 0, $subDf );
my			 ( $MM_2_KM3, $MM_2_km3, $MM_2_m3 )	     = map $_*$subDf, $mm_2_KM3,$mm_2_km3,$mm_2_m3;
			#  $MM_2_KM3	- conversion of mm/day to sub-daily km3 (active area)
			#  $MM_2_km3	- conversion of mm/day to sub-daily km3 (soil area)
			#  $MM_2_m3	- conversion of mm/day to sub-daily  m3 (soil area)

#######################################################################
	###  Input- Check or Build Cell Table and Connectivity Network (IBT- Inter-Basin Transfer network)

my($route, $connectivity, $cell_table, $up_area) = add_connectivity($runSet, $CELL_AREA);
my $flow_outPrev= zeroes(double,$cell_table->dim(1));
my $outletIdx	= find_outlets( $cell_table, $CELL_AREA, $runSet);	# Outlets indices (see comment below)
		# hash keys in Cell Table (all endorheic exorheic); in Network (ALL ENDORHEIC EXORHEIC)
						# NB- it sets flowdir to 0 for all outlets in $network
saveEndoMask($outletIdx, $runSet) if $wbmParam{saveEndorhMask};	# Flag to save endorheic basins mask to a file
		# Upstream area file is needed for WBM validation post-processing only (e.g. validation_660.pl)
write_gridascii($runIO{Area_dir}.'upstream_area.asc',   $up_area, $extent, {FORMAT=>"%.2f"})
      unless -e $runIO{Area_dir}.'upstream_area.asc' || $noOutput;

my @diversion	= ();		### Diversion summary data
my %cell_ind = map(($cell_table->at(3,$_).'_'.$cell_table->at(4,$_) => $_), 0..$cell_table->dim(1)-1);

#######################################################################
	### Read routing parameters (backward compartible with earlier WBM versions < 17.4.1 )

$runIO{Routing} = {'method' => 'LRR'} unless $runIO{Routing};	# Default routing method is LRR
my %routing	= $runIO{Routing} =~ m/^\w+$/ ? ('method'=>$runIO{Routing}) : read_param_str(\%runIO, 'Routing');
my $routingMethod = $routing{method};
die "Unknown Routing Method: $routingMethod...\n" unless $routingMethod =~ m/Muskingum|LRR|Flush/i;

	### Warning for switching Routing
	###	TBD- Add abstractions to Muskingum method (as per discussions with Balazs)
if (($runIO{ConnectivityNetwork} || $runIO{WaterDemand} || $runIO{Irrigation} || $runIO{openWaterEvap}) &&
	$routingMethod =~ m/Muskingum/) {
  print <<EOF;
Warning- Muskingum routing is not compatible with:
	Inter-basin water transfer;
	Water demands and Irrigation;
	Water evaporation from river channels.
Linear Reservoir Routing (LRR) is used instead.\n
EOF
  $routingMethod = 'LRR';
}

#######################################################################
################   Dataset Inputs/Initialization   ####################
#######################################################################

#######################################################################
		### Reservoir Data
my ($damData,$reservoir,$resInput) = add_reservoirs($runSet,$metaDaily,$CELL_AREA,\%cell_ind,$saveDams);

			### Initial Storage (units- m3) is 60 % of reservoir/lake Capacity
my @resStorage	= ($cell_table->addReservoirs(@$reservoir, 0.6, $$j_date_list[0], $network->dims));
my $spillDam	= which($$reservoir[0]->((4),) == 0) if @$damData;	# Spillway   dam indices
my $irrigDam	= which($$reservoir[0]->((4),) == 4) if @$damData;	# Irrigation dam indices
my $obsDam	= which($$reservoir[0]->((4),) == 7) if @$damData;      # Observed outflow dams

			### Irrigation demand daily frequency to control irrigation reservoir water use
if ($$resInput{IrrFrequency}) {
   $dataSet{IrrFrequency} = new RIMS::DataSet(attrib($$resInput{IrrFrequency},$MagicT));
   push @{$runIO{spool_list}}, [$dataSet{IrrFrequency},{}];		# Add item to build_spool_batch.pl utility
}

			### Small Irrigation Reservoirs (SIR)
my $smResFlag	= $$resInput{smReservoirFile} ? 1 : 0;	# Flag for SIR processing
my $smResCpcty	= $$resInput{smReservoirFile} ?		# in m3
	read_GDAL($extent,\%meta,1,$$resInput{smReservoirFile},1,0)->lclip(0) * $cell_area : $zeroes;
my $smResArea	= $smResCpcty / 2;			# in m2 assuming SIR depth = 2 m
   $smResCpcty /= $mm_2_m3;				# Convert SIR capacity to mm
my @smResStrg	= (0.6 * $smResCpcty);			# SIR storage, mm

			### Endorheic lakes
my $endoStrg	= $zeroes->indexND($$outletIdx{ENDORHEIC})->sever;
my($endoArea, $endoEvap) = map $endoStrg->copy, 1..2;
my $endoSlope	= 5e-3;		# Slope for endorheic lakes geometry (circular cone)

#############################################################################################
		##### Climate drivers #####

foreach my $id (qw(MT_Precip MT_airT MT_cloudFr MT_windU MT_windV MT_humidity MT_albedo)) {
  next unless $runIO{$id};
		### Make "Primary" tag for climate driver datasets, if needed for backward compatibility
  $runIO{$id}{Primary}	= delete($runIO{$id})	if exists $runIO{$id}{Code_Name};
		### Check "Secondary" climate driver dataset entries
  delete($runIO{$id}{Secondary})		if exists $runIO{$id}{Secondary} and !($runIO{$id}{Secondary});
}
			### Precipitation
   $dataSet{precip}	= new RIMS::DataSet(attrib($runIO{MT_Precip}{Primary},	$MagicT,'MT_Precip->Primary'),{MIN_TS_RES=>'monthly'});
if (exists $runIO{MT_Precip}{Secondary}) {
   $dataSet{precipPtch}	= new RIMS::DataSet(attrib($runIO{MT_Precip}{Secondary},$MagicT,'precipPtch'));
   die "Primary and Secondary precipitation datasets must be of the same time series type and units. Aborting...\n\n"
	if lc($dataSet{precip}->{MT_attrib}{Time_Series}) ne lc($dataSet{precipPtch}->{MT_attrib}{Time_Series}) &&
	   lc($dataSet{precip}->{MT_attrib}{Units})       ne lc($dataSet{precipPtch}->{MT_attrib}{Units});
}
push @{$runIO{spool_list}}, [$dataSet{precip}, {PATCH_VALUE=>$dataSet{precipPtch}}];	# Add item to build_spool_batch.pl utility

if ($dataSet{precip}->monthly) {
   die "\"precipFraction\" must be used for monthly Precipitation data. Aborting...\n\n" unless $runIO{precipFraction};
   $dataSet{precipFrac}	= new RIMS::DataSet(attrib($runIO{precipFraction},$MagicT,'precipFraction'),
	{MIN_TS_RES=>'daily',UNITS=>'frac'});
   push @{$runIO{spool_list}}, [$dataSet{precipFrac},{RESAMPLE=>0,PATCH_VALUE=>0}];	# Add item to build_spool_batch.pl utility
}
if ($dataSet{precip}->daily) {
   die "\"precipFraction\" cannot be used for daily Precipitation data. Aborting...\n\n" if     $runIO{precipFraction};
}
	### Precipitation Unit conversion per the model time step (dt). Source units can be mm/month mm/day or mm/hour
if ($dataSet{precip}->{MT_attrib}{Time_Series} =~ m/monthly/) {
  die "\nPricipitation units of input data must be mm/month. Aborting...\n\n"			# Check precipitation units
	unless $dataSet{precip}->{MT_attrib}{Units} =~ m/^mm\/m/i;				# for monthly TS
} else {
  die "\nPricipitation units of input data must be mm/day or mm/hour. Aborting...\n\n"		# Check precipitation units
	unless $dataSet{precip}->{MT_attrib}{Units} =~ m/^mm\/(d|h)/i;				# for daily/hourly TS
}
my $precip_scale = ($dataSet{precip}->{MT_attrib}{Units} =~ m/^mm\/(d|m)/ ? 1 : 24) *		# The converstion to mm/day
	set_default($runIO{MT_Precip}{Scale}, 1);	# Precipitation scale factor (recommended in forecast mode)

			### Air Temperature
if (exists $runIO{MT_airT}{Primary}) {
   $dataSet{airT}	= new RIMS::DataSet(attrib($runIO{MT_airT}{Primary},	$MagicT,'MT_airT->Primary'));
   $dataSet{airT_Ptch}	= new RIMS::DataSet(attrib($runIO{MT_airT}{Secondary},	$MagicT,'MT_airT->Secondary'))
	if exists $runIO{MT_airT}{Secondary};
    push @{$runIO{spool_list}}, [$dataSet{airT},{PATCH_VALUE=>$dataSet{airT_Ptch}}];	# Add item to build_spool_batch.pl utility
}
else {
  $dataSet{airTmin}	= new RIMS::DataSet(attrib($runIO{MT_airT}{airTmin},	$MagicT,'MT_airT->airTmin'));
  $dataSet{airTmax}	= new RIMS::DataSet(attrib($runIO{MT_airT}{airTmax},	$MagicT,'MT_airT->airTmax'));
  push @{$runIO{spool_list}},[$dataSet{airTmin},{}],[$dataSet{airTmax},{}];	# Add 2 items to the spool_list
}
			### Other climate drivers
my $OWEC_flag  =  $runIO{openWaterEvap} =~ m/Calculate/i;	# Flag to calculate open water evaporation
my $waterTemp  =  m/_twt$/ ~~ @list_out;			# Flag to calculate water temperature
if($runIO{PET} =~ m/^FAO-56|Penman-Monteith$/ || $waterTemp || $OWEC_flag) {
  print "Initialization of datasets for required additional climate drivers...\n";
  die "\"MT_cloudFr\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{MT_cloudFr};
  die "\"MT_windU\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{MT_windU};
  die "\"MT_windV\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{MT_windV};
  die "\"MT_humidity\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{MT_humidity};

  $dataSet{cloudFr}	  = new RIMS::DataSet(attrib($runIO{MT_cloudFr}{Primary},	$MagicT,'MT_cloudFr->Primary'));
  $dataSet{cloudFr_Ptch}  = new RIMS::DataSet(attrib($runIO{MT_cloudFr}{Secondary},	$MagicT,'MT_cloudFr->Secondary'))
	if exists $runIO{MT_cloudFr}{Secondary};
  $dataSet{windU}	  = new RIMS::DataSet(attrib($runIO{MT_windU}{Primary},		$MagicT,'MT_windU->Primary'));
  $dataSet{windU_Ptch}	  = new RIMS::DataSet(attrib($runIO{MT_windU}{Secondary},	$MagicT,'MT_windU->Secondary'))
	if exists $runIO{MT_windU}{Secondary};
  $dataSet{windV}	  = new RIMS::DataSet(attrib($runIO{MT_windV}{Primary},		$MagicT,'MT_windV->Primary'));
  $dataSet{windV_Ptch}	  = new RIMS::DataSet(attrib($runIO{MT_windV}{Secondary},	$MagicT,'MT_windV->Secondary'))
	if exists $runIO{MT_windV}{Secondary};
  $dataSet{humidity}	  = new RIMS::DataSet(attrib($runIO{MT_humidity}{Primary},	$MagicT,'MT_humidity->Primary'));
  $dataSet{humidity_Ptch} = new RIMS::DataSet(attrib($runIO{MT_humidity}{Secondary},	$MagicT,'MT_humidity->Secondary'))
	if exists $runIO{MT_humidity}{Secondary};

  push @{$runIO{spool_list}},	[$dataSet{cloudFr}, {PATCH_VALUE=>$dataSet{cloudFr_Ptch}} ],
				[$dataSet{windU},   {PATCH_VALUE=>$dataSet{windU_Ptch}}   ],
				[$dataSet{windV},   {PATCH_VALUE=>$dataSet{windV_Ptch}}   ],
				[$dataSet{humidity},{PATCH_VALUE=>$dataSet{humidity_Ptch}}];

			### Humidity input validation - Check and unify units
  if    ($dataSet{humidity}	->{MT_attrib}{Units} =~m/^per|^%/i)    { $dataSet{humidity}	->{MT_attrib}{Units} = '%';     }
  elsif ($dataSet{humidity}	->{MT_attrib}{Units} =~m/^kg/i)        { $dataSet{humidity}	->{MT_attrib}{Units} = 'kg/kg'; }
  elsif ($dataSet{humidity}	->{MT_attrib}{Units} =~m/^deg*.*C|^C/i){ $dataSet{humidity}	->{MT_attrib}{Units} = 'C';     }
  else  {die "\nUnknown units of Humidity dataset input. Aborting...\n\n"; }
     if ($dataSet{humidity_Ptch}) {
  if    ($dataSet{humidity_Ptch}->{MT_attrib}{Units} =~m/^per|^%/i)    { $dataSet{humidity_Ptch}->{MT_attrib}{Units} = '%';     }
  elsif ($dataSet{humidity_Ptch}->{MT_attrib}{Units} =~m/^kg/i)        { $dataSet{humidity_Ptch}->{MT_attrib}{Units} = 'kg/kg'; }
  elsif ($dataSet{humidity_Ptch}->{MT_attrib}{Units} =~m/^deg*.*C|^C/i){ $dataSet{humidity_Ptch}->{MT_attrib}{Units} = 'C';     }
  else  {die "\nUnknown units of Humidity patch dataset input. Aborting...\n\n"; }
  die "\nMismatch of units in Primary and Secondary dataset for humidity. Aborting...\n\n"
	if $dataSet{humidity_Ptch}->{MT_attrib}{Units} ne $dataSet{humidity}->{MT_attrib}{Units}; }
  die "\n\"Elevation\" dataset is required in case of Specific Humidity is used in the \"MT_humidity\" input. Aborting...\n\n"
	if $dataSet{humidity}->{MT_attrib}{Units} eq 'kg/kg' && !$runIO{Elevation};
}
if ($runIO{PET} eq 'Penman-Monteith' || $waterTemp) {
  die "\"canopyHt\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{canopyHt};
  die "\"MT_LAI\"	input is required for this WBM setup. Aborting...\n\n" unless $runIO{MT_LAI};
} die "\"Elevation\"	input is required for this WBM setup. Aborting...\n\n" if    !$runIO{Elevation} && $OWEC_flag;

			### PET
my @grossRad =((0) x  365);
my @dayLen;	###  For Hamon PET- dayLen is fraction of day (24 hours) with daylight
       $runIO{PET} =    'Hamon' unless $runIO{PET};
if (   $runIO{PET} =~ m/^Hamon$/) { @dayLen	= map dayLength($lon,$lat,$_), 0..364; }
elsif ($runIO{PET} =~ m/^FAO-56|Penman-Monteith$/) {
  print "Initialization of datasets for $runIO{PET} PET model...\n";
  die "\"MT_albedo\"	input is required for $runIO{PET} PET method. Aborting...\n\n" unless $runIO{MT_albedo};

  $dataSet{albedo}	  = new RIMS::DataSet(attrib($runIO{MT_albedo}{Primary},	$MagicT,'MT_albedo->Primary'));
  $dataSet{albedo_Ptch}	  = new RIMS::DataSet(attrib($runIO{MT_albedo}{Secondary},	$MagicT,'MT_albedo->Secondary'))
	if exists $runIO{MT_albedo}{Secondary};
  push @{$runIO{spool_list}}, [$dataSet{albedo},{PATCH_VALUE=>$dataSet{albedo_Ptch}}];	# Add item to build_spool_batch.pl utility

  @grossRad	= map grossRadiation($lon,$lat,$_),0..364 unless $hr_flag;	# Calculate grossRad for each day of the year

		### Set some Penman-Monteith PET constants
  $wbmParam{CLeafMax}	 = set_default($wbmParam{CLeafMax}, 6);	# Maximum Leaf Conductance, Dingman, 2008, Table (7-5)
  $wbmParam{CLeafMax}	 = read_Layer($extent,\%meta, $wbmParam{CLeafMax}, $MagicT, {KEY=>'CLeafMax'});
}
else { die "Unknown PET method: \"$runIO{PET}\". Aborting...\n\n"; }

			### Free-Water, Lake, and Wetland Evaporation
if ($runIO{openWaterEvap} && !$OWEC_flag) {
   $dataSet{waterEvap} = new RIMS::DataSet(attrib($runIO{openWaterEvap},$MagicT,'openWaterEvap'));
   push @{$runIO{spool_list}}, [$dataSet{waterEvap},{}];	# Add item to build_spool_batch.pl utility
}
			### Leaf Area Indices (LAI), and Canopy Height, m
my $LAI	= $zeroes->copy;
   $dataSet{LAI} = new RIMS::DataSet(attrib(  $runIO{MT_LAI},	$MagicT,'MT_LAI'))	  if $runIO{MT_LAI};
my @canopySh	 = scale_MinMax($dataSet{LAI},$runSet)					  if $runIO{MT_LAI};
my $canopyHt	 = read_Layer($extent,\%meta, $runIO{canopyHt},	$MagicT,{PATCH_VALUE=>0}) if $runIO{canopyHt}; # m
my $MDConstInterceptCI	= set_default($wbmParam{InterceptPar}, 0.2);	# Canopy intercept parameter, mm
						# 0.2 by Dickinson (1984); 0.15 by FrAMS and Dingman (2002)
push @{$runIO{spool_list}}, [$dataSet{LAI},{}] if $runIO{MT_LAI};	# Add item to build_spool_batch.pl utility

			### Elevation, m
my $elevation = $runIO{Elevation} ? read_Layer($extent,\%meta, $runIO{Elevation}, $MagicT,{PATCH_VALUE=>0}) : $zeroes;

			### Glacier Area, Melt and Volume (optional)
my ($glMelt, $glArea, $glVolume) = ($zeroes->copy, $zeroes->copy, $zeroes->copy);
my  $glScale = 1;
if ($runIO{Glaciers}) {
  print "Initialization of datasets for Glacier inputs...";
  my %param		= read_param_str(\%runIO, 'Glaciers');
  $dataSet{glMelt}	= new RIMS::DataSet(attrib($param{glMelt},  $MagicT, 'Glaciers->glMelt'));
  $dataSet{glArea}	= new RIMS::DataSet(attrib($param{glArea},  $MagicT, 'Glaciers->glArea')); if (exists $param{glVolume}){
  $dataSet{glVolume}	= new RIMS::DataSet(attrib($param{glVolume},$MagicT),	{UNITS=>'m3'});
    my $glMeltExt	= get_extent($dataSet{glMelt}  ->{fileList}[0][0],	{MASK =>0});
    my $glVolumeExt	= get_extent($dataSet{glVolume}->{fileList}[0][0],	{MASK =>0});
    die "Mismatch in \"glVolume\" and \"glMelt\" dataset spatial resolutions (presently required). Aborting...\n\n"
	if abs($$glMeltExt{cellsizeX} - $$glVolumeExt{cellsizeX}) > 0.0001 * $$glVolumeExt{cellsizeX} ||
	   abs($$glMeltExt{cellsizeY} - $$glVolumeExt{cellsizeY}) > 0.0001 * $$glVolumeExt{cellsizeY};

  push @{$runIO{spool_list}},	[$dataSet{glVolume}, {RESAMPLE=>0,PATCH_VALUE=>0}]; }
  push @{$runIO{spool_list}},	[$dataSet{glMelt},   {RESAMPLE=>0,PATCH_VALUE=>0}],
				[$dataSet{glArea},   {RESAMPLE=>5,PATCH_VALUE=>0}];

  $glArea = read_dateLayer($dataSet{glArea},$$date_list[0],$extent,$runIO{spool},
			{DATE_SEARCH_OPT=>{YR_UP=>1}, RESAMPLE=>5})->clip(0, 0.99);
  die "\n\nGlacier Area dataset must be in land/area fraction units. Aborting...\n\n"
	unless $dataSet{glArea}->{MT_attrib}{Units} =~ m/frac/i;
			### Calculate glacier melt scaling factor due to difference resolutions of source and Network grids
  $glScale	= gl_scale($dataSet{glMelt}, $dataSet{glArea}, $glArea, $CELL_AREA, $$date_list[0], $runSet);
  printf " Avg scaling = %.4f\n", ref($glScale) ? $glScale->where($glScale > 0)->avg : $glScale;
}
			### Bias correction for climate simulation data (e.g. IPCC AR5)
my @precip_bias	= read_climate_bias($dataSet{precip}, $runIO{WM_mod_precip},$runIO{WM_obs_precip},$runSet,$MagicT);
my @airT_bias	= exists $dataSet{airT} ?
		  read_climate_bias($dataSet{airT},   $runIO{WM_mod_airT},  $runIO{WM_obs_airT},  $runSet,$MagicT):
		  read_climate_bias($dataSet{airTmin},$runIO{WM_mod_airT},  $runIO{WM_obs_airT},  $runSet,$MagicT);

			### Water Temperature Drivers
my ($cal2Kj, $baseflowTw, $grndWaterTw, $airT_rAvg, $airT_bAvg, $rAvgDays, $bAvgDays, $bflowScl);
if ($waterTemp) {
  print "Initialization of datasets for Water Temperature model...\n";
  die "\"airT_yc\"	input is required for Water Temperature model. Aborting...\n\n" unless $runIO{airT_yc};

  $grndWaterTw		= read_Layer($extent,\%meta, $runIO{airT_yc},	$MagicT,{PATCH_VALUE=>0,KEY=>'airT_yc'});
  $cal2Kj		= 4.1868 / 1000 * 100**2;	# Conversion coeff from Cal/cm2/d to kJ/m2/d
  $airT_rAvg		= $zeroes->copy;		# Running average air temperature (surface runoff)
  $airT_bAvg		= $zeroes->copy;		# Running average air temperature (baseflow)
  $rAvgDays		= 5;	# Number of days in running average window for air temperature (surface runoff)
  $bAvgDays		= 15;	# Number of days in running average window for air temperature (baseflow)
  $bflowScl		= 0.59;	# Baseflow/groundwater temperature scaling factor
  @grossRad		= map grossRadiation($lon,$lat,$_),0..364 unless ref $grossRad[0] || $hr_flag;
	 $waterTemp	= 1;
} else { $waterTemp	= 0; } # Do not run water Temperature unless wanted in the output

#######################################################################
		### Dissolved Inorganic Nitrogen (DIN) Drivers

my (%DIN_param, $luSub, $luAgr, $propSub, $propAgr, $DIN_Land_Load, $DIN_Load, $DIN_Used, $DIN_Prev,
    @DIN_WWTP,  $DIN_WWTP_Input, $DIN_WWTP_Load, $DIN_WWTP_Removal);
if (m/^DIN/ ~~ @list_out) {
  print "Initialization of datasets for DIN model...\n";
  die "DIN model requires \"DIN\" input...\n" unless $runIO{DIN};

  my %param		= read_param_str(\%runIO, 'DIN');
  $dataSet{luSub}	= init_DSet($extent,\%meta,$param{luSub},	$MagicT);	# Fraction developed
  $dataSet{luAgr}	= init_DSet($extent,\%meta,$param{luAgr},	$MagicT,	# Fraction agriculture
	{DATASET_OPT=>{START_YEAR_CLIP=>1800}});
  $DIN_param{scale}	= set_default($param{scale},	12.200);	# Spread of logistic function
  $DIN_param{asym}	= set_default($param{asym},	 1.400);	# Maximum (e.g. peak) DIN conc from human land-use/development
  $DIN_param{asym_ag}	= set_default($param{asym_ag},4.800) if $runIO{Irrigation};	# Optional Maximum DIN conc from agricultural land # Note: Irrigation is needed because that is where when we save out cropAreaFrac
  $DIN_param{xMid_b}	= set_default($param{xMid_b},	40.300);	# Intercept and
  $DIN_param{xMid_m}	= set_default($param{xMid_m},	19.500);	# Slope relating log(runoff) to xMid
  $DIN_param{BFI}	= set_default($param{BFI},	 0.600);	# Baseflow Index
  $DIN_param{intcpt}	= set_default($param{intercept},-2.975);	# Denitrification intercept
  $DIN_param{slope}	= set_default($param{slope},	-0.493);	# Denitrification slope
  $DIN_param{TnQ}	= set_default($param{TnQ},	 2.000);	# Water temperature factor
  $DIN_param{TnRef}	= set_default($param{TnRef},	20.000);	# Reference water temperature
  $DIN_param{SzRes}	= set_default($param{SzRes},	 0    );	# Seitzinger denitrification in reservoirs

		### DIN from Waste Water Treatment Plants (WWTP)
  if ($param{WWTP}) {
    die "Missing DIN WWTP parameters in the WBM Input file...\n\n"
	unless $param{WWTP}{PopDensity} && $param{WWTP}{WWTP_File};
    my $refYear		= $param{WWTP}{refYear};
    my $search_dist	= set_default($param{WWTP}{search_dist}, 5);	# Search distance for largest upstream, km
		# @DIN_WWTP units are kg/pix			#  Convert search radius to pixels (5 km default)
   @DIN_WWTP = add_DIN_WWPT($runSet, $param{WWTP}, $up_area, floor($search_dist / sqrt($CELL_AREA))->long);

    $dataSet{PopDens}	= new RIMS::DataSet(attrib($param{WWTP}{PopDensity}, $MagicT, 'WWTP->PopDens'));
    $DIN_param{refPop}	= read_dateLayer($dataSet{PopDens},$refYear.'-00-00',$extent,$runIO{spool})->lclip(1);
    $DIN_param{WWTP}	= 1;
    push @{ $runIO{spool_list}}, [$dataSet{PopDens}, {}];		# Add item to build_spool_batch.pl utility
  } else { ($DIN_WWTP_Input, $DIN_WWTP_Load, $DIN_WWTP_Removal) = ($zeroes, $zeroes, $zeroes); }
}   else {  $runIO{DIN}	= ''; }		# Do not run DIN  unless wanted in the output

#######################################################################
		### Open water mask (Lakes and Reservoirs)
my $lakeMask		= $zeroes->copy;	my $landMask = $ones->copy;	my $lakeMaskIdx = zeroes(2,0);
my $fracOpenWater	= $zeroes->copy;	my $doMasks  = 0;
my $openWaterMaskValue	= 0;
if ($runIO{openWater}) {
  my %param	= read_param_str(\%runIO, 'openWater');
  $openWaterMaskValue	= set_default($param{openWaterMaskValue}, 0);
  die "\"MaskDataset\" parameter is required in \"openWater\" entry. Aborting...\n\n" unless defined $param{MaskDataset};
	# Read lake mask
  if (isDataSet_TS($param{MaskDataset},$MagicT)) {
    $dataSet{lakeMask}	= new RIMS::DataSet(attrib($param{MaskDataset}, $MagicT));
    $doMasks		= 1;
    push @{$runIO{spool_list}}, [$dataSet{lakeMask}, {RESAMPLE=>6,PATCH_VALUE=>$openWaterMaskValue+1}];
  }
  else {
    $lakeMask	= read_Layer($extent,\%meta,$param{MaskDataset},$MagicT,   #  Resample 6 is for GDAL "mode" method
	{RESAMPLE=>6, PATCH_VALUE=>$openWaterMaskValue+1}) == $openWaterMaskValue;
    $lakeMaskIdx = whichND($lakeMask);
  }
	# Read openwater fraction of land
  if ($param{fracOpenWater}) {
    if (isDataSet_TS($param{fracOpenWater},$MagicT)) {
      $dataSet{fracOpenWater}	= new RIMS::DataSet(attrib($param{MaskDataset}, $MagicT));
      $doMasks			= 1;
      push @{$runIO{spool_list}}, [$dataSet{fracOpenWater}, {RESAMPLE=>1,PATCH_VALUE=>0}];
    }
    else {
      $fracOpenWater = read_Layer($extent,\%meta,$param{fracOpenWater},$MagicT,{RESAMPLE=>1,PATCH_VALUE=>0}) * $ones;
} } }

#######################################################################
		### Impervious Surface mask
my $imprSurf	= $zeroes->copy;
if($runIO{Impervious}) {
  if (isDataSet_TS($runIO{Impervious},$MagicT)) {
    $dataSet{Impervious} = new RIMS::DataSet(attrib($runIO{Impervious}, $MagicT, 'Impervious'));
    $doMasks		 = 1;
    push @{$runIO{spool_list}}, [$dataSet{Impervious}, {RESAMPLE=>1,PATCH_VALUE=>0}];
  }
  else {
    $imprSurf	= read_Layer($extent,\%meta,$runIO{Impervious},$MagicT,{PATCH_VALUE=>0,KEY=>'Impervious'}) * $landMask;
  }
}
#######################################################################
		### Scale down impervious surface and open water where it is too high (> 0.975)
my ($lakeMask0, $fracOpenWater0, $imprSurf0);
my ($imprCoef,  $imprArea, $rmArea);

if ($doMasks) {
  $lakeMask0		= $lakeMask     ->copy unless exists $dataSet{lakeMask};
  $fracOpenWater0	= $fracOpenWater->copy unless exists $dataSet{fracOpenWater};
  $imprSurf0		= $imprSurf     ->copy unless exists $dataSet{Impervious};
}
else {
	# Add full cell fracOpenWater to the lake mask
  $lakeMask     ->where(($fracOpenWater == 1) & ($lakeMask == 0))	.= 1;	$lakeMaskIdx = whichND($lakeMask);
  $fracOpenWater->where( $fracOpenWater == 1)				.= 0;
  $landMask  = !$lakeMask;
  $imprSurf *=  $landMask;		### Update impervious surface layer by new land mask
	# Scaling
  {
    my $sum	= $imprSurf + $fracOpenWater;
    my $idx	= whichND( $sum > 0.975);

    $imprSurf	->indexND($idx)	.= ($imprSurf      * 0.975 / $sum)->indexND($idx);
    $fracOpenWater->indexND($idx)	.= ($fracOpenWater * 0.975 / $sum)->indexND($idx);
  }
# my $imprCoef	= 0.2;			### Fraction of precip that goes to surface runoff # Pellerin 2008 Ipswich
  $imprCoef	= $imprSurf ** 0.4;	### Fraction of precip that goes to surface runoff # Alley and Veenhius 1983
  $imprArea	= $imprSurf * $CELL_AREA;
  $rmArea	= $imprSurf + $fracOpenWater;
  $glArea	= $landMask * $glArea->clip(0, 0.99 - $rmArea);
		### Update cell area layers
 ($cell_area, $soil_area, $mm_2_KM3, $mm_2_km3, $mm_2_m3, $m3_2_mm) = area_layers($CELL_AREA,$glArea,$rmArea,$lakeMask);
			( $MM_2_KM3, $MM_2_km3, $MM_2_m3 )	    = map $_*$subDf, $mm_2_KM3,$mm_2_km3,$mm_2_m3;
}
		### Save cell area layers
my $doArea	= $doMasks ?  1 : (exists($dataSet{glArea}) ? (isDataSet_TS($dataSet{glArea}) ? 1 : 0) : 0);
write_cell_area($CELL_AREA, $cell_area, $soil_area, $doArea, \%runIO, $grid, $credits)	unless $noOutput;

#######################################################################
		### Climate lapse rate downscaling
my ($airTLapse, $lapseDelta) = lapseDownscale_init($runSet, $MagicT);

#######################################################################
		### Snow Melt
my $MDSnowFallThreshold	= -1;
my $MDSnowMeltThreshold	=  1;
my($snowFall, $snowMelt, $snowPack, $sPackChg, $snowInFr, $rainInFr) = map($zeroes->copy, 1..6);
		### Snow Bands
my($indxBand, $snowIndN, $snowIndS, $elevBand, $airTBand, $snowBand, $sFall, $sMelt) = snowBands_init(
	$runIO{Network},$airTLapse,$snowFall,$snowMelt,$lat->dummy(0,$$extent{ncols}),$glArea)
		if $runIO{snowBands};
my $snowRouteN	= int(1/sqrt($CELL_AREA->avg))+1;	# Number of snow routing steps

#######################################################################
		### Soil Moisture, Surplus and Infiltration Parameters
	### Soil moisture drying coeff Alpha (1 is wetter, 10 is drier, 20 is max)
	### $alpha = 2 (Alex P.); 5 (Dominik); 1.2 (Rob Stewart for NE US and Hamon PET)
my $alpha		= set_default($wbmParam{alpha},		5);
my $Infiltration	= set_default($wbmParam{Infiltration},	'FrAMES');
my $MDInfiltrationFrac	= set_default($wbmParam{infiltrFrac},	0.5);	# Can be a number or spatially variable layer
   $MDInfiltrationFrac	=($ones * read_Layer($extent, \%meta, $MDInfiltrationFrac, $MagicT, {PATCH_VALUE=>0.5}))->lclip(0.001);
my $MDGroundWatBETA	= set_default($wbmParam{groundWatBETA},	0.0167);
   $MDGroundWatBETA	= $ones * read_Layer($extent, \%meta, $MDGroundWatBETA,    $MagicT, {PATCH_VALUE=>0.0167});
my $HBV_Beta		= set_default($wbmParam{HBV_Beta},	60);
my $RhRt2		= set_default($wbmParam{RhRt2},		0.56);	### Runoff storage release (R_hole/R_tank)^2
   $RhRt2		= $ones * read_Layer($extent, \%meta, $RhRt2,		   $MagicT, {PATCH_VALUE=>0.56});
my $G2			= 2 * 9.80665;					### 2 * acceleration of gravity, m/sec2
my $irrGm               = set_default($wbmParam{irrGm},         0);     ### Flag to simulate stage 2 soil gamma AET
                                                                        ###   in irrigated crop areas
my $MaxSurfRffStorage	= set_default($wbmParam{maxRffStorage},	1e3);	### Limit to runoff storage, mm
my $openWtrCoeff	= set_default($wbmParam{openWtrCoeff},	1);	### Open water evaporation scaling relative to PET
my $scrPrintScale	= set_default($wbmParam{scrPrintScale},	1);
my $outputScale		= set_default($wbmParam{outputScale}, 'Cell');	### Scaling output vars to full cell area
   $outputScale		= $outputScale =~ m/Cell/i ? 1 : 0;

		### Groundwater, Rain Interception
my @grdWater		= ($zeroes->copy);	### Initialized with zeroes-Problem (?)
my $grdWaterChg		=  $zeroes->copy ;	### Groundwater change in dt time step
my $InterceptionStorage	=  $zeroes->copy ;	### I suggest to use long term climatology
my @surfRffStorage	= ($zeroes->copy);	### Surface Runoff Retention Pool
my @irrRffStorage	= ($zeroes->copy);	### Irrigation Runoff Retention Pool
my $UGW			=  $zeroes->copy ;	### Unsustainable groundwater pool

		### Discharge
my $flowInPrev	= $zeroes->copy;
my $discharge	= $zeroes->copy;		### Initialized with zeroes-Problem (?)
my @mDischarge	= map $zeroes->copy+1e-6, 0..5;	### Mean Discharge sum and count for
my @mDischargeN	= map 0, @mDischarge;		  # 5-year running average
my $mDischargeK = 0;

		### Routing velocity method. Presently 'Constant' and AASH (At-A-Site Hydraulics)
my $velMethod	= set_default($routing{velMethod},	'Constant');	# Method of flow velocity
die "Unknown Routing Velocity Method: $velMethod...\n" unless $velMethod =~ m/Constant|AASH/i;

		### Routing: LRR/Muskingum routing constants
my $wgtLength	= set_default($routing{weightedLength},	'No');		# LRR
my $beta	= set_default($routing{beta},		2   );		# Muskingum
my $slope	= set_default($routing{slope},		0.1/1000);	# Muskingum
my $xi		= 1 + $beta*(2/3)/($beta+1);				# Muskingum
# Test (30', NCEP)-  2915.7 km3 instream storage.
# Test (30', MERRA)- 2147.2 km3; (6')- 6859.9; monthly- 2298.2

my $eta		= set_default($routing{eta},		0.25);		# Stream Geometry # Depth (Coefficient)
my $nu		= set_default($routing{nu},		0.40);		# Stream Geometry # Depth (exponent)
my $tau		= set_default($routing{tau},		8   );		# Stream Geometry # Width (Coefficient)
my $phi		= set_default($routing{phi},		0.58);		# Stream Geometry # Width (exponent)
my $cVelocity	= set_default($routing{velocity},	2.18);		# Constant flow velocity@30min, km/h
my $aas_b	= set_default($routing{aas_b},		0.10);		# At-a-site stream geometry # Width
my $aas_f	= 0.5 - $aas_b;	# (b,f,m) relations: b+f+m=1; b+f=m	# At-a-site stream geometry # Depth
my $aas_m	= $aas_b + $aas_f;					# At-a-site stream geometry # Velocity
my $vDelta	= 1 / $eta / $tau;
my $vEpsilon	= 1 - $nu  - $phi;					# Stream geometry parameters (altogether)
my $riverParams	=[$eta, $tau, $vDelta, $nu, $phi, $vEpsilon, $aas_f, $aas_b, $aas_m];

		### Constant flow velocity for LRR (Linear Reservoir Routing)
		# 30' network flow velocity (2.18 km/hr) is corrected for
		# stream length using eq. (3) from Fekete et al., 2001
die "Fix code below for non-geographical projections. Aborting...\n\n"
	if $$extent{projection} ne 'epsg:4326';
my $flowSpeed	= $cVelocity * (1-0.077*log($$extent{cellsize}/0.5));
# Velocity tests (30', MERRA)- 2.4->1927.7; 2.2->2102.9; 2.18->2122.2	(match is 2120 km3)
# Velocity tests (6', MERRA)- 2.18*FeketeCorrection->2035.2
my $flowCoeff	= 1/(1 + $dL*3600/$flowSpeed/$dt)->copybad($network);

		### Stream Geometry
my ($depth, $width, $length, $velocity) =  map $ones->copy, 1..4;
my  $lStream	= $dL*1000 / (1 - 0.077*log(sqrt($CELL_AREA)));	# Stream segment length, in m
	# It accournt for the length increase factor relative to 1 km cells (Fekete et al., 2001)

#######################################################################
	### Aquifers, Springs/Karst Data

my($aqfType,  $aqf_data, $AQF_DATA,$aquiferCap)	= aquifer_init($runSet, $lakeMask);
my($spr_data, $snk_data, $spr_dschCap)		= springs_init($runSet, $aqfType, $aqf_data);
my @snkStack    = make_stack($snk_data, ['Infil','Aqf_ID','INFIL'], \%cell_ind);	# Sink data stack for routing

my $spr_flag	= keys(%$spr_data) ? 1 : 0;				# Flag to use springs
my $snk_flag	= $snkStack[1]->dim(0) > 1;				# Flag to use sinks
my $snk_idx	= $cell_table(3:4,$snkStack[1]->(:-2))	if $snk_flag;	# 2D indices of sink locations
my $aquiferID	= aqf_ID_mask($aqf_data, $extent)	if $aqfType;	# 2D mask of aquifer IDs
my @aqf_IDs	= sort keys %$aqf_data			if $aqfType;	# Sorted aquifer IDs
my $AQF_IDs	= $aqfType ? pdl(@aqf_IDs) : pdl();			# PDL object of the above
my $spr_dschT_y	= 0;							# Annual total discharge from springs,	km3
my $snk_inflT_y	= 0;							# Annual total inflows from sinks,	km3
foreach my $aqf_ID (@aqf_IDs) { map $$aqf_data{$aqf_ID}{$_.'_y'} = 0, @{$$AQF_DATA{sumKeys}}; }	# Initialization

#######################################################################
	### USGS Data Assimilation for the Forecast mode

my $usgs_data	= usgs_init($runSet);
my @usgsStack	=(zeroes(3,1), long([-1]));	# Required to run "routing" in case of lack of USGS sites
my $usgsDecay	= 0.9;				# USGS disch delta inverse decay rate per day: $usgsDecay=(1-decay rate)
my $usgsDeltaT_y= 0;				# Annual total flow difference from USGS sites, km3
		# Mapping between Cell Table ID (used in stack) and USGS site ID
my %usgs_CT	= map(($cell_ind{$$usgs_data{$_}{Col}.'_'.$$usgs_data{$_}{Row}} => $_),keys(%$usgs_data)) if $usgs_data;

#######################################################################
	###  Buckets of water components

	### Component functionality switch
my @compSwitch	= (	# Bucket # 0 (water age)
	m/streamAge/					~~ @list_out	? 1 : 0,
			# Bucket # 1 (by runoff source)
	m/_(gm|sm|rr|bf)$/				~~ @list_out	? 1 : 0,
			# Bucket # 2 (by primary source)
	m/_(pg|ps|pr|pu)$/				~~ @list_out	? 1 : 0,
			# Bucket # 3 (by irrigation use)
	m/_(pst|irr|use|rlt)$/				~~ @list_out	? 1 : 0,
			# Bucket # 4 (water temperature)
	m/(runoff|discharge)_twt$/			~~ @list_out	? 1 : 0,
			# Bucket # 5 (spatial runoff mask)
	$runIO{Runoff_mask}						? 1 : 0,
			# Bucket # 6 DIN - Dissolved Inorganic Nitrogen
	m/^DIN/						~~ @list_out	? 1 : 0);

$compSwitch[3]	= 0 unless $runIO{Irrigation};				# Cancel Irr tracking, if Irr turned off
my %compBal;								# Hash to store values for tracking component balance
my $endoComp	=  m/^endoStrg_(mm_|km3_)*(pg|ps|pr|pu)/~~ @list_out;	# Flag to run endorheic lakes  components
my $BwGwFlag	=  m/(Moist(Gr|Bl)|[BG]W_(ET|SM))/	~~ @list_out;	# Flag to run Green/Blue water components
my $compBalFlag	= set_default($wbmParam{compBalance},  0);		# Flag to run tracking component balance
   $compBalFlag	= 0 unless List::Util::sum(@compSwitch[2,3]);		# Cancel tracking balance, if no need
my $skipCompBal	= 0;							# Skip balance check (one time only)
my $nIrr	= irrCycle_init(\%runIO, \@list_out, $noOutput);	# N tracking of irrigation water reuse cycles
my @compName	= qw/Age rSource pSource Irr Temp Msk DIN/;		# Short names for printing tracking buckets
my $cString	= join(' ', map(($compSwitch[$_] ? $compName[$_] : ''), 0..$#compSwitch));
   $cString	=~ s/^\s+|\s+$//g;		# Trim
   $cString	=~ s/(Irr)/$1\($nIrr\)/g;	# Add number of irr tracking cycles

#######################################################################

my $cSwitch	= pdl(long,\@compSwitch);
my $zeroes_4	= zeroes(double,$network->dims,4)	if $compSwitch[1] || $compSwitch[2] || ($compSwitch[3] && $nIrr==1);
my $zeroes_N	= $nIrr==1 ? $zeroes_4 :
		  zeroes(double,$network->dims,3+$nIrr)	if $compSwitch[3];
my $px4a	= pdl(1,0,0,0)->reshape(1,1,4);
my $px4b	= pdl(0,1,0,0)->reshape(1,1,4);
my $px4c	= pdl(0,0,1,0)->reshape(1,1,4);
my $px4d	= pdl(0,0,0,1)->reshape(1,1,4);
my $pxNa	= pdl(0,1,((0) x ($nIrr+1)))->reshape(1,1,$nIrr+3);
my $pxNd	= pdl(1,  ((0) x ($nIrr+2)))->reshape(1,1,$nIrr+3);
my($cSIR_prev, $cSIR_P_prev,  $cSIR_Irr_prev, $cSIR_Msk_prev) = (0,0,0,0);	# Needed for SIR special tracking order

	### Pre-processing of spatial mask components (Bucket # 5)
my($rnffMask, $rnffID, $mskModel) = runoffMask_init($runSet, $runAttr, \@list_out, $noOutput);

	# Bucket # 0: Water age
my $streamAge	= $compSwitch[0] ? zeroes(double,$network->dims) : pdl();

	# Bucket # 1: 0- Snow melt; 1- Glacial melt; 2- Rain sfc runoff; 3- Base flow
my $cRffStg	= $compSwitch[1] ? $zeroes_4->copy : pdl();
my $cRunoff	= $compSwitch[1] ? $zeroes_4->copy : pdl();
my $cSIR	= $compSwitch[1] ? $zeroes_4->copy : pdl();
my $cVIS	= $compSwitch[1] ? $zeroes_4->copy : pdl();
my @cStream	= $compSwitch[1] ?($zeroes_4->copy): pdl();			#	non-zero initial storages
   $cStream[0]->(,,2) .= 1				if $compSwitch[1];	# Rainwater = 1
   $cSIR	(,,2) .= 1				if $compSwitch[1];

	# Bucket # 2: 0- Snow melt; 1- Glacial melt; 2- Rain water; 3- Unsustainable water
my $cRffStgP	= $compSwitch[2] ? $zeroes_4->copy : pdl();
my $cRunoffP	= $compSwitch[2] ? $zeroes_4->copy : pdl();
my $cSoilP	= $compSwitch[2] ? $zeroes_4->copy : pdl();
my $cSIR_P	= $compSwitch[2] ? $zeroes_4->copy : pdl();
my $cVIS_P	= $compSwitch[2] ? $zeroes_4->copy : pdl();
my @cStreamP	= $compSwitch[2] ?($zeroes_4->copy): pdl();
my $cGrWaterP	= $compSwitch[2] ? $zeroes_4->copy : pdl();			# Must be initialized for all
my $cEndoP	= $compSwitch[2] ?($endoComp?$zeroes_4->copy:$zeroes_4):pdl();	#	non-zero initial storages
   $cStreamP[0]->(,,2) .= 1				if $compSwitch[2];	# Rain = 1
   $cSoilP	 (,,2) .= 1				if $compSwitch[2];
   $cSIR_P	 (,,2) .= 1				if $compSwitch[2];
my %cAqWaterP = map(($_=>$px4c->copy), @aqf_IDs)	if $compSwitch[2];

	# Bucket # 3: 0- Relict/Unsustainable water; 1- Pristine; 2- Dom/Ind/Stk use return; 3+ - Irrigation return
my $cRffStgIrr	= $compSwitch[3] ? $zeroes_N->copy : pdl();
my $cRunoffIrr	= $compSwitch[3] ? $zeroes_N->copy : pdl();
my $cSoilIrr	= $compSwitch[3] ? $zeroes_N->copy : pdl();
my $cSIR_Irr	= $compSwitch[3] ? $zeroes_N->copy : pdl();
my $cVIS_Irr	= $compSwitch[3] ? $zeroes_N->copy : pdl();
my @cStreamIrr	= $compSwitch[3] ?($zeroes_N->copy): pdl();			# Must be initialized for all
my $cGrWaterIrr	= $compSwitch[3] ? $zeroes_N->copy : pdl();			#	non-zero initial storages
   $cStreamIrr[0]->(,,0) .= 1				if $compSwitch[3];	# Relict = 1
   $cSoilIrr	   (,,0) .= 1				if $compSwitch[3];
   $cSIR_Irr	   (,,0) .= 1				if $compSwitch[3];
my %cAqWaterIrr= map(($_=>$pxNd->copy), @aqf_IDs)	if $compSwitch[3];

	# Bucket # 4: Water Temperature
my $cRunoffTw	= $compSwitch[4] ? zeroes(double,$network->dims) : pdl();
my $cStreamTw	= $compSwitch[4] ? zeroes(double,$network->dims) : pdl();

	# Bucket # 5: Spatial  masks
my $cRffStgMsk	= $compSwitch[5] ? zeroes(double,$rnffMask->dims) : pdl();
my $cGrWaterMsk	= $compSwitch[5] ? zeroes(double,$rnffMask->dims) : pdl();
my $cRunoffMsk	= $compSwitch[5] ? $rnffMask->copy : pdl();			# Case of "simple" model
my $cSIR_Msk	= $compSwitch[5] ? $rnffMask->copy : pdl();			# Must be initialized for all
my $cVIS_Msk	= $compSwitch[5] ? $rnffMask->copy : pdl();
my @cStreamMsk	= $compSwitch[5] ?($rnffMask->copy): pdl();

	# Bucket # 6: DIN - Dissolved Inorganic Nitrogen
my $cRunoffDIN	= $compSwitch[6] ? zeroes(double,$network->dims) : pdl();	# Must be initialized for all
my $cStreamDIN	= $compSwitch[6] ? zeroes(double,$network->dims) : pdl();
my $cStmConDIN	= $compSwitch[6] ? zeroes(double,$network->dims) : pdl();	# Conservative DIN

#######################################################################
	### Initialization of Land/Crop and Irrigation datasets

my %land	= land_init(@$runSet);
my $irrCropList	= delete $land{irrCrop}{cropList};
my $landPatch	= delete $land{patch};
my @sMoist	= ($zeroes->copy);
my $sMoistFrac	= 0.5 * $ones;		# Initial soil moisture fraction
my(%sMoistGr, %sMoistGrET, %sMoistET);	# BW/GW additions-
my(%sMoist,   %sMoistPrev,   %sMoistFrac);
my(%sMoistVs, %sMoistPrVs,   %sMoistFrVs);	### Virtual soil for irrigated crops to track potential Irr demand
my(%cropMask, %cropMaskPrev, %irr_demand, %irr_appRte, %irr_demVs, %irr_demVrt, %irrigation, %irrTech);
my @irrVIS	= ($zeroes->copy);		### Virtual Irrigation Storage to simulate daily irrigation use
my $irrEqpArea	= 0;	my $cropAreaFrac;

		### Irrigation parameters
my $irrEfficiencyData;
my $nIrrCrops       = $runIO{Irrigation} ? scalar(keys(%{$land{irrCrop}})) : 0;
my $irrDemVsT_y     =   0;			### Annual Net/Denamd Irr total		# Virtual soil (Net = Demand)
my @irrDemVsT_y     = ((0) x $nIrrCrops);	### Annual Net/Denamd Irr by crop	# Virtual soil (Net = Demand)
my @irrNetT_y       = ((0) x $nIrrCrops);	### Annual Irr (Net)      by crop
my($irrNetT_y, $irrGrossT_y)	= (0,0);	### Annual Irr, Net and Gross
my @irrXW_y	    = ((0) x 4);		### Annual Irr from (Surface, Ground, Aquifer, Unsust) storages
my $irrPaddyT_y     =   0;			### Annual Irr for rice paddies
my $irrIneffRnff_y  =   0;			### Annual Inefficient Irr water runoff
my $irrIneffPerc_y  =   0;			### Annual Inefficient Irr water percolation
my $irrIneffEvap_y  =   0;			### Annual Inefficient Irr water evaporation
my $irrPercDeliv_y  =   0;			### Annual Delivery    Irr water percolation
my $irrEvapDeliv_y  =   0;			### Annual Delivery    Irr water evaporation
my $SW_ratio;					### Surface to Groundwater ratio for irrigation water intake
my %irrParam        = read_param_str(\%runIO, 'Irrigation');
   $irrParam{IrrSearchDist}	= $irrParam{SearchDist} if exists $irrParam{SearchDist}; # Backward compatibility
my $irrEfficiency   = set_default($irrParam{IrrEfficiency},1);	### Irrigation efficiency
my $dailyIrr	    = set_default($irrParam{dailyIrr},	 0  );	### 0- by crop depletion factor;    1- daily Irr
my $useVIS	    = set_default($irrParam{useVIS},	 1  );	### 0- no VIS, Virtual Irr Storage; 1- use VIS
my $irrAppRate	    = set_default($irrParam{irrAppRate}, 50 );	### Max Irr application rate, mm/day
my $irrOrder	    = set_default($irrParam{Order},	 1  );	### Irr Order: 0- Grnd Water; 1- Stream priority;
my $irrReturnCoef   = set_default($irrParam{ReturnCoeff},1.0);	### Irrigation return partitioning
my $irrExtraCoeff   = set_default($irrParam{ExtraCoeff}, 1.0);	### Factor of unsustainable water withdrawal
my $irrAqfCoeff     = set_default($irrParam{AqfCoeff},   1.0);	### Factor of water withdrawal from aquifers
my $irrSearchDist   = set_default($irrParam{IrrSearchDist},100);	### Search distance for stream water, km
   $irrSearchDist   = floor($irrSearchDist/sqrt($CELL_AREA))->long;	### Convert search distance to pixels
my $irrGrwthRefYr   = set_default($irrParam{irrGrwthRefYr},0);	### Irrigated area growth reference year
my $irrAreaGrwth    = set_default($irrParam{irrAreaGrwth}, 0);	### Irrigated area growth, 1/yr (0.01179274)
my $crpGrwthRefYr   = set_default($irrParam{crpGrwthRefYr},0);	### Cropland  area growth reference year (2000)
my $crpAreaGrwth    = set_default($irrParam{crpAreaGrwth}, 0);	### Cropland  area growth, 1/yr (0.00237444)
my $extendCropTS    = set_default($irrParam{extendCropTS}, 0);	### Extend crop TS data to the run dates
my $ricePercRate    = set_default($irrParam{ricePercolation}, 5);	### Rice paddy percolation rate, mm/day
my $ricePaddyDepth  = set_default($irrParam{ricePaddyDepth}, 50);	### Rice paddy depth, mm
my $ricePaddyStrg   = $runIO{Irrigation} ? $zeroes->copy : $zeroes;	### Rice paddy water storage, mm
my $irrRunoff       =   0;					### Irrigation runoff, mm/day
my($precipT_y,    $runoffT_y,    $RunoffT_y, $dischT_y,		### Tracking annual total precip, runoff, discharge,
   $etNonCropsT_y,$etIrrCropsT_y,$etRfdCropsT_y) = ((0) x 7);	###	ET for non/irr/rfd lands
my $irrFormat_str   = "%.2f : ".join(' ',("%.2f")x($nIrrCrops));	### Format string (by number of crops)
								### Flag for virtual soil and potential Net irrigaton
my $vs_flag	    = $aqfType || (isNumber($irrExtraCoeff) && $irrExtraCoeff < 1) ? 1 : 0;

my $riceMask	= $runIO{Irrigation} ? $zeroes->copy : 0;	### Irrigated rice masks
my($irrArea,	  $nonIrrArea,
   $IrrUseStrgLoc,$IrrUseFlowLoc,$irrUseFlowLoc,		### Irr water use from local  sources
   $IrrUseStrgRmt,$IrrUseFlowRmt,$irrUseFlowRmt,$irrUseFlowRID,	### Irr water use from remote sources
   $irrUseSIR,	  $irrUseGrwt,	 $irrUseAqf,			### Irr water use from SIR: groundwater storages
   $irrUseExtra,  $irrVIS_delta)	= ((0) x 14);		### Irr water use from unsustanable groundwater
								### Irr water re-use from tracking (line below)
my($UseReuseOutGross_irr, $IrrReuseInGross_irr, $IrrReuseInGrossFlow_irr, $IrrReuseInGrossGrwt_irr, $IrrReuseInGrossAqf_irr,$IrrReuseOutNet_irr, $IrrReuseOutNonben_irr,$irrFracInGross);

if ($runIO{Irrigation}) {
  $dataSet{CropAreaFrac} = delete $land{allCropFr};
		### Find/Set reference and first/last year for total CropAreaFrac dataset [time series]
  my $isTS	 = ref($dataSet{CropAreaFrac}) =~ m/DataSet/ && !$dataSet{CropAreaFrac}{climatology};
  $crpGrwthRefYr = 0 if $isTS;
  $irrParam{CropArea}{fstYear} = $isTS ? substr($dataSet{CropAreaFrac}{dateList}[ 0],0,4) : 0;	# Fist TS year
  $irrParam{CropArea}{lstYear} = $isTS ? substr($dataSet{CropAreaFrac}{dateList}[-1],0,4) : 0;	# Last TS year
  die "Parameter 'crpGrwthRefYr' must be set for this WBM simulation. Aborting...\n\n"
	if !$isTS && ($crpGrwthRefYr == 0 && $crpAreaGrwth != 0);
		### Find/Set reference and first/last year for each Irr CropAreaFrac dataset [time series]
  my($ldFr, $check, $fstYr, $lstYr) = ('LandFraction', undef, 0, 0);
  foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (keys %{$land{$lnd}}) {
    my $isTS	   = ref($land{$lnd}{$tp}{$ldFr}) =~ m/DataSet/ && !$land{$lnd}{$tp}{$ldFr}{climatology} ? 1 : 0;
    $irrGrwthRefYr = 0 if $isTS;
    $land{$lnd}{$tp}{$ldFr}{fstYear} = $isTS ? substr($land{$lnd}{$tp}{$ldFr}{dateList}[ 0],0,4) : 0;	# Fist TS year
    $land{$lnd}{$tp}{$ldFr}{lstYear} = $isTS ? substr($land{$lnd}{$tp}{$ldFr}{dateList}[-1],0,4) : 0;	# Last TS year
    die "Parameter 'irrGrwthRefYr' must be set for this WBM simulation. Aborting...\n\n"
	if !$isTS && (!$irrGrwthRefYr && $irrAreaGrwth);
    die "Irrigated crop fraction must be time series or static for all crops...\n" if defined $check && $check != $isTS;
   ($check,$fstYr,$lstYr) = ($isTS, $land{$lnd}{$tp}{$ldFr}{fstYear}, $land{$lnd}{$tp}{$ldFr}{lstYear});
    $sMoist{$lnd}{$tp}	  = 0;	# This needs to be initialized
  }}
		### Irrigation Efficiency can be file or time series (defined in the Magic table)
  $irrEfficiencyData = init_DSet($extent,\%meta,$irrEfficiency,$MagicT,{PATCH_VALUE=>100});
		### Surface to Groundwater ratio for irrigation abstractions, from the FAO
  $SW_ratio = $irrParam{SW_GW_ratio} ? read_Layer($extent,\%meta,$irrParam{SW_GW_ratio},$MagicT,{PATCH_VALUE=>1}) :
	$zeroes+$irrOrder;
  die "Presently \"SW_GW_ratio\" cannot be a time series. Aborting...\n\n" if ref($SW_ratio) =~ m/DataSet/;
		### Rice paddy percolation rate, mm/day
  $ricePercRate	= read_Layer($extent, \%meta, $ricePercRate,	$MagicT,{PATCH_VALUE=>5});
  die "Presently \"ricePercolation\" cannot be a time series. Aborting...\n\n" if ref($ricePercRate) =~ m/DataSet/;
		### Factor of unsustainable water withdrawal (grid)
  if (isNumber($irrExtraCoeff) && isNumber($irrAqfCoeff) && $irrExtraCoeff == 1 && $irrAqfCoeff == 1) {
    $irrExtraCoeff = $ones; }
  else {
    $irrExtraCoeff = read_Layer($extent,\%meta,$irrExtraCoeff,	$MagicT,{PATCH_VALUE=>1});
    my $irrAqfCff  = read_Layer($extent,\%meta,$irrAqfCoeff,	$MagicT,{PATCH_VALUE=>1});
    die "Presently \"ExtraCoeff\" cannot be a time series. Aborting...\n\n" if ref($irrExtraCoeff) =~ m/DataSet/;
    die "Presently \"AqfCoeff\"   cannot be a time series. Aborting...\n\n" if ref($irrAqfCff)     =~ m/DataSet/;
    $irrExtraCoeff = $aqfType ? $irrExtraCoeff*(!$aquiferID) + $irrAqfCff*(!!$aquiferID) : $ones*$irrExtraCoeff;
  }
		### Irrigation Technology initialization
  %irrTech	= irrTech_init($extent,\%meta, $MagicT, \%irrParam);
		### Print irr schedule info
  printf "Irrigation schedule    : %s\n", $dailyIrr ? 'daily application' : 'by crop depletion factor';
  printf "Virtual Irr storage    : %s\n\n", $useVIS ? 'Yes'               : 'No';
  printf "Total cropland growth rate outside data range: %s\n", $crpAreaGrwth  ? $crpAreaGrwth  : 'None';
  printf "Total cropland growth reference year(s)      : %s\n", $crpGrwthRefYr ? $crpGrwthRefYr :
	sprintf "[%d, %d]", $irrParam{CropArea}{fstYear}, $irrParam{CropArea}{lstYear};
  printf "Irrigated land growth rate outside data range: %s\n", $irrAreaGrwth  ? $irrAreaGrwth  : 'None';
  printf "Irrigated land growth reference year(s)      : %s\n", $irrGrwthRefYr ? $irrGrwthRefYr :
	sprintf "[%d, %d]", $fstYr, $lstYr;
  print "\n";

  die "Daily irrigation schedule is not compatible with process based irrigation efficiency model. Aborting...\n\n"
	if $dailyIrr && $irrTech{Application}{Process};
}
		### Land Fraction and awCap initialization for Non-Crops
		### + Initialization of dataset slices
my (%landFrac, %awCap, %crpDF, %ricePaddyWater, %ind);
foreach my $lnd (keys %land) {				next if $lnd =~ /crop/i;
  foreach my $tp (keys %{$land{$lnd}}) {
    $awCap{$lnd}{$tp}	 = $land{$lnd}{$tp}{awCap};	next if $lnd =~ /fallow/i;

    $landFrac{$lnd}{$tp} = $land{$lnd}{$tp}{LandFraction}->copy; # copy- will be modified by croplands
    $ind{$lnd}{$tp}	 = whichND($landFrac{$lnd}{$tp} > 0);
}}
my %Kc_par = ('generalLand' => {'land' => $ones}, 'fallow' => {'land' => $ones});

#######################################################################
	### Water Demand for domestic, industrial, and livestock consumption

my (%LiveStock,   $livestockDemand, $domesticDemand, $industryDemand);
my ($domReturnFr, $DomUseStrgLoc,   $DomUseFlowLoc,  $DomUseStrgRmt, $DomUseFlowRmt, $domUseFlowRID,
    $domUseSIR,   $domUseGrwt,      $domUseExtra,    $domUseAqf,     $domUseFlowRmt, $domUseFlowLoc,
    $indReturnFr, $IndUseStrgLoc,   $IndUseFlowLoc,  $IndUseStrgRmt, $IndUseFlowRmt, $indUseFlowRID,
    $indUseSIR,   $indUseGrwt,      $indUseExtra,    $indUseAqf,     $indUseFlowRmt, $indUseFlowLoc,
    $stkReturnFr, $StkUseStrgLoc,   $StkUseFlowLoc,  $StkUseStrgRmt, $StkUseFlowRmt, $stkUseFlowRID,
    $stkUseSIR,   $stkUseGrwt,      $stkUseExtra,    $stkUseAqf,     $stkUseFlowRmt, $stkUseFlowLoc,
    $domUseGross, $domUseEvap,      $domUseSfWt,     $domUseGrWt,
    $indUseGross, $indUseEvap,      $indUseSfWt,     $indUseGrWt,
    $stkUseGross, $stkUseEvap,      $stkUseSfWt,     $stkUseGrWt,    $stkRefYear,
    $domGrossT_y, $domNetT_y,       $indGrossT_y,    $indNetT_y,     $stkGrossT_y, $stkNetT_y) = ((0) x 55);
my  @domXW_y	    = ((0) x 4);		### Annual Dom from (Surface, Ground, Aquifer, Unsust) storages
my  @indXW_y	    = ((0) x 4);		### Annual Ind from (Surface, Ground, Aquifer, Unsust) storages
my  @stkXW_y	    = ((0) x 4);		### Annual Lsk from (Surface, Ground, Aquifer, Unsust) storages
my ($domSearchDist, $indSearchDist, $stkSearchDist);	# Water demand surface water search distance, km

if ($runIO{WaterDemand}) {
  my %param		= read_param_str(\%runIO, 'WaterDemand');
  $dataSet{Population}	= new RIMS::DataSet(attrib($param{PopDensity}, $MagicT, 'WaterDemand->Population'));
  push @{$runIO{spool_list}}, [$dataSet{Population}, {}];	# Add item to build_spool_batch.pl utility

		### Domestic demand parameters (per capita, per year)
  $dataSet{DomDemandPP}	= init_DSet($extent,\%meta,$param{DomDemandPP},$MagicT,{PATCH_VALUE=>0});
  $domReturnFr		= set_default($param{DomDemandReturnFr},	 0.84);	### Return fraction in domestic water use
  $domSearchDist	= exists $param{DomSearchDist} ? floor($param{DomSearchDist}/sqrt($CELL_AREA))->long : $irrSearchDist;

		### Industrial demand parameters (per capita, per year)
  $dataSet{IndDemandPP}	= init_DSet($extent,\%meta,$param{IndDemandPP},$MagicT,{PATCH_VALUE=>0});
  $indReturnFr		= set_default($param{IndDemandReturnFr},	 0.89);	### Return fraction in industrial water use
  $indSearchDist	= exists $param{IndSearchDist} ? floor($param{IndSearchDist}/sqrt($CELL_AREA))->long : $irrSearchDist;

		### Livestock demand parameters
  die "\nValid LivestockParam entry is required for the WaterDemand block. Aborting...\n\n"
	unless defined($param{LivestockParam}) && -e $param{LivestockParam};
  %LiveStock		= stock_init($param{LivestockParam});
  $stkReturnFr		= set_default($param{StkDemandReturnFr},	 0.95);	### Return fraction in livestock water use
  $stkSearchDist	= exists $param{StkSearchDist} ? floor($param{StkSearchDist}/sqrt($CELL_AREA))->long : $irrSearchDist;
  $stkRefYear		= exists $param{StkRefYear} ? $param{StkRefYear} : 2005;
  map	$LiveStock{$_}{DensityRefYr} =
	read_GDAL($extent,\%meta,1,$LiveStock{$_}{DensityFile},1,0.0 ), keys(%LiveStock);
}

#######################################################################
	### Make post-processing script
my $run_PP	= post_processing(\%runIO, $runAttr, \%path, \@list_out);
my %Agg_var	= map(($_ => {				### Runtime aggregation datasets, cumulatives, etc.
	dSet => { day  => $run_daily ?
			  new RIMS::DataSet(attrib($idPrfx.$_.'_d', $runIO{Output_MT})) : '',
		  month=> new RIMS::DataSet(attrib($idPrfx.$_.'_m', $runIO{Output_MT})),
		  year => new RIMS::DataSet(attrib($idPrfx.$_.'_y', $runIO{Output_MT}))},
	data => { day=>0, month=>0, year=>0},
	data2=> { day=>0, month=>0, year=>0},
	count=> { day=>0, month=>0, year=>0}}), @list_out) unless $noOutput;
my @Agg_str = qw( day     month     year);
my @Agg_Str = qw( daily   monthly   yearly);
my @Agg_off =   ( 0,      30/2,     366/2);	# Time series day offset for aggregations

#######################################################################
	### Make build spool script	Note, it must be before "idump"
make_build_spool(\%runIO, $date_list);

#######################################################################
		### Dump initialization parameters to files
if ($idump) {
  if ($noOutput) { print "Cannot dump init parameters when \"no output\" or \"test\" flag is used...\n"; }
  else {
	### This run init
    $idump = $runIO{Output_dir}.'/init_files';
    dump_init("$idump/$runIO{ID}.init", \%runIO);

	### Datasets from local MT
    my ($hdr, @metadata)	= read_table($runIO{Output_MT});
    shift @metadata while ($metadata[0][0]);	### Skip output datasets and the separating empty line
    shift @metadata;
    foreach my $row (@metadata) {		### Dump attributes of the input datasets
      my %attr	= map(($_ => $$row[$$hdr{$_}]), keys(%$hdr));
      dump_init("$idump/datasets/".$attr{Code_Name}.'.init', \%attr);
    }
    print "Init files are saved to:   $idump/\n";

	### EarthAtlas URL
    make_URL($runSet);
} }

#######################################################################
		### Read previous run state

my  $state_file = $dState || $runIO{Spinup}{State_ID} ? $run_state_dir.$runIO{Spinup}{State_ID} : $run_state_file;
my ($n_count, $n_count_done, $n_count_print, $n_spool, $init_flag) = (0, 0, 0, 0, 1);

if (-e $state_file.'.dat' && !$noState) {
  my ($last_j_date, $list);   $skipCompBal = 0;

	### Major parameters
  readState( $state_file.'.dat', $list,		# Read saved state of the previous run
    $mDischarge[0], $mDischarge[1], $mDischarge[2], $mDischarge[3], $mDischarge[4], $mDischarge[5],
    $snowPack, $grdWater[0], $discharge, $resStorage[0], $flow_outPrev, $sMoist[0],
    $InterceptionStorage, $surfRffStorage[0], $irrRffStorage[0], $ricePaddyStrg,
    $smResStrg[0], $irrVIS[0], $UGW, $endoStrg);

 ($last_j_date, $mDischargeK, @mDischargeN) = $list->list;			# unpack perl variables
  if ($runIO{Spinup}{Force_ID_Date} && $state_file eq $run_state_dir.$runIO{Spinup}{State_ID}) {
    $last_j_date = julian_day(split(m/-/, $runIO{Spinup}{Force_ID_Date}));	# Forse date of the state data
  } elsif ($state_file =~ m/\.spinup$/) { 
    $n_count_done = $n_spinup;
    $last_j_date  = $$j_date_list[$n_count_done]-1;
  } else {
    $n_count_done = $last_j_date < 0 ? -$last_j_date : $n_spinup + $last_j_date - $$j_date_list[$n_spinup] + 1;
    $last_j_date  = $$j_date_list[$n_count_done-1]	if $last_j_date < 0;
  }
	### Bucket components
  readState($state_file.'.bucket-0.dat',$streamAge)							if $compSwitch[0];
  readState($state_file.'.bucket-1.dat',$cStream[0],	$cSIR,       $cVIS,    $cRffStg)		if $compSwitch[1];
  readState($state_file.'.bucket-2.dat',$cStreamP[0],	$cGrWaterP,  $cSIR_P,  $cVIS_P,  $cRffStgP,
							$cSoilP,     $cEndoP)				if $compSwitch[2];
  readState($state_file.'.bucket-3.dat',$cStreamIrr[0],	$cGrWaterIrr,$cSoilIrr,$cSIR_Irr,$cVIS_Irr,
							$cRffStgIrr)					if $compSwitch[3];
  readState($state_file.'.bucket-4.dat',$cStreamTw,	$airT_bAvg,  $airT_rAvg)			if $compSwitch[4];
  readState($state_file.'.bucket-5.dat',$cStreamMsk[0],	$cGrWaterMsk,$cSIR_Msk,$cVIS_Msk,$cRffStgMsk)	if $compSwitch[5];
  readState($state_file.'.bucket-6.dat',$cStreamDIN,	$cStmConDIN)					if $compSwitch[6];
	### Snow Bands
  $snowBand = [readflex($state_file.'.snowBand.dat')] if $runIO{snowBands};

	### Landcover/Crop parameters (soil moisture)
  foreach my $lnd (keys %land) { foreach my $tp (keys %{$land{$lnd}}) {
    $sMoist    {$lnd}{$tp} = readflex($state_file.".sM.$lnd.$tp.dat");
    $sMoistVs  {$lnd}{$tp} = readflex($state_file.".Vs.$lnd.$tp.dat")	if $lnd eq 'irrCrop' && $vs_flag;
    $sMoistGr  {$lnd}{$tp} = readflex($state_file.".Gr.$lnd.$tp.dat")	if $BwGwFlag;	### BW/GW addition
		### Decode crop mask from encoded sMoist value
    if ($lnd =~ m/Crop|fallow/) {
      $cropMask{$lnd}{$tp} = $sMoist  {$lnd}{$tp} < 0;
      $sMoist  {$lnd}{$tp}.= condition($cropMask{$lnd}{$tp}, (-$sMoist{$lnd}{$tp}-1)->lclip(0), $sMoist{$lnd}{$tp});
    }
    $sMoistPrev{$lnd}{$tp} = $sMoist  {$lnd}{$tp}->copy;
    $sMoistPrVs{$lnd}{$tp} = $sMoistVs{$lnd}{$tp}->copy		if exists $sMoistVs{$lnd}{$tp};
  }}
	### Aquifer data
  if ( $aqfType ) {}		# Removed from public domain

		### End of year balances
  my ($ALL, $domXW_y, $indXW_y, $stkXW_y);	### Use all variables listed in "Reset annual totals to zero" section!!!
  eval(htm_template($state_file.'.yr_data.json'));
  ($runoffT_y,	$RunoffT_y, $dischT_y, $precipT_y, $etNonCropsT_y, $etIrrCropsT_y, $etRfdCropsT_y,
		$spr_dschT_y, $snk_inflT_y, $usgsDeltaT_y,
		$domGrossT_y, $domNetT_y,
		$indGrossT_y, $indNetT_y,
		$stkGrossT_y, $stkNetT_y)	= @$ALL;
  @domXW_y = @$domXW_y;
  @indXW_y = @$indXW_y;
  @stkXW_y = @$stkXW_y;

		### USGS assimilation data
  if ($usgs_data) {}		# Removed from public domain

		### Component tracking data
  if ($compBalFlag) {
    my $COMPBAL;
    eval(htm_template($state_file.'.compBal.json'));
    %compBal	= %{$COMPBAL};
  }
		### Reset spinup tracking, if requested
  reset_spinup_tracking(2,3) if $wbmParam{resetSpinupTracking} && $state_file eq $run_state_dir.$runIO{Spinup}{State_ID};

		##########################

  if ($last_j_date >= $$j_date_list[-1]) {		# quit if nothing to update
    print "Destination dataset is up to date! Nothing to do...\n\n";
    exit;
  }
  printf "Previous run state files are read! Resume date is %04d-%02d-%02d.\n",
	inverse_julian_day($last_j_date+1);
  $init_flag = 0;
}

###############      End of Input                 #####################
#######################################################################

print  "\n";
printf "Output directory:   %s\n", $noOutput	? 'No Output' : $runIO{Output_dir}. '/';
printf "Output var scaling: %s\n", $outputScale	? 'Full cell area' : 'None';
printf "Build spool batch:  %s\n", $runIO{SB_script};
printf "Post-Processing:    %s\n", $runIO{PostProcessing} ? $runIO{PP_script} : 'None';
printf "PET     method:     %s\n", $runIO{PET};
printf "Routing method:     %s\n",($routingMethod !~ m/LRR/i) ? $routingMethod : 'Linear Reservoir Routing (LRR) with '.
	($velMethod eq 'Constant' ? sprintf("%.2f km/hr",$flowSpeed) : 'AASH') . ' flow velocity';
printf "Infiltration model: %s\n", $Infiltration;
printf "Snow Band model:    %s\n", $runIO{snowBands}	? scalar(@$snowBand).' bands'	: '1 band';
printf "Lapse Downscale:    %s\n", defined $lapseDelta	? "Yes, by $airTLapse C/km"	: 'No';
printf "Component tracking: %s\n", length( $cString)	? $cString : 'None';
printf "WBM Network cells:  %d: %.1f %% in (%dx%d) layer\n", $nCells, 100*$nCells/$network->nelem, $network->dims; if ($noRun){
print  "No run mode- WBM initialization and auxilary files are done!\n\n"; $date_list = []; } else {
print  "WBM starts now!\n\n"; }

#######################################################################
###############      The Water Balance Model        ###################
#######################################################################
my $time_strtY	= Benchmark->new if $bMarkY;
my $list_meta;
my %dataSum;	my %dataCount;		# Need to make monthly averages

foreach my $date (@$date_list)
{
  next if $n_count_done > $n_count++;	### Skip previously done or unneeded dates
  my $time_strt	= Benchmark->new if $bMarkD;

  my @date	= split(m/-/,$date);
  my $hour	= $date[2] =~ s/T(\d{2}).+// ? int($1) : 0;
  my $j_date	= julian_day(@date);			# Line below- $jDay is this year 365 day index
  my $jDay	= $j_date-julian_day($date[0],1,1) - (is_leap($date[0]) && $date[1]>2 ? 1:0);
  my $yrBegCond	= $date =~ m/^\d{4}-$yearBeg/;		# Start of year  condition
  my $yrEndCond	= $date =~ m/^\d{4}-$yearEnd/;		# End   of year  condition
  my $mnEndCond = $date[2] == &$days_in(@date[0,1]);	# End   of month condition
  my $doOutput	= $n_count > $n_spinup && !$noOutput;	# Flag to write output files
     $$extent{spool_writes} = 0;			# Reset spool writes counter

  bMarkMap(-1, $bMarkM);				# Check for lock file to pause benchmarking
  bMarkMap( 0, $bMarkM, "Year = $date[0]") if $yrBegCond;
  bMarkMap( 1, $bMarkM, "Date = $date");
  bMarkMap( 2, $bMarkM, 'Read climate data');
  bMarkMap( 3, $bMarkM, 'Read basic data');

	###############################################################
	###############################################################
	#####   Read Climate Data for this date

		### Output file
  my $run_save	= $run_daily || $mnEndCond;
  my $run_file	= $dataSet{run}->dateLayer($date,1) if $run_save && $n_count > $n_spinup;

		### Temperature, Celsius
  my $airT = exists $dataSet{airT} ?
		read_dateLayer($dataSet{airT},   $date,$extent,$runIO{spool},{PATCH_VALUE=>$dataSet{airT_Ptch}}) : 0.5*(
		read_dateLayer($dataSet{airTmin},$date,$extent,$runIO{spool}) +
		read_dateLayer($dataSet{airTmax},$date,$extent,$runIO{spool}));
     $airT = $airT - readflex($airT_bias[$jDay]) if scalar  @airT_bias;  # DO NOT use PDL overloaded operators -= or +=
     $airT = $airT + $lapseDelta		 if defined $lapseDelta; # It will modify reused monthly values

		### Precipitation, mm/day
  my $precip =  read_dateLayer($dataSet{precip},$date,$extent,$runIO{spool},{PATCH_VALUE=>$dataSet{precipPtch}});
     $precip = ($precip - readflex($precip_bias[$jDay]))->lclip(0) if scalar @precip_bias;	# Bias correction
     $precip =  $precip * read_dateLayer($dataSet{precipFrac},$date,$extent,$runIO{spool},{RESAMPLE=>0, PATCH_VALUE=>0})
		if $dataSet{precip}->monthly;				# Monthly->daily conversion
     $precip*=  $precip_scale;						# Unit conversion (to mm/day) and scaling

		### PET & Water Temperature Drivers (we assume these are all daily data)
  my ($cloudFr, $windSpeed, $rHumidity, $albedo, $grossRad, @LAI_set);
  if ($runIO{PET} =~ m/^FAO-56|Penman-Monteith$/ || $waterTemp || $OWEC_flag) {
     $rHumidity	= get_relHumidity([ $dataSet{humidity},   $date,$extent,  $runIO{spool},
				{PATCH_VALUE=>$dataSet{humidity_Ptch}} ], $airT, $elevation);
     $windSpeed	= sqrt(	read_dateLayer($dataSet{windU},   $date,$extent,  $runIO{spool},
				{PATCH_VALUE=>$dataSet{windU_Ptch}})**2 +
			read_dateLayer($dataSet{windV},   $date,$extent,  $runIO{spool},
				{PATCH_VALUE=>$dataSet{windV_Ptch}})**2);
     $cloudFr	=	read_dateLayer($dataSet{cloudFr}, $date,$extent,  $runIO{spool},
				{PATCH_VALUE=>$dataSet{cloudFr_Ptch}});
     $albedo	=	read_dateLayer($dataSet{albedo},  $date,$extent,  $runIO{spool},
				{PATCH_VALUE=>$dataSet{albedo_Ptch}})  if ref $dataSet{albedo};
     $grossRad	=  $hr_flag ? grossRadiation($lon,$lat,$jDay,$hour,$subDh) : $grossRad[$jDay];	# MJ/m2/day
  }
  my $dayLen	=  $hr_flag ? dayLength($lon,$lat,$jDay,$hour,$subDh) : $dayLen[$jDay] if $runIO{PET} eq 'Hamon';
  my @PM_set	= ($airT, $windSpeed, $cloudFr, $albedo, $rHumidity, $grossRad, $elevation, \%wbmParam);

		### Leaf Area Indices (needed for Land Cover types only)
  if ($runIO{MT_LAI}) {
    $LAI	= read_dateLayer($dataSet{LAI},$date,$extent,$runIO{spool}) * $landMask;
    @LAI_set	= ($LAI, $canopyHt, readflex($canopySh[$jDay]));
  }

		#################################################################################
		### Open water and Impervious surfaces, if any of them is a Time Series (optional)
  if ($doMasks) {
    bMarkMap(3, $bMarkM, 'Read impervious and open water masks');
    if ($runIO{openWater}) {
      $lakeMask		= !(exists $dataSet{lakeMask})		? $lakeMask0->copy	:
	read_dateLayer($dataSet{lakeMask},	$date,$extent,$runIO{spool},		#  Resample 6 = "mode" method
	  {RESAMPLE=>6, PATCH_VALUE=>$openWaterMaskValue+1}) == $openWaterMaskValue;
      $fracOpenWater	= !(exists $dataSet{fracOpenWater})	? $fracOpenWater0->copy	:
	read_dateLayer($dataSet{fracOpenWater},	$date,$extent,$runIO{spool}, {RESAMPLE=>1,PATCH_VALUE=>0}) * $ones;
		# Add full cell fracOpenWater to the lake mask
      $lakeMask     ->where(($fracOpenWater == 1) & ($lakeMask == 0))	.= 1;	$lakeMaskIdx = whichND($lakeMask);
      $fracOpenWater->where( $fracOpenWater == 1)			.= 0;
      $landMask	= !$lakeMask;
    }
		### Impervious Surface mask (optional), if it is a Time Series
    if ($runIO{Impervious}) {
      $imprSurf		= !(exists $dataSet{Impervious})	? $imprSurf0->copy	:
	read_dateLayer($dataSet{Impervious}, $date,$extent,$runIO{spool},{PATCH_VALUE=>0}) * $landMask;
    }
		### Scale down impervious surface and open water where it is too high (> 0.975)
    {
      my $sum	= $imprSurf + $fracOpenWater;
      my $idx	= whichND( $sum > 0.975);

      $imprSurf	->indexND($idx)		.= ($imprSurf      * 0.975 / $sum)->indexND($idx);
      $fracOpenWater->indexND($idx)	.= ($fracOpenWater * 0.975 / $sum)->indexND($idx);
    }
    # $imprCoef	= 0.2;			### Fraction of precip that goes to surface runoff # Pellerin 2008 Ipswich
    $imprCoef	= $imprSurf ** 0.4;	### Fraction of precip that goes to surface runoff # Alley and Veenhius 1983
    $imprArea	= $imprSurf * $CELL_AREA;
    $rmArea	= $imprSurf + $fracOpenWater;
  }
		#######################################################
		### Glacier Area, Melt and Volume (optional)
  if ($runIO{Glaciers}) {
    bMarkMap(3, $bMarkM, 'Read Glacier data');
					# Notes:	1. Resample $glArea using "average" method (5)
					#		2. Snow routing prohibits $glArea to be a full pixel (0.99 limit)
    $glMelt	= read_dateLayer($dataSet{glMelt},  $date,$extent,$runIO{spool},{RESAMPLE=>0,PATCH_VALUE=>0})->lclip(0) * $glScale;
    $glArea	= read_dateLayer($dataSet{glArea},  $date,$extent,$runIO{spool},
		{DATE_SEARCH_OPT=>{YR_UP=>1}, RESAMPLE=>5, PATCH_VALUE=>0})->clip(0, 0.99 - $rmArea);
    $glVolume	= read_dateLayer($dataSet{glVolume},$date,$extent,$runIO{spool},
		{DATE_SEARCH_OPT=>{YR_UP=>1},RESAMPLE=>0,PATCH_VALUE=>0})->lclip(0) * $glScale if defined $dataSet{glVolume};
    map $_     *= $landMask, $glMelt,$glArea,$glVolume;		# Apply land mask to the glacier data layers
  }
		### Update and save cell area layers (annual time series)
  if ($doArea) {
   ($cell_area, $soil_area, $mm_2_KM3, $mm_2_km3, $mm_2_m3, $m3_2_mm) = area_layers($CELL_AREA,$glArea,$rmArea,$lakeMask);
			  ( $MM_2_KM3, $MM_2_km3, $MM_2_m3 )	      = map $_*$subDf, $mm_2_KM3,$mm_2_km3,$mm_2_m3;
    write_cell_area($CELL_AREA, $cell_area, $soil_area, $doArea, \%runIO, $grid, $credits, $date[0]) if $doOutput;
  }

		#######################################################
		### Dissolved Inorganic Nitrogen (DIN) Drivers
  if ($runIO{DIN}) {
    bMarkMap(3, $bMarkM, 'Read DIN inputs');
    $DIN_param{Population} =
		  read_dateLayer($dataSet{PopDens},$date,$extent,$runIO{spool}) if $DIN_param{WWTP};
    $luAgr	= read_dateLayer($dataSet{luAgr},  $date,$extent,$runIO{spool})*100*$landMask;	# Fraction agriculture, %
    $luSub	= read_dateLayer($dataSet{luSub},  $date,$extent,$runIO{spool})*100*$landMask;	# Fraction developed, %
    $propAgr	= condition_slice($luSub + $luAgr > 0, $luAgr / ($luSub + $luAgr), 0);
    $propSub	= condition_slice($luSub + $luAgr > 0, $luSub / ($luSub + $luAgr), 1);
    $DIN_Prev	= $cStreamDIN  *  $resStorage[0];
  }
  compBalance_Prev();

	###############################################################
	#####   End of reading/preparing of data for this date.
	#####   Actual WBM starts here-
	###############################################################

  bMarkMap(2, $bMarkM, 'Process climate derivatives');
  bMarkMap(3, $bMarkM, 'PET and Open Water Evap.');

	#####   PET - Potential Evapotranspiration by different methods
  my $pet = $runIO{PET} eq 'Hamon'		? PET_Hamon		($airT, $dayLen)	:
	    $runIO{PET} eq 'Penman-Monteith'	? PET_PenmanMonteith	(@PM_set, @LAI_set)	:
	    $runIO{PET} eq 'FAO-56'		? PET_FAO_56		(@PM_set, $LAI    )	:
	    die "Unknown PET method. Aborting...\n\n";
	#####   Open Water, Lake, and Wetland Evaporation
  my $waterEvap	= $OWEC_flag			? calc_openWaterEvap	(@PM_set)		:
		  $runIO{openWaterEvap}		? read_dateLayer($dataSet{waterEvap},$date,$extent,$runIO{spool}) :
						  $openWtrCoeff * $pet;		# Note "coeff*$pet" is better than nothing

	###############################################################
	#####   Snow Melt

  bMarkMap(3, $bMarkM, 'Snow');
				#######################################
  if ($runIO{snowBands}) {	########### Snow bands processing  ####
    bMarkMap(4, $bMarkM, 'Snow bands');
    $snowFall *= 0;
    $snowMelt *= 0;	# Reset these vars (NB- they have children slices)
    for (my $i=0; $i<=$#$indxBand; $i++) {
      my @balance  = snow_balance($$snowBand[$i], $airT->indexND($$indxBand[$i])+$$airTBand[$i],
		$precip->indexND($$indxBand[$i]), $MDSnowFallThreshold, $MDSnowMeltThreshold, $subDf);
      $$sFall[$i] += $balance[0] * $$elevBand[$i];
      $$sMelt[$i] += $balance[1] * $$elevBand[$i];
    }
    $sPackChg  = $snowFall - $snowMelt;			# mm/day
    $snowPack += $sPackChg * $subDf;
		# Shift downslope snowbands to avoid forming glacier
    shift_snowBands($snowBand,$elevBand,$snowIndN) if $date =~ m/-08-15/ && !$hour;	# Minimum snow date (North)
    shift_snowBands($snowBand,$elevBand,$snowIndS) if $date =~ m/-02-15/ && !$hour;	# Minimum snow date (South)
#   snowBandBalance($snowBand,$elevBand,$indxBand, $snowPack);		# Check balance of snow bands
  }									# Comment out the balance check after testing
				#######################################
  else {			########### No snow bands option   ####
    ($snowFall, $snowMelt, $sPackChg) =			# mm/day
	snow_balance($snowPack, $airT, $precip, $MDSnowFallThreshold, $MDSnowMeltThreshold, $subDf);
  }
	###############################################################
	#####   Routing excess snow downstream to avoid snow accumulation problem
	#####   during minimum snowpack days in Southern and Northern hemispheres

  if ( $date   =~ m/-08-15|-02-15/ && !$hour ) {
    bMarkMap(4, $bMarkM, 'Snow routing');
    my $lat2D  =  $lat->dummy(0,$$extent{ncols});
    foreach (1 .. $snowRouteN) {			# Snow (ice-pack) routing has to be greater than 1 km
      my $sMask= ($snowPack > 5000) & ($date =~ m/-02-15/ ? $lat2D < 0 : $lat2D >= 0);
      if($sMask->sum) {
	my ($gMelt,  $gPackChg) = $cell_table->snow_routing($snowPack, $sMask, $cell_area, 5000); # Ice-pack changes
	$sPackChg += $gPackChg	* $subDi;	# convert mm -> mm/day
	$snowMelt += $gMelt	* $subDi;			# $gMelt here is actually glacier ablation
	update_snowBands($gPackChg, $snowBand, $elevBand, $indxBand) if $runIO{snowBands};
  } } }

	###############################################################
	#####   Rain intercept by vegitation

  bMarkMap(3, $bMarkM, 'Canopy intercept');

  my $rainFall	= $precip - $snowFall;
  my $ICap	= $LAI * $MDConstInterceptCI;		# From the VIC model. Reference: Dickinson 1984

  my $IStrgFrac	  = ($InterceptionStorage   / $ICap)->setnonfinitetobad->setbadtoval(0)->clip(0,1);
  my $canopyET	  = ($waterEvap * $IStrgFrac**(2/3))->clip(0, $InterceptionStorage);	# mm/day
  my $throughFall = ($rainFall  + $InterceptionStorage - $canopyET - $ICap)->lclip(0);	# mm/day

  my $interceptChg	=($rainFall - $throughFall - $canopyET) * $subDf;		# mm/dt
  $InterceptionStorage += $interceptChg;						# mm

  my $waterIn	 = $throughFall + $snowMelt;	### Liquid water on the ground		# mm/day
     $snowInFr	.= ($snowMelt / $waterIn)->setnonfinitetobad->setbadtoval(0);
     $rainInFr	.= 1 - $snowInFr;		### All pixels must sum to 1 (do not change this!)
					### otherwise there will be a problem at crop rotations
	###############################################################
	#####   WBM by different land cover/crop types (all units are mm)

			### Variable name definitions:
	# $evapotrans		= Total evaporation and evapotranspiration from all sources. Sum of
	# 	$netwkEvap + $canopyET + $etNonCrops + $etRfdCrops + $etIrrCrops + $otherEvap
	# $netwkEvap		= Evaporation from open water (rivers, lakes, reservoirs)
	# $canopyET		= Evaporation from canopy intercept (primary source is Rain only)
	# $etNonCrops		= Evapotranspiration from "pristine" land	: NB- from Soil
	# $etRfdCrops		= Evapotranspiration from rainfed crops		: NB- from Soil
	# $etIrrCrops		= Evapotranspiration from Irrigated crops	: NB- from Soil
	# $otherEvap		= Evaporation from Domestic+Industrial+Livestock water use

  push @grdWater,	  $grdWater[0]	->copy;		### Some initializations
  push @smResStrg,	 ($smResFlag ?	$smResStrg[0]->copy : $zeroes);	# Save some memory
  push @irrVIS,		  $irrVIS[0]	->copy;
  push @sMoist,		  $zeroes	->copy;
  my $surfRunoff	= $zeroes	->copy;
  my $evapotrans	= $zeroes	->copy;
  my $dischargePrev	= $discharge	->copy;
  my $resStoragePrev	= $resStorage[0]->copy;
     $grdWaterChg      .= 0;				# Reset it

	####################################################################
	### Initialization of groundwater and aquifer infiltration variables

		### Calculate riverbed geometry based on streamflow (required for Muskingum routing & ModFlow aquifers)
  my $meanDischarge = $mDischargeN[0] ? $mDischarge[0]/$mDischargeN[0]   :  $mDischarge[0];
  my($yMean, $wMean,  $vMean, $yStream, $wStream, $vStream) = flowGeometry([$discharge,$meanDischarge], $riverParams);

  my($Aqf_Storage,		# Aquifer storage grid, mm
     $Aqf_infiltPrecip,		# Infiltration recharge to groundwater aquifer (feeding springs)  from Precip,	mm/day
     $Aqf_infiltIrr,		# Infiltration recharge to groundwater aquifer (feeding springs)  from Irr,	mm/day
     $GWt_infiltPrecip,		# Infiltration recharge to shallow groundwater (feeding baseflow) from Precip,	mm/day
     $GWt_infiltIrr,		# Infiltration recharge to shallow groundwater (feeding baseflow) from Irr,	mm/day
     $Aqf_infiltFrac,		# Infiltration fraction to groundwater aquifer (from surplus)
     $GWt_infiltFrac,		# Infiltration fraction to shallow groundwater (feeding baseflow)
     $Aqf_dQ,			# Loading term- ModFlow unconfined and confining layers (exposed to land surface)
     $Cfd_dQ,			# Loading term- ModFlow unconfined and confined  layer  (bottom layer, e.g. abstractions)
     $Aqf_RIV_rech,		# ModFlow aquifer recharge  from surface streams
     $Aqf_RIV_dsch,		# ModFlow aquifer discharge to   surface streams
     $spr_dischargeT,		# Total discharge from springs	(needed for Global balances)
     $riv_exchangeT)	=	# Total RIV exchange		(needed for Global balances)
	(map($zeroes->copy, 1..6), $MDInfiltrationFrac->copy, ($aqfType==3 ? map($zeroes->copy,1..4) : ((0)x4)), 0,0);

  if($aqfType) {}		# Removed from public domain

  bMarkMap(2, $bMarkM, 'Miscellaneous and Non-soil runoff');
	###############################################################

  my $GWt_infiltRatio	= $GWt_infiltFrac / ($GWt_infiltFrac+$Aqf_infiltFrac);	# GWt portion of infiltration
  my $Aqf_infiltRatio	= 1 - $GWt_infiltRatio;					# Aqf portion of infiltration

	###############################################################
	###	Initialization of irrigation related variables
  my $ricePaddyRain	= $runIO{Irrigation} ? $zeroes->copy : 0; # units- mm
  my $irr_demand_total	= $runIO{Irrigation} ? $zeroes->copy : 0; # units- mm
  my $irr_demVrt_total	= $runIO{Irrigation} ? $zeroes->copy : 0; # units- mm
  my $irr_demVs_total	= $vs_flag	     ? $zeroes->copy : 0; # units- mm
  my($irrigationNet,	$irrigationGross,	$irrigationGrossT,
     $irrigationFlow,	$irrFactor,		$irrFactorPct,
     $ricePaddyWater,	$riceSoilWater,			### units- mm
     $ricePaddyPerc,	$ricePaddyFlood,		### Paddy percolation and flooding (Irr + Rain)

     $visUseFlowLoc,	$visUseFlowRmt,			### VIS refill from local and remote sources
     $visUseFlowRID,	$visUseSIR,			### VIS refill from SIR: Small Irr Reservoirs
     $visUseGrwt,	$visUseExtra,			### VIS refill from ground and unsustainable storages

     $useFlow,		$useAqf,	$useExtra,	### Water use cumulatives (domestic, industrial, livestock)
     $returnGrW,	$returnRff,			### Return flow components from the water use
     $irrFloodRice,	$irrPercRice,			### Components of rice flood/perc from irr  (witout rain)
     $rainFloodRice,	$rainPercRice,			### Components of rice flood/perc from rain (witout irr)
     $irrPercIneff,	$irrEvapIneff,	$irrRnffIneff,	### Components of irr loss at application
     $irrPercDeliv,	$irrEvapDeliv) = ((0) x 30);	### Components of irr loss at delivary

						### ET components
  my($etNonCrops, $etRfdCrops, $etIrrCrops) = map $zeroes->copy, 1..3;


	###############################################################
	#####   Open Water (e.g. lakes) and Impervious Surfaces

  my $WATERIN	 = $waterIn + ($rainFall - $throughFall);	# Corrected waterIn over non-soil area (no foliage)
			### Flush over open water (e.g. lakes)
  my $lakeWater	 = $WATERIN  * ($fracOpenWater + $lakeMask) * $CELL_AREA*1e3;	# Volume in m3/day
			### Flush over impervious surface
  my $imprWater	 = 0;
  if ($runIO{Impervious}) {
    $imprWater	 = $WATERIN   *    $imprArea * 1e3;				# Volume in m3/day
    $waterIn	+= $imprWater * (1-$imprCoef)* $m3_2_mm;			# in mm/day over soil area
    $imprWater	*=		   $imprCoef;
  }
			### Combined flush water from all non-soil areas
  my $flushWater =($lakeWater + $imprWater)  * $subDf;				# m3
     $waterIn   *= $landMask;							# mm/day
 map $_		 = undef, $rainFall,$throughFall,$ICap,$WATERIN;		# Destroy to save memory

	###############################################################
	#####   Active layer water capacity (awCap) and Kc by land cover type

  if ($runIO{LandCover} && !$hour) {
    foreach my $tp (keys %{$land{landCover}}) { # time varying root depth
#       $awCap{landCover}{$tp} = $land{landCover}{$tp}{awDelta} *
# 	($LAI / $land{landCover}{$tp}{MaxLAI} * $land{landCover}{$tp}{RootDepth})->setnonfinitetobad->setbadtoval(0);

      $Kc_par{landCover}{$tp} = $land{landCover}{$tp}{KcMin} +
	($land{landCover}{$tp}{KcMax} - $land{landCover}{$tp}{KcMin}) * (1 - exp(-0.7*$LAI));
    }
  }

	###############################################################
	#####   Land Fractions in a grid cell by land/crop type

  if ($runIO{Irrigation} && !$hour) {
    bMarkMap(2, $bMarkM, 'Irrigation Processing # 1');
    bMarkMap(3, $bMarkM, 'Processing of cropland fractions');

		### Calculate total cropland area growth coefficient
    my ($areaCoeff, $cropDate) = growth_coeff( 1, $crpAreaGrwth, \@date,
		$crpGrwthRefYr, $irrParam{CropArea}{fstYear}, $irrParam{CropArea}{lstYear});
		### Read total cropland area fraction
    $cropAreaFrac	= condition_slice($landMask,	# The ratio below is the soil factor term to account for
	$CELL_AREA / $soil_area *			# "CropAreaFrac" being fraction of whole pixel area
	$areaCoeff *	read_dateLayer( $dataSet{CropAreaFrac}, $cropDate, $extent, $runIO{spool},
	{PATCH_VALUE=>	defined $$landPatch{CropAreaFrac} ? $$landPatch{CropAreaFrac} :	# Patch dataset for CropAreaFrac
	0, PPATCH_VALUE=>0 } ), 0)->clip(0,1);
    my($rfdArea,    $sum)	= (0, 0);
      ($nonIrrArea, $irrArea)	= (0, $zeroes->copy);	# Scope of $nonIrrArea and $irrArea is the daily loop

		### Read each crop land fractions (NB- "near" resample)
    my $ldFr = 'LandFraction';
    foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (keys %{$land{$lnd}}) {
      my $TS_flag = $extendCropTS ? 1 : ($lnd eq 'irrCrop');
     ($areaCoeff, $cropDate) =  growth_coeff( $TS_flag, $irrAreaGrwth, \@date,
		$irrGrwthRefYr, $land{$lnd}{$tp}{$ldFr}{fstYear}, $land{$lnd}{$tp}{$ldFr}{lstYear});

      $landFrac{$lnd}{$tp}  = ($cropAreaFrac * $areaCoeff)->hclip(1) * ( read_dateLayer(
	$land{$lnd}{$tp}{$ldFr},	$cropDate,$extent,$runIO{spool},{RESAMPLE=>0,PATCH_VALUE=>
		defined $$landPatch{$lnd}{$tp}{$ldFr} ? $$landPatch{$lnd}{$tp}{$ldFr} :	# Patch dataset
	0, PPATCH_VALUE=>0 }))->double;
      if ($lnd eq 'irrCrop')	{ $irrArea    += $landFrac{$lnd}{$tp}; }
      else			{ $nonIrrArea += $landFrac{$lnd}{$tp}; }
    }}
    $cropAreaFrac *= ($irrArea + $nonIrrArea) > 0;	### Mask out cropland outside of MIRCA pixels

		### Re-Scale cropland fractions
			### Irrigated
    my  $scale	 	=($irrArea->hclip($cropAreaFrac) / $irrArea)	->setnonfinitetobad->setbadtoval(0);
    map $landFrac{irrCrop}{$_}	*= $scale,			keys(%{$land{irrCrop}});
        $irrArea->inplace->hclip($cropAreaFrac);
        $irrEqpArea	= condition($irrArea > $irrEqpArea, $irrArea, $irrEqpArea);
			### Non-Irrigated
        $scale	 	=(($cropAreaFrac - $irrArea) / $nonIrrArea)	->setnonfinitetobad->setbadtoval(0);
    map $landFrac{rfdCrop}{$_}	*= $scale,			keys(%{$land{rfdCrop}});
    map $rfdArea		+= $landFrac{rfdCrop}{$_},	keys(%{$land{rfdCrop}});
        $landFrac{fallow}{land}	 = ($cropAreaFrac - $irrArea - $rfdArea)->lclip(0); # Scaling cannot be used
								# for pixels where non-irrigated area is zero
		### Re-Scale non-crop landcover fractions to reminder of cropland
    if ($runIO{LandCover}) {
      map $landFrac{landCover}{$_} = $land{landCover}{$_}{$ldFr}->copy, keys(%{$land{landCover}});
      delete $landFrac{landCover}{Croplands};

      map $sum  += $landFrac{landCover}{$_},	keys(%{$land{landCover}});
          $scale = ((1 - $cropAreaFrac) / $sum)				->setnonfinitetobad->setbadtoval(0);
      map $landFrac{landCover}{$_} *= $scale,	keys(%{$land{landCover}});
    }
    else {$landFrac{generalLand}{land} = $land{generalLand}{land}{$ldFr} - $cropAreaFrac; }

# foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {
#   printf "%11s => %13s : N = %d : min = %f : max = %f : sum = %f\n",$lnd,$tp,
# 	$landFrac{$lnd}{$tp}->ngood,$landFrac{$lnd}{$tp}->min,$landFrac{$lnd}{$tp}->max,$landFrac{$lnd}{$tp}->sum;
# }} print "\n";
    bMarkMap(3, $bMarkM, 'Reading crop parameters');

		#######################################################
		### Read crop parameters (NB- "near" resample)
    $riceMask	*= 0;		# Reset it
    foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (keys %{$land{$lnd}}) {
     ($areaCoeff, $cropDate) =  growth_coeff( $extendCropTS, 0, \@date,
		$irrGrwthRefYr, $land{$lnd}{$tp}{$ldFr}{fstYear}, $land{$lnd}{$tp}{$ldFr}{lstYear});
			# Growing crop masks
      $cropMaskPrev{$lnd}{$tp}	 = $init_flag ? $landFrac{$lnd}{$tp} > 0 :	# Initializition
				   $cropMask{$lnd}{$tp}->copy;	### Previous day crop mask
      $cropMask{$lnd}{$tp}	 = $landFrac{$lnd}{$tp} > 0;	### This     day crop mask
      $sMoist{$lnd}{$tp}	*= $cropMask{$lnd}{$tp};	### sMoist can be used as crop mask in run-state
      $sMoistGr{$lnd}{$tp}	*= $cropMask{$lnd}{$tp} if $BwGwFlag;	### BW/GW addition
      next if $lnd eq 'fallow';
			# Available water capacity
      $awCap{$lnd}{$tp} =
	read_dateLayer($land{$lnd}{$tp}{awCap},		$cropDate,$extent,$runIO{spool},{RESAMPLE=>0,PATCH_VALUE=>
		defined $$landPatch{$lnd}{$tp}{awCap} ?	$$landPatch{$lnd}{$tp}{awCap} :	# Patch dataset for awCap
	0, PPATCH_VALUE=>0 });
			# Crop depletion factor (crpDF)
      if ($lnd eq 'irrCrop') {
	$crpDF{$tp} = $dailyIrr ? $ones :
	read_dateLayer($land{$lnd}{$tp}{crpDF},		$cropDate,$extent,$runIO{spool},{RESAMPLE=>0,PATCH_VALUE=>
		defined $$landPatch{$lnd}{$tp}{crpDF} ?	$$landPatch{$lnd}{$tp}{crpDF} :	# Patch dataset for crpDF
	0, PPATCH_VALUE=>0 });
			# Irrigated rice mask (in cell fraction units)
	$riceMask += $landFrac{$lnd}{$tp}	if $tp =~ m/^rice/i;
      }
			# Crop Coefficients (Kc)
      $Kc_par{$lnd}{$tp} =
	read_dateLayer($land{$lnd}{$tp}{Kc},		$cropDate,$extent,$runIO{spool},{RESAMPLE=>0,PATCH_VALUE=>
		defined $$landPatch{$lnd}{$tp}{Kc} ?	$$landPatch{$lnd}{$tp}{Kc} :	# Patch dataset for Kc
	0, PPATCH_VALUE=>0 });
    }}
		### Keep or unlock rice paddy water from $irrRffStorage[1], if $riceMask changes (calculated above)
    $ricePaddyStrg	*= $riceMask > 0;
  }
		### Soil Moisture Initializition
  if ($init_flag) {
    foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
      $sMoist    {$lnd}{$tp} = $awCap{$lnd}{$tp} * $sMoistFrac;
      $sMoistPrev{$lnd}{$tp} = $sMoist{$lnd}{$tp}->copy;
      $sMoist[0] += $sMoistPrev{$lnd}{$tp} * $landFrac{$lnd}{$tp};
			### BW/GW additions
      if ($BwGwFlag) {
	$sMoistGr  {$lnd}{$tp} = $sMoist{$lnd}{$tp}->copy;
	$sMoistGrET{$lnd}{$tp} = $zeroes->copy;
	$sMoistET  {$lnd}{$tp} = $zeroes->copy;
      }
			### Virtual soil to track potential Irr demand
      if ($lnd eq 'irrCrop' && $vs_flag) {
	$sMoistVs  {$lnd}{$tp} = $sMoist    {$lnd}{$tp}->copy;
	$sMoistPrVs{$lnd}{$tp} = $sMoistPrev{$lnd}{$tp}->copy;
      }
    }}
    compBalance_PrevSoil();
    $init_flag = 0;
  }

	###############################################################
	#####   Make dataset slices
  bMarkMap(3, $bMarkM, 'Updating soil slices');

		### Update domain slice indices for new crop fractions
  if ($slice && $runIO{Irrigation}) {
    foreach my $lnd (keys %land) { foreach my $tp (keys %{$land{$lnd}}) {
      $ind{$lnd}{$tp} = whichND($landFrac{$lnd}{$tp} > 0);
    }}
  }		### Make dataset slices
  my (%waterIn, %et, %et_IrrCrp,  %et_RfdCrp, %et_NonCrp, %surfRunoff, %ricePaddyRain,
      %GWt_infilt, %Aqf_infilt,   %Aqf_inFrac, %GWt_inFrac);
  foreach my $lnd (keys %land) { foreach my $tp (keys %{$land{$lnd}}) {
       set_to_zero($sMoistFrac{$lnd}{$tp}, $zeroes);			if ($BwGwFlag){		# BW/GW
       set_to_zero($sMoistET  {$lnd}{$tp}, $zeroes);
       set_to_zero($sMoistGrET{$lnd}{$tp}, $zeroes); }
    my $opt  = $slice ? '->indexND($ind{$lnd}{$tp})' : '';
    my $code = <<EOS;
      \$landFrac  {slc}{\$lnd}{\$tp} = \$landFrac  {\$lnd}{\$tp}$opt;
      \$sMoist    {slc}{\$lnd}{\$tp} = \$sMoist    {\$lnd}{\$tp}$opt;	if (\$BwGwFlag) {	# BW/GW
      \$sMoistGr  {slc}{\$lnd}{\$tp} = \$sMoistGr  {\$lnd}{\$tp}$opt;
      \$sMoistGrET{slc}{\$lnd}{\$tp} = \$sMoistGrET{\$lnd}{\$tp}$opt;
      \$sMoistET  {slc}{\$lnd}{\$tp} = \$sMoistET  {\$lnd}{\$tp}$opt; }
      \$sMoistFrac{slc}{\$lnd}{\$tp} = \$sMoistFrac{\$lnd}{\$tp}$opt;
      \$sMoistPrev{slc}{\$lnd}{\$tp} = \$sMoistPrev{\$lnd}{\$tp}$opt;
      \$awCap     {slc}{\$lnd}{\$tp} = \$awCap     {\$lnd}{\$tp}$opt;
      \$waterIn   {slc}{\$lnd}{\$tp} = \$waterIn		$opt;
      \$sMoist    {SLC}{\$lnd}{\$tp} = \$sMoist[1]		$opt;	### {SLC} allows to use the same hash twice
      \$sMoistFrac{SLC}{\$lnd}{\$tp} = \$sMoistFrac		$opt;
      \$et        {slc}{\$lnd}{\$tp} = \$evapotrans		$opt;
      \$surfRunoff{slc}{\$lnd}{\$tp} = \$surfRunoff		$opt;
      \$GWt_infilt{slc}{\$lnd}{\$tp} = \$GWt_infiltPrecip	$opt;
      \$Aqf_infilt{slc}{\$lnd}{\$tp} = \$Aqf_infiltPrecip	$opt;
      \$Aqf_inFrac{slc}{\$lnd}{\$tp} = \$Aqf_infiltFrac		$opt;
      \$GWt_inFrac{slc}{\$lnd}{\$tp} = \$GWt_infiltFrac		$opt;
EOS
    if ($lnd =~ m/Crop|fallow/) { $code .=
     "\$cropMask  {slc}{\$lnd}{\$tp} = \$cropMask{\$lnd}{\$tp}	$opt;\n" ;
    }
    if ($lnd eq 'irrCrop') {
      set_to_zero($irr_demand{$tp}, $zeroes) if ref($irr_demand{$tp}) ne 'PDL';
      set_to_zero($irr_appRte{$tp}, $zeroes);
      set_to_zero($irr_demVrt{$tp}, $zeroes);
      $code .= <<EOS;
      \$irr_demand	 {slc}{\$tp} = \$irr_demand{\$tp}	$opt;
      \$irr_appRte	 {slc}{\$tp} = \$irr_appRte{\$tp}	$opt;
      \$irr_demVrt	 {slc}{\$tp} = \$irr_demVrt{\$tp}	$opt;
      \$irr_demand	 {SLC}{\$tp} = \$irr_demand_total	$opt;
      \$irr_demVrt	 {SLC}{\$tp} = \$irr_demVrt_total	$opt;
      \$et_IrrCrp {slc}{\$lnd}{\$tp} = \$etIrrCrops		$opt;
EOS
      if ($tp =~ m/^rice/i) { $code .=
      "\$ricePaddyRain   {slc}{\$tp} = \$ricePaddyRain		$opt;\n" ;
      }
      if ($vs_flag) {		### Virtual soil to track potential Irr demand
	set_to_zero($sMoistFrVs{$lnd}{$tp}, $zeroes);
	set_to_zero($irr_demVs{$tp},	    $zeroes);
	$code .= <<EOS;
      \$sMoistVs  {slc}{\$lnd}{\$tp} = \$sMoistVs  {\$lnd}{\$tp}$opt;
      \$sMoistFrVs{slc}{\$lnd}{\$tp} = \$sMoistFrVs{\$lnd}{\$tp}$opt;
      \$sMoistPrVs{slc}{\$lnd}{\$tp} = \$sMoistPrVs{\$lnd}{\$tp}$opt;
      \$irr_demVs	 {slc}{\$tp} = \$irr_demVs{\$tp}	$opt;
      \$irr_demVs	 {SLC}{\$tp} = \$irr_demVs_total	$opt;
EOS
      }
    } else {		$code .= $lnd eq 'rfdCrop' ?
     "\$et_RfdCrp {slc}{\$lnd}{\$tp} = \$etRfdCrops		$opt;\n" :
     "\$et_NonCrp {slc}{\$lnd}{\$tp} = \$etNonCrops		$opt;\n" ;
    }
    eval $code;
  }}

	###############################################################
	#####   Soil moisture change at land transition when crops rotation occurs
				### or when cropland fraction change

  if ($runIO{Irrigation} && !$hour) {
    bMarkMap(3, $bMarkM, 'Processing of crop rotations');

    foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
			### Planting crop (converting fallow land to crop land)
      my  $plantMask = ($cropMask{$lnd}{$tp} - $cropMaskPrev{$lnd}{$tp}) == 1;
      if ($plantMask->sum) { if ($BwGwFlag) {
	$sMoistGr  {$lnd}{$tp} .= condition($plantMask, $sMoistFrac*$awCap{$lnd}{$tp}, $sMoistGr{$lnd}{$tp});}	# BW/GW
	$sMoistPrev{$lnd}{$tp} .= condition($plantMask, $sMoistFrac*$awCap{$lnd}{$tp}, $sMoistPrev{$lnd}{$tp});
	$sMoistPrVs{$lnd}{$tp} .= condition($plantMask, $sMoistPrev{$lnd}{$tp}       , $sMoistPrVs{$lnd}{$tp})
		if $lnd eq 'irrCrop' && $vs_flag;
      }	$sMoistPrev{$lnd}{$tp}->inplace->hclip($awCap{$lnd}{$tp});
    }}
			### Calculate Grid Cell soil moisture change
    foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
      $sMoist[1] += $sMoistPrev{$lnd}{$tp} * $landFrac{$lnd}{$tp};	# Cell weighted average
    }}
			### Vertical water re-distribution at crop rotations
    $grdWaterChg .= ($sMoist[1] - $sMoist[0])->hclip($grdWater[0]);
    if (whichND($grdWaterChg)->dim(1)) {
      compUpdate_CropRotation();
      $grdWater[1] -= $grdWaterChg;

			### Horizontal water re-distribution at crop rotations
      my $sMoistChg   = (($sMoist[0] + $grdWaterChg) / $sMoist[1])->setnonfinitetobad->setbadtoval(1)->clip(0,1);
      foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
	$sMoistGr  {slc}{$lnd}{$tp} *= $slice? $sMoistChg->indexND($ind{$lnd}{$tp}): $sMoistChg	if $BwGwFlag;
	$sMoistPrev{slc}{$lnd}{$tp} *= $slice? $sMoistChg->indexND($ind{$lnd}{$tp}): $sMoistChg;
	$sMoistPrVs{slc}{$lnd}{$tp} *= $slice? $sMoistChg->indexND($ind{$lnd}{$tp}): $sMoistChg
		if $lnd eq 'irrCrop' && $vs_flag;
  } } }}
  $sMoist[1]	*= 0;	# Reset these vars (NB- they have children slices)
  $sMoistFrac	*= 0;

	###############################################################
	#####   Soil Moisture

  bMarkMap(2, $bMarkM, 'Soil Moisture');

  foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
			### Skip in case of unneeded processing
    if    ($slice)		{ next unless $ind{$lnd}{$tp}->dim(1); }
    elsif ($runIO{Irrigation})	{ next unless $landFrac{slc}{$lnd}{$tp}->sum; }

    my $condition_Vs = $lnd eq 'irrCrop' && $vs_flag;		# Virtual soil to track potential Irr demand

			### Direct groundwater recharge by HBV method
	# HBV method presently makes surface runoff always to be zero (PROMLEM???)
	# Water tracking is not done yet for the HBV method
    my $directRecharge	= $Infiltration !~ m/HBV/i ? 0 : $waterIn{slc}{$lnd}{$tp} *
	($sMoistPrev{slc}{$lnd}{$tp} / $awCap{slc}{$lnd}{$tp})->setnonfinitetobad->setbadtoval(0) ** $HBV_Beta;
    my $WaterIn		= $waterIn{slc}{$lnd}{$tp} - $directRecharge;			# mm/day

			### Soil moisture change calculations
    my $gm	= ($lnd =~ m/irrCrop/ && !$irrGm) ? 1 :
	((1 - exp(-$alpha*$sMoistPrev{slc}{$lnd}{$tp} / $awCap{slc}{$lnd}{$tp})) / (1 - exp(-$alpha)))
			->setnonfinitetobad->setbadtoval(0);
    my $petLC	= $slice? $pet->indexND($ind{$lnd}{$tp})* $Kc_par{$lnd}{$tp}->indexND($ind{$lnd}{$tp}):
			  $pet				* $Kc_par{$lnd}{$tp};
    my $inDelta	= $subDf*($WaterIn - $petLC);						# mm/dt

    my $sMoistChg = $subDi *								# mm/day
	condition($awCap{slc}{$lnd}{$tp} > 0,		# (!) Must use $inDelta in mm/dt
		  condition($WaterIn > $petLC,
		 	    condition($inDelta < $awCap{slc}{$lnd}{$tp}-$sMoistPrev{slc}{$lnd}{$tp},
				      $inDelta,  $awCap{slc}{$lnd}{$tp}-$sMoistPrev{slc}{$lnd}{$tp}),
		 	    $inDelta * $gm),
		  0)->clip(-$sMoistPrev{slc}{$lnd}{$tp},$awCap{slc}{$lnd}{$tp}-$sMoistPrev{slc}{$lnd}{$tp});

    $sMoist    {slc}{$lnd}{$tp} .= $sMoistPrev{slc}{$lnd}{$tp} + $sMoistChg * $subDf;	# mm
    $sMoistFrac{slc}{$lnd}{$tp} .=($sMoist{slc}{$lnd}{$tp} / $awCap{slc}{$lnd}{$tp})->setnonfinitetobad->setbadtoval(1)
	->copybad($slice?$$extent{mask}->indexND($ind{$lnd}{$tp}):$$extent{mask});

    $sMoist{SLC}{$lnd}{$tp}     += $sMoist{slc}{$lnd}{$tp}    * $landFrac{slc}{$lnd}{$tp};# Cell weighted value
    $sMoistFrac{SLC}{$lnd}{$tp} += $sMoistFrac{slc}{$lnd}{$tp}* $landFrac{slc}{$lnd}{$tp};# Cell weighted value
	# Note - Soil moisture change here is due to precipitation/evaporation

    if ($condition_Vs) {		# Virtual soil to track potential Irr demand
      my $sMoistChgVs = $subDi *							# mm/day
	condition($awCap{slc}{$lnd}{$tp} > 0,
		  condition($WaterIn > $petLC,
		 	    condition($inDelta < $awCap{slc}{$lnd}{$tp}-$sMoistPrVs{slc}{$lnd}{$tp},
		 	  	      $inDelta,  $awCap{slc}{$lnd}{$tp}-$sMoistPrVs{slc}{$lnd}{$tp}),
		 	    $inDelta),		# $gm for crops = 1
		  0)->clip(-$sMoistPrVs{slc}{$lnd}{$tp},$awCap{slc}{$lnd}{$tp}-$sMoistPrVs{slc}{$lnd}{$tp});

      $sMoistVs  {slc}{$lnd}{$tp} .= $sMoistPrVs{slc}{$lnd}{$tp} + $sMoistChgVs * $subDf;	# mm
      $sMoistFrVs{slc}{$lnd}{$tp} .=($sMoistVs  {slc}{$lnd}{$tp} / $awCap{slc}{$lnd}{$tp})->setnonfinitetobad->setbadtoval(1)
	->copybad($slice?$$extent{mask}->indexND($ind{$lnd}{$tp}):$$extent{mask});
    }

	###############################################################
	#####   Evapotranspiration, mm/day

    my $delta_prcp = $WaterIn - $sMoistChg;
    my $evapoTrans = condition($petLC < $delta_prcp, $petLC, $delta_prcp);
    my $et	   = $evapoTrans * $landFrac{slc}{$lnd}{$tp};	# Cell weighted value
    $et       {slc}{$lnd}{$tp} += $et;
    $et_IrrCrp{slc}{$lnd}{$tp} += $et	if $lnd eq 'irrCrop';
    $et_RfdCrp{slc}{$lnd}{$tp} += $et	if $lnd eq 'rfdCrop';
    $et_NonCrp{slc}{$lnd}{$tp} += $et	if $lnd !~ m/Crop/;

	###############################################################
	#####   Surplus, Infiltration and Groundwater, mm/day

    my $surplus   =($WaterIn - $evapoTrans - $sMoistChg)->lclip(0);

		### Rice paddy water collected from rain water surplus, mm
    if ($lnd eq 'irrCrop' && $tp =~ m/^rice/i) {
      my $paddyDepthReal	 = condition_slice($riceMask, $ricePaddyStrg/$riceMask, 0);
      my $paddyRain		 =($ricePercRate + $ricePaddyDepth - $paddyDepthReal)->lclip(0) * ($riceMask > 0);
         $paddyRain		 = $paddyRain->indexND($ind{$lnd}{$tp}) if $slice;
         $surplus		-= $paddyRain->inplace->hclip($surplus);		# mm/day
      $ricePaddyRain{slc}{$tp}	+= $paddyRain * $subDf * $landFrac{slc}{$lnd}{$tp};	# mm
    }
		### Surplus and Infiltration after rice paddy water has been collected
    my $GWt_infilt = $surplus * $GWt_inFrac{slc}{$lnd}{$tp} + $directRecharge;
    my $Aqf_infilt = $surplus * $Aqf_inFrac{slc}{$lnd}{$tp};
    $GWt_infilt{slc}{$lnd}{$tp}+= $landFrac{slc}{$lnd}{$tp} * $GWt_infilt;	# Cell weighted value
    $Aqf_infilt{slc}{$lnd}{$tp}+= $landFrac{slc}{$lnd}{$tp} * $Aqf_infilt;	# Cell weighted value
    $surfRunoff{slc}{$lnd}{$tp}+= $landFrac{slc}{$lnd}{$tp} * $surplus *
	(1 - $GWt_inFrac{slc}{$lnd}{$tp} - $Aqf_inFrac{slc}{$lnd}{$tp});	# Surface runoff portion of surplus

		### BW/GW additions
    if ($BwGwFlag) {
      my $sum = $subDf*($evapoTrans  + $sMoistChg);
      my $sMoistGrFr =(($sMoistGr{slc}{$lnd}{$tp} + $sum) /
		     ($sMoistPrev{slc}{$lnd}{$tp} + $sum))->setnonfinitetobad->setbadtoval(0)->clip(0,1);
      $sMoistGr  {slc}{$lnd}{$tp} .=  $sMoistGrFr * $sMoist{slc}{$lnd}{$tp};
      $sMoistGrET{slc}{$lnd}{$tp} .=  $sMoistGrFr * $evapoTrans;		# mm/day
      $sMoistET  {slc}{$lnd}{$tp} .=  $evapoTrans;				# mm/day
    }
	###############################################################
	###   Irrigation Part I - calculation of the water demand
    next unless $lnd eq 'irrCrop';

	### MIRCA Model- demand is when soil is below CropDepletionFactor. Irrigate to top of soil layer capacity.
    my $cropDepletionFactor  = $slice? $crpDF{$tp}->indexND($ind{$lnd}{$tp}) : $crpDF{$tp};

		#######################################################
		### Irrigation demand by Crop Depletion Factor		($irr_demand)
		### Irrigation application by daily rate limit		($irr_appRte)
		### Virtual daily demand				($irr_demVrt)
    my $soil_delta	    = $awCap{slc}{$lnd}{$tp} - $sMoist{slc}{$lnd}{$tp};
    my $irr_demand	    = condition($cropMask{slc}{$lnd}{$tp},
	condition($sMoistFrac{slc}{$lnd}{$tp} < $cropDepletionFactor, $soil_delta,0), 0)->lclip(0);
    $irr_demVrt{slc}{$tp}  .= condition($cropMask{slc}{$lnd}{$tp},    $soil_delta,    0)->lclip(0) if $useVIS;

    $irr_demand{slc}{$tp} .= condition($irr_demand, $irr_demand,		# Reset  $irr_demand,    mm
			     $irr_demand{slc}{$tp}->hclip($soil_delta));	# Reduce $irr_demand by rain water
    $irr_appRte{slc}{$tp} .= $irr_demand{slc}{$tp}->hclip($irrAppRate*$subDf);	# Irr volume to apply,   mm
    $irr_demand{slc}{$tp} -= $irr_appRte{slc}{$tp};				# $irr_demand remainder, mm

    $irr_appRte{slc}{$tp} *= $landFrac{slc}{$lnd}{$tp};
    $irr_demVrt{slc}{$tp} *= $landFrac{slc}{$lnd}{$tp} * $subDf;
    $irr_demand{SLC}{$tp} += $irr_appRte{slc}{$tp};			### units are mm
    $irr_demVrt{SLC}{$tp} += $irr_demVrt{slc}{$tp};			### units are mm
		### Virtual soil to track potential Irr demand
    if ($condition_Vs) {
      $irr_demVs{slc}{$tp}	.= condition($cropMask{slc}{$lnd}{$tp},		### Virtual soil Net Irr demand
	condition($sMoistFrVs{slc}{$lnd}{$tp} < $cropDepletionFactor,
		$awCap{slc}{$lnd}{$tp} - $sMoistVs{slc}{$lnd}{$tp},0), 0)->lclip(0);
      $sMoistVs{slc}{$lnd}{$tp}	+= $irr_demVs{slc}{$tp};	# <- Irrigation of virtual soil
      $irr_demVs{slc}{$tp}	*= $landFrac{slc}{$lnd}{$tp};
      $irr_demVs{SLC}{$tp}	+= $irr_demVs{slc}{$tp};		### units are mm
  }}}
	###############################################################
	###	Soil moisture full mixing model in comp. tracking
  compUpdate_Soil( $waterIn);
  compBalance_Soil($waterIn, $flushWater, $evapotrans);

	###############################################################
	###	Aquifer infiltration of surface water surplus

  $Aqf_infiltPrecip	*=  $mm_2_m3		     if $aqfType;	# Convert units from mm/day to m3/day
  $Aqf_dQ		+=  $Aqf_infiltPrecip*$subDf if $aqfType == 3;	# Add to ModFlow aquifers, m3
  foreach my $aqf_ID (@aqf_IDs) {
    my $sum =  $Aqf_infiltPrecip->indexND($$aqf_data{$aqf_ID}{idx})->sum * $subDf;	# m3
    $$aqf_data{$aqf_ID}{StoragePrev}	 = $$aqf_data{$aqf_ID}{Storage};
    $$aqf_data{$aqf_ID}{Storage}	+=  $sum;
    $$aqf_data{$aqf_ID}{Delta}		+=  $sum;
    $$aqf_data{$aqf_ID}{InfIn}		 =  $sum;
  }

	###############################################################
	###	Groundwater/Aquifer water Composition/Fractions (update)
  compUpdate_GrWater($GWt_infiltPrecip, $Aqf_infiltPrecip);		# Component Tracking
  $grdWater[1]	+= $GWt_infiltPrecip * $subDf;			# mm

	###############################################################
	###	Water Demand for domestic, industrial, and livestock consumption
				# Units are mm/day over soil area,
  if ($runIO{WaterDemand}) {	# because it uses the same "demandFromStreamGwater" function to draw water as irrigation

    bMarkMap(2, $bMarkM, 'WaterDemand Processing');

		#######################################################
		### Do this only ones for the whole year or first time after resume
    if ($yrBegCond || $n_count == $n_count_done+1) {
      my $popDensityS	= read_dateLayer($dataSet{Population}, $date,$extent,$runIO{spool}) / 365 * $CELL_AREA*$m3_2_mm;
      $domesticDemand	= read_dateLayer($dataSet{DomDemandPP},$date,$extent,$runIO{spool}) * $popDensityS;
      $industryDemand	= read_dateLayer($dataSet{IndDemandPP},$date,$extent,$runIO{spool}) * $popDensityS;

      foreach my $stock (keys %LiveStock) {
        $LiveStock{$stock}{Density} = ($LiveStock{$stock}{DensityRefYr} * (1 + $LiveStock{$stock}{AnimalGrowthRate} * ($date[0] - $stkRefYear)))->lclip(0);
      }
    }
		########################################
		### Do this for every day of the year
    $livestockDemand	= livestock_demand(\%LiveStock, $airT);

	#########################################################################
	### Take water out of stream and ground water storages (all units are mm. NB- over soil area of the cell)

				### Domestic demand- do not use SIR storage; $irrExtraCoeff=1
    bMarkMap(3, $bMarkM, 'Domestic demands');
		# Pack aquifer arguments for the demandFromStreamGwater funcion
    my @aqfArr  = $aqfType== 3 ? (Aqf_Well_Storage($AQF_DATA,$Aqf_dQ), $Cfd_dQ) : ($zeroes,$zeroes);
    my @aqfSet	= ($aqfType, $AQF_IDs, $ones, $$AQF_DATA{gwtUseC}, $aqfType ? $aquiferID : $zeroes,	# 0 .. 4
       @aqfArr,pdl($aqfType? map([[$$aqf_data{$_}{Storage},$$aqf_data{$_}{Delta}]], @aqf_IDs):[[0,0]]));# 5 .. 7
    my @argSet	= ($mm_2_m3, $ones, $domSearchDist);
		# Run domestic water withdrawals. Notes:  I/O units are volume, mm;
    ($DomUseStrgLoc, $DomUseFlowLoc, $DomUseStrgRmt, $DomUseFlowRmt,  $domUseFlowRID,
     $domUseSIR,     $domUseGrwt,    $domUseAqf,     $domUseExtra) = ($domesticDemand * $subDf)->
	demandFromStreamGwater($grdWater[1], $resStorage[0], $zeroes, $discharge, @aqfSet, @argSet, $dt);
		# Update aquifer data hash
    $aqf_data = arr_to_hash(unpdl_data($aqfSet[-1],{REF=>1}), [\@aqf_IDs,['Storage','Delta']], $aqf_data, -1) if $aqfType;

					# Convert mm -> mm/day
     map  $_ *= $subDi,	    $DomUseStrgLoc,  $DomUseFlowLoc, $DomUseStrgRmt, $DomUseFlowRmt, $domUseSIR,	# mm/day
			    $domUseGrwt,     $domUseAqf,     $domUseExtra	if $hr_flag;
     $domUseFlowLoc	 =  $DomUseStrgLoc + $DomUseFlowLoc;
     $domUseFlowRmt	 =  $DomUseStrgRmt + $DomUseFlowRmt;
     my $taken		 =  $domUseFlowLoc + $domUseFlowRmt + $domUseSIR + $domUseGrwt + $domUseAqf + $domUseExtra;
     my $return		 =  $taken  * $domReturnFr;	# mm/day for all "taken" components
     $domUseGross	 =  $domesticDemand;
     $domUseEvap	 =  $taken  - $return;
     $domUseSfWt	 =  $domUseFlowLoc + $domUseFlowRmt;
     $domUseGrWt	 =  $domUseGrwt    + $domUseAqf + $domUseExtra;
     $returnRff		+=  $return;			# Cumulative water use return to surface runoff
     $evapotrans	+=  $domUseEvap;		# Cumulative water use evaporation
     $useFlow		+=  $domUseSfWt;		# Cumulative water use from flow
     $domGrossT_y	+= ($domUseGross	* $MM_2_km3)->sum;		# volume, km3
     $domNetT_y		+= ($domUseEvap		* $MM_2_km3)->sum;
     $domXW_y[0]	+= ($domUseSfWt		* $MM_2_km3)->sum;
     $domXW_y[1]	+= ($domUseGrwt		* $MM_2_km3)->sum;
     $domXW_y[2]	+= ($domUseAqf		* $MM_2_km3)->sum;
     $domXW_y[3]	+= ($domUseExtra	* $MM_2_km3)->sum;

				### Industrial demand- do not use SIR storage; $irrExtraCoeff=1
    bMarkMap(3, $bMarkM, 'Industiral demands');
		# Pack aquifer arguments for the demandFromStreamGwater funcion
    @aqfSet	= ($aqfType, $AQF_IDs, $ones, $$AQF_DATA{gwtUseC}, $aqfType ? $aquiferID : $zeroes,	# 0 .. 4
    @aqfArr,   pdl($aqfType? map([[$$aqf_data{$_}{Storage},$$aqf_data{$_}{Delta}]], @aqf_IDs):[[0,0]]));# 5 .. 7
    @argSet	= ($mm_2_m3, $ones, $indSearchDist);
		# Run industrial water withdrawals. Notes:  I/O units are volume, mm;
    ($IndUseStrgLoc, $IndUseFlowLoc, $IndUseStrgRmt, $IndUseFlowRmt,  $indUseFlowRID,
     $indUseSIR,     $indUseGrwt,    $indUseAqf,     $indUseExtra) = ($industryDemand * $subDf)->
	demandFromStreamGwater($grdWater[1], $resStorage[0], $zeroes, $discharge, @aqfSet, @argSet, $dt);
		# Update aquifer data hash
    $aqf_data = arr_to_hash(unpdl_data($aqfSet[-1],{REF=>1}), [\@aqf_IDs,['Storage','Delta']], $aqf_data, -1) if $aqfType;

					# Convert mm -> mm/day
     map  $_ *= $subDi,	    $IndUseStrgLoc,  $IndUseFlowLoc, $IndUseStrgRmt, $IndUseFlowRmt, $indUseSIR,	# mm/day
			    $indUseGrwt,     $indUseAqf,     $indUseExtra	if $hr_flag;
     $indUseFlowLoc	 =  $IndUseStrgLoc + $IndUseFlowLoc;
     $indUseFlowRmt	 =  $IndUseStrgRmt + $IndUseFlowRmt;
     $taken		 =  $indUseFlowLoc + $indUseFlowRmt + $indUseSIR + $indUseGrwt + $indUseAqf + $indUseExtra;
     $return		 =  $taken  * $indReturnFr;	# mm/day for all "taken" components
     $indUseGross	 =  $industryDemand;
     $indUseEvap	 =  $taken  - $return;
     $indUseSfWt	 =  $indUseFlowLoc + $indUseFlowRmt;
     $indUseGrWt	 =  $indUseGrwt    + $indUseAqf + $indUseExtra;
     $returnRff		+=  $return;			# Cumulative water use return to surface runoff
     $evapotrans	+=  $indUseEvap;		# Cumulative water use evaporation
     $useFlow		+=  $indUseSfWt;		# Cumulative water use from flow
     $indGrossT_y	+= ($indUseGross	* $MM_2_km3)->sum;		# volume, km3
     $indNetT_y		+= ($indUseEvap		* $MM_2_km3)->sum;
     $indXW_y[0]	+= ($indUseSfWt		* $MM_2_km3)->sum;
     $indXW_y[1]	+= ($indUseGrwt		* $MM_2_km3)->sum;
     $indXW_y[2]	+= ($indUseAqf		* $MM_2_km3)->sum;
     $indXW_y[3]	+= ($indUseExtra	* $MM_2_km3)->sum;

				### Livestock demand- use SIR storage; $irrExtraCoeff=1
    bMarkMap(3, $bMarkM, 'Livestock demands');
		# Pack aquifer arguments for the demandFromStreamGwater funcion
    @aqfSet	= ($aqfType, $AQF_IDs, $ones, $$AQF_DATA{gwtUseC}, $aqfType ? $aquiferID : $zeroes,	# 0 .. 4
    @aqfArr,   pdl($aqfType? map([[$$aqf_data{$_}{Storage},$$aqf_data{$_}{Delta}]], @aqf_IDs):[[0,0]]));# 5 .. 7
    @argSet	= ($mm_2_m3, $ones, $indSearchDist);
		# Run livestock water withdrawals. Notes:  I/O units are volume, mm;
    ($StkUseStrgLoc, $StkUseFlowLoc, $StkUseStrgRmt, $StkUseFlowRmt,  $stkUseFlowRID,
     $stkUseSIR,     $stkUseGrwt,    $stkUseAqf,     $stkUseExtra) = ($livestockDemand * $subDf)->
	demandFromStreamGwater($grdWater[1], $resStorage[0], $smResStrg[1], $discharge, @aqfSet, @argSet, $dt);
		# Update aquifer data hash
    $aqf_data = arr_to_hash(unpdl_data($aqfSet[-1],{REF=>1}), [\@aqf_IDs,['Storage','Delta']], $aqf_data, -1) if $aqfType;

					# Convert mm -> mm/day
     map  $_ *= $subDi,	    $StkUseStrgLoc,  $StkUseFlowLoc, $StkUseStrgRmt, $StkUseFlowRmt, $stkUseSIR,	# mm/day
			    $stkUseGrwt,     $stkUseAqf,     $stkUseExtra	if $hr_flag;
     $stkUseFlowLoc	 =  $StkUseStrgLoc + $StkUseFlowLoc;
     $stkUseFlowRmt	 =  $StkUseStrgRmt + $StkUseFlowRmt;
     $taken		 =  $stkUseFlowLoc + $stkUseFlowRmt + $stkUseSIR + $stkUseGrwt + $stkUseAqf + $stkUseExtra;
     $return		 =  $taken  * $stkReturnFr;	# mm/day for all "taken" components
     $stkUseGross	 =  $livestockDemand;
     $stkUseEvap	 =  $taken  - $return;
     $stkUseSfWt	 =  $stkUseFlowLoc + $stkUseFlowRmt + $stkUseSIR;
     $stkUseGrWt	 =  $stkUseGrwt    + $stkUseAqf     + $stkUseExtra;
     $returnRff		+=  $return;			# Cumulative water use return to surface runoff
     $evapotrans	+=  $stkUseEvap;		# Cumulative water use evaporation
     $useFlow		+=  $stkUseSfWt - $stkUseSIR;	# Cumulative water use from flow
     $stkGrossT_y	+= ($stkUseGross	* $MM_2_km3)->sum;		# volume, km3
     $stkNetT_y		+= ($stkUseEvap		* $MM_2_km3)->sum;
     $stkXW_y[0] +=(($stkUseSfWt + $stkUseSIR)	* $MM_2_km3)->sum;
     $stkXW_y[1]	+= ($stkUseGrwt		* $MM_2_km3)->sum;
     $stkXW_y[2]	+= ($stkUseAqf		* $MM_2_km3)->sum;
     $stkXW_y[3]	+= ($stkUseExtra	* $MM_2_km3)->sum;
		### WaterDemand totals for aqifers and UGW
     $useAqf		 =  $domUseAqf   + $indUseAqf   + $stkUseAqf;		# mm/day
     $useExtra		 =  $domUseExtra + $indUseExtra + $stkUseExtra;		# mm/day
  }

	###############################################################
	#####	Irrigation Part II -
	#####	moving water to soil from ground and river stream storages

			### Variable name definitions:
	# $ricePaddyWater	= Water added to rice paddies from Irr to maintain flooding (without rain portion)
	# $ricePaddyRain	= Rainwater that contributes to rice paddy percolation and flooding
	# $ricePaddyPerc	= Water that floods the rice paddy, then percolates to groundwater (Irr + Rain)
	# $ricePaddyFlood	= Added flood paddy water to reach paddy depth (Irr + Rain)
	# $rainPercRice		= Rain       portion of $ricePaddyPerc  above
	# $rainFloodRice	= Rain       portion of $ricePaddyFlood above
	# $irrPercRice		= Irrigation portion of $ricePaddyPerc  above
	# $irrFloodRice		= Irrigation portion of $ricePaddyFlood above
	# $ricePaddyStrg	= Rice paddy  water storage
	# $riceSoilWater	= Water from $ricePaddyWater that goes to soil moisture
	# $return		= Inefficient portion of irrigation.  Does NOT include $ricePaddyWater
	# $irrEvapDeliv		= Evaporation from irr inefficient water at delivery
	# $irrPercDeliv		= Percolation of   irr inefficient water at delivery
	# $irrEvapIneff		= Evaporation from irr inefficient water at application
	# $irrPercIneff		= Percolation of   irr inefficient water at applicaton
	# $irrRnffIneff		= Surf runoff of   irr inefficient water at applicaton (adds to irr runoff storage)
	# $GWt_infiltIrr	= Total percolation of irr water (inefficient + $ricePaddyWater)
	# $irrRunoff		= Surface runoff of irr water from irr runoff storage)

	# $irrigationGross	= All water extracted from water sources for both irr_demand AND added water,
	#				accounts for inefficiency
	# $irrigationNet	= Efficient portion of irrigation, including ricePaddyWater.
	# $irrFactor		= Fraction of delivered irrigation water from the requested ammount

  if ($runIO{Irrigation}) {
    bMarkMap(2, $bMarkM, 'Irrigation Processing # 2');

    my $lnd		= 'irrCrop';
    my $lossEvapRate	= [$waterEvap, ($pet-$etIrrCrops)->lclip(0)];	# Inefficient water evaporation rate
       $irrEfficiency	=  $irrTech{Application}{Flag} ? $irrTech{Application}{Efficiency} :
		read_dateLayer($irrEfficiencyData,$date,$extent,$runIO{spool},
			{RESAMPLE=>0, PATCH_VALUE=>100})->lclip(10)/100;

		### Demands for rice paddy water. Presently paddy water is a virtual storage filled at the plant day.
		###	Note- $riceMask is a fraction of land where rice is planted
		###	All vars units are volume, mm
      $ricePaddyPerc	 = $ricePercRate   * $subDf * $riceMask;		# Needed rice paddy percolation
      $ricePaddyFlood	 =($ricePaddyDepth * $riceMask - $ricePaddyStrg)->lclip(0);# Needed rice paddy flooding
    my $percFloodRatio	 =($ricePaddyPerc  /($ricePaddyPerc + $ricePaddyFlood))->setnonfinitetobad->setbadtoval(0);
      $rainPercRice	 = $ricePaddyRain  *     $percFloodRatio;		# Irrigation portion of Percolation
      $rainFloodRice	 = $ricePaddyRain  *(1 - $percFloodRatio);		# Irrigation portion of Flooding
      $irrPercRice	 = $ricePaddyPerc  - $rainPercRice;			# Irrigation portion of Percolation
      $irrFloodRice	 = $ricePaddyFlood - $rainFloodRice;			# Irrigation portion of Flooding
      $ricePaddyWater	 = $irrPercRice    + $irrFloodRice;			# Rice paddy water to bring from Irr
		### Convert some vars above from volume to fluxes, mm/day
      map $_ *= $subDi,	   $rainPercRice, $rainFloodRice, $irrPercRice, $irrFloodRice if $hr_flag;	# mm/day

	# Note- $irr_demand_total is for soil only and does not include rice added water!
    $irrigationNet	 = $irr_demand_total + $ricePaddyWater;		# Volume, mm

		### Irrigation inefficient water losses, mm/day
    bMarkMap(3, $bMarkM, 'Irrigation losses calculation');
   ($irrEvapDeliv, $irrPercDeliv,					# Delivery losses,	mm/day
    $irrEvapIneff, $irrPercIneff, $irrRnffIneff) =			# Application losses,	mm/day
	irrLosses( $irr_demand_total*$subDi, $irrigationNet*$subDi, \%irr_appRte, $irrArea, \%irrTech, $irrEfficiency,
		   $irrReturnCoef,   $lossEvapRate,  $ricePercRate, \%awCap, \%land, \%landFrac, $irrRffStorage[0]);
    my $lossSum  =($irrEvapDeliv + $irrPercDeliv + $irrEvapIneff + $irrPercIneff + $irrRnffIneff)*$subDf; # mm

	#################################################################
	### Irrigation water withdrawal from available water as requested, mm

    my $irr_reqGross  = $irrigationNet + $lossSum;	# Gross = Net + Loss irrigation
    my $irr_request   = $irr_reqGross	unless $useVIS;

	#################################################################
	### Virtual daily irrigation components/quantities, mm
    if ( $useVIS ) {
      my $irr_vrtGross	=  $irr_demVrt_total + $ricePaddyWater + $lossSum;	#  Gross demand for virtual irrigation
		### (Gross) Irrigation request is for virtual Irrigation minus Storage in VIS
      $irr_request	= ($irr_vrtGross - $irrVIS[1])->clip(0, $irr_reqGross->lclip($irr_vrtGross));
    }
	#################################################################
	### Irrigation water withdrawal from available water as requested

		### Irrigation water withdrawal in addition to VIS
    bMarkMap(3, $bMarkM, 'Irrigation water withdrawal');
		# Pack aquifer arguments for the demandFromStreamGwater funcion
    my @aqfSet	= ($aqfType, $AQF_IDs, $irrExtraCoeff, $$AQF_DATA{gwtUseC}, $aqfType ? $aquiferID : $zeroes,	# 0 .. 4
	$aqfType== 3 ? (Aqf_Well_Storage($AQF_DATA,$Aqf_dQ),$Cfd_dQ) : ($zeroes,$zeroes),		# 5 .. 6
        pdl($aqfType ? map([[$$aqf_data{$_}{Storage},$$aqf_data{$_}{Delta}]], @aqf_IDs) : [[0,0]]));	# 7
    my @argSet	= ($mm_2_m3, $SW_ratio, $irrSearchDist);
		# Run irrigation water withdrawals. Notes:  I/O units are volume, mm; $aqf_data to be updated in-place
    ($IrrUseStrgLoc, $IrrUseFlowLoc, $IrrUseStrgRmt, $IrrUseFlowRmt, $irrUseFlowRID,
     $irrUseSIR,     $irrUseGrwt,    $irrUseAqf,     $irrUseExtra) = $irr_request->demandFromStreamGwater(
	$grdWater[1],$resStorage[0], $smResStrg[1],  $discharge, @aqfSet, @argSet, $dt);
		# Update aquifer data hash
    $aqf_data = arr_to_hash(unpdl_data($aqfSet[-1],{REF=>1}), [\@aqf_IDs,['Storage','Delta']], $aqf_data, -1) if $aqfType;

					# Convert mm -> mm/day
    map $_ *= $subDi,	  $IrrUseStrgLoc,   $IrrUseFlowLoc, $IrrUseStrgRmt, $IrrUseFlowRmt, $irrUseSIR,
			  $irrUseGrwt,      $irrUseAqf,     $irrUseExtra	if $hr_flag;
    $irrUseFlowLoc	= $IrrUseStrgLoc  + $IrrUseFlowLoc;
    $irrUseFlowRmt	= $IrrUseStrgRmt  + $IrrUseFlowRmt;
    $irrigationFlow	= $irrUseFlowLoc  + $irrUseFlowRmt;
    $irrigationGross	= $irrigationFlow + $irrUseGrwt   + $irrUseAqf + $irrUseExtra + $irrUseSIR;
    $irrigationGrossT	=($irrigationGross* $MM_2_km3)->sum;	# Total is volume, mm
    my $irrGross	= $irrigationGross->copy * $subDf;	# $irrGross (mm) will change, if VIS

		### Add withdrawn irrigation water to the VIS storage
    if ( $useVIS || $irrVIS_delta ) {
      $irrVIS[1]	+= $irrigationGross * $subDf;		# Volume, mm
		### Use VIS storage for actual irrigation demand (called by the depletion factor)
      $irrGross		.= $irr_reqGross - ($irr_reqGross-$irrVIS[1])->lclip(0);
      $irrVIS[1]	-= $irrGross;	# and subtract $irrGross from VIS storage
      $irrVIS_delta	 = $irrVIS[1]->sum	unless $useVIS;
    }
		### $irrFactor is a fraction of actually withdrawn water compared to requested volume
    $irrFactor	  = ($irrGross / $irr_reqGross)->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask});
    $irrFactorPct = irrFactorPct($irrigationGrossT, ($irr_request * $mm_2_km3)->sum);

		### Scale irrigation water components by $irrFactor (actual amount of extracted water)
    map $_ *= $irrFactor,  $irrigationNet, $irrPercRice,  $irrFloodRice, $ricePaddyWater,
			   $irrEvapDeliv,  $irrPercDeliv, $irrEvapIneff, $irrPercIneff, $irrRnffIneff;
#   map{ $riceSoilWater	+= $irrFactor * $irr_appRte{$_} if m/^rice/i } sort keys %{$land{irrCrop}}; # Not yet used in output

		### Remove groundwater portion from delivery losses-
		###	only in case of efficiency based water delivery model
    if ($irrTech{Delivery}{Flag}) { unless  ($irrTech{Delivery}{Process}) {
      my  $delivFrac	 =($irrigationFlow / $irrigationGross)->setnonfinitetobad->setbadtoval(0);
      my  $delivLoss	 = $irrEvapDeliv   + $irrPercDeliv;		# Water delivery loss
      map $_*=$delivFrac,  $irrEvapDeliv,    $irrPercDeliv;
      my  $delivDelta	 = $delivLoss      -($irrPercDeliv + $irrEvapDeliv);
	  $irrVIS[1]	+= $delivDelta     * $subDf;			# Volume, mm
	  $irrVIS_delta  = 1	unless $useVIS;
    }}
	#######################################################
		### Managing irrigation return water

    bMarkMap(3, $bMarkM, 'Irrigation water returns');

    $evapotrans		+= $irrEvapIneff + $irrEvapDeliv;
	# irrPercIneff is zero here in case of irrigation technologies.  irrPercIneff is updated after routing,.
    my $percolation	 = $irrPercIneff + $irrPercRice + $rainPercRice + $irrPercDeliv;
    $GWt_infiltIrr	 = $percolation  * $GWt_infiltRatio;		# Percolation to shallow groundwater,     mm/day
    $Aqf_infiltIrr	 = $percolation  * $Aqf_infiltRatio* $mm_2_m3	if $aqfType;	# to groundwater aquifer, m3/day
    $Aqf_dQ		+= $Aqf_infiltIrr* $subDf			if $aqfType==3;	# to ModFlow aquifers,    m3
		### Update aquifer storages
    foreach my $aqf_ID (@aqf_IDs) {}		# Removed from public domain

		### Re-calculate soil moisture after the irrigation is applied
    bMarkMap(3, $bMarkM, 'Irrigation water application to soil');

    foreach my $tp (sort keys %{$land{$lnd}}) {		### ADDED SORT-NEED!
      $irrigation{$tp}	= $irrFactor * $irr_appRte{$tp};						# mm
      my $irrigation_S	=($slice? $irrigation{$tp}->indexND($ind{$lnd}{$tp}): $irrigation{$tp});	# mm

      $sMoist{slc}{$lnd}{$tp}     +=($irrigation_S / $landFrac{slc}{$lnd}{$tp})     ->setnonfinitetobad->setbadtoval(0);
      $sMoistFrac{slc}{$lnd}{$tp} .=($sMoist{slc}{$lnd}{$tp}/$awCap{slc}{$lnd}{$tp})->setnonfinitetobad->setbadtoval(0);
    }
	# Note - Soil moisture change now is due to precipitation + evaporation + irrigation
    $sMoist[1]	*= 0;	# Reset these vars (NB- they have children slices)
    $sMoistFrac	*= 0;
    foreach my $lnd (sort keys %land) { foreach my $tp (sort keys %{$land{$lnd}}) {	### ADDED SORT-NEED!
      $sMoist{SLC}{$lnd}{$tp}     += $sMoist{slc}{$lnd}{$tp}	* $landFrac{slc}{$lnd}{$tp};
      $sMoistFrac{SLC}{$lnd}{$tp} += $sMoistFrac{slc}{$lnd}{$tp}* $landFrac{slc}{$lnd}{$tp};
    }}
  }
  my $sMoistChg      = $sMoist[1] - $sMoist[0];

		### Remove water from unsustainable groundwater (UGW) pool
  $UGW -= $useExtra + $irrUseExtra if $n_count > $n_spinup;	# Exclude spinup from $UGW accumulation

	###############################################################
	#####   Baseflow and Runoff

  bMarkMap(2, $bMarkM, 'Runoff');

  my $baseFlow	  = $grdWater[1] * $MDGroundWatBETA;		# mm/day
  $grdWater[1]   -= $baseFlow    * $subDf;			# mm

	###############################################################
	#####   Surface Runoff Storage (Retention Pool)

  push @surfRffStorage, $surfRffStorage[0]+$surfRunoff->inplace->lclip(0)*$subDf;
  push @irrRffStorage,	$irrRffStorage[0]->copy;

  compUpdate_RffStg($surfRunoff);			# Update Tracking Components in it
  my $pristineIrrComp = [$subDf*$waterIn, $glMelt*$dt + $flushWater]	if $compBalFlag;
				# mm/day   m3/sec	    m3

	###############################################################
	#####   Surface and Total Runoff

		### Water release from surface runoff storages (Prusevich: by tank drain formulation)
		### See- https://www.engineeringtoolbox.com/flow-liquid-water-tank-d_1753.html
			### Runoff from inefficent irrigation water. i.e "$irrRffStorage" (NB: [1]=[0] 2 lines above).
			### The $irrRunoff should be done before routing, and composition of $irrRffStorage[0] is known!
  if ($runIO{Irrigation}) {
    my $irrRffStrg	= ($irrRffStorage[1] - $ricePaddyStrg)->lclip(0);	# Rice paddy water is locked for this

    my $dryArea	= 1 - $irrArea;
    my $surfStg	= condition_slice($dryArea > 0, $surfRffStorage[1]/$dryArea, $surfRffStorage[1]); # avoids division by zero
    my $irrgStg	= condition_slice($irrArea > 0, $irrRffStrg    /$irrArea, $irrRffStrg );
						# Water release by tank drain formulation
    $surfRunoff	= $dryArea * condition($landMask, ($RhRt2 * sqrt($G2 * $surfStg))->hclip($surfStg*$subDi), $surfStg*$subDi);
    $irrRunoff	= $irrArea * condition($landMask, ($RhRt2 * sqrt($G2 * $irrgStg))->hclip($irrgStg*$subDi), $irrgStg*$subDi);
						# Subtract runoff from surface retention storage
    $surfRffStorage[1]	-=($subDf * $surfRunoff)->hclip($surfRffStorage[1]);	# "hclip" is needed for
    $irrRffStorage [1]	-=($subDf * $irrRunoff )->hclip($irrRffStrg);		# Float problem on very small numbers
  } else {
    $surfRunoff	= condition($landMask, ($RhRt2 * sqrt($G2 * $surfRffStorage[1]))->hclip($surfRffStorage[1]*$subDi),
					$surfRffStorage[1]);
    $surfRffStorage[1]	-=($subDf * $surfRunoff)->hclip($surfRffStorage[1]);	# Subtract runoff from surface storage
  }
		### Storm runoff from above soil saturation
  my $addCapSurfRff	 =($surfRffStorage[1] - $MaxSurfRffStorage)->lclip(0);	# mm
     $surfRunoff	+= $addCapSurfRff * $subDi;				# mm/day
     $surfRffStorage[1]	-= $addCapSurfRff;					# mm

		### Water refill to Small Irrigation Reservoirs (SIR)
  my ($sirIrrTaken, $smResRefill) = (0, 0);
  if ($smResFlag) {
    $sirIrrTaken	 =($smResStrg[0] - $smResStrg[1]) * $subDi;	# SIR water used for irr and livestock, mm/day
    $smResRefill	 =($smResCpcty   - $smResStrg[1])->hclip($surfRunoff*$subDf);	# mm
    $smResStrg[1]	+= $smResRefill;						# mm
    $surfRunoff		-= $smResRefill  * $subDi;					# mm/day
   ($cSIR_prev, $cSIR_P_prev, $cSIR_Irr_prev, $cSIR_Msk_prev) =
	map { $_->copy if ref $_; } ($cSIR, $cSIR_P, $cSIR_Irr, $cSIR_Msk);
  }
		### Total Runoff
  my $runoff	=  $surfRunoff + $irrRunoff + $baseFlow;		# mm/day
  my $runoffVol	= ($runoff*$MM_2_m3 + $flushWater)/$dt + $glMelt;	# m3/sec

		### Update Tracking Components in Runoff
  compUpdate_Runoff($surfRunoff,$irrRunoff,$baseFlow,$runoffVol,$flushWater,$smResRefill);

	### Debugging example
#   my ($LON,$LAT)= (117.25, 30.75);	# China
#   my  @colRow   = colRow($extent,$LON,$LAT);
#   printf "\tRunoff at (%.2f,%.2f) = %.2f\n",$LON,$LAT, $runoff->at(@colRow);
#   imag $runoff(,-1:0);		# Graphics output by PDL::Graphics::PGPLOT

	###############################################################
	#####   Discharge (Routing)
	###############################################################

  bMarkMap(2, $bMarkM, 'Routing');

  my $rtMethod;
  my ($C0, $C1, $C2) = ($ones->copy, $zeroes->copy, $zeroes->copy);

	#####   Combine Reservoir and Stream geometry, in meters
  my $minRsvStrg= $yStream * $wStream * $lStream;	### Minimum reservoir storage (used to skip too small dams)
    ($depth,$width,$length,$velocity) =
	streamGeometry($yStream,$wStream,$lStream,$vStream,$damData,$resStorage[0],$minRsvStrg,$dischargePrev,$j_date);

	###############################################################
	#####   Calculate optimal reservoir capacity for spillway dams only (Prusevich, in prep)
  if ($runIO{Reservoirs}) {
    bMarkMap(3, $bMarkM, 'Reservoir parameterization');

    my $head = $yMean->index2d($$reservoir[0]->((0),$spillDam),$$reservoir[0]->((1),$spillDam))->flat
		->lclip(0.01);	# Do not allow head to be below 1 cm (avoid division by zero)
    #  Optimal Capacity     =  Head * Reservoir_Area * (1 + Alpha * Head)
    $$reservoir[1]->((3),$spillDam) .= $head * $$reservoir[1]->((1),$spillDam) *
					  (1 + $$reservoir[1]->((2),$spillDam) * $head);

	#####   Get discharge for today's observed reservoirs
    my $DATE	= substr($date,0,10);		# Remove hours from the date string
    foreach my $obsDamIdx ($obsDam->list) {
      $$reservoir[1]->(13, $obsDamIdx) .= exists $$damData[$obsDamIdx][22]{$DATE} ? $$damData[$obsDamIdx][22]{$DATE} : 0;
    }
  }
	#####   Calculate parameters for irrigation dams as a function of irrigation demand (Prusevich, in prep)
  if ($dataSet{IrrFrequency}) {					# Daily Max IrrFrequency is      0.015
    my $irrFrequency = read_dateLayer($dataSet{IrrFrequency},$date,$extent,$runIO{spool})->hclip(0.015)->
			index2d($$reservoir[0]->((0),$irrigDam), $$reservoir[0]->((1),$irrigDam))->flat;
    $$reservoir[1]->( (7),$irrigDam) .= $$reservoir[1]->((4), $irrigDam) +
		365 * $irrFrequency   * $$reservoir[1]->((12),$irrigDam) * (1 - $$reservoir[1]->((4),$irrigDam));
#     $$reservoir[1]->( (8),$irrigDam) .= d_root_solver([map $$reservoir[1]->(($_),$irrigDam), 2.. 7]);#Can comment out
    $$reservoir[1]->( (9),$irrigDam) .= a_dam_equaton([map $$reservoir[1]->(($_),$irrigDam), 2.. 7]);
    $$reservoir[1]->((10),$irrigDam) .= b_dam_equaton([map $$reservoir[1]->(($_),$irrigDam), 2.. 8]);
    $$reservoir[1]->((11),$irrigDam) .= B_dam_equaton([map $$reservoir[1]->(($_),$irrigDam), 2..10]);
  }

	###############################################################
	#####   Evaporation from Lakes, Rivers, Reservoirs, and Canals

  bMarkMap(3, $bMarkM, 'Evaporation from open water');

			### Reservoirs
  my $rsrvrArea   =  $cell_table->resArea(@$reservoir, $j_date, $network->dims);
  my $rsrvrEvap   = ($rsrvrArea/1000* $waterEvap)->hclip($resStorage[0]*$subDi);	# m3/day
  $resStorage[0] -=  $rsrvrEvap * $subDf;						# m3
			### Small Irrigation Reservoirs (SIR)
			### NB- we are using Dominik's 0.6*PET or $waterEvap for evaporation from SIR
  my $sirEvap     =($smResArea/1000* $waterEvap*$m3_2_mm)->hclip($smResStrg[1]*$subDi);		# mm/day
  $smResStrg[1]  -= $sirEvap * $subDf;
			### Rivers
  my $riverEvap   = ($dL*$wStream  * $waterEvap)->hclip($resStorage[0]*$subDi);		# m3
  $resStorage[0] -=  $riverEvap * $subDf;
			### Connectivity Canals open water evaporation at Recipient cells
  $$connectivity[1]->((5),) .= $waterEvap->index2d($$connectivity[0]->((2),),$$connectivity[0]->((3),))->flat
	if $$connectivity[0]->dims;						# mm/day

  my $netwkEvap	 = $rsrvrEvap + $riverEvap;	# This is part of river network balance, m3/day
  $evapotrans	+= $sirEvap   + $canopyET;	# This is part of pixel balance, mm/day
  compBalance_EvapRiv($netwkEvap, $sirEvap);

	###############################################################
	#####   Water Temperature of the stream (Routing) by (Dingman, 1972)

  my ($waterTe, $waterExp) = (pdl(), pdl());
  if ($waterTemp) {						# Cloud plus canopy shading
    bMarkMap(3, $bMarkM, 'Water temperature pre-processing');

    my $cloudShd= $cloudFr + (1 - $cloudFr) * readflex($canopySh[$jDay]) * ($canopyHt/$width->lclip(1))->hclip(1);
    my $solarRad= netSolarRadiation($grossRad, $cloudShd);			# MJ/m2/day
			# Running average air temperature to use for surface runoff water temperature
    $airT_rAvg	= (($rAvgDays-1)*$airT_rAvg + $airT) / $rAvgDays;
    $airT_bAvg	= (($bAvgDays-1)*$airT_bAvg + $airT) / $bAvgDays;

			### Reservoir/Stream Water Temperature
    my $waterEo	= condition($cloudShd<0.95,105+23 *$windSpeed,-73+9.1*$windSpeed)*$cal2Kj; # Heat loss rate, KJ/m2/d
    my $waterEc	= condition($cloudShd<0.95, 35+4.2*$windSpeed, 37+4.6*$windSpeed)*$cal2Kj; # Energy exchange coeff
       $waterTe	= $airT + ($solarRad*1000 - $waterEo)/$waterEc;	# In-stream equilibrium temperature
		### Wet bulb temperature correction (http://www.srh.noaa.gov/images/epz/wxcalc/wetBulbTdFromRh.pdf)
    my $lnEs	= log($rHumidity * 10**(7.5*$waterTe/(237.3+$waterTe)));
       $waterTe	= (237.3 * $lnEs / (17.27 - $lnEs))->lclip(0);

	# Notes for $waterExp:	1. No heat exchange over open water pixel (it is done over dam pixel): $landMask
			#	2. Water residence time for heat exchange cannot exceed one time step: hclip(1)
			#	3. Temperature affected depth cannot exceed 20 m (ask Nihar?).
       $waterExp= exp(-$waterEc*$landMask*($length/($velocity*$dt))->hclip($subDf) / (4186.8*$depth->hclip(20)));
			### Connectivity Canals (waterEc at Recipient cells)
       $$connectivity[1]->((7),) .= $waterEc->index2d($$connectivity[0]->((2),),$$connectivity[0]->((3),))->flat
		if $$connectivity[0]->dims;
  }

	###############################################################
	#####   Dissolved Inorganic Nitrogen (DIN) by (Wollheim, 2008; Jordan, 1997)

  my  @DIN_Args	= ((pdl()) x 3);		### Must be defined as three-element array
  if ($runIO{DIN}) {
    bMarkMap(3, $bMarkM, 'DIN loading calcualtions');

      ### Loading Concentrations, mg/L
      ###  Both the Wollheim Suburban (or combined Human Land-Use) and Jordan Agricultural
      ###  loading functions account for background concentrations.  Therefore, these loading
      ###  functions apply to the WHOLE grid-cell - hence we use propAgr and propSub.
    my $runoffVolume	= $runoffVol * $dt;
    my $mask_out	=($runoffVolume / ($cell_area*1e3)) < 1e-5;	# runoff(mm) < 1e-5
    my $xMid		= condition_slice($mask_out, 0, $DIN_param{xMid_b} + $DIN_param{xMid_m} * log10($runoff));
    my ($Conc_Sub_DIN, $Conc_Agr_DIN);
    if ($runIO{Irrigation}) {
	$Conc_Sub_DIN = condition_slice($mask_out, 0,
			  $DIN_param{asym} / (1 + exp(($xMid - $luSub ) / $DIN_param{scale})));		# Wollheim, 2008
	$Conc_Agr_DIN = condition_slice($mask_out, 0,
			  $DIN_param{asym_ag} / (1 + exp(($xMid - $luAgr) / $DIN_param{scale})));	#  Attempt to distinguish higher Ag loading
    } else {
	$Conc_Sub_DIN = condition_slice($mask_out, 0,
			  $DIN_param{asym} / (1 + exp(($xMid - $luSub - $luAgr ) / $DIN_param{scale})));		# Wollheim, 2008
	$Conc_Agr_DIN = $Conc_Sub_DIN->Copy();
    }
        # condition_slice($mask_out, 0,
        # ((0.1796*$DIN_param{BFI}-0.0501)*$luAgr-1.561*$DIN_param{BFI}-1.132)->lclip(0));	# Jordan, 1997
    my $Conc_DIN	= $propSub * $Conc_Sub_DIN + $propAgr * $Conc_Agr_DIN;	# mg/L
       $cRunoffDIN	= condition_slice($mask_out,0,$Conc_DIN / 1000.0);	# g/L

		### Natural DIN Loading, kg/day
    my $Load_Sub_DIN	= $runoffVolume * $propSub * $Conc_Sub_DIN / 1000;	# kg/pixel/day
    my $Load_Agr_DIN	= $runoffVolume * $propAgr * $Conc_Agr_DIN / 1000;	# kg/pixel/day
       $DIN_Land_Load	=($Load_Sub_DIN + $Load_Agr_DIN) / $cell_area;		# kg/km2/day
       $DIN_Load	= $DIN_Land_Load->copy;					# kg/km2/day
							#   Cell area excludes impervious and glacier areas
		### DIN load from Waste Water Treatment Plants (WWTP)
    if ($DIN_param{WWTP}) {							# Scale by population change
      my $scale	= ($DIN_param{Population} / $DIN_param{refPop})->clip(0.25, 4);	# relative to reference year
     ($DIN_WWTP_Input, $DIN_WWTP_Removal, $DIN_WWTP_Load) = map $_ * $scale, @DIN_WWTP;	# kg/pixel/day
      my $norunoff_idx = whichND($mask_out);
      $DIN_WWTP_Removal->indexND($norunoff_idx)	+= $DIN_WWTP_Load->indexND($norunoff_idx);
      $DIN_WWTP_Load->indexND($norunoff_idx)	.= 0;	# set WWTP loading to 0 when runoff == 0
      $DIN_Load   += $DIN_WWTP_Load / $cell_area;					# kg/km2/day
      $cRunoffDIN +=($DIN_WWTP_Load / $runoffVolume)->setnonfinitetobad->setbadtoval(0);	# g/L
    }
		### DIN Removal. Notes:
			#	1. Water residence time for denitrification cannot exceed one time step: hclip(1)
    my $hydraulicLoad	= $depth->hclip(20) / ($length/($velocity*$dt))->hclip(1);	# m/day (hydraulic load)
    my $temperature	= $waterTemp ? $cStreamTw : $airT;
    my $DIN_coef	= 10**$DIN_param{intcpt} * ($dt/100 / $hydraulicLoad) *	# cm/sec (hydraulic load)
		$DIN_param{TnQ}**(($temperature - $DIN_param{TnRef})/10);	# Correction for water temperature

		### Parameters to be passed to routing() funcion
    @DIN_Args	= ($DIN_coef, $DIN_param{slope}, int(!$DIN_param{SzRes}));

		### Calculate DIN Removed by water use (Dom/Ind/Lsk/Irr) from stream storage ($DIN_Used)
    my $resStorage	= $resStorage[0]  + $netwkEvap*$subDf;
    $DIN_Used		= $DIN_Prev - $resStorage * $cStreamDIN;
    $cStreamDIN->where(!$resStorage) .= 0;
    $cStmConDIN->where(!$resStorage) .= 0;

		### DIN Evapoconcentration
    $DIN_Used	+= condition_slice($resStorage[0],0,$netwkEvap*$subDf * $cStreamDIN);	# Account for dried streams
    $cStreamDIN	*= condition_slice($resStorage[0], ($netwkEvap*$subDf + $resStorage[0])/$resStorage[0], 0);
    $cStmConDIN	*= condition_slice($resStorage[0], ($netwkEvap*$subDf + $resStorage[0])/$resStorage[0], 0);
  }

	###############################################################
	#####   USGS data assimilation

  if ($usgs_data) {}		# Removed from public domain

	###############################################################
  bMarkMap(3, $bMarkM, 'Routing call');

	###############################################################
	#####	WBM Muskingum or Flush Routing

  if ($routingMethod =~ m/Muskingum|Flush/i) {
    $rtMethod	= 1;

    if ($routingMethod =~ m/Muskingum/i) {
      my $C	= $xi * $vMean * $dt/$lStream;
      my $D	= $yMean / ($lStream * $slope * $xi);
      my $E	=  ( 1 + $C + $D);

         $C0	= ((-1 + $C + $D) / $E)->setnonfinitetobad->setbadtoval(1);
         $C1	= (( 1 + $C - $D) / $E)->setnonfinitetobad->setbadtoval(0);
         $C2	= (( 1 - $C + $D) / $E)->setnonfinitetobad->setbadtoval(0);
    } else {}	### Flush Routing
  }
	###############################################################
	#####	WBM LRR Routing

  elsif ($routingMethod =~ m/LRR/i) {
    $rtMethod	= 2;
    my $weight	= $wgtLength =~ m/No/i ? 1 :	### Weighted Length of the pixel stream reach (by Shantar Zuidema)
      ($flowInPrev/$dischargePrev + ($runoffVol*$dt+$resStoragePrev)/($dischargePrev*$dt)/2)
      ->setnonfinitetobad->setbadtoval(1)->clip(0.5,1);

    $C0		= ($velMethod =~ m/Constant/i && $wgtLength =~ m/No/i) ? $flowCoeff->copy :
	1/(1 + $weight*$lStream/($velMethod =~ m/Constant/i ? $flowSpeed : $velocity)/$dt)->copybad($network);
  }
  else { die "Unknown routing method ($routingMethod). Aborting...\n\n" }

	###############################################################
	#####	WBM Flush Routing over open water, e.g. lakes

  $C0->indexND($lakeMaskIdx) .= 1;
  $C1->indexND($lakeMaskIdx) .= 0;
  $C2->indexND($lakeMaskIdx) .= 0;

	###############################################################
	#####	Routing Function Call
							# Discharge used for irrigation/etc.
  my  $dischUsed	= $dischargePrev - $discharge;	# It can be truncated during routing (not enough water)
  my  $dischUsedPrev	= $dischUsed->copy; 		# causing some small flow missbalance

  my ($disch,$disch_in, $wAge, $dischFr, $dischPmFr, $dischIrrFr, $dischTw, $dischMsk, $dischDIN, $consrDIN, $DIN_Denit,
      $strg, $flow_out, $routeDiversion) = $cell_table->routing(
	$rtMethod,	$C0, $C1, $C2,
	$runoffVol,	$cRunoff,	$cRunoffP,	$cRunoffIrr,	$cRunoffTw, $cRunoffMsk,   $cRunoffDIN,
	$dischargePrev,	$dischUsed,
	$streamAge,	$cStream[0],	$cStreamP[0],	$cStreamIrr[0],	$cStreamTw, $cStreamMsk[0],
	$cStreamDIN,	$cStmConDIN,
	$flow_outPrev,	$meanDischarge,	$resStorage[0],	$minRsvStrg,	@$reservoir,$dt,	   $j_date,
	$cSwitch,	@$connectivity,	$waterTe,	$waterExp,	@DIN_Args,  @snkStack,	   @usgsStack);

  push	@resStorage, $strg->lclip(0);
	$discharge	= $disch ->lclip(0);
	$streamAge	= $wAge      ->copybad($network)	if $compSwitch[0];
  push	@cStream	, $dischFr   ->copybad($network)	if $compSwitch[1];
  push	@cStreamP	, $dischPmFr ->copybad($network)	if $compSwitch[2];
  push	@cStreamIrr	, $dischIrrFr->copybad($network)	if $compSwitch[3];
	$cStreamTw	= $dischTw   ->copybad($network)	if $compSwitch[4];
  push	@cStreamMsk	, $dischMsk  ->copybad($network)	if $compSwitch[5];
	$cStreamDIN	= $dischDIN  ->copybad($network)	if $compSwitch[6];
	$cStmConDIN	= $consrDIN  ->copybad($network)	if $compSwitch[6];

  rehash_stack($usgs_data, ['disch','DISCH','delta'], \%usgs_CT, @usgsStack) if $usgs_data;
  my $aquiferSnk	= $snkStack [0]->((2),);	# Sink  flow (m3/sec) to an aquifer at sink sites locations
  my $usgsDelta		= $usgsStack[0]->((2),);	# Added flow (m3/sec) to discharge  at USGS sites locations
  my $flowUseDelta	= $dischUsedPrev - $dischUsed;	# Water taken from underpredicted discharge (see above)
     $flowInPrev	=($discharge - $runoffVol)->lclip(0);

		### Update data for 5-year running average Discharge
  my $mDischargeS = $mDischargeK == $#mDischarge;	# Case of first 5-year span
  map { $mDischargeN[$_]++;
        $mDischarge [$_]+= $discharge; } $mDischargeS..$mDischargeK;

		### Update endorheic lake water balance and geometry
		# Removed from public domain

	###############################################################
	#####	Return flows from irrigation and other water uses,
	#####		and
	#####	Tracking water components in Ground water and Runoff Retention Storage
	###############################################################
	#	This should be done after routing when we know
	#   component ratios in discharge which were used for irrigation
	#   and, thus, return flows create feedback.
	#	The solution used-
	#   Do the ground water recharge and runoff storage refill after routing.
	###############################################################

  bMarkMap(2, $bMarkM, 'Tracking various components after routing');
  compUpdate_VIS();					# This must be done first in this row
  compBalance_Use(	$irrigationNet, $irrigationGross, $irrEvapIneff, $irrEvapDeliv, $ricePaddyWater, $pristineIrrComp);
  compBalance_EvapIrr(	$irrEvapIneff, $irrEvapDeliv);	# This must be done after VIS components are updated
  compUpdate_Returns(	$irrRnffIneff, $irrFloodRice, $rainFloodRice, $GWt_infiltRatio, $Aqf_infiltRatio,
			$irrPercIneff, $irrPercRice,  $rainPercRice,  $irrPercDeliv,    $returnRff);
  compUpdate_SoilIrr(	$irrigationNet,$ricePaddyWater);

		### Update runoff and groundwater storages
  $surfRffStorage[1]	+= $subDf *  $returnRff;
  $irrRffStorage[1]	+= $subDf * ($irrRnffIneff + $irrFloodRice + $rainFloodRice);
  $grdWater[1]		+= $subDf *  $GWt_infiltIrr;
  $ricePaddyStrg	+= $subDf * ($irrFloodRice + $rainFloodRice);	### Add water to rice patty storage

	###############################################################
	#####	Processes-based inefficient irrigation water-
	#####
	#####	First,  inefficent irrigation water is  added to  irr runoff retention pool/storage (just 3 lines above)
	#####	Second, evaporation and percolation are done from irr runoff retention pool/storage
	#####
	#####	Repeat: Tracking water components in Ground water
	###############################################################

  if ($irrTech{Application}{Process}) {

    bMarkMap(2, $bMarkM, 'Irrigation runoff storage pool processing');

	### Process-based calculations of inefficient water evaporation and percolation from runoff storage pool
    my $irrRffStrg	 = ($irrRffStorage[1] - $ricePaddyStrg)->lclip(0);	# Rice paddy water is locked for this

    my $irrEvapRff	 = ($irrArea * $irrTech{Application}{DU} * $pet         )->hclip($irrRffStrg);
    $irrRffStorage[1]	-=  $irrEvapRff   * $subDf;	# Subtract evaporation from runoff storage pool
    $irrPercIneff	 = ($irrArea * $irrTech{Application}{DU} * $ricePercRate)->hclip($irrRffStrg-$irrEvapRff);
    $irrRffStorage[1]	-=  $irrPercIneff * $subDf;	# Subtract percolation from runoff storage pool
    my $GWt_infltIneff	 =  $irrPercIneff * $GWt_infiltRatio;				# mm/day
    my $Aqf_infltIneff	 =  $irrPercIneff * $Aqf_infiltRatio * $mm_2_m3 if $aqfType;	# m3/day

		### Update aquifer storages
    foreach my $aqf_ID (@aqf_IDs) {}		# Removed from public domain

		### Update groundwater and aquifer tracking components
    bMarkMap(3, $bMarkM, 'Tracking of inefficent returns');
    compUpdate_GrWaterIrr($GWt_infltIneff,$Aqf_infltIneff);

		### Update groundwater and other storages
    $grdWater[1]	+= $GWt_infltIneff * $subDf;
    $GWt_infiltIrr	+= $GWt_infltIneff;
    $Aqf_infiltIrr	+= $Aqf_infltIneff		if $aqfType;
    $Aqf_dQ		+= $Aqf_infltIneff * $subDf	if $aqfType == 3;	# Add to ModFlow aquifers
    $evapotrans		+= $irrEvapRff;
		### Combine application evap. (sprinkle spray) with evaporation from irr. runoff storage pool
    $irrEvapIneff	+= $irrEvapRff;
    compBalance_irrEvapRff($irrEvapRff);
  }

	###############################################################
	###	Update changes in groundwater and other storages or fluxes

    $grdWaterChg .= $grdWater[1]	- $grdWater[0];
  my $smResStChg  = $smResStrg[1]	- $smResStrg[0];
  my $irrVISChg   = $irrVIS[1]		- $irrVIS[0];
  my $sRffStChg   = $surfRffStorage[1]	- $surfRffStorage[0] + $irrRffStorage[1] - $irrRffStorage[0];
  my $Aqf_infilt  = $Aqf_infiltPrecip	+ $Aqf_infiltIrr;		# m3/day

		### Update lumped aquifer storage with sinking river flows
  if ($snk_flag) {}		# Removed from public domain

	###############################################################
	###	ModFlow fluid Dynamics (presently EFDM solver only)

  if ($aqfType == 3) {}		# Removed from public domain

	###############################################################

  if ($aqfType) {}		# Removed from public domain

	###############################################################
	###   Calculate critical WBM balances
	###############################################################

  bMarkMap(2, $bMarkM, 'Water Balances and Output');
  bMarkMap(3, $bMarkM, 'Balances');

  my $nRoffCells	= $runoff->ngood;
  die "Mismatch of good cell numbers in Network and Runoff data ($nCells != $nRoffCells). Aborting...\n\n" if $nCells != $nRoffCells;

		### Irrigation Global Balances
  my $useFlowT		= ($useFlow		* $MM_2_km3)->sum;
  my $useExtraT		= ($useExtra		* $MM_2_km3)->sum;
  my $useAqfT		= ($useAqf		* $MM_2_km3)->sum;
  my $irrigationNetT	= ($irrigationNet	* $mm_2_km3)->sum;
  my @irrNetT		= map(($irrigation{$_}	* $mm_2_km3)->sum, @$irrCropList);
  my $irrPaddyT		= ($ricePaddyWater	* $mm_2_km3)->sum;
  my $irrigationFlowT	= ($irrigationFlow	* $MM_2_km3)->sum;
  my $irrExtraT		= ($irrUseExtra		* $MM_2_km3)->sum;
  my $irrUseAqfT	= ($irrUseAqf		* $MM_2_km3)->sum;
  my $etIrrCropsT	= ($etIrrCrops		* $MM_2_km3)->sum;
     $irrGrossT_y      += $irrigationGrossT;
     $irrNetT_y        += $irrigationNetT;
     $irrXW_y[0]       +=($irrUseSIR		* $MM_2_km3)->sum + $irrigationFlowT;
     $irrXW_y[1]       +=($irrUseGrwt		* $MM_2_km3)->sum;
     $irrXW_y[2]       += $irrUseAqfT;
     $irrXW_y[3]       += $irrExtraT;
     $irrPaddyT_y      += $irrPaddyT;
     $irrIneffRnff_y   +=($irrRunoff		* $MM_2_km3)->sum;
     $irrIneffPerc_y   +=($irrPercIneff		* $MM_2_km3)->sum;
     $irrIneffEvap_y   +=($irrEvapIneff		* $MM_2_km3)->sum;
     $irrPercDeliv_y   +=($irrPercDeliv		* $MM_2_km3)->sum;
     $irrEvapDeliv_y   +=($irrEvapDeliv		* $MM_2_km3)->sum;
 map $irrNetT_y[$_]    += $irrNetT[$_],		  0..$#irrNetT;
     $etNonCropsT_y    +=($etNonCrops		* $MM_2_km3)->sum;
     $etIrrCropsT_y    += $etIrrCropsT;
     $etRfdCropsT_y    +=($etRfdCrops		* $MM_2_km3)->sum;
  my $aqfWaterUseT	= $useAqfT + $irrUseAqfT;
		### Potential Irr demand
  my $irrDemVsT		= $irrigationNetT	- $irrPaddyT;
  if ($vs_flag) {
    my @irrDemVsT	= map(($irr_demVs{$_}	* $mm_2_km3)->sum, @$irrCropList);
       $irrDemVsT	= ($irr_demVs_total	* $mm_2_km3)->sum;
       $irrDemVsT_y    += $irrDemVsT;
   map $irrDemVsT_y[$_]+= $irrDemVsT[$_],	  0..$#irrDemVsT;
  } else { $irrDemVsT_y	= $irrNetT_y-$irrPaddyT_y;@irrDemVsT_y	=  @irrNetT_y; }

		### Land Surface/Pixel Global Balances
  my $sPackChgT		= ($sPackChg	* $MM_2_KM3)->sum;		# Per active area
  my $interceptChgT	= ($interceptChg* $mm_2_km3)->sum;
  my $sMoistChgT	= ($sMoistChg	* $mm_2_km3)->sum;
  my $evapotransT	= ($evapotrans	* $MM_2_km3)->sum;
  my $grdWaterChgT	= ($grdWaterChg	* $mm_2_km3)->sum;
  my $Aqf_infiltT	= ($Aqf_infilt  *      1e-9)->sum;
  my $runoffT		= ($runoffVol	* $dt* 1e-9)->sum;
  my $sRffStChgT	= ($sRffStChg	* $mm_2_km3)->sum;
  my $smResStChgT	= ($smResStChg  * $mm_2_km3)->sum;
  my $irrVISChgT	= ($irrVISChg   * $mm_2_km3)->sum;
  my $glMeltT		= ($glMelt	* $dt* 1e-9)->sum;
  my $precipT		= ($precip	* $MM_2_KM3)->sum;		# Per active area
  my $netwkEvapT	= ($netwkEvap	*      1e-9)->sum * $subDf;
  my $conntEvapT	=  $$connectivity[0]->dims ? $$connectivity[1]->(6,)->sum*1e-9 : 0;	# conn(6,0), m3
     $runoffT_y        += $runoffT;
     $RunoffT_y        += $runoffT - $glMeltT;
     $precipT_y        += $precipT;
  my $pixelBalance	= $aqfWaterUseT +
	($precipT  + $glMeltT    + $irrigationFlowT+ $useFlowT    + $irrExtraT   + $useExtraT) -
	($sPackChgT+ $sMoistChgT + $interceptChgT  + $evapotransT + $grdWaterChgT+ $Aqf_infiltT+
	 $runoffT  + $smResStChgT+ $irrVISChgT     + $sRffStChgT);

  if (abs($pixelBalance) > 1e-6) {
    printf "\n Pixel balance mismatch: %.4f = %.4f +
	 ( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f ) -
	 ( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f +
	   %.4f + %.4f + %.4f + %.4f )
	Pixel balance = aqfWaterUse +
	( precip   + glMelt     + irrigationFlow + useFlow    + irrExtra    + useExtra ) -
	( sPackChg + sMoistChg  + interceptChg   + evapotrans + grdWaterChg + Aqf_infilt +
	  runoff   + smResStChg + irrVISChg      + sRffStChg )\n", $pixelBalance, $aqfWaterUseT,
	$precipT, $glMeltT, $irrigationFlowT, $useFlowT, $irrExtraT, $useExtraT, $sPackChgT, $sMoistChgT,
	$interceptChgT, $evapotransT, $grdWaterChgT, $Aqf_infiltT, $runoffT, $smResStChgT, $irrVISChgT, $sRffStChgT;
  }
		### Flow/Routing Global Balances
  my $flowOutT		= ($flow_out->index($$outletIdx{all})*$dt*1e-9)	->sum;
  my $dischUsedT	= ($dischUsed	   *$dt	*1e-9)			->sum;
  my $flowUseDeltaT	= ($flowUseDelta   *$dt	*1e-9)			->sum;
  my $resStoragePrevT	= ($resStoragePrev	*1e-9)			->sum;
  my $resStorageT	= ($resStorage[1]	*1e-9)			->sum;
  my $resStorChgT	=  $resStorageT   - $resStoragePrevT;
  my $smResStrgT	= ($smResStrg[1]  * $mm_2_km3)			->sum;
  my $snk_inflT		= ($aquiferSnk	  * $dt	*1e-9)			->sum;
  my $usgsDeltaT	= ($usgsDelta	  * $dt	*1e-9)			->sum;
     $dischT_y         +=  $flowOutT;
     $spr_dschT_y      +=  $spr_dischargeT;
     $snk_inflT_y      +=  $snk_inflT;
     $usgsDeltaT_y     +=  $usgsDeltaT;
  my $flowBalance	= ($runoffT + $spr_dischargeT + $riv_exchangeT + $flowUseDeltaT + $usgsDeltaT) - ($flowOutT + $resStorChgT + $irrigationFlowT + $useFlowT + $netwkEvapT + $conntEvapT + $snk_inflT);

  if (abs($flowBalance) > 1e-6) {
    printf "\n Flow balance mismatch: %.4f = ( %.4f + %.4f + %.4f + %.4f + %.4f ) -
	  ( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f )
	Flow balance = ( runoff + spr_discharge  + riv_exchange + flowUseDeltaT + usgsDelta ) -
	( flowOut + resStorChg  + irrigationFlow + useFlow      + netwkEvap     + conntEvap + snk_infl )\n",
	$flowBalance, $runoffT, $spr_dischargeT, $riv_exchangeT, $flowUseDeltaT, $usgsDeltaT,
	$flowOutT, $resStorChgT, $irrigationFlowT, $useFlowT, $netwkEvapT, $conntEvapT, $snk_inflT;
  }
		### DIN Global Balances
  if ($runIO{DIN}) {
	 $DIN_Used	+=  $cStreamDIN    * $dischUsed * $dt;	# Add DIN removed for Dom/Ind/Lsk/Irr from streamflow
      my $usgs_idx	 =  $cell_table(3:4, $usgsStack[1]->(:-2)) if $usgs_data;	# 2D indices of sink locations

		# Total Network DIN Balances
      my $DINresStrg	 =  $cStreamDIN    * $resStorage[1];	# DIN mass in streamflow storages
      my $DINLoadT	 = ($DIN_Load      * $cell_area)	->sum;				# kg/d
      my $DINlandLoadT	 = ($DIN_Land_Load * $cell_area)	->sum;				# kg/d
      my $DINWWTPLoadT	 =  $DIN_WWTP_Load			->sum;				# kg/d
      my $DINLoadBalance =  $DINLoadT      - $DINlandLoadT - $DINWWTPLoadT;
      if (abs($DINLoadBalance) > 1e-6) {
	  printf "DIN Load balance (kg/d): %0.3f = load %0.2f - wwtp %0.2f - land %0.2f\n",
	  	$DINLoadT,  $DINWWTPLoadT, $DINlandLoadT, $DINLoadBalance; }
      my $DINflowOutT	 =(($cStreamDIN->indexND($$outletIdx{ALL}) * $flow_out->index($$outletIdx{all}))->sum + # kg/dt
	($snk_flag?	   ($cStreamDIN->indexND($snk_idx)*$aquiferSnk(:-2))->sum : 0)) * $dt;	# Add DIN gone to sinks
      my $DINresChgT	 = ($DINresStrg  - $DIN_Prev)		->sum;				# kg/dt
      my $DIN_UsedT	 =  $DIN_Used				->sum;				# kg/dt
      my $DIN_usgsT	 =  $usgs_data ? ($cStreamDIN->indexND($usgs_idx)*$usgsDelta(:-2))->sum * $dt : 0; # kg/td
      my $DINdenitT	 =  $DIN_Denit				->sum * $subDf;			# kg/dt
      my $DINflowBalance =  $DINLoadT + $DIN_usgsT - $DINflowOutT - $DINresChgT - $DIN_UsedT - $DINdenitT;
      if (abs($DINflowBalance) > 1e-3) {	# 1 gram precision check
	printf "\tDIN Flow balance (kg/dt): " .
		"%0.4f = Load %0.4f + USGS %0.4f - Q out %0.4f - rivChg %0.4f - Used %0.4f - Denit %0.4f\n",
		$DINflowBalance, $DINLoadT, $DIN_usgsT, $DINflowOutT, $DINresChgT, $DIN_UsedT, $DINdenitT; }
  }
		### Total Global Balances
  my $totalBalance	= $aqfWaterUseT +
	($precipT    + $glMeltT    + $irrExtraT    + $useExtraT   + $flowUseDeltaT + $spr_dischargeT + $riv_exchangeT) -
	($sPackChgT  + $sMoistChgT + $interceptChgT+ $grdWaterChgT+ $Aqf_infiltT   + $sRffStChgT     +
	 $smResStChgT+ $irrVISChgT)-($flowOutT     + $resStorChgT + $evapotransT   + $netwkEvapT     + $conntEvapT +
	 $snk_inflT  - $usgsDeltaT);

  if (abs($totalBalance) > 1e-5) {
    printf "\n Total water balance mismatch: %.4f = %.4f +
	( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f ) -
	( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f ) -
	( %.4f + %.4f + %.4f + %.4f + %.4f + %.4f - %.4f )
	Total_Balance = AqfWaterUse +
	( precip + glmelt + irrExtra + useExtra + irrExtraFlow + spr_discharge + riv_exchangeT ) -
	( sPackChg + sMoistChg + intercptChg + grdWaterChg + Aqf_infilt + sRffStChg + smResStChg + irrVISChg ) -
	( flowOut + resStorChg + evapotrans + netwkEvap + conntEvap + snk_infl - usgsDelta)\n",
	$totalBalance, $aqfWaterUseT,
	$precipT,$glMeltT,$irrExtraT,$useExtraT,$flowUseDeltaT,$spr_dischargeT,$riv_exchangeT,
	$sPackChgT,$sMoistChgT,$interceptChgT,$grdWaterChgT,$Aqf_infiltT,$sRffStChgT,$smResStChgT,$irrVISChgT,
	$flowOutT, $resStorChgT,$evapotransT, $netwkEvapT,  $conntEvapT, $snk_inflT, $usgsDeltaT;
  }
  $irrExtraT	+= $flowUseDeltaT;		# This line must be after the line above ($totalBalance = ...)
  $n_spool	+= $$extent{spool_writes};	# Number of spool files added in this cycle/loop

		### Total Global Component Tracking Balances
  if ($compBalFlag) {
    compBalance_Final($flowUseDelta, $flow_out);
    my $checkDelta  = 1e-5;
#    printf "\t(P) = %e\n", $pixelBalance;		# Debugging

    if ($compSwitch[2]) {
      my @comp	= qw(Snow Glacial Rain Relict/Extra);
      my %bal	= %{$compBal{Prm}};
      for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
	my $prmBalance = (	# Change in storage
	 $bal{Soil}[$i]    -  $bal{Soil_prev}[$i]    +
	 $bal{VIS}[$i]     -  $bal{VIS_prev}[$i]     + $bal{RffStg}[$i]  - $bal{RffStg_prev}[$i]   +
	 $bal{SIR}[$i]     -  $bal{SIR_prev}[$i]     + $bal{Stream}[$i]  - $bal{Stream_prev}[$i]   +
	 $bal{GrWater}[$i] -  $bal{GrWater_prev}[$i] + $bal{AqWater}[$i] - $bal{AqWater_prev}[$i]) -
				# Change by fluxes
	($bal{WaterIn}[$i] - ($bal{EvapSoil}[$i]     + $bal{EvapDIL}[$i] + $bal{EvapIrr}[$i] +
	 $bal{EvapTech}[$i]+  $bal{EvapRiv}[$i]      + $bal{Out}[$i]     - $bal{StrDlt}[$i]));
#	printf "\t($i) = %e\n", $prmBalance;		# Debugging

	if (abs($prmBalance) > $checkDelta && !$skipCompBal) {
	printf "\n Primary source tracking balance mismatch ($comp[$i] component): %.4f =
	(%.4f - %.4f + %.4f - %.4f + %.4f - %.4f + %.4f - %.4f +
	 %.4f - %.4f + %.4f - %.4f + %.4f - %.4f)-
	(%.4f -(%.4f + %.4f + %.4f + %.4f + %.4f + %.4f - %.4f))
	Total_Balance =
	(Soil    - Soil      + VIS     - VIS     + RffStg   - RffStg  + SIR - SIR +
	 Stream  - Stream    + GrWater - GrWater + AqWater  - AqWater) -
	(WaterIn - (EvapSoil + EvapDIL + EvapIrr + EvapTech + EvapRiv + Out - Dlt)\n", $prmBalance,
	 $bal{Soil}[$i],$bal{Soil_prev}[$i],$bal{VIS}[$i], $bal{VIS_prev}[$i], $bal{RffStg}[$i],
	 $bal{RffStg_prev}[$i],$bal{SIR}[$i], $bal{SIR_prev}[$i], $bal{Stream}[$i], $bal{Stream_prev}[$i],
	 $bal{GrWater}[$i], $bal{GrWater_prev}[$i], $bal{AqWater}[$i], $bal{AqWater_prev}[$i],
	 $bal{WaterIn}[$i], $bal{EvapSoil}[$i], $bal{EvapDIL}[$i], $bal{EvapIrr}[$i],
	 $bal{EvapTech}[$i], $bal{EvapRiv}[$i], $bal{Out}[$i], $bal{StrDlt}[$i];
    } } }
    if ($compSwitch[3]) {
      my @comp	= (qw(Relict/Extra Pristine Dom/Ind/Lsk), map("Irrigation_$_",1..$nIrr));
      my %bal	= %{$compBal{Irr}};
      for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
	my $bal_Net    = $i > 2 ? 0 : $bal{Net}[$i];	# Irrigation Net is part of the soil storage
	my $irrBalance = (	# Change in storage
	 $bal{Soil}[$i]    -  $bal{Soil_prev}[$i]    +
	 $bal{VIS}[$i]     -  $bal{VIS_prev}[$i]     + $bal{RffStg}[$i]  - $bal{RffStg_prev}[$i]   +
	 $bal{SIR}[$i]     -  $bal{SIR_prev}[$i]     + $bal{Stream}[$i]  - $bal{Stream_prev}[$i]   +
	 $bal{GrWater}[$i] -  $bal{GrWater_prev}[$i] + $bal{AqWater}[$i] - $bal{AqWater_prev}[$i]) -
				# Change by fluxes
	($bal{Gross}[$i]   - ($bal{EvapSoil}[$i] + $bal_Net           +
			      $bal{UsedDIL}[$i]  + $bal{UsedIrr}[$i]  +
			      $bal{EvapIrr}[$i]  + $bal{EvapTech}[$i] + $bal{EvapRiv}[$i] +
			      $bal{Out}[$i]      - $bal{StrDlt}[$i]));

	if (abs($irrBalance) > $checkDelta && !$skipCompBal) {
	printf "\n Irrigation tracking balance mismatch ($comp[$i] component): %.4f =
	(%.4f - %.4f + %.4f - %.4f + %.4f - %.4f + %.4f - %.4f +
	 %.4f - %.4f + %.4f - %.4f + %.4f - %.4f)-
	(%.4f -(%.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f - %.4f))
	Total_Balance =
	(Soil   - Soil    + VIS     - VIS     + RffStg   - RffStg   + SIR      - SIR     +
	 Stream - Stream  + GrWater - GrWater + AqWater  - AqWater) -
	(Gross  -(EvapSoil+ Net     + UsedDIL  + UsedIrr + EvapIrr  + EvapTech + EvapRiv + Out - Dlt)\n",
	 $irrBalance,
	 $bal{Soil}[$i],$bal{Soil_prev}[$i],$bal{VIS}[$i], $bal{VIS_prev}[$i], $bal{RffStg}[$i], $bal{RffStg_prev}[$i],
	 $bal{SIR}[$i], $bal{SIR_prev}[$i], $bal{Stream}[$i], $bal{Stream_prev}[$i],
	 $bal{GrWater}[$i], $bal{GrWater_prev}[$i], $bal{AqWater}[$i], $bal{AqWater_prev}[$i],
	 $bal{Gross}[$i], $bal{EvapSoil}[$i],$bal_Net, $bal{UsedDIL}[$i], $bal{UsedIrr}[$i], $bal{EvapIrr}[$i],
	 $bal{EvapTech}[$i], $bal{EvapRiv}[$i], $bal{Out}[$i], $bal{StrDlt}[$i];
    } } }
  }
	###############################################################
	###	Data Output Section: Save WBM results	###############
	###############################################################

  bMarkMap(3, $bMarkM, 'Output');

  my $band	= 0;
  my $yrCycle	= $n_SP ? int(1+($n_count-1)/$n_SP) : 1;
  my $path	= $n_count > $n_spinup && $noOutput ? 'No output files'      :
		  $doOutput ?	sprintf("File wbm_%04d-%02d.nc", @date[0,1]) :	# Monthly output run
				sprintf("Spinup %d/%d",$yrCycle,$runIO{Spinup}{Loops});

  if ($doOutput) {

		### Reference cell area for output variables
    my $soilRA	= { reference_area => 'soil portion of grid cell area' };
    my $actvRA	= { reference_area => 'active portion of grid cell area' };
    my $fullRA	= { reference_area => 'full grid cell area' };

    my $UnsStr	= 'Unsustainable';			# String alias
    my $sA	= $soil_area;				# Soil   area alias to make name shorter
    my $sRatio	= $soil_area/ $CELL_AREA;		# Soil   area ratio fraction in the cell
    my $aRatio	= $cell_area/ $CELL_AREA;		# Actuve area ratio fraction in the cell
    my $mm_km2	= m/_km2/  ~~ @list_out ?		# Convert irrigation mm to m3/km2 of irrigated land
					$mm_2_m3  /($CELL_AREA * $irrArea) : $zeroes;
    my($cStream, $cStreamP, $cStreamIrr_0, $cStreamIrr, $cStreamMsk,	# Tracking aliases for output (convenient)
	  $cGrossIrrP)  = ( $cStream[1],   $cStreamP[1],$cStreamIrr[0], $cStreamIrr[1], $cStreamMsk[1], $cVIS_P);
    my %endorheic;					# Compose endorheic lake output variables
    if (grep(m/^endo|total_mass/, @list_out)) {
       $endorheic{Strg}	= $zeroes->copy;	$endorheic{Strg}->indexND($$outletIdx{ENDORHEIC}) .= $endoStrg;
       $endorheic{Area}	= $zeroes->copy;	$endorheic{Area}->indexND($$outletIdx{ENDORHEIC}) .= $endoArea*1e-6;
       $endorheic{Evap}	= $zeroes->copy;	$endorheic{Evap}->indexND($$outletIdx{ENDORHEIC}) .= $endoEvap;
    }
		### Re-scaling output variables to full or partial cell area
    $surfRunoff		*= $sRatio			if 'surfRunoff'		~~ @list_out;	# Equal
    $baseFlow		*= $sRatio			if m/^baseflow/		~~ @list_out;	# Match
    $GWt_infiltPrecip	*= $sRatio			if 'GWt_infiltPrecip'	~~ @list_out;	# Equal
       $grdWaterChg	*= $sRatio			if 'grdWaterChg'	~~ @list_out;	# Equal
    my $UGW_out		 = $sRatio*$UGW			if 'UGW'		~~ @list_out;	# Equal
    my $grdWater	 = $sRatio*$grdWater[1];

		### Calculated output variables (if required by the output variable list)
    my $disch_out	='discharge_out'~~@list_out? table_to_grid( $flow_out, $cell_table, $zeroes )	   :$zeroes;
    my $domFlowOrig	= 'domFlowOrig' ~~@list_out? $domUseFlowLoc+$domUseFlowRmt->rmtOrig($domUseFlowRID):$zeroes;
    my $indFlowOrig	= 'indFlowOrig' ~~@list_out? $indUseFlowLoc+$indUseFlowRmt->rmtOrig($indUseFlowRID):$zeroes;
    my $stkFlowOrig	= 'stkFlowOrig' ~~@list_out? $stkUseFlowLoc+$stkUseFlowRmt->rmtOrig($stkUseFlowRID):$zeroes;
    my $irrFlowOrig	= 'irrFlowOrig' ~~@list_out? $irrUseFlowLoc+$irrUseFlowRmt->rmtOrig($irrUseFlowRID):$zeroes;
    my $domUseSGW	= 'domUseSGW'   ~~@list_out? $domUseGrWt   -$domUseExtra			   :$zeroes;
    my $indUseSGW	= 'indUseSGW'   ~~@list_out? $indUseGrWt   -$indUseExtra			   :$zeroes;
    my $stkUseSGW	= 'stkUseSGW'   ~~@list_out? $stkUseGrWt   -$stkUseExtra			   :$zeroes;
    my $irrRffStorage	='irrRffStorage'~~@list_out? $irrRffStorage[1] - $ricePaddyStrg			   :$zeroes;

    my $cRunoffP_mm	= $compSwitch[2] && m/^runoff_mm/	~~@list_out? $cRunoffP  *$runoff  *$sRatio :$zeroes_4;
    my $cBaseflowP_mm	= $compSwitch[2] && m/^baseflow_mm/	~~@list_out? $cGrWaterP *$baseFlow	   :$zeroes_4;
    my $cStreamP_m3s	= $compSwitch[2] && m/^discharge_m3s/	~~@list_out? $cStreamP  *$discharge	   :$zeroes_4;
    my $cGrWaterP_mm	= $compSwitch[2] && m/^grndWater_mm/	~~@list_out? $cGrWaterP *$grdWater	   :$zeroes_4;
    my $cGrossIrrP_mm	= $compSwitch[2] && m/^GrossIrr_mm/	~~@list_out? $cGrossIrrP*$irrigationGross  :$zeroes_4;
    my $cNetIrrP_mm	= $compSwitch[2] && m/^NetIrr_mm/	~~@list_out? $cGrossIrrP*$irrigationNet	   :$zeroes_4;
    my $cIrrUseFlowP_mm	= $compSwitch[2] && m/^IrrFlow_mm/	~~@list_out?
			$cStreamP *$irrUseFlowLoc + $irrUseFlowRmt *$cStreamP->indexND($irrUseFlowRID)	   :$zeroes_4;
    my $cIrrUseGrwtP_mm	= $compSwitch[2] && m/^IrrGrwt_mm/	~~@list_out? $cGrWaterP *$irrUseGrwt	   :$zeroes_4;
    my $cIrrEvap_mm	= $compSwitch[2] && m/^IrrEvap_mm/	~~@list_out? $cGrossIrrP*$irrEvapIneff	   :$zeroes_4;
    my $cIrrPercRice_mm	= $compSwitch[2] && m/^IrrPercRice_mm/	~~@list_out? $cGrossIrrP*$irrPercRice	   :$zeroes_4;
    my $cIrrPercIneff_mm= $compSwitch[2] && m/^IrrPercIneff_mm/	~~@list_out? $cGrossIrrP*$irrPercIneff	   :$zeroes_4;
    my $etIrrCrops_mm   = $compSwitch[2] && m/^etIrrCrops_mm/   ~~@list_out? $cSoilP    *$etIrrCrops       :$zeroes_4;
		### Green/Blue water output aggregations for output variables:
    my($sMoistGr, $sMoistBl, $GW_ET, $BW_ET) =			# sMoistGr, sMoistBl, GW_ET, BW_ET
	make_GW_BW_output(\%sMoistGr, \%sMoistGrET, \%sMoist, \%sMoistET, \%landFrac, \@list_out);
		### Cell total water mass storage. Note- $ricePaddyStrg is part of $surfRffStorage[1]
    my $total_mass	=                   'total_mass'	~~@list_out?($irrRffStorage[1] +
	$UGW + $surfRffStorage[1] +  $sMoist[1] + $grdWater[1]  + $smResStrg[1])*$sRatio +
	$snowPack*$actvRA + $Aqf_Storage  + ($resStorage[1] + $glVolume + $endorheic{Strg})   *	$M3_2_MM   :$zeroes;
		### Convert units to mm/day
       $Aqf_infiltPrecip= $aqfType	 && 'Aqf_infiltPrecip'	~~@list_out? $Aqf_infiltPrecip*	$M3_2_MM   :$zeroes;
       $Aqf_infiltIrr	= $aqfType	 && 'Aqf_infiltIrr'	~~@list_out? $Aqf_infiltIrr   *	$M3_2_MM   :$zeroes;
    my $Aqf_Storage_T	= $aqfType == 3	 && 'AQF_Storage_T'	~~@list_out?$$AQF_DATA{Strg_T}*	$M3_2_MM   :$zeroes;
    my $Aqf_Storage_B	= $aqfType == 3	 && 'AQF_Storage_B'	~~@list_out?$$AQF_DATA{Strg_B}*	$M3_2_MM   :$zeroes;
		### Make some ModFlow output variables as needed
				# $Aqf_head_T- False/Dry top layer head elevation in case of dry top cells
    my $Aqf_head_T	= $aqfType == 3	? condition($$AQF_DATA{DC_T}, $$AQF_DATA{h_B}, $$AQF_DATA{h_T})	   :$zeroes;
    my $Aqf_H_Delta	= $aqfType == 3	 && 'AQF_H_Delta'	~~@list_out?
	condition($$AQF_DATA{mask_T}, $$AQF_DATA{h_T}, $$AQF_DATA{h_B})  -  $$AQF_DATA{Net_DEM}		   :$zeroes;
    my($Aqf_DH_Depth_T, $Aqf_H_Depth_T, $Aqf_H_Depth_B, $Aqf_H_Depth) =
			  $aqfType == 3 && m/AQF_H_Depth/~~@list_out ? map($_ - $$AQF_DATA{DEM},
	$Aqf_head_T, $$AQF_DATA{h_T}, $$AQF_DATA{h_B}, $$AQF_DATA{h_B}->hclip($Aqf_head_T))		  :($zeroes) x 3;

			### This is the list of output variable key words
    my %data_out = (
		### Major parameters
				# Output variable "runoff" does not include "flush" and "glacial melt" waters
	'runoff'	=> [$runoff,		'Runoff',					'mm/day', $sRatio],
	'surfRunoff'	=> [$surfRunoff,	'Surface runoff',				'mm/day', $fullRA],
	'baseflow'	=> [$baseFlow,		'Baseflow',					'mm/day', $fullRA],
	'discharge_in'	=> [$disch_in,		"Inflow discharge by $routingMethod routing",	'm3/sec'],
	'discharge'	=> [$discharge,		"Discharge by $routingMethod routing",		'm3/sec'],# Inflow+rnff
	'discharge_out'	=> [$disch_out,		"Outflow discharge by $routingMethod routing",	'm3/sec'],
	'discharge_abs' => [$dischUsed,		'Streamflow abstracted for human use',		'm3/sec'],
	'yMean'		=> [$yMean,		'Long term average Stream Depth',		'm'],
	'wMean'		=> [$wMean,		'Long term average Stream Width',		'm'],
	'vMean'		=> [$vMean,		'Long term average Stream Velocity',		'm/sec'],
	'depth'		=> [$yStream,		'Instantaneous Stream/Reservoir Depth',		'm'],
	'width'		=> [$wStream,		'Instantaneous Stream/Reservoir Width',		'm'],
	'speed'		=> [$velocity,		'Instantaneous Stream/Reservoir Velocity',	'm/sec'],
	'length'	=> [$length,		'Stream/Reservoir Segment Length',		'm'],
	'precip'	=> [$precip,		'Precipitation',				'mm/day'],
	'airT'		=> [$airT,		'Temperature',					'deg C'],
	'snowPack'	=> [$snowPack,		'Snow Pack Water Equivalent',			'mm',	  $actvRA],
	'snowPackChg'	=> [$sPackChg,		'Snow Pack Water Equivalent Change',		'mm/day', $actvRA],
	'snowMelt'	=> [$snowMelt,		'Snow melt',					'mm/day', $actvRA],
	'snowFall'	=> [$snowFall,		'Snow fall',					'mm/day', $actvRA],
	'dayLength'	=> [$dayLen,		'Day Length',					'fraction'],
	'pet'		=> [$pet,		'Potential Evapotranspiration',			'mm/day', $fullRA],
	'evapotrans'	=> [$evapotrans,	'Evapotranspiration from all sources',		'mm/day', $soilRA],
	'etIrrCrops'	=> [$etIrrCrops,	'Evapotranspiration from irrigated crops',	'mm/day', $soilRA],
	'etRfdCrops'	=> [$etRfdCrops,	'Evapotranspiration from rainfed crops',	'mm/day', $soilRA],
	'etNonCrops'	=> [$etNonCrops,	'Evapotranspiration from non-crop land',	'mm/day', $soilRA],
	'openWaterEvap'	=> [$netwkEvap*$M3_2_MM,'Evaporation from rivers & reservoirs',		'mm/day', $fullRA],
	'soilMoist'	=> [$sMoist[1],		'Soil Moisture Water Equivalent',		'mm',	  $soilRA],
	'soilMoistChg'	=> [$sMoistChg,		'Soil Moisture Water Equivalent Change',	'mm/day', $soilRA],
		# Note - Soil moisture change is due to precipitation/evaporation/irrigation
		#	and it does not account for land re-distribution of crop rotations
	'soilMoistFrac'	=> [$sMoistFrac,	'Soil Moisture Water Equivalent Fraction',	'fraction'], # scale?
	'sMoistGr'	=> [$sMoistGr,		'Green Water soil moisture equivalent',		'mm',	  $soilRA],
	'sMoistBl'	=> [$sMoistBl,		'Blue  Water soil moisture equivalent',		'mm',	  $soilRA],
	'GW_ET'		=> [$GW_ET,		'Green Water soil evapotranspiration',		'mm/day', $soilRA],
	'BW_ET'		=> [$BW_ET,		'Blue  Water soil evapotranspiration',		'mm/day', $soilRA],
	'grdWater'	=> [$grdWater,		'Groundwater',					'mm',	  $fullRA],
	'grdWaterChg'	=> [$grdWaterChg,	'Groundwater Change',				'mm/day', $fullRA],
	'UGW'		=> [$UGW_out,		"Accumulated $UnsStr groundwater use",		'mm',	  $fullRA],
      'GWt_infiltPrecip'=> [$GWt_infiltPrecip,	'Infiltration to Groundwater from Precip',	'mm/day', $fullRA],
      'Aqf_infiltPrecip'=> [$Aqf_infiltPrecip,	'Infiltration to Aquifer from Precip',		'mm/day', $fullRA],
	'GWt_infiltIrr'	=> [$GWt_infiltIrr,	'Infiltration to Groundwater from Irrigation',	'mm/day', $fullRA],
	'Aqf_infiltIrr'	=> [$Aqf_infiltIrr,	'Infiltration to Aquifer from Irrigation',	'mm/day', $fullRA],
	'Aqf_Storage'	=> [$Aqf_Storage,	'Groundwater Aquifer Storage',			'mm',	  $fullRA],
	'resStorage'	=> [$resStorage[1],	'Stream and Reservoir Storage',		'm3/pixel'],
	'surfRffStorage'=> [$surfRffStorage[1],	'Storm Runoff Retention Pool Storage',	'mm',		  $soilRA],
	'irrRffStorage'	=> [$irrRffStorage,	'Irrig Runoff Retention Pool Storage',	'mm',		  $soilRA],
	'total_mass'	=> [$total_mass,	'Total water mass',			'mm',		  $fullRA],
	'endoStrg'	=> [$endorheic{Strg},	'Endorheic lakes water storage',	'm3/pixel'],
	'endoArea'	=> [$endorheic{Area},	'Endorheic lakes Area',			'km2/pixel'],
	'endoEvap'	=> [$endorheic{Evap},	'Endorheic lakes water evaporation',	'm3/pixel/day'],
		### Water demand parameters
		$runIO{WaterDemand} ? (			# Original "soil" reference area is forced to "full"
	'domesticDemand'=> [$domesticDemand,	'Domestic Water Demand',		'mm/day',	$sRatio],
	'domUseGross'	=> [$domUseGross,	'Domestic Water Gross withdrawal',	'mm/day',	$sRatio],
	'domUseEvap'	=> [$domUseEvap,	'Domestic Evaporation component',	'mm/day',	$sRatio],
	'domUseSfWt'	=> [$domUseSfWt,	'Domestic Surface water withdrawal',	'mm/day',	$sRatio],
	'domUseGrWt'	=> [$domUseGrWt,	'Domestic All Groundwater withdrawal',	'mm/day',	$sRatio],
	'domUseSGW'	=> [$domUseSGW,		'Domestic Shallow Groundwater withdrawal','mm/day',	$sRatio],
        'domUseAqf'	=> [$domUseAqf,		'Domestic Aquifer water withdrawal',    'mm/day',	$sRatio],
	'domUseLoc'	=> [$domUseFlowLoc,	'Domestic local surface withdrawal',	'mm/day',	$sRatio],
	'domUseRmt'	=> [$domUseFlowRmt,	'Domestic remote surface withdrawal',	'mm/day',	$sRatio],
	'domFlowOrig'	=> [$domFlowOrig,	  'Origin of remote domestic water',	'mm/day',	$sRatio],
	'domUseProvX'   => [$domUseFlowRID((0),,),'Origin of remote domestic water, X',	'col #'],
	'domUseProvY'   => [$domUseFlowRID((1),,),'Origin of remote domestic water, Y',	'row #'],
	'industryDemand'=> [$industryDemand,	'Industrial Demand',			'mm/day',	$sRatio],
	'indUseGross'	=> [$indUseGross,	'Industrial Water Gross withdrawal',	'mm/day',	$sRatio],
	'indUseEvap'	=> [$indUseEvap,	'Industrial Evaporation component',	'mm/day',	$sRatio],
	'indUseSfWt'	=> [$indUseSfWt,	'Industrial Surface water withdrawal',	'mm/day',	$sRatio],
	'indUseGrWt'	=> [$indUseGrWt,	'Industrial All Groundwater withdrawal','mm/day',	$sRatio],
        'indUseSGW'	=> [$indUseSGW,		'Industrial Shallow Groundwater withdrawal','mm/day',	$sRatio],
	'indUseAqf'	=> [$indUseAqf,		'Inudstrial Aquifer water withdraWal',  'mm/day',	$sRatio],
	'indUseLoc'	=> [$indUseFlowLoc,	'Industrial local surface withdrawal',	'mm/day',	$sRatio],
	'indUseRmt'	=> [$indUseFlowRmt,	'Industrial remote surface withdrawal',	'mm/day',	$sRatio],
	'indFlowOrig'	=> [$indFlowOrig,	  'Origin of remote industrial water',	'mm/day',	$sRatio],
	'indUseProvX'   => [$indUseFlowRID((0),,),'Origin of remote industrial water, X','col #'],
	'indUseProvY'   => [$indUseFlowRID((1),,),'Origin of remote industrial water, Y','row #'],
	'livestockDemand'=>[$livestockDemand,	'Livestock Demand',			'mm/day',	$sRatio],
	'stkUseGross'	=> [$stkUseGross,	'Livestock Water Gross withdrawal',	'mm/day',	$sRatio],
	'stkUseEvap'	=> [$stkUseEvap,	'Livestock Evaporation component',	'mm/day',	$sRatio],
	'stkUseSfWt'	=> [$stkUseSfWt,	'Livestock Surface water withdrawal',	'mm/day',	$sRatio],
	'stkUseGrWt'	=> [$stkUseGrWt,	'Livestock All Groundwater withdrawal',	'mm/day',	$sRatio],
	'stkUseSGW'	=> [$stkUseSGW,		'Livestock Shallow Groundwater withdrawal','mm/day',	$sRatio],
        'stkUseAqf'	=> [$stkUseAqf,		'Livestock Aquifer water withdrawal',   'mm/day',	$sRatio],
	'stkUseLoc'	=> [$stkUseFlowLoc,	'Livestock local surface withdrawal',	'mm/day',	$sRatio],
	'stkUseRmt'	=> [$stkUseFlowRmt,	'Livestock remote surface withdrawal',	'mm/day',	$sRatio],
	'stkFlowOrig'	=> [$stkFlowOrig,	  'Origin of remote livestock water',	'mm/day',	$sRatio],
	'stkUseProvX'   => [$stkUseFlowRID((0),,),'Origin of remote livestock water, X','col #'],
	'stkUseProvY'   => [$stkUseFlowRID((1),,),'Origin of remote livestock water, Y','row #']
	) : (),
		### Irrigation parameters
		$runIO{Irrigation} ? (
	'irrArea'	=> [$irrArea	 *$CELL_AREA,	'Irrigated cropland area',		'km2/pixel'],
	'nonIrrArea'	=> [$nonIrrArea	 *$CELL_AREA,	'Rainfed and Fallow cropland area',	'km2/pixel'],
	'cropArea'	=> [$cropAreaFrac*$CELL_AREA,	'Cropland area',			'km2/pixel'],
	'cropAreaFrac'	=> [$cropAreaFrac,		'Cropland area fraction',		'fraction'],
	'irrDemandTotal'=> [$irr_demand_total,		'Gross Irr demand total',		'mm/day',  $soilRA],
	'irrDemand_km2'	=> [$irr_demand_total*$mm_km2,	'Gross Irr demand per irrigated km2',	'm3/km2/day'],
	'irrigationGross'=>[$irrigationGross,		'Gross Irrigation',			'mm/day',  $soilRA],
	'irrGross_km2'	=> [$irrigationGross *$mm_km2,	'Gross Irrigation per irrigated km2',	'm3/km2/day'],
	'irrigationNet'	=> [$irrigationNet*$subDi,	'Net Irrigation',			'mm/day',  $soilRA],
	'irrNet_km2'	=> [$irrigationNet   *$mm_km2,	'Net Irrigation per irrigated km2',	'm3/km2/day'],
	'irrFactor'	=> [$irrFactor,		'Irrigation factor',				'fraction'],
	'irrRunoff'	=> [$irrRunoff,		'Irrigation return runoff',			'mm/day',  $soilRA],
	'irrFlowLoc'	=> [$irrUseFlowLoc,	'Gross Irrigation from local river storage',	'mm/day',  $soilRA],
	'irrFlowRmt'	=> [$irrUseFlowRmt,	'Gross Irrigation from remote river storage',	'mm/day',  $soilRA],
	'irrFlowOrig'	=> [$irrFlowOrig,	  'Origin of remote irrigation water',		'mm/day',  $soilRA],
	'irrFlowProvX'  => [$irrUseFlowRID((0),,),'Origin of remote irrigation water, X',	'col #' ],
	'irrFlowProvY'  => [$irrUseFlowRID((1),,),'Origin of remote irrigation water, Y',	'row #' ],
	'irrigationFlow'=> [$irrigationFlow,	'Gross Irrigation from river storage',		'mm/day',  $soilRA],
	'irrigationGrwt'=> [$irrUseGrwt,	'Gross Irrigation from ground water',		'mm/day',  $soilRA],
	'irrigationAqf'	=> [$irrUseAqf,		'Gross Irrigation from aquifer water',		'mm/day',  $soilRA],
	'irrigationExtra'=>[$irrUseExtra,	'Gross Irrigation from Extra water',		'mm/day',  $soilRA],
	'addedWater'	=> [$ricePaddyWater,	'Added water to rice paddy percolation',	'mm/day',  $soilRA],
	'ricePaddyStrg'	=> [$ricePaddyStrg,	'Irrigation water in flooded rice paddies',	'mm',	   $soilRA],
	'smResStrg'	=> [$smResStrg[1],	'Storage in small irrigation reservoirs',	'mm',	   $soilRA],
	'sirIrrTaken'	=> [$sirIrrTaken,	'Water used from small irrigation reservoirs',	'mm/day',  $soilRA],
	'sirEvap'	=> [$sirEvap*$M3_2_MM,	'Evaporation from small irrigation reservoirs',	'mm/day',  $fullRA],
	'irrPercDeliv'	=> [$irrPercDeliv,	'Percolation from irrigatoin conveyances',	'mm/day',  $soilRA],
	'irrPercIneff'	=> [$irrPercIneff,	'Percolation from inefficient irrigation',	'mm/day',  $soilRA]) : (),
			### Glacier data
		$runIO{Glaciers} ? (
	'glMelt'	=> [$glMelt,		'Glacier melt runoff in m3/sec',	'm3/sec'],
	'glArea'	=> [$glArea,		'Glacier area fraction',		'fraction'],
	'glVolume'	=> [$glVolume*$M3_2_MM,	'Glacier water storage',		'mm',	$fullRA]) : (),

		###  Component Tracking variables  #####################
			### Stream Age
		$compSwitch[0] ? (
	'streamAge'	=> [$streamAge,		'Stream Water Age','day']) : (),
			### Water components by runoff origin (fractions)
		$compSwitch[1] ? (
	'runoff_sm'	=> [$cRunoff(,,(0)),	'Snow melt fraction in runoff',			'fraction'],
	'runoff_gm'	=> [$cRunoff(,,(1)),	'Glacial melt fraction in runoff',		'fraction'],
	'runoff_rr'	=> [$cRunoff(,,(2)),	'Rain/surface fraction in runoff',		'fraction'],
	'runoff_bf'	=> [$cRunoff(,,(3)),	'Base flow fraction in runoff',			'fraction'],
	'discharge_sm'	=> [$cStream(,,(0)),	'Snow melt fraction in discharge',		'fraction'],
	'discharge_gm'	=> [$cStream(,,(1)),	'Glacial melt fraction in discharge',		'fraction'],
	'discharge_rr'	=> [$cStream(,,(2)),	'Rain/surface fraction in discharge',		'fraction'],
	'discharge_bf'	=> [$cStream(,,(3)),	'Base flow fraction in discharge',		'fraction'],
	'runoffStg_sm'	=> [$cRffStg(,,(0)),	'Snow melt fraction in runoff storage',		'fraction'],
	'runoffStg_gm'	=> [$cRffStg(,,(1)),	'Glacial melt fraction in runoff storage',	'fraction'],
	'runoffStg_rr'	=> [$cRffStg(,,(2)),	'Rain/surface fraction in runoff storage',	'fraction'],
	'runoffStg_bf'	=> [$cRffStg(,,(3)),	'Base flow fraction in runoff storage',		'fraction']) : (),
			### Water components by source (fractions)
		$compSwitch[2] ? (
	'runoff_ps'	=> [$cRunoffP(,,(0)),	'Snow melt fraction in runoff (p)',		'fraction'],
	'runoff_pg'	=> [$cRunoffP(,,(1)),	'Glacial melt fraction in runoff (p)',		'fraction'],
	'runoff_pr'	=> [$cRunoffP(,,(2)),	'Rainwater fraction in runoff (p)',		'fraction'],
	'runoff_pu'	=> [$cRunoffP(,,(3)),	"$UnsStr water fraction in runoff (p)",		'fraction'],
	'discharge_ps'	=> [$cStreamP(,,(0)),	'Snow melt fraction in discharge (p)',		'fraction'],
	'discharge_pg'	=> [$cStreamP(,,(1)),	'Glacial melt fraction in discharge (p)',	'fraction'],
	'discharge_pr'	=> [$cStreamP(,,(2)),	'Rainwater fraction in discharge (p)',		'fraction'],
	'discharge_pu'	=> [$cStreamP(,,(3)),	"$UnsStr water fraction in discharge (p)",	'fraction'],
	'runoffStg_ps'	=> [$cRffStgP(,,(0)),	'Snow melt fraction in runoff storage (p)',	'fraction'],
	'runoffStg_pg'	=> [$cRffStgP(,,(1)),	'Glacial melt fraction in runoff storage (p)',	'fraction'],
	'runoffStg_pr'	=> [$cRffStgP(,,(2)),	'Rainwater fraction in runoff storage (p)',	'fraction'],
	'runoffStg_pu'	=> [$cRffStgP(,,(3)),	"$UnsStr water fraction in runoff storage (p)",	'fraction'],
	'smResStg_ps'	=> [$cSIR_P(,,(0)),	'Snow melt fraction in small reservoirs (p)',	'fraction'],
	'smResStg_pg'	=> [$cSIR_P(,,(1)),	'Glacial melt fraction in small reservoirs (p)','fraction'],
	'smResStg_pr'	=> [$cSIR_P(,,(2)),	'Rainwater fraction in small reservoirs (p)',	'fraction'],
	'smResStg_pu'	=> [$cSIR_P(,,(3)),	"$UnsStr water frac. in small reservoirs (p)",	'fraction'],
	'grndWater_ps'	=> [$cGrWaterP(,,(0)),	'Snow melt fraction in ground water (p)',	'fraction'],
	'grndWater_pg'	=> [$cGrWaterP(,,(1)),	'Glacial melt fraction in ground water (p)',	'fraction'],
	'grndWater_pr'	=> [$cGrWaterP(,,(2)),	'Rain/surface fraction in ground water (p)',	'fraction'],
	'grndWater_pu'	=> [$cGrWaterP(,,(3)),	"$UnsStr water fraction in groundwater (p)",	'fraction'],
	'soilMoist_ps'	=> [$cSoilP(,,(0)),	'Snow melt fraction in soil (p)',		'fraction'],
	'soilMoist_pg'	=> [$cSoilP(,,(1)),	'Glacial melt fraction in soil (p)',		'fraction'],
	'soilMoist_pr'	=> [$cSoilP(,,(2)),	'Rainwater fraction in soil (p)',		'fraction'],
	'soilMoist_pu'	=> [$cSoilP(,,(3)),	"$UnsStr water fraction in soil (p)",		'fraction'],
	'cEndoP_ps'	=> [$cEndoP(,,(0)),	'Snow melt fraction in endorheic lakes (p)',	'fraction'],
	'cEndoP_pg'	=> [$cEndoP(,,(1)),	'Glacial melt fraction in endorheic lakes (p)',	'fraction'],
	'cEndoP_pr'	=> [$cEndoP(,,(2)),	'Rainwater fraction in endorheic lakes (p)',	'fraction'],
	'cEndoP_pu'	=> [$cEndoP(,,(3)),	"$UnsStr water fraction in endorheic lakes (p)",'fraction'],
		      ### Water components in mm or m3/s
	'runoff_mm_ps'	=> [$cRunoffP_mm(,,(0)),'Snow melt in runoff (p)',			'mm/day', $fullRA],
	'runoff_mm_pg'	=> [$cRunoffP_mm(,,(1)),'Glacial melt in runoff (p)',			'mm/day', $fullRA],
	'runoff_mm_pr'	=> [$cRunoffP_mm(,,(2)),'Rainwater in runoff (p)',			'mm/day', $fullRA],
	'runoff_mm_pu'	=> [$cRunoffP_mm(,,(3)),"$UnsStr water in runoff (p)",			'mm/day', $fullRA],

	'baseflow_mm_ps'=> [$cBaseflowP_mm(,,(0)), 'Snow melt mm in baseflow (p)',		'mm/day', $fullRA],
	'baseflow_mm_pg'=> [$cBaseflowP_mm(,,(1)), 'Glacial melt mm in baseflow (p)',		'mm/day', $fullRA],
	'baseflow_mm_pr'=> [$cBaseflowP_mm(,,(2)), 'Rainwater mm in baseflow (p)',		'mm/day', $fullRA],
	'baseflow_mm_pu'=> [$cBaseflowP_mm(,,(3)), "$UnsStr water mm in baseflow (p)",		'mm/day', $fullRA],

	'discharge_m3s_ps'=> [$cStreamP_m3s(,,(0)), 'Snow melt in discharge (p)',		'm3/sec'],
	'discharge_m3s_pg'=> [$cStreamP_m3s(,,(1)), 'Glacial melt in discharge (p)',		'm3/sec'],
	'discharge_m3s_pr'=> [$cStreamP_m3s(,,(2)), 'Rainwater in discharge (p)',		'm3/sec'],
	'discharge_m3s_pu'=> [$cStreamP_m3s(,,(3)), "$UnsStr dwater in discharge (p)",		'm3/sec'],

	'grndWater_mm_ps' => [$cGrWaterP_mm(,,(0)), 'Snow melt in ground water (p)',		'mm',	  $fullRA],
	'grndWater_mm_pg' => [$cGrWaterP_mm(,,(1)), 'Glacial melt in ground water (p)',		'mm',	  $fullRA],
	'grndWater_mm_pr' => [$cGrWaterP_mm(,,(2)), 'Rain/surface in ground water (p)',		'mm',	  $fullRA],
	'grndWater_mm_pu' => [$cGrWaterP_mm(,,(3)), "$UnsStr water in ground water (p)",	'mm',	  $fullRA],

			$runIO{Irrigation} ? (
	'GrossIrr_ps'	  => [$cGrossIrrP(,,(0)),   'Snow melt fraction in gross irr (p)',	'fraction'],
	'GrossIrr_pg'	  => [$cGrossIrrP(,,(1)),   'Glacial melt fraction in gross irr (p)',	'fraction'],
	'GrossIrr_pr'	  => [$cGrossIrrP(,,(2)),   'Rain/surface fraction in gross irr (p)',	'fraction'],
	'GrossIrr_pu'	  => [$cGrossIrrP(,,(3)),   "$UnsStr water fraction in gross irr (p)",	'fraction'],
	'GrossIrr_mm_ps'  => [$cGrossIrrP_mm(,,(0)),'Snow melt in gross irr (p)',		'mm/day', $soilRA],
	'GrossIrr_mm_pg'  => [$cGrossIrrP_mm(,,(1)),'Glacial melt in gross irr (p)',		'mm/day', $soilRA],
	'GrossIrr_mm_pr'  => [$cGrossIrrP_mm(,,(2)),'Rain/surface in gross irr (p)',		'mm/day', $soilRA],
	'GrossIrr_mm_pu'  => [$cGrossIrrP_mm(,,(3)),"$UnsStr water in gross irr (p)",		'mm/day', $soilRA],

	'NetIrr_mm_ps'	=> [$cNetIrrP_mm(,,(0)), 'Snow melt in net irr (p)',			'mm/day', $soilRA],
	'NetIrr_mm_pg'	=> [$cNetIrrP_mm(,,(1)), 'Glacial melt in net irr (p)',			'mm/day', $soilRA],
	'NetIrr_mm_pr'	=> [$cNetIrrP_mm(,,(2)), 'Rain/surface in net irr (p)',			'mm/day', $soilRA],
	'NetIrr_mm_pu'	=> [$cNetIrrP_mm(,,(3)), "$UnsStr water in net irr (p)",		'mm/day', $soilRA],

	'IrrFlow_mm_ps'=> [$cIrrUseFlowP_mm(,,(0)),'Snow melt in irr from river storage (p)',	'mm/day', $soilRA],
	'IrrFlow_mm_pg'=> [$cIrrUseFlowP_mm(,,(1)),'Glacial melt in irr from river storage (p)','mm/day', $soilRA],
	'IrrFlow_mm_pr'=> [$cIrrUseFlowP_mm(,,(2)),'Rain/surface in irr from river storage (p)','mm/day', $soilRA],
	'IrrFlow_mm_pu'=> [$cIrrUseFlowP_mm(,,(3)),"$UnsStr water in irr from river storage (p)",'mm/day',$soilRA],

	'IrrGrwt_mm_ps'=> [$cIrrUseGrwtP_mm(,,(0)),'Snow melt in irr from groundwater (p)',	'mm/day', $soilRA],
	'IrrGrwt_mm_pg'=> [$cIrrUseGrwtP_mm(,,(1)),'Glacial melt in irr from groundwater(p)',	'mm/day', $soilRA],
	'IrrGrwt_mm_pr'=> [$cIrrUseGrwtP_mm(,,(2)),'Rain/surface in irr from groundwater(p)',	'mm/day', $soilRA],
	'IrrGrwt_mm_pu'=> [$cIrrUseGrwtP_mm(,,(3)),"$UnsStr water in irr from groundwater (p)",	'mm/day', $soilRA],

	'IrrEvap_mm_ps'=> [$cIrrEvap_mm(,,(0)),'Snow melt in irr water evaporation (p)',	'mm/day', $soilRA],
	'IrrEvap_mm_pg'=> [$cIrrEvap_mm(,,(1)),'Glacial melt in irr water evaporation (p)',	'mm/day', $soilRA],
	'IrrEvap_mm_pr'=> [$cIrrEvap_mm(,,(2)),'Rain/surface in irr water evaporation (p)',	'mm/day', $soilRA],
	'IrrEvap_mm_pu'=> [$cIrrEvap_mm(,,(3)),"$UnsStr water in irr water evaporation (p)",	'mm/day', $soilRA],

	'IrrPercRice_mm_ps'=>[$cIrrPercRice_mm(,,(0)),'Snow melt in rice paddy percolation (p)',   'mm/day', $soilRA],
	'IrrPercRice_mm_pg'=>[$cIrrPercRice_mm(,,(1)),'Glacial melt in rice paddy percolation (p)','mm/day', $soilRA],
	'IrrPercRice_mm_pr'=>[$cIrrPercRice_mm(,,(2)),'Rain/surface in rice paddy percolation (p)','mm/day', $soilRA],
	'IrrPercRice_mm_pu'=>[$cIrrPercRice_mm(,,(3)),"$UnsStr water in rice paddy percolation (p)",'mm/day',$soilRA],

	'IrrPercIneff_mm_ps'=>[$cIrrPercIneff_mm(,,(0)),'Snow melt in inefficient irr percolation (p)',   'mm/day',$soilRA],
	'IrrPercIneff_mm_pg'=>[$cIrrPercIneff_mm(,,(1)),'Glacial melt in inefficient irr percolation (p)','mm/day',$soilRA],
	'IrrPercIneff_mm_pr'=>[$cIrrPercIneff_mm(,,(2)),'Rain/surface in inefficient irr percolation (p)','mm/day',$soilRA],
	'IrrPercIneff_mm_pu'=>[$cIrrPercIneff_mm(,,(3)),"$UnsStr water in ineff irr percolation (p)",	  'mm/day',$soilRA],

	'etIrrCrops_mm_ps' =>[$etIrrCrops_mm(,,(0)),"Snow  water   in ET from irr crops (p)",		  'mm/day',$soilRA],
	'etIrrCrops_mm_pg' =>[$etIrrCrops_mm(,,(1)),"Glacial melt  in ET from irr crops (p)",		  'mm/day',$soilRA],
	'etIrrCrops_mm_pr' =>[$etIrrCrops_mm(,,(2)),"Rain/surface  in ET from irr crops (p)",		  'mm/day',$soilRA],
	'etIrrCrops_mm_pu' =>[$etIrrCrops_mm(,,(3)),"$UnsStr water in ET from irr crops (p)",		  'mm/day',$soilRA]
		) : ()) : (),
			### Water components by irrigation return flow (fractions)
		$compSwitch[3] ? (
	'runoff_rlt'	  =>[$cRunoffIrr(,, (0)),   'Relict water fraction in runoff',			'fraction'],
	'runoff_pst'	  =>[$cRunoffIrr(,, (1)),   'Pristine water fraction in runoff',		'fraction'],
	'runoff_use'	  =>[$cRunoffIrr(,, (2)),   'Dom/Ind/Stk return fraction in runoff',		'fraction'],
	'discharge_rlt'	  =>[$cStreamIrr(,, (0)),   'Relict water fraction in discharge',		'fraction'],
	'discharge_pst'   =>[$cStreamIrr(,, (1)),   'Pristine water fraction in discharge',		'fraction'],
	'discharge_use'   =>[$cStreamIrr(,, (2)),   'Dom/Ind/Stk return fraction in discharge',		'fraction'],
        'grndWater_rlt'	  =>[$cGrWaterIrr(,,(0)),   'Relict water in groundwater',			'fraction'],
	'grndWater_pst'	  =>[$cGrWaterIrr(,,(1)),   'Pristine water fraction in groundwater',		'fraction'],
	'grndWater_use'	  =>[$cGrWaterIrr(,,(2)),   'Dom/Ind/Stk return fraction in groundwater',	'fraction'],
		$nIrr == 1 ? (
	'runoff_irr'	  =>[$cRunoffIrr(,, (3)),   'Irrigation return frac in runoff',			'fraction'],
	'discharge_irr'	  =>[$cStreamIrr(,, (3)),   'Irrigation return frac in discharge',		'fraction'],
	'grndWater_irr'	  =>[$cGrWaterIrr(,,(3)),   'Irrigation return frac in groundwater',		'fraction']):(
	'runoff_irr'	  =>[$cRunoffIrr(,,0:2)->glue(2,$cRunoffIrr(,,3:2+$nIrr)->mv(2,0)->sumover),
						    "Irrigation return frac in runoff",			'fraction'],
   map(("runoff_irr_$_"	  =>[$cRunoffIrr(,,(2+$_)), "Irrigation return frac in runoff, cycle $_",	'fraction']),1..$nIrr),
	'discharge_irr'	  =>[$cStreamIrr(,,0:2)->glue(2,$cStreamIrr(,,3:2+$nIrr)->mv(2,0)->sumover),
						    "Irrigation return frac in discharge",		'fraction'],
   map(("discharge_irr_$_"=>[$cStreamIrr(,,(2+$_)), "Irrigation return frac in discharge, cycle $_",	'fraction']),1..$nIrr),
	'grndWater_irr'	  =>[$cGrWaterIrr(,,0:2)->glue(2,$cGrWaterIrr(,,3:2+$nIrr)->mv(2,0)->sumover),
						    "Irrigation return frac in groundwater",		'fraction'],
   map(("grndWater_irr_$_"=>[$cGrWaterIrr(,,(2+$_)),"Irrigation return frac in groundwater, cycle $_",	'fraction']),1..$nIrr)),
			####  Irrigation re-use and fates
			$compBalFlag ? ( map ((
	"openWaterEvap_irr_$_"		=> [$cStreamIrr_0(,,($_))*$netwkEvap*$M3_2_MM,
	   "Irrigation return water evaporated from river network, cycle $_",		'mm/day', $fullRA],
	"irrFracInGross_irr_$_"		=> [$$irrFracInGross[$_],
           "Reused irrigation water fraction in Gross irr water, cycle $_",	      'fraction', $soilRA],
	"UseReuseOutGross_irr_$_"	=> [$$UseReuseOutGross_irr[$_],
           "Irrigation return water removed for DIL consumption, cycle $_",		'mm/day', $soilRA],
	"IrrReuseInGross_irr_$_"	=> [$$IrrReuseInGross_irr[$_],
	   "Gross irrigation reuse of irrigation return water, cycle $_",		'mm/day', $soilRA],
	"IrrReuseInGrossFlow_irr_$_"	=> [$$IrrReuseInGrossFlow_irr[$_],
	   "Gross irrigation reuse of irrigation return water from flow, cycle $_",	'mm/day', $soilRA],
	"IrrReuseInGrossAqf_irr_$_"	=> [$$IrrReuseInGrossAqf_irr[$_],
	   "Gross irrigation reuse of irrigation return water from aquifers, cycle $_",	'mm/day', $soilRA],
	"IrrReuseInGrossGrwt_irr_$_"	=> [$$IrrReuseInGrossGrwt_irr[$_],
	   "Gross irrigation reuse of irrigation return water from grndwater, cycle $_",'mm/day', $soilRA],
	"IrrReuseOutNet_irr_$_"		=> [$$IrrReuseOutNet_irr[$_],
	   "Net irrigation reuse of irrigation return water, cycle $_",			'mm/day', $soilRA],
	"IrrReuseOutNonben_irr_$_"	=> [$$IrrReuseOutNonben_irr[$_],
			"Non-beneficial loss of irrigation return water, cycle $_",	'mm/day', $soilRA]),1..$nIrr)):()):(),
			### Water temperature
		$compSwitch[4] ? (
	'discharge_twt'	=> [$cStreamTw,		'Streamflow water temperature',	'C'],
	'baseflow_twt'	=> [$baseflowTw,	'Baseflow water temperature',	'C']) : (),
			### Water components by runoff mask
		$compSwitch[5] ? (	map { my ($id, $name) = @{$$rnffID[$_]};
	("dischMsk_$id"	=> [$cStreamMsk(,,($_)), "Water from $name in discharge; ID= $id", 'fraction'],
	 "runoffMsk_$id"=> [$cRunoffMsk(,,($_)), "Water from $name in runoff; "."ID= $id", 'fraction'],
	 "grndWtMsk_$id"=> [$cGrWaterMsk(,,($_)),"Water from $name in grndwater; ID= $id", 'fraction'])} 0..$#$rnffID) : (),
			### Hydrobiochemistry (Dissolved Inorganic Nitrogen)
		$compSwitch[6] ? (
	'DIN_Land_Load'	  => [$DIN_Land_Load,	'Dissolved Inorganic Nitrogen loading from land','kg/km2/day'],
	'DIN_Load'	  => [$DIN_Load,	'Dissolved Inorganic Nitrogen total loading',	'kg/km2/day'],
	'DIN_Denit'	  => [$DIN_Denit,	'DIN denitrification in streamflow',		'kg/pixel/day'],
	'DIN_Conc_Rnff'	  => [$cRunoffDIN,	'Dissolved Inorganic Nitrogen in runoff',	'g/L'],
	'DIN_Conc_Dsch'	  => [$cStreamDIN,	'Dissolved Inorganic Nitrogen in discharge',	'g/L'],
	'DIN_Conc_Cons'	  => [$cStmConDIN,	'Conservative Inorganic Nitrogen in discharge',	'g/L']) : (),
			### Waste Water Treatment Plants
		$DIN_param{WWTP} ? (
	'DIN_WWTP_Input'  => [$DIN_WWTP_Input,	'Dissolved Inorganic Nitrogen WWTP input',	'kg/pixel/day'],
	'DIN_WWTP_Removal'=> [$DIN_WWTP_Removal,'Dissolved Inorganic Nitrogen WWTP removal',	'kg/pixel/day'],
	'DIN_WWTP_Load'	  => [$DIN_WWTP_Load,	'Dissolved Inorganic Nitrogen WWTP load',	'kg/pixel/day']) : (),
			### ModFlow Aquifers
		($aqfType == 3) ? (
	'AQF_DryHead_T'	=> [$Aqf_head_T,	'Dry Head in confining aquifer (ASL)',			'm'],
	'AQF_Head_T'	=> [$$AQF_DATA{h_T},	'Head in confining aquifer (ASL)',			'm'],
	'AQF_Head_B'	=> [$$AQF_DATA{h_B},	'Head in unconfined and confined aquifers (ASL)',	'm'],
	'AQF_H_Depth'	=> [$Aqf_H_Depth,	'Head depth',						'm'],
	'AQF_DH_Depth_T'=> [$Aqf_DH_Depth_T,	'Dry Head depth in confining aquifer',			'm'],
	'AQF_H_Depth_T'	=> [$Aqf_H_Depth_T,	'Head depth in confining aquifer',			'm'],
	'AQF_H_Depth_B'	=> [$Aqf_H_Depth_B,	'Head depth in unconfined and confined aquifers',	'm'],
	'AQF_H_Delta'	=> [$Aqf_H_Delta,	'Head level relative to streams',			'm'],
	'AQF_Storage_T'	=> [$Aqf_Storage_T,	'Storage in confining aquifer',				'mm', $fullRA],
	'AQF_Storage_B'	=> [$Aqf_Storage_B,	'Storage in unconfined and confined aquifers',		'mm', $fullRA],
	'AQF_FluxU_T'	=> [$$AQF_DATA{QhT_U},	'Avg U (E-W) flux in confining aquifer',		'm3/sec'],
	'AQF_FluxV_T'	=> [$$AQF_DATA{QhT_V},	'Avg V (N-S) flux in confining aquifer',		'm3/sec'],
	'AQF_FluxU_B'	=> [$$AQF_DATA{QhB_U},	'Avg U (E-W) flux in unconfined and confined aquifers',	'm3/sec'],
	'AQF_FluxV_B'	=> [$$AQF_DATA{QhB_V},	'Avg V (N-S) flux in unconfined and confined aquifers',	'm3/sec'],
	'AQF_Flux_Z'	=> [$$AQF_DATA{Qv},	'Z (down) flux from confining to confined aquifers',	'm3/sec'],
	'AQF_DC_T'	=> [$$AQF_DATA{DC_T},	'Dry cell mask in confining aquifers',			'unitless'],
	'AQF_DC_B'	=> [$$AQF_DATA{DC_B},	'Dry cell mask in confined aquifers',			'unitless'],
	'AQF_RIV_dsch'	=> [$Aqf_RIV_dsch,	'Aquifer discharge to surface streams',			'm3/sec'],
	'AQF_RIV_rech'	=> [$Aqf_RIV_rech,	'Aquifer recharge from surface streams',		'm3/sec']) : ()
     );			### End of %data_out (DO NOT REMOVE THIS COMMENT)

			### BW/GW additions
    if (m/_[BG]W_(ET|SM)$/ ~~ @list_out) {
      foreach my $crop (keys %{$land{irrCrop}}) {
	$data_out{$crop.'_BW_ET'}  = [$landFrac{irrCrop}{$crop}*($sMoistET{irrCrop}{$crop} - $sMoistGrET{irrCrop}{$crop}),
					"Irrigated $crop Blue  Water soil moisture",	'mm/day',	$soilRA];
	$data_out{$crop.'_GW_ET'}  = [$landFrac{irrCrop}{$crop}* $sMoistGrET{irrCrop}{$crop},
					"Irrigated $crop Green Water soil moisture",	'mm/day',	$soilRA];
	$data_out{$crop.'_BW_SM'}  = [$landFrac{irrCrop}{$crop}*($sMoist{irrCrop}{$crop} - $sMoistGr{irrCrop}{$crop}),
					"Irrigated $crop Blue  Water evaporation",	'mm',		$soilRA];
	$data_out{$crop.'_GW_SM'}  = [$landFrac{irrCrop}{$crop}* $sMoistGr{irrCrop}{$crop},
					"Irrigated $crop Green Water evaporation",	'mm',		$soilRA];
    } }
			### Irrigation by crop
    foreach my $crp (keys %{$land{irrCrop}}) {
      $data_out{"$crp\_irrigation"} = [$irrigation{$crp}, "$crp Net Irrigation",	'mm/day',$soilRA];
      $data_out{"$crp\_irrDemand"}  = [$irr_demand{$crp}, "$crp Net Irrigation Demand",	'mm/day',$soilRA];
    }
			### Soil moisture by crop
    foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (keys %{$land{$lnd}}) {
      $data_out{"$tp\_$lnd\_soilMoistFrac"} =
	[$sMoistFrac{$lnd}{$tp},	"Soil Moisture Fraction for $tp in $lnd land",	'fraction'];
    } }
			### PET by crop
    if (m/_PET$/ ~~ @list_out) {
    foreach my $lnd (qw(irrCrop rfdCrop fallow)) { foreach my $tp (keys %{$land{$lnd}}) {
      $data_out{"$tp\_$lnd\_PET"} =
	[$pet*$Kc_par{$lnd}{$tp}, "Potential Evapotranspiration for $tp in $lnd land",	'mm/day',$soilRA];
    } } }
			### Check for valid output variables (add to STRICT mode)
    if  ($n_count == $n_spinup+1 || $n_count == $n_count_done+1) {	# First time output is created
      for (my $i=0; $i<=$#list_out; $i++) {
	my $message =	(!exists($data_out{$list_out[$i]}))		? 'exist or valid in the data list' :
			(    ref($data_out{$list_out[$i]}[0]) ne 'PDL')	? 'have values' : '';
	if ($message) {
	  print "\tOutput variable \"$list_out[$i]\" does not $message: Removed from the list.\n";
	  splice @list_out,           $i, 1;
	  splice @{$runIO{agg_vars}}, $i, 1; $i--;
    } } }
			### ModFlow case: Mask out pixels outside aquifers with nodata
    if ($aqfType == 3) { foreach  my $var (grep m/^AQF_/, @list_out) {
      $data_out{$var}[0] = $data_out{$var}[0]->setbadif(!$$AQF_DATA{mask_B});
      $data_out{$var}[0] = $data_out{$var}[0]->setbadif( $$AQF_DATA{mask_S}) if $var =~ m/_(T|Z)$/;
    } }
			### Accumulate for monthly averages
    unless ($run_daily) {
      foreach my $var (@list_out) { if ( $date =~ m/-01$/ ) {
	set_to_zero(	$dataSum  {$var},$zeroes);
			$dataCount{$var}  = 0; }
	$dataSum  {$var} += $data_out{$var}[0];
	$dataCount{$var} += 1;
    } }
			###############################################
			### Finalize and save WBM output to a NetCDF file
    if ($run_file) {
      my $jDate   = $run_daily ? $j_date : julian_day(@date[0,1],15);
      my $nc_Time = $jDate - $jDate_1900;
	 $nc_Time = $nc_Time*24 + $hour	if $hr_flag;
					### Make monthly averages
      unless ($run_daily) { foreach my $var (@list_out) {
	$dataSum {$var}   /= $dataCount{$var}	if $data_out{$var}[2] !~ m/\/month$/;
	$data_out{$var}[0] = $dataSum  {$var};
      } }

      ($path,$band) = @$run_file;
       $path =~ s/.+:(.+):.+/$1/;	# Strip NetCDF extras

			### Add sort order for the variables in the output NetCDF files
      map $data_out{$list_out[$_]}[4] = $_, 0 .. $#list_out;

			### Change cell reference area for all or some output variables
      for (my $i=0; $i<=$#list_out; $i++) {	next unless defined $data_out{$list_out[$i]}[3];
		# Scale output regardless of "outputScale" flag (NB- do not use overloaded *=)
	if (ref($data_out{$list_out[$i]}[3]) eq 'PDL') {
	  $data_out{$list_out[$i]}[0]	= $data_out{$list_out[$i]}[0] * $data_out{$list_out[$i]}[3];
	  $data_out{$list_out[$i]}[3]	= $fullRA;
	}
	else {	# Scale output if "outputScale" is requested
	  my $refArea	= $data_out{$list_out[$i]}[3]->{reference_area};
	  next if !$outputScale || $refArea =~ m/^full/;
				# Scale all output variables (NB- do not use overloaded *=)
	  $data_out{$list_out[$i]}[0]	= $data_out{$list_out[$i]}[0] * $sRatio if $refArea =~ m/^soil/;
	  $data_out{$list_out[$i]}[0]	= $data_out{$list_out[$i]}[0] * $aRatio if $refArea =~ m/^active/;
	  $data_out{$list_out[$i]}[3]	= $fullRA;
      } }
			### Finalize the data list to write
      my %dataToWrite = map(($_=>$data_out{$_}),@list_out);
			### Write data to file
      write_nc($path, $band, $nc_Time, $grid, \%dataToWrite,	{TS_RESOLUTION => $run_TS, NC4 => $runIO{NC4},
		TYPE => \&float, CREDITS => $credits, CALENDAR => $runIO{calendar}, ATTRIB => $global_attrib});
      $path = 'File '.basename($path);
    }
			###############################################
			###	Runtime aggregations
    my @Agg_do	= ($hr_flag && $hour==$last_hr, $mnEndCond, $yrEndCond);
    foreach my $i (0..2) { foreach my $var (@{$runIO{agg_vars}}) {
      next unless $runIO{TA_depth}[$i];

      var_accumulate($Agg_str[$i], $Agg_var{$var}, $data_out{$var}, $date);
      if ($Agg_do[$i]) {
	my ($path,$band,$dataToWrite,$aggrRange) = var_aggregate($Agg_str[$i], $var, $Agg_var{$var}, $data_out{$var}, $date);
	my  $nc_Time = $j_date - $jDate_1900 - $Agg_off[$i];
	write_nc($path, $band, $nc_Time, $grid, $dataToWrite,
		{TS_RESOLUTION => $Agg_Str[$i], NC4 => $runIO{NC4}, TYPE => \&float, CALENDAR => $runIO{calendar},
		 CREDITS => $credits, ATTRIB => [@$RT_global_attrib,@$aggrRange,['AggrMethod','average']]});
	var_reset($Agg_str[$i], $Agg_var{$var});
    } }}
		### Save irrigation equipped area
    if ( $yrEndCond && ref($irrEqpArea) ) {
      my $path    = "$runIO{Output_dir}/yearly/irrEquipArea/irrEquipArea.nc";
      my $band    = $date[0] - substr($runIO{Run_Start},0,4) + 1;
      my $nc_Time = julian_day($date[0],7,1) - $jDate_1900;
      write_nc($path, $band, $nc_Time, $grid,
		{'irrEquipArea'	=> [$irrEqpArea*$sRatio, 'Irrigation equipped area', 'fraction']},
		{TS_RESOLUTION => 'yearly', NC4 => $runIO{NC4}, TYPE => \&float, CALENDAR => $runIO{calendar},
		 CREDITS => $credits, ATTRIB => $global_attrib});
    }
  }
	###############################################################
	###   Diversion and Lumped/Virtual aquifer summaries
  if ($yrCycle > $runIO{Spinup}{Loops} && !$noOutput) {
    diversion_yr_total(	\@diversion, $routeDiversion, \@date, \%runIO, $route, $dt);
    aquifer_summary(	 $aqfType,   $aqf_data,        $date, \%runIO,\%cAqWaterP, \%cAqWaterIrr);
    compBalance_save(	\%compBal,  \@compSwitch,      $date, \%runIO, $nIrr) if $compBalFlag && !$skipCompBal;
  }
  $skipCompBal	= 0;

	###############################################################
	###   Print information and water balances to the screen

  my $units	= $scrPrintScale == 1 ? 'km3' : "$scrPrintScale*km3";
  my $sp_str	= $$extent{spool_writes} ? " Spool+=$$extent{spool_writes}" : '';

  map $_*=1/$scrPrintScale, $pixelBalance, $flowBalance, $totalBalance, $runoffT,
	$resStorageT, $smResStrgT, $resStorChgT, $glMeltT, $irrExtraT, $etIrrCropsT,
	$irrigationGrossT, $irrigationNetT, @irrNetT, $usgsDeltaT	if $scrPrintScale != 1;

		### Balance string
  my $str = sprintf "%s (%s) Balance=(%.2f,%.2f,%.2f, %6.2f, %7.2f,%7.2f,%6.2f, %.2f,%.2f)%s\n", $path, $date,
	abs($pixelBalance), abs($flowBalance), abs($totalBalance), $runoffT, $resStorageT+$smResStrgT, $resStorChgT,
	$glMeltT, $irrExtraT, $etIrrCropsT, $sp_str;
		### Balance header string
  if ($n_count_print++ == 0) {
    print "Order of Irrigated Crops in the output below:\n\t",join('  ', @$irrCropList),"\n\n" if $runIO{Irrigation};
    print ' 'x CORE::index($str,'Balance'),"Balance=(Pixl,Flow,Totl, RffTot, FlowStg,  DStrg, GlMlt, IrEx,IrET) $units\n";
  }		### Irrigation balances string
  $str .= sprintf "   Irrigation(Gross/Net Paddy : Crops)=(%.2f/%.2f $irrFormat_str); Avg Supply = %s %%\n",
	$irrigationGrossT, $irrigationNetT, $irrPaddyT, @irrNetT, $irrFactorPct if $runIO{Irrigation};
		### USGS Delta string
  $str .= sprintf "\tusgsDelta     = %.4f\n", $usgsDeltaT if $usgs_data;
  print $str;				# Print daily/monthly water balance

		### Print end-of-the-year balances
  if ($yrEndCond) {
    printf  "\n   Summary for Year $date[0]%s:\n", $path =~ m/^Spinup/ ?  " ($path)" : '';
    if ($runIO{Irrigation}) {
	printf	"   Irrig. Yr Supply (Total / Crops)      = (%s / "     .join('  ',("%s")  x $nIrrCrops   ).") %%\n".
		"   Irrig. Yr(Gross/Net : SW/GW/AW/UW) PD = (%.2f/%.2f : %.2f/%.2f/%.2f/%.2f) $irrFormat_str) km3\n".
		"   Ineff. Yr(Runoff / Perc / Evap )      = (%.2f/%.2f/%.2f) km3; Efficiency = %.1f %%\n"	.
		"   Deliv. Yr(         Perc / Evap )      = (%.2f/%.2f) km3\n"					.
		"   Cropland/IrrEquipped Area=(%.2f / %.2f) mln Ha\n\n",
		 irrFactorPct($irrNetT_y-$irrPaddyT_y,$irrDemVsT_y),
		 	map(irrFactorPct($irrNetT_y[$_],$irrDemVsT_y[$_]),0..$#irrNetT_y),
		 $irrGrossT_y,$irrNetT_y,@irrXW_y,$irrPaddyT_y,@irrNetT_y,
		 $irrIneffRnff_y, $irrIneffPerc_y, $irrIneffEvap_y, $irrNetT_y/$irrGrossT_y*100,
		 $irrPercDeliv_y, $irrEvapDeliv_y,
		($cropAreaFrac*$cell_area)->sum * 1e-4, ($irrEqpArea*$cell_area)->sum * 1e-4;
				### Reset annual totals to zero
	map $_=0, $irrNetT_y,$irrGrossT_y,$irrNetT_y,@irrXW_y,$irrIneffRnff_y,$irrIneffPerc_y,$irrIneffEvap_y,
		  $irrPercDeliv_y,$irrEvapDeliv_y,$irrPaddyT_y,@irrNetT_y,$irrDemVsT_y,@irrDemVsT_y,$irrEqpArea;
    }
		### End of year Non-irrigation water use balances
    if ($runIO{WaterDemand}) {
	printf	"   Domestic   Yr(Gross/Net : SW/GW/AW/UW) = (%.3f/%.3f : %.3f/%.3f/%.3f/%.3f) km3\n"  .
		"   Industrial Yr(Gross/Net : SW/GW/AW/UW) = (%.3f/%.3f : %.3f/%.3f/%.3f/%.3f) km3\n"  .
		"   Livestock  Yr(Gross/Net : SW/GW/AW/UW) = (%.3f/%.3f : %.3f/%.3f/%.3f/%.3f) km3\n\n",
		$domGrossT_y, $domNetT_y, @domXW_y,
		$indGrossT_y, $indNetT_y, @indXW_y,
		$stkGrossT_y, $stkNetT_y, @stkXW_y	if $runIO{WaterDemand};
    }
		### End of year runoff/precipitation balance
    my $wbm_dischT_y	= $dischT_y - $usgsDeltaT_y if $usgs_data;
    printf	"   Rff-Gl/Precip  Yr = (%.2f / %.2f), %.2f %% \n", $RunoffT_y,$precipT_y,$RunoffT_y/$precipT_y*100;
    printf	"   Runoff-Dischg  Yr = (%.2f - %.2f), %.2f km3\n", $runoffT_y,$dischT_y, $runoffT_y-$dischT_y;
    printf	"   USGS-WBM Dsch  Yr = (%.2f - %.2f), %.2f km3\n", $dischT_y,$wbm_dischT_y,$usgsDeltaT_y if $usgs_data;
    printf	"   ET Non/Irr/Rfd Yr = (%.2f / %.2f / %.2f) km3\n",$etNonCropsT_y, $etIrrCropsT_y, $etRfdCropsT_y;
    printf	"   Surface W Storage =  %.2f km3\n",   ($resStorageT + $smResStrgT) * $scrPrintScale;
    printf	"   Grndwater Storage =  %.2f km3\n\n", ($grdWater[1] * $mm_2_km3)->sum;

		### End of year aquifer balances
      if ( $aqfType ) {}		# Removed from public domain

    printf "  Y=%04d %s\n", $date[0], "Spool files added = $n_spool";

		### Reset annual totals to zero:
		###	NB - List of these variables must match those in "*.yr_data.json" file
    map $_=0,	$runoffT_y,   $RunoffT_y, $dischT_y, $precipT_y, $etNonCropsT_y, $etIrrCropsT_y, $etRfdCropsT_y,
		$spr_dschT_y, $snk_inflT_y, $usgsDeltaT_y,
		$domGrossT_y, $domNetT_y,   @domXW_y,
		$indGrossT_y, $indNetT_y,   @indXW_y,
		$stkGrossT_y, $stkNetT_y,   @stkXW_y;
  }

	###############################################################
	###   Clear previous day
  $flow_outPrev	.= $flow_out;
  map shift(@{$_}), \@grdWater,\@resStorage,\@sMoist,\@surfRffStorage,\@irrRffStorage,\@smResStrg,\@irrVIS;
  foreach my $lnd (keys %land) { foreach my $tp (keys %{$land{$lnd}}) {
    $sMoistPrev{$lnd}{$tp} = $sMoist  {$lnd}{$tp}->copy;
    $sMoistPrVs{$lnd}{$tp} = $sMoistVs{$lnd}{$tp}->copy		if exists $sMoistVs{$lnd}{$tp};
  }}
  shift @cStream	if $compSwitch[1];   shift @cStreamP	if $compSwitch[2];
  shift @cStreamIrr	if $compSwitch[3];   shift @cStreamMsk	if $compSwitch[5];

			### Clear expired 5-year running average for Discharge
  if ($yrEndCond) {
    if ($mDischargeS) {
      shift @mDischarge;		push @mDischarge, $zeroes->copy+1e-6;
      shift @mDischargeN;		push @mDischargeN, 0;
    }
    else { $mDischargeK++ }
  }

	###############################################################
	###   Save state of the run at the end of spinup, each year, when finished, etc.
  my $stFileStr = '';
#   die	if $date eq '1981-11-08';				# Debugging
  if (	!$noState && (						# Flag for not writing state
# 	 $date =~ m/1981-10-15/ ||				# Debugging
	($n_count == $n_spinup) || ($n_count > $#$date_list) ||	#(End of spinup) || (End of run) ||
	 $yrEndCond					     ||	# End of Year    ||
	($dState  && ($hour == $last_hr))		     ||	#(Daily  state   && (End of Day))
	 0						)) {	# Do not remove it (zero)
    bMarkMap(3, $bMarkM, 'Writing run state files');

    my ($pfx, $sfx) = ('Run', $dState ? ".$date" : '');
		### Reset Date in case of (End of Spinup)
    if     ($n_count  < $n_spinup) {
      ($pfx, $sfx, $j_date)	= ('Spinup', '', -$n_count); }
    elsif  ($n_count == $n_spinup) {
      ($pfx, $sfx, $j_date)	= ('Spinup', '.spinup', julian_day(split(m/-/,$$date_list[$n_spinup]))-1);
		### Reset spinup tracking, if requested
      reset_spinup_tracking(2,3) if $wbmParam{resetSpinupTracking};
    }
		### Major parameters
    writeflex("$run_state_file$sfx.dat",
	long($j_date,$mDischargeK,@mDischargeN), @mDischarge,
	$snowPack,@grdWater,$discharge,@resStorage,$flow_out,@sMoist,
	$InterceptionStorage,@surfRffStorage,@irrRffStorage,$ricePaddyStrg,@smResStrg,@irrVIS,$UGW,$endoStrg);

		### Bucket components
    writeflex("$run_state_file$sfx.bucket-0.dat",$streamAge)					if $compSwitch[0];
    writeflex("$run_state_file$sfx.bucket-1.dat",@cStream,   $cSIR,       $cVIS,    $cRffStg)	if $compSwitch[1];
    writeflex("$run_state_file$sfx.bucket-2.dat",@cStreamP,  $cGrWaterP,  $cSIR_P,  $cVIS_P,
						 $cRffStgP,  $cSoilP,	  $cEndoP)		if $compSwitch[2];
    writeflex("$run_state_file$sfx.bucket-3.dat",@cStreamIrr,$cGrWaterIrr,$cSoilIrr,$cSIR_Irr,
						 $cVIS_Irr,  $cRffStgIrr)			if $compSwitch[3];
    writeflex("$run_state_file$sfx.bucket-4.dat",$cStreamTw, $airT_bAvg,  $airT_rAvg)		if $compSwitch[4];
    writeflex("$run_state_file$sfx.bucket-5.dat",@cStreamMsk,$cGrWaterMsk,$cSIR_Msk,$cVIS_Msk,
						 $cRffStgMsk)					if $compSwitch[5];
    writeflex("$run_state_file$sfx.bucket-6.dat",$cStreamDIN,$cStmConDIN)			if $compSwitch[6];

		### Snow Bands
    writeflex("$run_state_file$sfx.snowBand.dat",@$snowBand)		if $runIO{snowBands};

		### Landcover/Crop parameters (soil moisture)
    foreach my $lnd (keys %land) { foreach my $tp (keys %{$land{$lnd}}) {
			### Encode crop mask into sMoist value by adding a large number
      if ($lnd =~ m/Crop|fallow/) {
	my $encode = condition($cropMask{$lnd}{$tp},  -$sMoist{$lnd}{$tp}-1, $sMoist{$lnd}{$tp});
      writeflex("$run_state_file$sfx.sM.$lnd.$tp.dat", $encode); } else {
      writeflex("$run_state_file$sfx.sM.$lnd.$tp.dat", $sMoist  {$lnd}{$tp}); }
      writeflex("$run_state_file$sfx.Vs.$lnd.$tp.dat", $sMoistVs{$lnd}{$tp}) if $lnd eq 'irrCrop' && $vs_flag;
      writeflex("$run_state_file$sfx.Gr.$lnd.$tp.dat", $sMoistGr{$lnd}{$tp}) if $BwGwFlag;	### BW/GW addition
    }}
		### Aquifer data
    if ( $aqfType ) {} 		# Removed from public domain

		### End of year balances
    my $dump_file	= $run_state_file."$sfx.yr_data.json";
    open (FILE,">$dump_file") or die "Couldn't open $dump_file, $!";
      my $dumper = Data::Dumper->new([		### Use all variables listed in "Reset annual totals to zero" section!!!
	[ unpdl_scalars(		# Convert PDL sums to perl var type (in new PDL v.2.063)
	   $runoffT_y,   $RunoffT_y, $dischT_y, $precipT_y, $etNonCropsT_y, $etIrrCropsT_y, $etRfdCropsT_y,
		$spr_dschT_y, $snk_inflT_y, $usgsDeltaT_y,
		$domGrossT_y, $domNetT_y,
		$indGrossT_y, $indNetT_y,
		$stkGrossT_y, $stkNetT_y)], [unpdl_scalars(@domXW_y)], [unpdl_scalars(@indXW_y)], [unpdl_scalars(@stkXW_y)]],
	[qw(ALL domXW_y indXW_y stkXW_y)]);
	$dumper->Purity(1)->Deepcopy(1);
      my $str = $dumper->Dump;	$str =~ s/\n\s+/\n/g;
      print FILE $str;
    close FILE;

		### USGS assimilation data
    if ($usgs_data) {} 		# Removed from public domain

		### Component tracking data
    if ($compBalFlag) {
      $dump_file	= $run_state_file."$sfx.compBal.json";
      open (FILE,">$dump_file") or die "Couldn't open $dump_file, $!";
	$dumper = Data::Dumper->new([\%compBal], ['COMPBAL']);
	  $dumper->Purity(1)->Deepcopy(1);
	$str = $dumper->Dump;	$str =~ s/\n\s+/\n/g;
	print FILE $str;
      close FILE;
    }
		##########################

    $stFileStr	= sprintf "\t$pfx state is saved.\n";
  }
		### Check "non-modifiable" variables.
		###	The memory address is $$zeroes, but it cannot detect internal changes done by ".=" operator
  die "Problem in the WBM code:\n\tNon-modifiable variable \$zeroes   has changed. Aborting...\n\n" if checkZero($zeroes);
  die "Problem in the WBM code:\n\tNon-modifiable variable \$zeroes_4 has changed. Aborting...\n\n" if checkZero($zeroes_4);
  die "Problem in the WBM code:\n\tNon-modifiable variable \$ones     has changed. Aborting...\n\n" if checkOnes($ones);

	###############################################################
	###   Report benchmarks
  if ($bMarkD) {	### Daily step CPU benchmark
    my $time_end	= Benchmark->new;
    printf "  D= %s\n",timestr(timediff($time_end, $time_strt));
  }
  bMarkMap(1, $bMarkM, 0, "Number of spool writes = $$extent{spool_writes}");	if ($yrEndCond) {
  bMarkMap(0, $bMarkM, 0, "Number of spool writes = $n_spool");	$n_spool = 0;
			### Yearly step CPU benchmark
  if ($bMarkY) {
    my $time_end	= Benchmark->new;
    printf "  Y=%04d %s\n", $date[0], timestr(timediff($time_end, $time_strtY));
    $time_strtY	= $time_end;
  } }
  print $stFileStr,($yrEndCond ? "\n" : '');

  last if $test;	### End run if "test" mode is on
}
printf "\nOutput directory:   %s\n", $noOutput	? 'No Output' : $runIO{Output_dir}. '/';

	###############################################################
	###   Run post-processing script
if ($run_PP && -e $runIO{PP_script} && !$noRun) {
 (my $pp_log = $runIO{PP_script}) =~ s/pl$/log/;
  system("perl -w $runIO{PP_script} > $pp_log &");

  print	"Data post-processing script has started as a separate system process.\n",
  	"Progress log file: $pp_log\n";
}

#######################################################################
						# Report Total Time
printf "\nTime used for the job - %d hours, %d minutes, and %d seconds\n\n",
	time_used($time_start,time());

#######################################################################

print "\nAll Done!\n\n";

close BMFILE if $bMarkM;
exit;

#######################################################################
######################  Functions  ####################################

sub get_paths
{
  my %path	= get_conf();	# Global defaults overwritten by Local defaults

	### Overwrite paths from WBM init file/inputs
  foreach my $key (qw(run_state_dir spool_dir MT_file credits)) {
    if ($key eq 'run_state_dir'	&& $run_state_dir) { $path{$key} = $run_state_dir; next; } # Command line inputs have
    if ($key eq 'spool_dir'	&& $spool_dir)	   { $path{$key} = $spool_dir;	   next; } # the highest priority
    $path{$key}	= $runIO{MT_Code_Name}{$key} if ref($runIO{MT_Code_Name}) and exists($runIO{MT_Code_Name}{$key});
  }
  $path{run_state_dir}	.= '/' unless $path{run_state_dir} =~m/\/$/;	# Add slash for the directory, if needed
  $path{spool_dir}	.= '/' unless $path{spool_dir}     =~m/\/$/;	# Add slash for the directory, if needed
  $run_state_dir= $path{run_state_dir};
  $spool_dir	= $path{spool_dir};
  $MT_file	= $path{MT_file};
  $credits	= $path{credits};
  copy_path(%path);			# Copy path hash to RIMS::WBM module
  return    %path ;
}

#######################################################################

sub dayLength
{
  my ($lon, $lat, $jday, $hrS, $hrLen) = @_;
     ($hrS, $hrLen)	 =(0, 24) unless defined $hrLen;	# Start and length of time period (TP), hours

  my $solstice	= 23.44/180 * pi;
  my $dec	= $solstice*cos(2*pi/365*($jday+10));
  my $arg	= -tan($dec) * tan($lat/180*pi);
     $arg	= -($arg<-1) + ($arg>1) + (abs($arg)<=1)*$arg;
  my $dayLen	= (1-acos($arg)/pi)->dummy(0,$lon->dim(0));	# Daylight in 24-hours, fraction

  return $dayLen if $hrLen == 24;

     $dayLen	= 24    * $dayLen;		# Convert day length to hours (NB- do not use overloaded *=)
  my $hrE	= $hrS  + $hrLen;
  my $noon	= 12    -($lon/15)->dummy(1,$lat->dim(0));	# Noon time by time zone shift
  my $half	= 0.5   * $dayLen;				# Half length of daylight, hours
  my $dltBeg	=($noon - $half) % 24;				# Begin time  of daylight (wrapped), hours
  my $dltEnd	=($noon + $half) % 24;				# End   time  of daylight (wrapped), hours
  my @hrCond	=(dayLightCondition($lon,$dayLen,$hrS), dayLightCondition($lon,$dayLen,$hrE));

		### Daylight length within sub-daily time step (fraction of time step)
 return condition( $hrCond[0] < $hrCond[1],	$hrE - $dltBeg,		# Sunrise	within the interval
	condition( $hrCond[0] > $hrCond[1],	$dltEnd - $hrS,		# Sunset	within the interval
	condition(($hrS < $dltBeg) & ($dltBeg < $hrE),
	condition( $hrCond[0],			$hrLen - 24 + $dayLen,	# Whole night	within the interval
						$dayLen),		# Whole day	within the interval
						$hrCond[0])))/$hrLen;	# No change	within the interval
}

sub dayLightCondition {			# Daylight condition check at given hour ($hr)
  my ($lon, $dayLen, $hr) = @_;
  return (cos(($hr/12 + 1 + $lon/180) * pi) - cos($dayLen/24 * pi) > 0);
}

#######################################################################

sub grossRadiation
{
  my ($lon, $lat, $jday, $hr, $hrLen) = @_;
     ($hr,  $hrLen)	 =(0, 24) unless defined $hrLen;
  my  $MDGrossRadStdTAU  = 1 ;				## $sp is Solar constant, Mj/m2/day
  my  $sp	= 1360.0 * 3600*24 * 0.041841/41860.0;	## FBM  0.041841 conversion from cal/cm2 to MJ/m2
  my  $sigma	=-(23.44/180 * pi) * cos(2*pi/365*($jday+10));
  my  $lambda	=   $lat/180 * pi;
  my  $grossRad	= 0;

  for (my $hour = $hr; $hour < $hr+$hrLen; $hour++) {
    my $time	= $hour;
    if($hrLen	< 24) {
       $lambda	= $lambda		   ->dummy(0,$lon->dim(0));
       $time	= (($hour+$lon/15+12) % 24)->dummy(1,$lat->dim(0)); }
    my $eta	=($time + 1) * pi/12.0;
    my $sinphi	=sin($lambda) * sin($sigma) + cos($lambda) * cos($sigma) * cos($eta);
    $grossRad  +=($sp* $sinphi * $MDGrossRadStdTAU**(1/$sinphi))->lclip(0);
  }
  $grossRad	= $grossRad->dummy(0,$lon->dim(0)) if $grossRad->ndims == 1;

  return $grossRad/$hrLen;		# MJ/m2/day
}

#######################################################################

sub netSolarRadiation	### Net energy input to surface/water
{			###	netEnergy = solar_radiation - coulds - soil heat influx
  my ($grossRad, $cloudFr, $albedo, $soilFlux) = @_;

  my $solarRad	 =  $grossRad * (0.803 - (0.34 * $cloudFr) - (0.458 * $cloudFr**2));
     $solarRad	*= (1 - $albedo)	if defined $albedo;
     $solarRad	*= (1 - $soilFlux)	if defined $soilFlux;

  return $solarRad;	# Units are same as in $grossRad
}

#######################################################################

sub saturation_pressure
{			### Water vapor saturation pressure and slope
  my $airT = shift();

  my $PressSat		= 0.61078	* condition($airT >= 0,		# Vapor saturation pressure (kPa)
	exp(17.26939*$airT / ($airT+237.3)),
	exp(21.87456*$airT / ($airT+265.5)));
  my $PressSatSlope	= $PressSat	* condition($airT >= 0,		# Slope of vapor saturation pressure
	17.26939*237.3 / ($airT+237.3)**2,				# relative to air temperature (kPa/K)
	21.87456*265.5 / ($airT+265.5)**2) if wantarray;

  return wantarray ? ($PressSat, $PressSatSlope) : $PressSat;
}

sub atm_pressure
{
  my $elevation	= shift();						# Elevation, m
  return 101.325 * (1 - 2.25577e-5*$elevation)**5.25588;		# Atmospheric pressure, kPa
}

#######################################################################

sub PET_Hamon
{
  my ($airT, $dayLen)	= @_;

  my  $rhoSat	= 2.167 * saturation_pressure($airT) / ($airT+273.15);		# Absolute humidity (kg/m3)
  return  165.1 * 2*$dayLen * $rhoSat;						# mm/day
}

#######################################################################

sub PET_PenmanMonteith		### All formulation by Dingman, 2008 - see eq. numbers in comments below
{				### plus some from FAO-56
  my($airT, $windSpeed, $cloudFr, $albedo, $rHumidity, $grossRad, $elev, $param, $LAI, $canopyHt, $canopySh) = @_;
  my $airT_lim		= $airT->clip(0, 40);					# In 0C < $airT < 40C range
  my $airT_K		= $airT+273.15;						# Kelvin
		### Water and vapor properties
  my $waterDens		= 1000;							# Water density, kg/m3
  my $evapH		= 2.5 - 0.00236 * $airT;				# Latent heat of vaporization, MJ/kg
  my $cp		= 0.001013;						# Heat capacity of air, MJ/kg/K
  my $mu		= 0.622;						# Mole ration of vapor to dry air
		### Atmosphere properties
  my($PressSat, $PressSatSlope) = saturation_pressure($airT);			# Vapor saturation & slope, kPa and kPa/K
  my $atmPressure	= atm_pressure($elev);					# Atmospheric pressure, kPa
  my $airDensity	= 100 / $airT_K / 0.288;				# Mass density of air, kg/m3
  my $rhoSat		= 2.167 * $PressSat / $airT_K;				# Absolute humidity, kg/m3
  my $DeltaRhoSat	= $rhoSat * (1 - $rHumidity);				# Absolute humidity deficit, kg/m3
  my $gamma		= $cp/$evapH/$mu * $atmPressure;			# Psychometric constant, kPa/K
  my $secDay		= 24*3600;						# Seconds per day, sec/day

  my $cloudFactor	= (0.9*$cloudFr)**3;					# See UNH weather station
  my $solarRad		= netSolarRadiation($grossRad, $cloudFactor, $albedo,	# Net solar radiation, MJ/m2/day
			    0);#  0.5*$dayLen + 0.1*(1 - $dayLen));		# Soil heat flux by SZ, fracton
  my $solarLoss		= 4.903e-9 * $airT_K**4  *				# Net outgoing long wave solar radiation
	(0.34 - 0.14*sqrt($PressSat*$rHumidity)) * (1.35 * ($solarRad+0.1)/($grossRad+0.1) - 0.35);	# MJ/m2/day
										# by FAO-56 (2018) parameterization
		### Atmospheric conductance.				# Eq.(7-49), m/day
						# Prandtl-Von Karman equation with FAO-56 (2018) parameterization
  my $windSpd2m		= $windSpeed * 4.87 / log(67.8 * 10 - 5.42);	# Convert wind speed from 10 to 2 m:
  my($z_h,$z_d,$z_0)	= (2, 0.7, 0.1);				# Atm. Conductance parameters, Eq. (7-49)
  my($Aa, $Ab)		= ($z_h/$z_0, (1 - $z_d)/$z_0);
  my $atmConduct	= $windSpd2m * $secDay / 6.25 / log($Aa/$canopyHt->lclip(0.1)+$Ab)**2; # Zm = canopyHt + $z_h (m)

		### Leaf conductance
  my $leafCondMax	= $$param{CLeafMax} * $secDay/1000;			# C*leaf in Table (7-5), m/day
								# Eqs. in Table	 (7-6). Factors for following:
  my $f_Kin		= 12.78 * $grossRad / (11.57*$grossRad + 104.4);		# Light
  my $f_dP		= (1 - 66.6 * $DeltaRhoSat)->lclip(0.232768);			# Vapor pressure deficit
  my $f_Ta		= $airT_lim * (40 - $airT_lim)**1.18 / 691;			# Leaf temperature
  my $f_sm		=  1;# 1 - 0.0011093  * exp(0.81*8.4 * (1 - $sMoistFrac));	# Leaf water content (1 for PET)
  my $leafConduct	= $leafCondMax * $f_Kin * $f_dP * $f_Ta * $f_sm;	# Eq. (7-52), m/day

		### Canopy conductance
  my $fs		=  1 - $canopySh / 3;					# Shelter factor. See eq. (7-55)
  my $canConduct	= ($fs * $LAI * $leafConduct)->lclip(0.01);		# Eq. (7-55), m/day

# my $denominator	= 1000 / ($waterDens * $evapH * ($PressSatSlope + $gamma*(1+$atmConduct/$canConduct)));
# my $radiationTerm	= $PressSatSlope*($solarRad-$solarLoss)				/ $denominator;
# my $windTerm		= $airDensity*$cp*$atmConduct*$PressSat*(1-$rHumidity)	/ $denominator;
# printf "  Avg Rad_PET = %.2f : Avg Wind_PET = %.2f\n", $radTerm->avg, $windTerm->avg;
# return $radiationTerm + $windTerm;

		### PET by Penman-Monteith. Equation (7-56), mm/day
  return (($PressSatSlope*($solarRad-$solarLoss) + $airDensity*$cp*$atmConduct*$PressSat*(1-$rHumidity)) /
	  ($waterDens * $evapH * ($PressSatSlope + $gamma*(1+$atmConduct/$canConduct))) * 1000)->lclip(0); # mm/day
}

#######################################################################

sub PET_FAO_56		### FAO-56 PET method
{
  my($airT, $windSpeed, $cloudFr, $albedo, $rHumidity, $grossRad, $elev, $param, $LAI) = @_;
  my($Cn, $Cd)		= (900,  0.34);						# FAO coefficients for short grass
# my($Cn, $Cd)		= (1600, 0.38);						# FAO coefficients for tall  grass
  my $Corr		= 2 * 10/$LAI;		# Crop to Natural C_can ratio (correction):
						# where 2 is stomata ratio; 10 is max(LAI) for reference crop
		### Water and vapor properties
  my $evapH		= 2.5 - 0.00236 * $airT;				# Latent heat of vaporization, MJ/kg
  my $cp		= 0.001013;						# Heat capacity of air, MJ/kg/K
  my $mu		= 0.622;						# Mole ration of vapor to dry air
		### Atmosphere properties
  my($PressSat, $PressSatSlope) = saturation_pressure($airT);			# Vapor saturation & slope, kPa and kPa/K
  my $atmPressure	= atm_pressure($elev);					# Atmospheric pressure, kPa
  my $PressH2O		= $PressSat * $rHumidity;				# Vapor pressure, kPa
  my $windSpd2m		= $windSpeed * 4.87 / log(67.8 * 10 - 5.42);		# Convert wind speed from 10 to 2 m:
  my $airT_K		= $airT+273.15;						# Kelvin

		### Penman-Monteith equation terms by FAO-56 handbook
  my $delta		= $PressSatSlope;					# Slope of staturation vapor pressure
  my $gamma		= $cp/$evapH/$mu * $atmPressure;			# Psychometric constant, kPa/K
  my $denominator	= $delta + $gamma*(1+$Cd*$windSpd2m*$Corr);		# Non-crop correction; Allen et al., 1996
  my $DT		= $delta/$evapH	/ $denominator;				# Delta      term for Radiation component
  my $PT		= $gamma	/ $denominator;				# Psi        term for Wind      component
  my $TT		= $windSpd2m * ($Cn / $airT_K);				# Tempeature term for Wind      component

		### Wind component
  my $ET_wind		= $PT * $TT  * ($PressSat - $PressH2O);			# mm/day

		### Radiation component
  my $cloudFactor	= (0.9*$cloudFr)**3;					# See UNH weather station
  my $solarRad		= netSolarRadiation($grossRad,$cloudFactor,$albedo,0);	# Net solar radiation, MJ/m2/day
  my $solarLoss		= 4.903e-9 * $airT_K**4  *				# Net outgoing long wave solar radiation
	(0.34 - 0.14*sqrt($PressSat*$rHumidity)) * (1.35 * ($solarRad+0.1)/($grossRad+0.1) - 0.35);	# MJ/m2/day
										# by FAO-56 (2018) parameterization
  my $ET_rad		= $DT * ($solarRad-$solarLoss);				# mm/day

# printf "  Avg Rad_PET = %.2f : Avg Wind_PET = %.2f\n", $ET_rad->avg, $ET_wind->avg;
  return $ET_wind + $ET_rad;							# mm/day
}

#######################################################################

sub get_relHumidity		### http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html
{					# Relative humidity cannot and not allowed to be < 10 %
  my ($argument, $airT, $elev)	= @_;
  my  $humidity_in		= read_dateLayer(@$argument);	# Read the input

	### Case of Relative Humidity, in % units
  if    ($$argument[0]->{MT_attrib}{Units} eq '%') {
    return ($humidity_in/100)->lclip(0.1);
  }					# https://www.engineeringtoolbox.com/air-altitude-pressure-d_462.html
	### Case of Specific Humidity, in kg/kg	units
  elsif ($$argument[0]->{MT_attrib}{Units} eq 'kg/kg') {
    my $atmPress=  atm_pressure($elev);					# Atmospheric pressure, kPa
    return (($humidity_in * $atmPress / (0.378*$humidity_in + 0.622)) /	# Vapor prressure, KPa
		saturation_pressure($airT))->clip(0.1, 1);		# Saturation  pressure, KPa
  }					# https://journals.ametsoc.org/view/journals/bams/86/2/bams-86-2-225.xml?tab_body=pdf
	### Case of Dewpoint Tempeature	# see eq (3)
  elsif ($$argument[0]->{MT_attrib}{Units} eq 'C') {
    return (	saturation_pressure($humidity_in) /			# Vapor prressure, KPa
		saturation_pressure($airT))->clip(0.1, 1);		# Saturation  pressure, KPa
  }
  else { die "\nUnknown case for relative humidity calculation. Aborting...\n\n"; }
}

#######################################################################

sub calc_openWaterEvap		### Evaporation from Water Surfaces
				### http://www.engineeringtoolbox.com/evaporation-water-surface-d_690.html
{
  my($airT, $windSpeed, $cloudFr, $albedo, $rHumidity, $grossRad, $elev, $param) = @_;

  my $atmPress	= atm_pressure($elev);						# Atmospheric pressure, kPa
  my $PressSat	= saturation_pressure($airT);					# Saturation  pressure, KPa
  my $PressH2O	= $PressSat * $rHumidity;					# Vapor pressure, kPa
  my $satrHum	= 0.62198   * $PressSat / ($atmPress - 0.378*$PressSat);	# Saturation  humidity, kg/kg
  my $specHum	= 0.62198   * $PressH2O / ($atmPress - 0.378*$PressH2O);	# Specific    humidity, kg/kg
  return condition($airT<2, 0, (25 + 19*$windSpeed)*($satrHum - $specHum)*24);	# Water evaporation,    mm/day
}

#######################################################################

sub scale_MinMax
			### Scale between min and max value
{
  my ($dataset,$runSet)		= @_;
  my ($runIO,  $meta, $extent)	= @$runSet;
  my  $spool_dir		= $$runIO{spool}.$dataset->{ID}.'_MinMax';
  unless (-e $spool_dir) { mkpath($spool_dir,0,0775) or die "Cannot create-\n$spool_dir\n"; }

		### Check if MinMax needs to be calculated
  opendir( DIR_HANDLE, $spool_dir ) || die "failed to create directory handle to $spool_dir: $!";
    my @files	= sort grep(m/\.dat$/i, readdir(DIR_HANDLE));
  closedir DIR_HANDLE;
  return map("$spool_dir/$_", @files) if scalar(@files) == 365;

  my @jDay	= map sprintf("%04d-%02d-%02d",inverse_julian_day(2451911+$_)), 0..364;
  my @data	= map read_dateLayer($dataset, $_, $extent, $$runIO{spool}, {PATCH_VALUE=>0}), @jDay;

  my %dSize	= ('float' => 4, 'double' => 8);
  my $pSize	= $data[0]->nelem * $dSize{$data[0]->type} * 365;
  my($data_min, $data_max);

  if ($pSize > 7.5e8) {		### Avoiding set "$PDL::BIGPDL = 1;"
    $data_min	= zeroes($data[0]->type, $data[0]->dims) + 1e16;
    $data_max	= zeroes($data[0]->type, $data[0]->dims) - 1e16;
    foreach my $data (@data) { $data_min = condition($data_min < $data, $data_min, $data); }
    foreach my $data (@data) { $data_max = condition($data_max > $data, $data_max, $data); }
  }
  else {
    my $data	= pdl(@data)->reorder(2,0,1);
    $data_min	= $data->minimum->copybad($data[0]);
    $data_max	= $data->maximum->copybad($data[0]);
  }
		### Write MinMax to spool files
  foreach my $day (0 .. 364) {
    my $data	= (($data[$day]-$data_min)/($data_max-$data_min))->setnonfinitetobad->setbadtoval(1)->clip(0,1);
    my $file	= sprintf "$spool_dir/%03d.dat", $day+1;
    $files[$day]= $file;
    writeflex(    $file, $data);	$$extent{spool_writes}++;
  }

  return @files;
}

#######################################################################

sub flowGeometry
	### Riverbed geometry based on streamflow
{
  my ($cDsch,$mDsch) = @{shift()};		### Instantaneous and LTM discharge
  my ($eta,  $tau, $vDelta, $nu, $phi, $vEpsilon, $aas_f, $aas_b, $aas_m) = @{shift()};
	#####   Stream Depth and Width (Park, 1977)
  my $yMean	= $eta   * $mDsch**$nu;		### Long term average Stream depth, m
  my $wMean	= $tau   * $mDsch**$phi;	### Long term average Stream width, m
  my $vMean	= $vDelta* $mDsch**$vEpsilon;	### Long term average Stream velocity, m/sec

  my $yStream	= $yMean * ($cDsch/$mDsch)**$aas_f;		### Instantaneous depth, m
  my $wStream	= $wMean * ($cDsch/$mDsch)**$aas_b;		### Instantaneous width, m
  my $vStream	= $vMean * ($cDsch/$mDsch)**$aas_m;		### Instantaneous Velocity, m/sec

  return $yMean, $wMean, $vMean, $yStream, $wStream, $vStream;
}

#######################################################################

sub streamGeometry
	### Combine Reservoir and Streamflow geometry
{
  my ($yStream, $wStream, $lStream, $vStream, $damData, $resStorage, $minRsvStrg, $discharge, $jdate) = @_;

		### Cumulative reservoir area, and capacity
  my  ($area, $rCap) = ($zeroes->copy, $zeroes->copy);
  map {$area(@$_[3,4])+=$$_[9] if $jdate >= $$_[5] && $jdate < $$_[6]} @$damData;
  map {$rCap(@$_[3,4])+=$$_[8] if $jdate >= $$_[5] && $jdate < $$_[6]} @$damData;
       $area->where($area & ($rCap < $minRsvStrg)) .= 0;	### Skip too small dams

		### Reservoir/Stream geometry, in meters
	### Assuming reservoir Aspect Ratio = 2; Reservoir/River min geometry = 0.01 m
  my $depth	= condition_slice($area, $resStorage / $area,		$yStream)->lclip(0.01);
  my $width	= condition_slice($area, sqrt(0.5*$area),		$wStream)->lclip(0.01);
  my $length	= condition_slice($area, sqrt(2.0*$area),		$lStream)->lclip(0.01);
  my $velocity	= condition_slice($area, $discharge/$depth/$width,	$vStream)->lclip(1e-6); # m/sec

  return $depth, $width, $length, $velocity;
}

#######################################################################

sub endoLakeGeometry
		### Geometry of endorheic lakes
{
  my ($volume, $slope)	= @_;

  return 0;			# Removed from public domain
}

#######################################################################

sub snowBands_init
{
  my ($file, $lapse, $snowFall, $snowMelt, $lat, $glArea) = @_;
 (my  $fileBase	= $file) =~ s/\.\w+$//;

		### Check existance of the source files
  die "Elevation band data not found..." unless (-e "$fileBase.elvBands.dat"
	&& -e "$fileBase.elvAverg.dat" && -e "$fileBase.elvBands.txt");

		### Check memory size of bands piddle to use BIGPDL
  my $mSize	= $1*($2 eq 'KB' ? 1e3:1e6) if $glArea->double->info('%M') =~ m/([\d\.]+)(\wB)$/;
  $PDL::BIGPDL	=  1 if !$PDL::BIGPDL && $mSize * 20 > 7.5e8;	# Assuming number of snowbands = 20

		### Read raw elevation band data
  my $elevBand	= readflex(  "$fileBase.elvBands.dat");		# m
  my $elevAvrg	= readflex(  "$fileBase.elvAverg.dat");		# m
  my($h, @band)	= read_table("$fileBase.elvBands.txt");
  my $elevGrid	= pdl(map(($$_[$$h{Min}]+$$_[$$h{Max}])/2,@band))
	->dummy(0,$$extent{ncols})->dummy(1,$$extent{nrows})->copybad($elevBand);
  my $airTBand	= $lapse/1000 * ($elevGrid - $elevAvrg);

		### Remove glacier area from high elevation bands
  if ($glArea->sum) {
    for (my $i=$#band; $i==0; $i--) {
      my $band	  = $elevBand(,,($i));
      my $delta	  = $glArea->clip(0,$band)->setbadtoval(0);
	 $band	 -= $delta;
	 $glArea -= $delta;
      }
    $elevBand->inplace->setvaltobad(0);
    $elevBand = $elevBand / $elevBand->mv(2,0)->sumover;	# Re-normilize elevation bands
  }

		### Process elevation band data for WBM use
  my (@indBandN,@indBandS,@elevBand,@airTBand,@snowBand,@snowFall,@snowMelt);
  my  @mask	= map $elevBand(,,($_))->isgood,0..$#band; # whichND: list context deprecated-
  my  @ind	= map $_=whichND($mask[$_]),	0..$#band; # So, must use $_=whichND(...)
  for (my $i=0; $i<=$#band; $i++) {
    push @elevBand, $elevBand(,,($i))	->indexND($ind[$i])->sever;
    push @airTBand, $airTBand(,,($i))	->indexND($ind[$i])->sever;
    push @snowBand, $zeroes		->indexND($ind[$i])->sever;
    push @snowFall, $snowFall		->indexND($ind[$i]);
    push @snowMelt, $snowMelt		->indexND($ind[$i]);
	last if $i==$#band;
    my $stack	= $mask[$i] & $mask[$i+1];
    my $stackN	= $stack & ($lat > 0);
    my $stackS	= $stack & ($lat < 0);
    push @indBandN,[$stackN->indexND($ind[$i])->sever,$stackN->indexND($ind[$i+1])->sever];
    push @indBandS,[$stackS->indexND($ind[$i])->sever,$stackS->indexND($ind[$i+1])->sever];
  }

  return \@ind,\@indBandN,\@indBandS,\@elevBand,\@airTBand,\@snowBand,\@snowFall,\@snowMelt;
}

#######################################################################

sub snow_balance
{
  my ($snowPack,$airT,$precip,$snowFallThreshold,$snowMeltThreshold, $subDf) = @_;

  my $snowFall  = ($airT < $snowFallThreshold) * $precip;
  my $snowMelt  =(($airT > $snowMeltThreshold) *
	(2.63 + 2.55*$airT + 0.0912*$airT*$precip))->hclip($snowPack);
  my $sPackChg  = $snowFall - $snowMelt;
     $snowPack += $sPackChg * $subDf;		### Snow pack is updated here

  return $snowFall, $snowMelt, $sPackChg;
}

#######################################################################

sub shift_snowBands
{
  my ($band, $frac, $ind) = @_;

  for (my $i=0; $i<=$#$ind; $i++) {
    my $band_L = $$band[$i  ]->where($$ind[$i][0]);
    my $band_H = $$band[$i+1]->where($$ind[$i][1]);
    my $frac_L = $$frac[$i  ]->where($$ind[$i][0]);
    my $frac_H = $$frac[$i+1]->where($$ind[$i][1]);
		### Shift snowbands downslope to avoid infinite accumulation
    $band_L += $band_H * $frac_H/$frac_L;
    $band_H *= 0;
  }
}

#######################################################################

sub update_snowBands
{
  my($totalChg, $band, $frac, $ind) = @_;
  my $snowAdd    = $totalChg * ($totalChg > 0);
  my $snowRemove = $totalChg * ($totalChg < 0);

		### Add total routed snow to the bottom snow band
  for (my $i=0; $i<=$#$ind; $i++) {
    my $add	 = condition_slice($$frac[$i], $snowAdd->indexND($$ind[$i]) / $$frac[$i], 0);
    $$band[$i]	+= $add;
    $snowAdd->indexND($$ind[$i]) -= $add * $$frac[$i];
  }
		### Remove total routed snow from top snow bands
  for (my $i=$#$ind; $i>=0; $i--) {
    my $remove	 = $snowRemove->indexND($$ind[$i])->lclip(-$$band[$i] * $$frac[$i]);
    $$band[$i]	+= condition_slice($$frac[$i], $remove / $$frac[$i], 0);
    $snowRemove->indexND($$ind[$i]) -= $remove;
  }
}

#######################################################################

sub snowBandBalance
{
  my ($band, $frac, $ind, $sPack) = @_;

  my ($sBndPack, $sPackSum) = ($zeroes->copy, $sPack->sum);
  map $sBndPack->indexND($$ind[$_]) += $$band[$_]*$$frac[$_], 0 .. $#$ind;
  my  $delta	= $sPackSum - $sBndPack->sum;
  die sprintf("\tProblem: Snow Band Balance = %f\n", $delta) if abs($delta/$sPackSum) > 1e-6;
}

#######################################################################

sub lapseDownscale_init
{
  my ($runSet, $MT)		= @_;
  my ($runIO,  $meta, $extent)	= @$runSet;
  my  $lapse	= -6.49;		# C/km - Default temperature lapse rate per km of altitude.
					# Reference: Rennick, M. A. (1977). "Parameterization of Tropospheric Lapse Rates in Terms of Surface-Temperature." Journal of the Atmospheric Sciences 34(6): 854-862.
  return ($lapse, undef) unless $$runIO{lapseDownscale};

  my %param	= read_param_str($runIO, 'lapseDownscale');

  die	"\"lapseDownscale\" cannot be used with Secondary temperature dataset. Aborting...\n\n"
		if exists $$runIO{MT_airT}{Secondary} && !$param{forceSecTemp};
  die	"Error in \"lapseDownscale\" input key (must be a hash block statement):\n",
	"   \"climElevation\" and \"netElevation\" inputs are required for lapse downscaling. Aborting...\n\n"
		unless defined $param{climElevation} && defined $param{netElevation};
  $lapse	= $param{lapseRate} if isNumber($param{lapseRate});

	# Check temperature climate driver extent to match grid of its elevation dataset
  my($t_str, $l_str, $n_str) = ($$runIO{MT_airT}{Primary}, $param{climElevation}, $param{netElevation});
  my $t_extent	=
	isNumber($t_str) ? die("\"MT_airT\" cannot be a number with lapseDownscale option. Aborting...\n\n") :
	get_extent(GDAL_FileCheck($t_str) ? $t_str : (new RIMS::DataSet(attrib($t_str,$MT)))->{fileList}[0][0]);
  my $l_extent	=
	isNumber($l_str) ? die("\"lapseDownscale\" cannot be a number. Aborting...\n\n") :
	get_extent(GDAL_FileCheck($l_str) ? $l_str : (new RIMS::DataSet(attrib($l_str,$MT)))->{fileList}[0][0],
		$$t_extent{projection});
  die "Mismatch in \"lapseDownscale\" and \"MT_airT\" dataset spatial resolutions. Aborting...\n\n"
	if  abs($$t_extent{cellsizeX} - $$l_extent{cellsizeX}) > 0.0001 * abs($$t_extent{cellsizeX}) ||
	    abs($$t_extent{cellsizeY} - $$l_extent{cellsizeY}) > 0.0001 * abs($$t_extent{cellsizeY});

	# Check Network elevation extent to match grid of the Network itself (flow direction)
  my $n_extent	=
	isNumber($n_str) ? die("\"netElevation\" cannot be a number. Aborting...\n\n") :
	get_extent(GDAL_FileCheck($n_str) ? $n_str : (new RIMS::DataSet(attrib($n_str,$MT)))->{fileList}[0][0]);
  die "Mismatch in \"netElevation\" and \"Network\" dataset spatial resolutions. Aborting...\n\n"
	if abs($$extent{cellsizeX} - $$n_extent{cellsizeX}) > 0.0001 * $$n_extent{cellsizeX} ||
	   abs($$extent{cellsizeY} - $$n_extent{cellsizeY}) > 0.0001 * $$n_extent{cellsizeY};

		### Read data
  my $elevPixel	= read_Layer($extent,$meta,$param{netElevation}, $MT,{RESAMPLE => 0, PATCH_VALUE => 0});  # m
  my $elevClimt	= read_Layer($extent,$meta,$param{climElevation},$MT,{RESAMPLE => 1, PATCH_VALUE => 0});  # m

  return $lapse, $lapse/1000 * ($elevPixel - $elevClimt);
}

#######################################################################

sub stock_init
{
  my ($hdr,@data) = read_table(shift());
  my %LiveStock;

  my $cCol = delete $$hdr{Stock};
  foreach my $row (@data) {
    $LiveStock{$$row[$cCol]}	= {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
  }

  return %LiveStock;
}

#######################################################################

sub livestock_demand
{
  my ($LiveStock, $temperature) = @_;
  my  $demand = 0;

	## Units ##########
	### density comes in number per km2
	### cell area comes in km2

  foreach my $stock (keys %$LiveStock) {
    $demand += ($$LiveStock{$stock}{SlopeValue} * $temperature->lclip(0) +
		$$LiveStock{$stock}{InterceptValue} + $$LiveStock{$stock}{ServiceWater}) * 1e-6 * $$LiveStock{$stock}{Density};	# in mm
  }

  return $demand;
}

#######################################################################

sub runoffMask_init
{
  my ($runSet, $runAttr, $list_out, $noOutput) = @_;
  my ($runIO,  $meta, $extent)	= @$runSet;

  return unless $$runIO{Runoff_mask};
  print "Initialization of datasets for Runoff Masks:\n";

  my @varOut = qw(dischMsk runoffMsk grndWtMsk);
  die "\nNo Runoff Masks output variables are listed (choices are: dischMsk, runoffMsk, grndWtMsk). Aborting...\n\n"
	unless List::Util::sum(map($_~~@$list_out, @varOut));

  my (@mask, @ID);
  my %maskParams = read_param_str($runIO, 'Runoff_mask');

	###############################################################
	### Case of master mask file where pixels contain mask IDs
  if (exists $maskParams{attFile}) {
    die "Runoff mask file was not found...\n" unless -e $maskParams{attFile};

    my %attr	  = read_table_hash($maskParams{attFile},$maskParams{colID});
    my $maskMaster= read_GDAL($extent,$meta,0,$maskParams{maskFile},1,-9999);	# nearest neighbor
    my $maskID	  = $maskMaster->uniq;				# List of IDs from master mask
    my $attrID	  = %attr ? pdl(keys %attr) :	$maskID;	# List of IDs from attributes
    my $ID	  = $attrID->intersect(		$maskID);	# List of IDs in both above
		### Filtering mask IDs by watershed mask, e.g. countries that share watersheds
    if (exists $maskParams{basinIDFile}) {
      die "Basin ID file was not found...\n" unless -e $maskParams{basinIDFile};
      my $basinID = read_GDAL($extent,$meta,0,$maskParams{basinIDFile},1,-9999);	# nearest neighbor
      my $crossID = pdl [];
      foreach my $id ($basinID->uniq->list) {
	my $list  = $maskMaster->where(($basinID == $id) & ($maskMaster != -9999))->uniq;
	$crossID  = $crossID->copy->append($list) if $list->nelem > 1;
      }
      $ID = $ID->intersect($crossID->uniq);			# List of IDs filtered by watershed mask
    }
		### Build mask attributes
    @ID	  = map [$_, (%attr ? $attr{$_}{$maskParams{colName}} : $_)], $ID->list;
    $maskMaster= condition($maskMaster->in($ID), $maskMaster,-9999);	# Mask-out non-listed IDs
    push @ID,[-9999,'Unknown'] if ($attrID == -9999)->sum;	# Add "Unknown" to the list of polygons
		### Build individual polygon masks
    @mask = map(($maskMaster == $$_[0]), @ID);
    map map(s/-9999/NA/,@$_), @ID;	# Replace ID -9999 with NA for "Unknown" areas (if any)
  }
	###############################################################
	### Case if idividual mask files where pixels contain fractions
  elsif (exists $maskParams{maskList}) {
    die "Runoff mask attribute file was not found...\n" unless -e $maskParams{maskList};

    my($h,@attr)= read_table($maskParams{maskList});
    die "There is only one fraction mask in the set which has to have at least two masks. Aborting...\n\n" unless $#attr;
    my $maskSum	= zeroes($$extent{ncols}, $$extent{nrows});
    foreach my $attrib (@attr) {
      my $data	= read_GDAL($extent,$meta,1,$$attrib[$$h{$maskParams{ColFile}}],1,0); # bi-linear; patch_val=0
		### Set individual polygon masks
      if (($data > 0)->sum) {
	$maskSum += $data;
	push @mask, $data;
	push @ID,  [$$attrib[$$h{$maskParams{colID}}], $$attrib[$$h{$maskParams{colName}}]];
      }
    }
    if (($maskSum == 0)->sum) {	# No any mask values in these pixels (it should never happen)
	push @mask, $maskSum == 0;
	push @ID,  ['NA', 'NA'];
    }
		### Normalize masks
    foreach my $i (0..$#mask) { $mask[$i] /= $maskSum; }
  }
	###############################################################
  else { die "Unknown input file(s) in \"Runoff_mask\" option...\n" }
  printf "\tNumber of Runoff Masks started = %d\n",scalar(@mask);

		### Check mask piddle size and set $PDL::BIGPDL if needed
  my $mSize	= $1*($2 eq 'KB' ? 1e3:1e6) if $mask[0]->double->info('%M') =~ m/([\d\.]+)(\wB)$/;
  $PDL::BIGPDL	=  1 if !$PDL::BIGPDL && $mSize * scalar(@mask) > 7.5e8;

	###############################################################
	### Prepare output variables
  my @varADD;
  foreach my $varOut (@varOut) {
    $$runIO{Output_vars} =~ s/\s*$varOut//;
    next unless  $varOut ~~ @$list_out;

    my ($idx)	= grep $varOut eq $$list_out[$_], 0 .. $#$list_out;	# Find index of the $varOut
    splice @$list_out, $idx, 1;						# Remove it from the @$list_out
    my @varAdd	= map $varOut."_$$_[0]", @ID;				# and replace it with its set
    push @$list_out, @varAdd;
    push @varADD,    @varAdd;
  }
	### Add output variables to the Local MT
  make_dataCube($runIO, $runAttr, \@varADD, \@ID) unless $noOutput;

  return pdl($mask[0]->type,@mask), \@ID, set_default($maskParams{model}, 0);
}

#######################################################################
###############   Irrigation  Functions   #############################

sub irrCycle_init
{
  my($runIO, $list_out, $noOutput) = @_;
  my %param	= read_param_str($runIO,'wbmParam');
  my $nIrr	= set_default($param{nIrrTracking}, 1);
  return $nIrr	if $nIrr == 1;

	### Prepare output variables relevant to irr water reuse cycles
  my @cycle	= (1 .. $nIrr);
  my @vars	= (@$list_out);		# Must be a copy to avoid infinite loop below
  my @varADD;
  foreach my $varOut (@vars) {
    next  if $varOut !~ m/_irr$/;

    my @varAdd	= map  $varOut."_$_", @cycle;
    my ($idx)	= grep $varOut eq $$list_out[$_], 0 .. $#$list_out;	# Find index of the $varOut
    splice @$list_out, $idx, 0, @varAdd;				# Add its list to @$list_out
    push   @varADD,    @varAdd;
    $$runIO{Output_vars} =~ s/$varOut/join(' ',$varOut,@varAdd)/e;
  }
	### Add output variables to the Local MT
  make_dataCube($runIO, $runAttr, \@varADD) if @varADD && !$noOutput;

  return $nIrr;
}

#######################################################################

sub land_init
	### Initialization of Land Cover (including crops) parameters
{
  my ($runIO, $meta, $extent) = @_;
  my  $MagicT = [$$runIO{Input_MT}, $$runIO{Output_MT}]; # [Global,Local] Magic Table files

  print "\nInitialization of Land Cover (including crops) parameters-\n";

	############   Initialization of General Land   ###############
  my $rootingDepth = read_Layer($extent,$meta,$$runIO{rootingDepth},$MagicT,{PATCH_VALUE=>1000, KEY=>'rootingDepth'});
	### awDelta come directly from soilAWCapacity (clipping to lowest value of 1 %)
	### otherwise calculated from FieldCap & WiltingPoint
  my $awDelta      = ($$runIO{soilAWCapacity} ?
	read_Layer($extent,$meta,$$runIO{soilAWCapacity},$MagicT,{PATCH_VALUE=>0})->lclip(10)/1000 : # Convert mm/m to frac
	read_Layer($extent,$meta,$$runIO{FieldCap},	 $MagicT,{PATCH_VALUE=>0.25}) -
	read_Layer($extent,$meta,$$runIO{WiltingPoint},	 $MagicT,{PATCH_VALUE=>0.10}))->lclip(0);

  my %land = ('generalLand' => {'land' => {
	'awCap'		=> ($awDelta*$rootingDepth)->lclip(0),	# Must be positive
	'LandFraction'	=> ones(double,$$extent{ncols},$$extent{nrows})}});

	###############################################################
	############   Check land collapse method   ###################
  my $collapse_method	= 'none';
  if ($$runIO{landCollapse} && ($$runIO{LandCover} || $$runIO{Irrigation})) {
    die "Unknown collapse method. Aborting...\n\n" unless $$runIO{landCollapse} =~ m/avg|average|one|dominant/i;
    $collapse_method	= $$runIO{landCollapse} =~ m/avg|average/i ? 'average' : 'dominant';
  }
  print "Landcover/Cropland collapse method     : $collapse_method\n";

	###############################################################
	############   Initialization of Land Cover types   ###########
  my $ldFr = 'LandFraction';
  my $CDF  = 'CropDepletionFactor';

  if ($$runIO{LandCover}) {
    delete $land{generalLand};		### Land Cover takes over the General land category
# 					### Presently "LandCover" cannot work together with "Irrigation"
#     die "Presently \"LandCover\" cannot work together with \"Irrigation\"..." if $$runIO{Irrigation};

    print "Initialization of Land Cover parameters:\n";
    my %landCover;
		### Read Land Cover parameter inputs
    my ($hdr,@data) = read_table($$runIO{LandCover});
    my $cCol = delete $$hdr{Description};
    foreach my $row (@data) {
      $landCover{$$row[$cCol]}	= {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
    }

		### Populate Land Cover Parameters
    foreach my $lc (map $$_[$cCol],@data) {
      print "\tLandCover = $lc\n";

      $landCover{$lc}{$ldFr}	= read_GDAL($extent,$meta,0,$landCover{$lc}{FractionFile},1,0);
      $landCover{$lc}{awDelta}	= $awDelta;
      $landCover{$lc}{awCap}	= $awDelta * $landCover{$lc}{RootDepth};

		### Minimum & Maximum Crop Coefficient (Kc)
      my $meteoFactor        = 1.0;		# This is from Rens v. B.
      $landCover{$lc}{KcMin} = 0.2;
      $landCover{$lc}{KcMax} = 1 + 0.1 * List::Util::min(2, $landCover{$lc}{VegetationHeight}) +
	$meteoFactor * ($landCover{$lc}{VegetationHeight} / 3)**0.3;
    }
		### Add land cover hash to the land cover category hash
    $land{landCover} = \%landCover;

		#########################################
		### Collapsing number of land cover types
    if (  $collapse_method ne 'none') { print "\tCollapsing Landcovers...\n";
      if ($collapse_method eq 'dominant') {
	my $id = dominant_Land($land{landCover});
	write_tif($extent, 'Int16', $$runIO{spool}.'landCoverOneID.tif', $id);
      }
			### Parameters required for collapsing/averaging of land cover types!!!
      my @required = qw/KcMin KcMax awDelta awCap RootDepth MaxLAI/;	# NB!!!
								  ### Croplands stay!!!
      foreach my $tp (keys %{$land{landCover}}) {		next if $tp  =~ /crop/i;
    map $land{landCover}{land}{$_}    += $land{landCover}{$tp}{$ldFr} * $land{landCover}{$tp}{$_}, @required;
	$land{landCover}{land}{$ldFr} += $land{landCover}{$tp}{$ldFr};
	delete $land{landCover}{$tp};		### Delete collapsed land cover types
      }
			### Calculate as an weighted average
      map $land{landCover}{land}{$_} = ($land{landCover}{land}{$_} / $land{landCover}{land}{$ldFr})
		  ->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask}), @required;
    }
    print "\n";
  }

	###############################################################
	############   Initialization of Crop Types   #################

  if ($$runIO{Irrigation}) {
    print "Initialization of Irrigation parameters:\n";

		### Read crop parameter inputs
    my %irrParam= read_param_str($runIO, 'Irrigation');
    my %crop	= read_land_param($irrParam{CropParFile},	['Land_ID','Group']);
    my %cPch	= read_land_param($irrParam{CropParFilePatch},	['Land_ID','Group'])	if $irrParam{CropParFilePatch};

		#######################################################
		### Populate Crop Parameters

    my $sfx	= $collapse_method eq 'average' ? '_av' :'';
    my $ldFrKey	= $collapse_method ne 'dominant' ? 'LandFracTS' : 'LandFracTS_Dom';

    foreach my $group ("Irrigated$sfx","Rainfed$sfx") { foreach my $crp
	(sort {$crop{$group}{$a}{Priority} <=> $crop{$group}{$b}{Priority}} keys %{$crop{$group}}) {

      printf("\tCROP %-12s => $crp\n",$group);

		### Crop land fraction within general cropland
      $crop{$group}{$crp}{$ldFr}	=
	new RIMS::DataSet(attrib($crop{$group}{$crp}{$ldFrKey},		$MagicT));
			### Patch
      $crop{patch}{$group}{$crp}{$ldFr}	=
	new RIMS::DataSet(attrib($cPch{$group}{$crp}{$ldFrKey},		$MagicT))	if %cPch;
      push @{$$runIO{spool_list}}, [$crop{$group}{$crp}{$ldFr},
	{PATCH_VALUE=>$crop{patch}{$group}{$crp}{$ldFr}, RESAMPLE=>0,PPATCH_VALUE=>0}];

		### Crop soil moisture capacity
      $crop{$group}{$crp}{awCap}	= ($collapse_method ne 'average') ?
	($awDelta*$crop{$group}{$crp}{RootDepth})->lclip(0) :
	new RIMS::DataSet(attrib($crop{$group}{$crp}{RootDepth},	$MagicT));
			### Patch
      $crop{patch}{$group}{$crp}{awCap}	=
	new RIMS::DataSet(attrib($cPch{$group}{$crp}{RootDepth},	$MagicT))	if %cPch && $collapse_method eq 'average';
      push @{$$runIO{spool_list}}, [$crop{$group}{$crp}{awCap},
	{PATCH_VALUE=>$crop{patch}{$group}{$crp}{awCap}, RESAMPLE=>0,PPATCH_VALUE=>0}] if ref($crop{$group}{$crp}{awCap}) ne 'PDL';
		### Crop Coefficients (Kc)
      $crop{$group}{$crp}{Kc}		=
	new RIMS::DataSet(attrib($crop{$group}{$crp}{Kc_TS},		$MagicT));
			### Patch
      $crop{patch}{$group}{$crp}{Kc}	=
	new RIMS::DataSet(attrib($cPch{$group}{$crp}{Kc_TS},		$MagicT))	if %cPch;
      push @{$$runIO{spool_list}}, [$crop{$group}{$crp}{Kc},
	{PATCH_VALUE=>$crop{patch}{$group}{$crp}{Kc}, RESAMPLE=>0,PPATCH_VALUE=>0}];

      if ($group =~ m/Irrigated/i) {
		### Crop Depletion Factor
	$crop{$group}{$crp}{crpDF}	= isNumber($crop{$group}{$crp}{$CDF}) ?
	  $crop{$group}{$crp}{$CDF} * ones(double,$$extent{ncols},$$extent{nrows}) :
	  new RIMS::DataSet(attrib($crop{$group}{$crp}{$CDF},		$MagicT));
			### Patch
	$crop{patch}{$group}{$crp}{crpDF} =
	  new RIMS::DataSet(attrib($cPch{$group}{$crp}{$CDF},		$MagicT))	if %cPch &&
									!isNumber($crop{$group}{$crp}{$CDF});
	push @{$$runIO{spool_list}}, [$crop{$group}{$crp}{crpDF},
	  {PATCH_VALUE=>$crop{patch}{$group}{$crp}{crpDF}, RESAMPLE=>0,PPATCH_VALUE=>0}]
						if ref($crop{$group}{$crp}{crpDF}) ne 'PDL';
		### Sorted list of irrigated crops
	push @{$crop{$group}{cropList}}, $crp;	# "cropList" key must be deleted after this function returns
      }
    }}
		### General cropland fraction
    my $patch_val = undef;
    if ($irrParam{CropAreaFracPatch}) {						### Get patch value or dataset
      my $patch	= init_DSet($extent,$meta,$irrParam{CropAreaFracPatch},	$MagicT,
	{RESAMPLE=>0, DATASET_OPT=>{START_YEAR_CLIP=>1800}});				# Use "near" resample
      if (ref($patch) =~ m/DataSet/) { $crop{patch}{CropAreaFrac} = $patch; }
      else			     { $patch_val		  = $patch; }
    }
    $crop{allCropFr} = init_DSet($extent,$meta,$irrParam{CropAreaFrac},	$MagicT,
	{RESAMPLE=>0, DATASET_OPT=>{START_YEAR_CLIP=>1800}, PATCH_VALUE=>$patch_val});	# Use "near" resample

		### Fallow land fraction within general cropland
    $crop{fallow}{land}{awCap}	= ($awDelta*$rootingDepth)->lclip(0);
    $crop{fallow}{land}{$ldFr}	= new RIMS::DataSet(attrib($crop{Fallow}{Fallow}{$ldFrKey}, $MagicT));
			### Patch
    $crop{patch}{fallow}{land}{$ldFr} = new RIMS::DataSet(attrib($cPch{Fallow}{Fallow}{$ldFrKey}, $MagicT)) if %cPch;
    push @{$$runIO{spool_list}}, [$crop{fallow}{land}{$ldFr},
	{PATCH_VALUE=>$crop{patch}{fallow}{land}{$ldFr}, RESAMPLE=>0,PPATCH_VALUE=>0}];

		### Add land cover hash to the land cover category hash
    $land{allCropFr}		= $crop{allCropFr};		### All crops (land fraction only)
    $land{irrCrop}		= $crop{"Irrigated$sfx"};	### Irrigated crops
    $land{rfdCrop}		= $crop{"Rainfed$sfx"};		### Rain fed crops
    $land{fallow}		= $crop{fallow};		### Fallow crop land
    if (%cPch) {
      $land{patch}{irrCrop}	= $crop{patch}{"Irrigated$sfx"};
      $land{patch}{rfdCrop}	= $crop{patch}{"Rainfed$sfx"};
      $land{patch}{fallow}	= $crop{patch}{fallow};
    } $land{patch}{CropAreaFrac}= $crop{patch}{CropAreaFrac};
    die "\nNo crops initialized. Possible error in the CropParFile -\n   $irrParam{CropParFile}\n\tAborting...\n\n"
	unless scalar(keys %{$land{irrCrop}}) || scalar(keys %{$land{rfdCrop}});

  }		### DO NOT MODIFY THIS HASH (%land) ANYWHERE IN THE CODE

  return %land;
}

#######################################################################

sub irrTech_init
	# Initialization of irrigation technologies
{
  my ($extent, $meta, $MagicT, $irrParam) = @_;

  my %parKeys = ( Delivery	=> { Process	=> [qw(AreaFrac EvapFactor PercFactor)],
				     Efficiency	=> [qw(EvapFraction Efficiency)]},
		  Application	=> { Process	=> [qw(DU AppEvap)],
				     Efficiency	=> [qw(EvapFraction Efficiency)]});

		### Read parameter entries from CSV files
  my %parFile = ( Delivery    => $$irrParam{IrrTechDeliveryFile},
		  Application => $$irrParam{IrrTechApplicationFile});
		### Make list of expected header names
  my %techPar	 =( $parFile{Delivery}    ?
	read_land_param($parFile{Delivery},    ['Technology','Group']) : (Delivery    => {Flag => 0}),
		    $parFile{Application} ?
	read_land_param($parFile{Application}, ['Technology','Group']) : (Application => {Flag => 0}));

	###############################################################
  foreach my $group (qw(Delivery Application)) {
    unless (exists $techPar{$group}{Flag}) { $techPar{$group}{Flag} = 1 }
    else   { next }
    my @technology;

		### Read data layers and remove unsued technologies
    foreach my $tech (keys %{$techPar{$group}}) {
      next if $tech eq 'Flag';
      if ($techPar{$group}{$tech}{CropLandFrac}) {
	push @technology, $tech;
	foreach my $key (keys %{$techPar{$group}{$tech}}) {
	  $techPar{$group}{$tech}{$key} =
		read_Layer($extent,$meta,$techPar{$group}{$tech}{$key},$MagicT,{PATCH_VALUE=>0});
	}
      } else { delete $techPar{$group}{$tech} }
    }
    die "\nAborting: No $group technologies are found in the input file:\n\t$parFile{$group}\n\n"
	unless @technology;

	# Add {Process} flag for irrigation loss sub-model:	0 - by loss efficiency and evap fraction
	#							1 - by loss process modeling
    my     $keyStrPrc	=  join('',sort(@{$parKeys{$group}{Process}},	'CropLandFrac'));
    my     $keyStrEff	=  join('',sort(@{$parKeys{$group}{Efficiency}},'CropLandFrac'));
    my     $keyStr	=  join('',sort(keys(%{$techPar{$group}{$technology[0]}})));
			# Check that headers are good!
    if    ($keyStr eq $keyStrPrc) {$techPar{$group}{Process} = 1; $parKeys{$group} = $parKeys{$group}{Process};}
    elsif ($keyStr eq $keyStrEff) {$techPar{$group}{Process} = 0; $parKeys{$group} = $parKeys{$group}{Efficiency};}
    else  { die "Unrecognized headers in Irrigation Technology CSV files...\n$keyStr\n$keyStrPrc\n$keyStrEff\n"; }

		### Normalize land Fractions
    my  $sum = zeroes(double,$$extent{ncols},$$extent{nrows})->copybad($$extent{mask});
    map $sum+= $techPar{$group}{$_}{CropLandFrac}, @technology;
    map        $techPar{$group}{$_}{CropLandFrac} =
	      ($techPar{$group}{$_}{CropLandFrac} / $sum)->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask}),
	@technology;

		### Parameter weighted averages by land Fractions
    foreach my $param (@{$parKeys{$group}}) {
      map $techPar{$group}{$param}  = 0, @technology;
      map $techPar{$group}{$param} += $techPar{$group}{$_}{CropLandFrac} * $techPar{$group}{$_}{$param}, @technology;
    }
    unless ($techPar{$group}{Process}) {
      $techPar{$group}{Efficiency}  /= 100;			# Convert efficiency % to fraction
      $techPar{$group}{Efficiency}->inplace->lclip(0.1);	# Change zeroes to a small number
    }
    map delete($techPar{$group}{$_}), @technology;		### Cleanup
  }
		### Print irr technology information to STDOUT
  printf "\nIrrigation delivery    : by %s\n" . "Irrigation application : by %s\n",
	$techPar{Delivery}   {Flag} ? $techPar{Delivery}   {Process} ? 'Process' : 'Efficiency' : 'Default',
	$techPar{Application}{Flag} ? $techPar{Application}{Process} ? 'Process' : 'Efficiency' : 'Default';

  return %techPar;
}

#######################################################################

sub irrLosses
{
  my ($demand, $demandN, $irr_demand, $irrArea, $irrTech, $efficiency, $percCoef, $evapRate, $percRate,
      $awCap,  $land,    $landFrac,   $rffStorage) = @_;

  my ($delEvap, $delPerc) = (0,0);		# Case of irr technologies are not used
  my ($appEvap, $appPerc, $appRnff) = (0,0,0);	# Case of application technology by process

	###############################################################
	###	Process based water delivery losses
  if ($$irrTech{Delivery}{Flag}) { if  ($$irrTech{Delivery}{Process}) {
    my $area	= $irrArea		* $$irrTech{Delivery}{AreaFrac};
    $delEvap	= $area * $$evapRate[0]	* $$irrTech{Delivery}{EvapFactor};
    $delPerc	= $area *  $percRate	* $$irrTech{Delivery}{PercFactor};
  }
		### Efficiency based water delivery losses
  else {
    my $delLoss	= $demandN * (1/$$irrTech{Delivery}{Efficiency} - 1);
    $delEvap	= $delLoss * $$irrTech{Delivery}{EvapFraction};
    $delPerc	= $delLoss - $delEvap;
  }}

	###############################################################
	###	Process based water application losses (method of Jagermeyr et al., 2015)
  if ($$irrTech{Application}{Process}) {
    my $storage	= condition_slice($irrArea> 0 , $rffStorage/$irrArea, 0);
    foreach my $tp (sort keys %{$$land{irrCrop}}) {		### IS SORT NEEDED?
      my $rice	= $tp =~ m/^rice/i ? $percRate : 0;	# This is done for avoiding rice water percolation
							# to be done twice, i.e. rice added water
      $appRnff += $$landFrac{irrCrop}{$tp} * condition($$irr_demand{$tp} > 0,
	(0.5 * $$awCap{irrCrop}{$tp} * $$irrTech{Application}{DU} - $storage - $rice)->lclip(0), 0);
    }
    $appEvap	= ($demand + $appRnff) * $$irrTech{Application}{AppEvap};
  }
		### Efficiency based water application losses
  else {
    my $appLoss	= $demandN * (1/$efficiency - 1);
    $appEvap	= $$irrTech{Application}{Flag} ? $appLoss * $$irrTech{Application}{EvapFraction} :
						($irrArea * $$evapRate[1])->hclip($appLoss);
    $appPerc	= $percCoef * ($appLoss - $appEvap);
    $appRnff	=($appLoss  -  $appPerc - $appEvap)->lclip(0);	# lclip is used for small float number cases...
  }

  return $delEvap, $delPerc, $appEvap, $appPerc, $appRnff;
}

sub irrFactorPct {
  my ($supply, $demand)	= @_;
  return $demand ? sprintf("%.1f",$supply / $demand * 100) : 'N/A';
}

#######################################################################

sub read_land_param
{
  my ($file,$field) = @_;
  my ($hdr, @data ) = read_table($file);
  my ($id,  $group) = (delete($$hdr{$$field[0]}), delete($$hdr{$$field[1]}), delete($$hdr{Comment}));
  my  %land;

  foreach my $rec (@data) {
    next unless $$rec[$group];
    map $land{$$rec[$group]}{$$rec[$id]}{$_} = $$rec[$$hdr{$_}], keys(%$hdr);
  }

  return %land;
}

#######################################################################

sub dominant_Land
	# Find dominant land cover or crop type and make its Land Fraction = 100 %
{
  my $landCover	= shift;
  my $ldFr	= 'LandFraction';

  my @types = keys %$landCover;
  for (my $i=0; $i<$#types; $i++) {		### Find the dominant type
    for (my $j=$i+1; $j<=$#types; $j++) {
      my $mask = $$landCover{$types[$i]}{$ldFr} > $$landCover{$types[$j]}{$ldFr};
      $$landCover{$types[$i]}{$ldFr} *= $mask;
      $$landCover{$types[$j]}{$ldFr} *= $mask == 0;
    }
  }		### Make land fraction of the dominant type to be 100 %
  map $$landCover{$_}{$ldFr} = $$landCover{$_}{$ldFr} > 0, @types;
		### Save dominant land cover ID file
  my  $id  = zeroes(short,$$landCover{$types[0]}{$ldFr}->dims);
  map $id->where($$landCover{$_}{$ldFr} > 0) .= $$landCover{$_}{ID}, @types;

  return $id;
}

#######################################################################

sub growth_coeff
	# Calculate crop fraction growth coefficient and crop growth reference year
{
  my ($flag, $coeff, $date, $refYear, $fstYear, $lstYear) = @_;
  return (1, sprintf("%04d-%02d-%02d",@$date)) unless $flag;

  my $cropYr =  $refYear		? $refYear :
		$$date[0] < $fstYear	? $fstYear :
		$$date[0] > $lstYear	? $lstYear :  $$date[0];
  my $yearDate   = sprintf "%04d-%02d-%02d", $cropYr, $$date[1], $$date[2];
  my $grthCoeff  = 1 + $coeff * ($$date[0] - $cropYr);

  return $grthCoeff, $yearDate;
}

###############   End of Land Initialization Functions   ##############
#######################################################################

	#### Define functions for calculating aquifer balance
sub aqf_head {
	# Define head calculator for this aquifer
  my ($C,$dZ,$z0)= @_;
  return sub {
	# Function for calculating head
  my $V = shift;
	# V/C = fractional storage in aquifer (m3/m3) * dZ (thickness m) + baseelev (m)
  return $V/$C*$dZ+$z0;}
}

sub sum_spr_outflow {
	# Outflow from all springs in this aquifer.
  my ($head, $sprgs) = @_;
  my %outflow = map { $_ => $$sprgs{$_}{Conduct} * List::Util::max($head - $$sprgs{$_}{Elevation},0) } keys %$sprgs;
  return %outflow;
}

sub aqf_balance {
	# Calculate aquifer balance for this timestep
	# initial volume (storage in timestep), outflow, and net landscape balance (infiltration - abstractions)
  my ($V0,$head_function,$discharge_function,$I,$sprgs,$dt) = @_;
	# Using a simple RK23 scheme.  Using 2nd/3rd order solutions for numeric error checking only. (Known as Bogacki-Shampine)
  my $Q= sub { my $v = shift; my %Qnow = &$discharge_function ( &$head_function( $v ), $sprgs); return pdl( values %Qnow )->sum*$dt;};
  my $k1 = $I - &$Q($V0);
  my $k2 = $I - &$Q($V0+0.5*$k1);
  my $k3 = $I - &$Q($V0+0.75*$k2);
  my $zn = $V0+ (2.*$k1/9.) + ($k2/3.) + (4.*$k3/9.);
  my $k4 = $I - &$Q($zn);
  my $yn = $V0 + (7.*$k1/24.)+($k2/4.) + ($k3/3.) + ($k4/8.);
  my $err= abs(($zn-$yn)/$yn);
  printf "Aquifer numerical misbalance encountered: 2nd O %f  3rd O %f with err %f !\n\tContinuing...\n",$zn,$yn,$err
		if $err > 1.0e-8;
	# Calculate final head and discharge at end of timestep
  my $head	= &$head_function($zn);
  my %discharge	= &$discharge_function( $head, $sprgs);
  return $zn, \%discharge, $head, $err;
}

#######################################################################
###############     Runtime aggregation functions        ##############

sub var_accumulate
{
  my ($ts, $data, $data_out, $date) = @_;

    $$data{start}{$ts}	= $date unless $$data{count}{$ts};
    $$data{data} {$ts} += $$data_out[0];
    $$data{data2}{$ts} += $$data_out[0]**2;
    $$data{count}{$ts}++;
}

sub var_aggregate
{
  my ($ts, $var, $data, $data_out, $date) = @_;
  my  $varS	 = $var.'_sigma';
		### Aggreate
  $$data{data2}{$ts} .= 1/$$data{count}{$ts}*sqrt(($$data{count}{$ts}*$$data{data2}{$ts}-$$data{data}{$ts}**2)->lclip(0));
  $$data{data} {$ts} /=   $$data{count}{$ts};
		### Make NC data structure
  my $dataToWrite =  {$var	=> [$$data{data} {$ts}, @$data_out[1..3]],
		     $varS	=> [$$data{data2}{$ts}, @$data_out[1..3]]};
     $$dataToWrite  {$varS}[1]	=  'Sigma of ' .  $$dataToWrite{$varS}[1];
		### Time series range of aggregated data, path and band
  my $aggrRange   = [['AggrRangeStart',$$data{start}{$ts}],['AggrRangeEnd',$date]];
  my $path_band   =   $$data{dSet}{$ts}->dateLayer($date,3);
    $$path_band[0]=~ s/.+:(.+):.+/$1/;				# Strip NetCDF extras

  return @$path_band, $dataToWrite, $aggrRange;
}

sub var_reset
{
  my ($ts, $data) = @_;

  set_to_zero(	$$data{data} {$ts}, $zeroes);
  set_to_zero(	$$data{data2}{$ts}, $zeroes);
		$$data{count}{$ts}  = 0 ;
		$$data{start}{$ts}  = '';
}

#######################################################################
###############           Tracking Functions             ##############

sub reset_spinup_tracking
{
  my @groups	= @_;
  my $reset_4c	= $zeroes_4->copy;
     $reset_4c(,,2) .= 1;
  my $reset_Nd	= $zeroes_N->copy;
     $reset_Nd(,,0) .= 1;
  $skipCompBal	= 1;
		### This function uses global scope of variables below
  if (2 ~~ @groups) {
    $cRffStgP		= $reset_4c->copy;
    $cRunoffP		= $reset_4c->copy;
    $cSoilP		= $reset_4c->copy;
    $cSIR_P		= $reset_4c->copy;
    $cVIS_P		= $reset_4c->copy;
    $cStreamP[0]	= $reset_4c->copy;
    $cGrWaterP		= $reset_4c->copy;
    $cEndoP		= $endoComp?$reset_4c->copy : $reset_4c;
    %cAqWaterP		= map(($_=>$px4c->copy), @aqf_IDs);
  }
  if (3 ~~ @groups) {
    $cRffStgIrr		= $reset_Nd->copy;
    $cRunoffIrr		= $reset_Nd->copy;
    $cSoilIrr		= $reset_Nd->copy;
    $cSIR_Irr		= $reset_Nd->copy;
    $cVIS_Irr		= $reset_Nd->copy;
    $cStreamIrr[0]	= $reset_Nd->copy;
    $cGrWaterIrr	= $reset_Nd->copy;
    %cAqWaterIrr	= map(($_=>$pxNd->copy), @aqf_IDs);
  }
  return 1;
}

#######################################################################

sub compUpdate_Aquifer
{
  my($comp, $aqf_data, $ID_from, $ID_to, $volumeAdd) = @_;

  $$comp{$ID_to} =($$comp{$ID_to}   * ($$aqf_data{$ID_to}{Storage} - $volumeAdd) +
		   $$comp{$ID_from} *  $volumeAdd ) / $$aqf_data{$ID_to}{Storage};
  return 1;
}

#######################################################################

sub compUpdate_AquiferSnk
{
  my($compAqf, $aqf_data, $ID, $compAdd, $volumeAdd) = @_;

  $$compAqf{$ID} =($$compAqf{$ID} * ($$aqf_data{$ID}{Storage} - $volumeAdd) +
		    $compAdd      *  $volumeAdd ) / $$aqf_data{$ID}{Storage};
  return 1;
}

#######################################################################

sub compUpdate_Springs
{
  my ($comp, $compAdd, $volumePrev, $volumeAdd, $options)	= @_;
  my  $clip = set_default($$options{C}, 1);	# Flag to perform ->clip(0,1)
  my  $sum  = set_default($$options{S}, 0);	# Flag to perform ->sumover

  my  $volProdPrev = $volumePrev * $comp;
  my  $volProdAdd  =($volumeAdd  * $compAdd);
      $volProdAdd  = $volProdAdd->sumover	if $sum;
      $volProdAdd  = $volProdAdd->reshape($volProdPrev->dims);

  $comp	.= ($volProdPrev + $volProdAdd) / ($volumePrev + ($sum?$volumeAdd->sum:$volumeAdd));
  $comp->inplace->clip(0,1) if $clip;

  return 1;
}

#######################################################################

sub compUpdate_CropRotation
{
  return unless List::Util::sum( @compSwitch[2,3] );	# Skip if no tracking needed

		### Moisture moved between soil and groundwater at crop rotations
  my $grdWaterAdd	 = $grdWaterChg * ($grdWaterChg < 0);		# Negative to groundwater
  my $soilAdd		 = $grdWaterChg * ($grdWaterChg > 0);		# Positive to soil

  if ($compSwitch[2]) {				### Primary water fractions
    $cGrWaterP		*= $grdWater[1];
    $cGrWaterP		-= $grdWaterAdd	* $cSoilP;
    $cGrWaterP		.= normalizeFr(	  $cGrWaterP, $extent );

    $cSoilP		*= $sMoist[0];
    $cSoilP		+= $soilAdd	* $cGrWaterP;
    $cSoilP		.= normalizeFr(	  $cSoilP, $extent );
  }
  if ($compSwitch[3]) {				### Irrigation water fractions
    $cGrWaterIrr	*= $grdWater[1];
    $cGrWaterIrr	-= $grdWaterAdd	* $cSoilIrr;
    $cGrWaterIrr	.= normalizeFr( $cGrWaterIrr, $extent );

    $cSoilIrr		*= $sMoist[0];
    $cSoilIrr		+= $soilAdd	* $cGrWaterIrr;
    $cSoilIrr		.= normalizeFr(	  $cSoilIrr, $extent );
  }
  return 1;
}

#######################################################################

sub compUpdate_RffStg
{
  my $surfRunoff = shift();			# mm/day

  if ($compSwitch[1]) {				### Basic water fractions in runoff storage
    $cRffStg		*= $surfRffStorage[0] + $irrRffStorage[0];
    $cRffStg(,,(0))	+= $surfRunoff*$subDf * $snowInFr;
    $cRffStg(,,(2))	+= $surfRunoff*$subDf * $rainInFr;
    $cRffStg		.= normalizeFr( $cRffStg, $extent );
  }
  if ($compSwitch[2]) {				### Primary water fractions in runoff storage
    $cRffStgP		*= $surfRffStorage[0] + $irrRffStorage[0];
    $cRffStgP		+= $surfRunoff*$subDf * $cSoilP;
    $cRffStgP		.= normalizeFr( $cRffStgP, $extent );
  }
  if ($compSwitch[3]) {				### Irrigation water fraction in runoff storage
    $cRffStgIrr		*= $surfRffStorage[0] + $irrRffStorage[0];
    $cRffStgIrr		+= $surfRunoff*$subDf * $cSoilIrr;
    $cRffStgIrr		.= normalizeFr( $cRffStgIrr, $extent );
  }
  if ($compSwitch[5] && $mskModel) {		### Spatial mask fractions in runoff storage
    $cRffStgMsk		*=($surfRffStorage[0] + $irrRffStorage[0]);
    $cRffStgMsk		+= $surfRunoff*$subDf * $rnffMask;
    $cRffStgMsk		.= normalizeFr( $cRffStgMsk, $extent );
  }

  return 1;
}

#######################################################################

sub compUpdate_GrWater
{
  my ($GWt_infiltPrecip, $Aqf_infiltPrecip) = @_;
	# mm/day		m3/day

  if ($compSwitch[2]) {		### Primary water fractions in ground water
    $cGrWaterP		*= $grdWater[1];
    $cGrWaterP		+=($GWt_infiltPrecip*$subDf) * $cSoilP;
    $cGrWaterP		.= normalizeFr( $cGrWaterP, $extent );
				### Primary water fractions in aquifers
    foreach my $ID ( @aqf_IDs ) {
      $cAqWaterP{$ID}	 = ($$aqf_data{$ID}{StoragePrev}*$cAqWaterP{$ID} +
	($Aqf_infiltPrecip*$subDf*($aquiferID == $ID) * $cSoilP)
	->sumover->sumover->reshape(1,1,4)) / $$aqf_data{$ID}{Storage};
    }
  }
  if ($compSwitch[3]) {		### Irrigation water fraction in ground water
    $cGrWaterIrr	*= $grdWater[1];
    $cGrWaterIrr	+=($GWt_infiltPrecip*$subDf) * $cSoilIrr;
    $cGrWaterIrr	.= normalizeFr( $cGrWaterIrr, $extent );
				### Irrigation water fraction in virtual/lumped aquifer
    foreach my $ID ( @aqf_IDs ) {
      $cAqWaterIrr{$ID}	 = ($$aqf_data{$ID}{StoragePrev}*$cAqWaterIrr{$ID} +
	($Aqf_infiltPrecip*$subDf*($aquiferID == $ID) * $cSoilIrr)
	->sumover->sumover->reshape($pxNa->dims)) / $$aqf_data{$ID}{Storage};
    }
  }
  if ($compSwitch[5] && $mskModel) {	###  Spatial  mask fractions
    $cGrWaterMsk	*= $grdWater[1];
    $cGrWaterMsk	+= $GWt_infiltPrecip*$subDf * $rnffMask;
    $cGrWaterMsk	.=($cGrWaterMsk /($grdWater[1] + $GWt_infiltPrecip*$subDf))
	->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask})->clip(0,1);
  }

  return 1;
}

#######################################################################

sub compUpdate_Runoff
{
  my ($surfRunoff,$irrRunoff,$baseFlow,$runoffVol,$flushWater,$smResRefill) = @_;
	# mm/day    mm/day     mm/day     m3/sec      m3           m3

  return unless List::Util::sum( @compSwitch[1..5] );	# Skip if no tracking needed
  my $surfRunoffVol	= ($surfRunoff+$irrRunoff) * $MM_2_m3;		# m3
  my $baseFlowVol	=  $baseFlow		   * $MM_2_m3;		# m3
  my $glMeltVolume	=  $glMelt		   * $dt;		# m3

  if ($compSwitch[1]) {				### Basic water fractions in runoff
    $cRunoff		.= $surfRunoffVol * $cRffStg;
    $cRunoff(,,(0))	+= $flushWater    * $snowInFr;
    $cRunoff(,,(2))	+= $flushWater    * $rainInFr;
    $cRunoff(,,(1))	+= $glMeltVolume;
    $cRunoff(,,(3))	+= $baseFlowVol;
    $cRunoff		.= normalizeFr( $cRunoff,  $extent );
	if ($smResFlag) {
    $cSIR		*= $smResStrg[1] - $smResRefill;
    $cSIR		+= $smResRefill  * $cRffStg;
    $cSIR		.=($cSIR / $smResStrg[1])	->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask})->clip(0,1);
  } }

  if ($compSwitch[2]) {				### Primary water fractions in runoff
    $cRunoffP		.= $surfRunoffVol * $cRffStgP + $baseFlowVol*$cGrWaterP;
    $cRunoffP(,,(0))	+= $flushWater    * $snowInFr;
    $cRunoffP(,,(1))	+= $glMeltVolume;
    $cRunoffP(,,(2))	+= $flushWater    * $rainInFr;
    $cRunoffP		.= normalizeFr( $cRunoffP,  $extent );
	if ($smResFlag) {
    $cSIR_P		*= $smResStrg[1] - $smResRefill;
    $cSIR_P		+= $smResRefill  * $cRffStgP;
    $cSIR_P		.= normalizeFr( $cSIR_P,  $extent );
  } }

  if ($compSwitch[3]) {				### Irrigation water fraction in runoff
    $cRunoffIrr		.= $surfRunoffVol* $cRffStgIrr + $baseFlowVol*$cGrWaterIrr;
    $cRunoffIrr(,,(1))	+= $glMeltVolume + $flushWater;		# Pristine component
    $cRunoffIrr		.= normalizeFr( $cRunoffIrr,  $extent );
	if ($smResFlag) {
    $cSIR_Irr		*=  $smResStrg[1] - $smResRefill;
    $cSIR_Irr		+=  $smResRefill  * $cRffStgIrr;
    $cSIR_Irr		.= ($cSIR_Irr / $smResStrg[1])	->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask})->clip(0,1);
  } }
						### Runoff water temperature
  if ($compSwitch[4]) {	# Mixing surface runoff (T air),  baseflow (T grnd), glacier melt (T=0C)-no need to add to sum
		### Baseflow water temperature by A. Prusevich model
    $baseflowTw	 = ($grndWaterTw +$bflowScl*($airT_bAvg - $grndWaterTw))->lclip(0);
    $cRunoffTw	 = ((($surfRunoffVol+$flushWater)*($snowInFr<0.1)*$airT_rAvg->lclip(0) + $baseFlowVol*$baseflowTw) /
	($runoffVol * $dt))->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask});
  }
  if ($compSwitch[5] && $mskModel) {		### Spatial  mask fractions in runoff
    $cRunoffMsk		*= $surfRunoffVol;
    $cRunoffMsk		+= $baseFlowVol*$cGrWaterMsk + ($glMeltVolume+$flushWater)*$rnffMask;
    $cRunoffMsk		.= normalizeFr( $cRunoffMsk,  $extent );
	if ($smResFlag) {
    $cSIR_Msk		*= $smResStrg[1] - $smResRefill;
    $cSIR_Msk		+= $smResRefill  * $cRffStgMsk;
    $cSIR_Msk		.=($cSIR_Msk / $smResStrg[1])	->setnonfinitetobad->setbadtoval(0)->copybad($$extent{mask})->clip(0,1);
  } }

  return 1;
}

#######################################################################

sub compUpdate_Endorheic
{
  my $flow_out	= shift();			# m3/sec

  if ($compSwitch[2]) {				### Primary water fractions in runoff
    my $storage	 = $zeroes->copy;	$storage->indexND($$outletIdx{ENDORHEIC}) .= $endoStrg;
    my $endFlow	 = $zeroes->copy;	$endFlow->indexND($$outletIdx{ENDORHEIC}) .= $flow_out->index($$outletIdx{endorheic});
    $cEndoP	*= $storage;
    $cEndoP	+= $endFlow*$dt * $cStreamP[1];
    $cEndoP	.= normalizeFr($cEndoP,  $extent );
  }
}

#######################################################################

sub compUpdate_VIS	### Note- $irrVIS[0] is zero ( which is OK!), if VIS is not used ($useVIS = 0)
{			###	  In this case $cVIS represents composition of $irrigationGross water!
  return unless $runIO{Irrigation};

  if ($compSwitch[1]) {
    my $cExtra	   = $zeroes_4->copy;	$cExtra(,,(3))  .= 1;
    my $irrUseComp =	($IrrUseStrgLoc*$cStream[0] + $IrrUseStrgRmt*$cStream[0]->indexND($irrUseFlowRID) +
			 $IrrUseFlowLoc*$cStream[1] + $IrrUseFlowRmt*$cStream[1]->indexND($irrUseFlowRID) +
			($irrUseGrwt + $irrUseExtra)*$cExtra + $irrUseSIR*$cSIR_prev) * $subDf;
    $cVIS	  *= $irrVIS[0];
    $cVIS	  += $irrUseComp;
    $cVIS	  .= normalizeFr( $cVIS,  $extent );
  }

  if ($compSwitch[2]) {
    my $cExtra	   = $zeroes_4->copy;	$cExtra(,,(3))  .= 1;
    my $irrUseComp =	($IrrUseStrgLoc*$cStreamP[0] + $IrrUseStrgRmt*$cStreamP[0]->indexND($irrUseFlowRID) +
			 $IrrUseFlowLoc*$cStreamP[1] + $IrrUseFlowRmt*$cStreamP[1]->indexND($irrUseFlowRID) +
			 $irrUseGrwt   *$cGrWaterP   + $irrUseSIR    *$cSIR_P_prev + $irrUseExtra*$cExtra) * $subDf;
   map $irrUseComp+= $irrUseAqf*$subDf *$cAqWaterP{$_} * ($aquiferID == $_), @aqf_IDs;		# Aquifers
    $cVIS_P	  *= $irrVIS[0];
    $cVIS_P	  += $irrUseComp;
    $cVIS_P	  .= normalizeFr( $cVIS_P,  $extent );
  }

  if ($compSwitch[3]) {
    my $cExtra	   = $zeroes_N->copy;	$cExtra(,,(0))  .= 1;
    my $irrUseComp =	($IrrUseStrgLoc*$cStreamIrr[0] + $IrrUseStrgRmt*$cStreamIrr[0]->indexND($irrUseFlowRID) +
			 $IrrUseFlowLoc*$cStreamIrr[1] + $IrrUseFlowRmt*$cStreamIrr[1]->indexND($irrUseFlowRID) +
			 $irrUseGrwt   *$cGrWaterIrr   + $irrUseSIR    *$cSIR_Irr_prev + $irrUseExtra*$cExtra) * $subDf;
   map $irrUseComp+= $irrUseAqf*$subDf *$cAqWaterIrr{$_} * ($aquiferID == $_), @aqf_IDs;	# Aquifers
		### Push irrigation water reuse to the water added to VIS
    foreach my $i (reverse(1 .. $cVIS_Irr->dim(2)-1)) {
      my $j = List::Util::max($i,3);
      $irrUseComp(,,($j  )) += $irrUseComp(,,($i-1));
      $irrUseComp(,,($i-1)) .= 0;
    }
		### Add it to VIS
    $cVIS_Irr	  *= $irrVIS[0];
    $cVIS_Irr	  += $irrUseComp;
    $cVIS_Irr	  .= normalizeFr( $cVIS_Irr,  $extent );
  }

  if ($compSwitch[5] && $mskModel) {
    my $cExtra = $rnffMask;
    my $irrUseComp =	($IrrUseStrgLoc*$cStreamMsk[0] + $IrrUseStrgRmt*$cStreamMsk[0]->indexND($irrUseFlowRID) +
			 $IrrUseFlowLoc*$cStreamMsk[1] + $IrrUseFlowRmt*$cStreamMsk[1]->indexND($irrUseFlowRID) +
			 $irrUseGrwt   *$cGrWaterMsk   + $irrUseSIR    *$cSIR_Msk_prev +
			($irrUseExtra  +$irrUseAqf)    * $subDf        *$cExtra) * $subDf;
    $cVIS_Msk	  *= $irrVIS[0];
    $cVIS_Msk	  += $irrUseComp;
    $cVIS_Msk	  .= normalizeFr( $cVIS_Msk,  $extent );
  }
  return 1;
}

#######################################################################

sub compUpdate_Soil
{
  my $waterIn = shift();			# mm/day

  if ($compSwitch[2]) {
    $cSoilP		*= $sMoist[0] + $grdWaterChg;
    $cSoilP(,,(0))	+= $waterIn*$subDf * $snowInFr;
    $cSoilP(,,(2))	+= $waterIn*$subDf * $rainInFr;
    $cSoilP		.= normalizeFr( $cSoilP, $extent );
  }
  if ($compSwitch[3]) {
    $cSoilIrr		*= $sMoist[0] + $grdWaterChg;
    $cSoilIrr(,,(1))	+= $waterIn*$subDf;
    $cSoilIrr		.= normalizeFr( $cSoilIrr, $extent );
  }
  return 1;
}

#######################################################################

sub compUpdate_SoilIrr
{
  return unless $runIO{Irrigation};	# Skip if no tracking needed
  my ($irrigationNet, $ricePaddyWater)	= @_;	# mm

  if ($compSwitch[2]) {
    my $irrNet	 = $irrigationNet - $ricePaddyWater;
    $cSoilP	*= $sMoist[1] - $irrNet;
    $cSoilP	+= $irrNet    * $cVIS_P;
    $cSoilP	.= normalizeFr( $cSoilP, $extent );
  }
  if ($compSwitch[3]) {
    my $irrNet	 = $irrigationNet - $ricePaddyWater;
    $cSoilIrr	*= $sMoist[1] - $irrNet;
    $cSoilIrr	+= $irrNet    * $cVIS_Irr;
    $cSoilIrr	.= normalizeFr( $cSoilIrr, $extent );
  }
  return 1;
}

#######################################################################

sub compUpdate_Returns
{
  return unless $runIO{Irrigation} || $runIO{WaterDemand};

  my ($irrRnffIneff, $irrFloodRice, $rainFloodRice, $GWt_infiltRatio, $Aqf_infiltRatio,		# All in mm
      $irrPercIneff, $irrPercRice,  $rainPercRice,  $irrPercDeliv,    $returnRff) = @_;		# except ratios

    my $rffStorage	= $surfRffStorage[1] + $irrRffStorage[1];	# Tracking components are shared between
									# surface and irrigation retention pools
    if ($compSwitch[1]) {	### Basic water fractions in runoff storage
      my $cRain		= $zeroes_4->copy;	$cRain (,,(2)).= 1;
      my $cExtra	= $zeroes_4->copy;	$cExtra(,,(3)).= 1;

		# Runoff return
      $cRffStg	    *= $rffStorage;
      $cRffStg	    += $subDf * ( ($runIO{WaterDemand} ?
	$domReturnFr*($DomUseStrgLoc*$cStream[0] + $DomUseStrgRmt*$cStream[0]->indexND($domUseFlowRID) +
		      $DomUseFlowLoc*$cStream[1] + $DomUseFlowRmt*$cStream[1]->indexND($domUseFlowRID) +
			($domUseGrwt + $domUseExtra + $domUseAqf)*$cExtra) +
	$indReturnFr*($IndUseStrgLoc*$cStream[0] + $IndUseStrgRmt*$cStream[0]->indexND($indUseFlowRID) +
		      $IndUseFlowLoc*$cStream[1] + $IndUseFlowRmt*$cStream[1]->indexND($indUseFlowRID) +
			($indUseGrwt + $indUseExtra + $indUseAqf)*$cExtra) +
	$stkReturnFr*($StkUseStrgLoc*$cStream[0] + $StkUseStrgRmt*$cStream[0]->indexND($stkUseFlowRID) +
		      $StkUseFlowLoc*$cStream[1] + $StkUseFlowRmt*$cStream[1]->indexND($stkUseFlowRID) +
			($stkUseGrwt + $stkUseExtra + $stkUseAqf)*$cExtra  + $stkUseSIR*$cSIR_prev) : 0) +
	($runIO{Irrigation} ? ($irrRnffIneff  + $irrFloodRice)   *$cVIS +$rainFloodRice*$cRain : 0)    );

      $cRffStg	    .= normalizeFr( $cRffStg, $extent );
    }
			### Primary water fractions in runoff storage and ground water
    if ($compSwitch[2]) {
      my $cExtra	= $zeroes_4->copy;	$cExtra(,,(3)).= 1;

		# Runoff return from Irrigation and Domestic/Industrial/Livestock water demands
      $cRffStgP	    *= $rffStorage;
      $cRffStgP	    += $subDf * ( ($runIO{WaterDemand} ?
	$domReturnFr*($DomUseStrgLoc*$cStreamP[0] + $DomUseStrgRmt*$cStreamP[0]->indexND($domUseFlowRID) +
		      $DomUseFlowLoc*$cStreamP[1] + $DomUseFlowRmt*$cStreamP[1]->indexND($domUseFlowRID) +
		      $domUseGrwt   *$cGrWaterP   + $domUseExtra  *$cExtra) +
	$indReturnFr*($IndUseStrgLoc*$cStreamP[0] + $IndUseStrgRmt*$cStreamP[0]->indexND($indUseFlowRID) +
		      $IndUseFlowLoc*$cStreamP[1] + $IndUseFlowRmt*$cStreamP[1]->indexND($indUseFlowRID) +
		      $indUseGrwt   *$cGrWaterP   + $indUseExtra  *$cExtra) +
	$stkReturnFr*($StkUseStrgLoc*$cStreamP[0] + $StkUseStrgRmt*$cStreamP[0]->indexND($stkUseFlowRID) +
		      $StkUseFlowLoc*$cStreamP[1] + $StkUseFlowRmt*$cStreamP[1]->indexND($stkUseFlowRID) +
		      $stkUseGrwt   *$cGrWaterP   + $stkUseExtra  *$cExtra + $stkUseSIR*$cSIR_P_prev) : 0) +
	($runIO{Irrigation} ? ($irrRnffIneff + $irrFloodRice)  *$cVIS_P + $rainFloodRice*$cSoilP   : 0)   );
		# Aquifers
     map $cRffStgP += $subDf* ($domReturnFr*$domUseAqf+$indReturnFr*$indUseAqf+$stkReturnFr*$stkUseAqf) *
			$cAqWaterP{$_} * ($aquiferID == $_), @aqf_IDs;

      $cRffStgP	   .= normalizeFr( $cRffStgP, $extent );

			### Irr return to groundwater and aquifers
      if ($runIO{Irrigation}) {
	my $cPerc   = $subDf*(($irrPercIneff + $irrPercRice + $irrPercDeliv)*$cVIS_P + $rainPercRice*$cSoilP);	# mm
		# Ground Water return
	$cGrWaterP *= $grdWater[1];
	$cGrWaterP += $GWt_infiltRatio * $cPerc;
	$cGrWaterP .= normalizeFr( $cGrWaterP, $extent );
		# Aquifer Water return
	$cPerc     *= $mm_2_m3 if $aqfType;
	foreach my $ID ( @aqf_IDs ) {
	  $cAqWaterP{$ID}  = ($$aqf_data{$ID}{StoragePrev}*$cAqWaterP{$ID} +
	    ($Aqf_infiltRatio*($aquiferID == $ID) * $cPerc)->sumover->sumover->reshape(1,1,4)) / $$aqf_data{$ID}{Storage};
    } } }
				### Used water fractions in runoff storage and ground water
    if ($compSwitch[3]) {
		# Irr return added to runoff storage (retention pool)
      $cRffStgIrr	 *= $rffStorage;
      $cRffStgIrr	 += $subDf *  $rainFloodRice*$cSoilIrr;
      $cRffStgIrr(,,(2)) += $subDf *  $returnRff;				# Dom/Ind/Stk component
      $cRffStgIrr	 += $subDf * ($irrRnffIneff + $irrFloodRice)*$cVIS_Irr;	# Irrigation  component
      $cRffStgIrr	 .= normalizeFr( $cRffStgIrr, $extent );
		# Irr return added to Ground Water storage
      my $cPerc	 = $subDf*(($irrPercIneff + $irrPercRice + $irrPercDeliv)*$cVIS_Irr + $rainPercRice*$cSoilIrr);	# mm
      $cGrWaterIrr	 *= $grdWater[1];
      $cGrWaterIrr	 += $GWt_infiltRatio * $cPerc;
      $cGrWaterIrr	  = normalizeFr( $cGrWaterIrr, $extent );
		# Aquifer Water return
         $cPerc		 *= $mm_2_m3 if $aqfType;	# m3
      foreach my $ID ( @aqf_IDs ) {
	$cAqWaterIrr{$ID}   = ($$aqf_data{$ID}{StoragePrev}*$cAqWaterIrr{$ID} +
	    ($Aqf_infiltRatio*($aquiferID == $ID) * $cPerc)->sumover->sumover->reshape($pxNa->dims)) / $$aqf_data{$ID}{Storage};
    } }
				### Spatial mask fractions in runoff storage and ground water
    if ($compSwitch[5] && $mskModel) {
		# Runoff return
      $cRffStgMsk	*= $rffStorage;
      $cRffStgMsk	+= $subDf * ( ($runIO{WaterDemand} ?
	$domReturnFr*($DomUseStrgLoc*$cStreamMsk[0] + $DomUseStrgRmt*$cStreamMsk[0]->indexND($domUseFlowRID) +
		      $DomUseFlowLoc*$cStreamMsk[1] + $DomUseFlowRmt*$cStreamMsk[1]->indexND($domUseFlowRID) +
		      $domUseGrwt   *$cGrWaterMsk   +($domUseExtra  +$domUseAqf) *$rnffMask) +
	$indReturnFr*($IndUseStrgLoc*$cStreamMsk[0] + $IndUseStrgRmt*$cStreamMsk[0]->indexND($indUseFlowRID) +
		      $IndUseFlowLoc*$cStreamMsk[1] + $IndUseFlowRmt*$cStreamMsk[1]->indexND($indUseFlowRID) +
		      $indUseGrwt   *$cGrWaterMsk   +($indUseExtra  +$indUseAqf) *$rnffMask) +
	$stkReturnFr*($StkUseStrgLoc*$cStreamMsk[0] + $StkUseStrgRmt*$cStreamMsk[0]->indexND($stkUseFlowRID) +
		      $StkUseFlowLoc*$cStreamMsk[1] + $StkUseFlowRmt*$cStreamMsk[1]->indexND($stkUseFlowRID) +
		      $stkUseGrwt   *$cGrWaterMsk   +($stkUseExtra  +$stkUseAqf) *$rnffMask  +
		      $stkUseSIR    *$cSIR_Msk_prev ) : 0) +
	($runIO{Irrigation} ? ($irrRnffIneff     + $irrFloodRice)*$cVIS_Msk   + $rainFloodRice * $rnffMask : 0));

      $cRffStgMsk	.= normalizeFr( $cRffStgMsk, $extent );
      if ($runIO{Irrigation}) {
	my $cPerc	 = $subDf*(($irrPercIneff + $irrPercRice + $irrPercDeliv)*$cVIS_Msk + $rainPercRice*$rnffMask);
		# Ground Water return
	$cGrWaterMsk	*= $grdWater[1];
	$cGrWaterMsk	+= $GWt_infiltRatio * $cPerc;
	$cGrWaterMsk	.= normalizeFr( $cGrWaterMsk, $extent );
    } }

  return 1;
}

#######################################################################

sub compUpdate_GrWaterIrr
{
  my($GWt_infltIneff, $Aqf_infltIneff) = @_;
	# mm/day	m3/day

  if ($compSwitch[2]) {
    $cGrWaterP *= $grdWater[1];
    $cGrWaterP += $GWt_infltIneff*$subDf * $cRffStgP;
    $cGrWaterP .= normalizeFr( $cGrWaterP, $extent );
			# Aquifer
    foreach my $ID ( @aqf_IDs ) {
      $cAqWaterP{$ID}	 =($$aqf_data{$ID}{StoragePrev} * $cAqWaterP{$ID} +
	($Aqf_infltIneff * $subDf * ($aquiferID == $ID) * $cRffStgP)
	->sumover->sumover->reshape($px4b->dims)) / $$aqf_data{$ID}{Storage};
  } }
  if ($compSwitch[3]) {
    $cGrWaterIrr	*= $grdWater[1];
    $cGrWaterIrr	+= $GWt_infltIneff*$subDf * $cRffStgIrr;	### Water from Irrigation return
    $cGrWaterIrr	.= normalizeFr( $cGrWaterIrr, $extent );
			# Aquifer
    foreach my $ID ( @aqf_IDs ) {
      $cAqWaterIrr{$ID}	 =($$aqf_data{$ID}{StoragePrev} * $cAqWaterIrr{$ID} +
	($Aqf_infltIneff * $subDf * ($aquiferID == $ID) * $cRffStgIrr)
	->sumover->sumover->reshape($pxNa->dims)) / $$aqf_data{$ID}{Storage};
  } }
  if ($compSwitch[5] && $mskModel) {
    $cGrWaterMsk *= $grdWater[1];
    $cGrWaterMsk += $GWt_infltIneff*$subDf * $cRffStgMsk;
    $cGrWaterMsk .= normalizeFr( $cGrWaterMsk, $extent );
  }

  return 1;
}

#######################################################################

sub normalizeFr
{
  my ($arr, $ext) = @_;
  $arr->inplace->lclip(0);
  return ($arr / $arr->mv(2,0)->sumover)->setnonfinitetobad->setbadtoval(0)->copybad($$ext{mask});
}

#######################################################################
###############    Start of Tracking Balances            ##############

sub compBalance_Prev
{
  return unless $compBalFlag;

  my $rffStorage	= $surfRffStorage[0] + $irrRffStorage[0];	# Tracking components are shared between
									# surface and irrigation retention pools
  if ($compSwitch[2]) {
    unless (exists $compBal{Prm}) {					# Update previous values
      for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
	$compBal{Prm}{Soil_prev}[$i]	= ($sMoist[0]	  * $cSoilP	  (,,($i)) * $mm_2_km3)->sum;
	$compBal{Prm}{RffStg_prev}[$i]	= ($rffStorage    * $cRffStgP	  (,,($i)) * $mm_2_km3)->sum;
	$compBal{Prm}{SIR_prev}[$i]	= ($smResStrg[0]  * $cSIR_P	  (,,($i)) * $mm_2_km3)->sum;
	$compBal{Prm}{VIS_prev}[$i]	= ($irrVIS[0]	  * $cVIS_P	  (,,($i)) * $mm_2_km3)->sum;
	$compBal{Prm}{GrWater_prev}[$i]	= ($grdWater[0]   * $cGrWaterP    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Prm}{Stream_prev}[$i]	= ($resStorage[0] * $cStreamP[0]->(,,($i)) * 1e-9     )->sum;
	$compBal{Prm}{AqWater_prev}[$i]	= $aqfType ? 1e-9 * List::Util::sum(
		map($$aqf_data{$_}{Storage} * $cAqWaterP{$_}->at(0,0,$i), @aqf_IDs)) : 0;
    } }
    else {								# Initialize previous values
      for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
        map $compBal{Prm}{$_.'_prev'}[$i] = $compBal{Prm}{$_}[$i], qw(Soil RffStg SIR VIS Stream GrWater AqWater);
    } }
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
        $compBal{Prm}{StrDlt}[$i]  = 0;					# Reset it
      if ($doArea) {							# Land rotation change
	$compBal{Prm}{StrDlt}[$i] -=
	$compBal{Prm}{Soil_prev}[$i]   - ($sMoist[0]   * $cSoilP   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Prm}{RffStg_prev}[$i] - ($rffStorage  * $cRffStgP (,,($i)) * $mm_2_km3)->sum +
	$compBal{Prm}{SIR_prev}[$i]    - ($smResStrg[0]* $cSIR_P   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Prm}{VIS_prev}[$i]    - ($irrVIS[0]   * $cVIS_P   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Prm}{GrWater_prev}[$i]- ($grdWater[0] * $cGrWaterP(,,($i)) * $mm_2_km3)->sum;
  } } }

  if ($compSwitch[3]) {
    unless (exists $compBal{Irr}) {					# Update previous values
      for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
	$compBal{Irr}{Soil_prev}[$i]	= ($sMoist[0]	  * $cSoilIrr	    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Irr}{RffStg_prev}[$i]	= ($rffStorage    * $cRffStgIrr	    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Irr}{SIR_prev}[$i]	= ($smResStrg[0]  * $cSIR_Irr	    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Irr}{VIS_prev}[$i]	= ($irrVIS[0]	  * $cVIS_Irr	    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Irr}{GrWater_prev}[$i]	= ($grdWater[0]   * $cGrWaterIrr    (,,($i)) * $mm_2_km3)->sum;
	$compBal{Irr}{Stream_prev}[$i]	= ($resStorage[0] * $cStreamIrr[0]->(,,($i)) * 1e-9     )->sum;
	$compBal{Irr}{AqWater_prev}[$i]	= $aqfType ? 1e-9 * List::Util::sum(
		map($$aqf_data{$_}{Storage} * $cAqWaterIrr{$_}->at(0,0,$i), @aqf_IDs)) : 0;
    } }
    else {								# Initialize previous values
      for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
        map $compBal{Irr}{$_.'_prev'}[$i] = $compBal{Irr}{$_}[$i], qw(Soil RffStg SIR VIS Stream GrWater AqWater);
    } }
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
        $compBal{Irr}{StrDlt}[$i]  = 0;					# Reset it
      if ($doArea) {							# Land rotation change
	$compBal{Irr}{StrDlt}[$i] -=
	$compBal{Irr}{Soil_prev}[$i]   - ($sMoist[0]   * $cSoilIrr   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Irr}{RffStg_prev}[$i] - ($rffStorage  * $cRffStgIrr (,,($i)) * $mm_2_km3)->sum +
	$compBal{Irr}{SIR_prev}[$i]    - ($smResStrg[0]* $cSIR_Irr   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Irr}{VIS_prev}[$i]    - ($irrVIS[0]   * $cVIS_Irr   (,,($i)) * $mm_2_km3)->sum +
	$compBal{Irr}{GrWater_prev}[$i]- ($grdWater[0] * $cGrWaterIrr(,,($i)) * $mm_2_km3)->sum;
  } } }
  return 1;
}

sub compBalance_PrevSoil
{
  return unless $compBalFlag;
  if ($compSwitch[2]) {
    map $compBal{Prm}{Soil_prev}[$_] = ($sMoist[0] * $cSoilP  (,,($_)) * $mm_2_km3)->sum, 0 .. $cRunoffP  ->dim(2)-1;
  }
  if ($compSwitch[3]) {
    map $compBal{Irr}{Soil_prev}[$_] = ($sMoist[0] * $cSoilIrr(,,($_)) * $mm_2_km3)->sum, 0 .. $cRunoffIrr->dim(2)-1;
  }
  return 1;
}

#######################################################################

sub compBalance_springs
{
  return unless $compBalFlag;
  my $resStoragePrev	= shift();
  $compBal{SprVolm}	= $spr_flag ?  $resStorage[0] - $resStoragePrev : 0;

  if ($compSwitch[2]) {
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{Springs}[$i] = $spr_flag ? ($resStorage[0] * $cStreamP[0]->(,,($i)))->sum * 1e-9 -
					  $compBal{Prm}{Stream_prev}[$i] : 0;
  } }

  if ($compSwitch[3]) {
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
      $compBal{Irr}{Springs}[$i] = $spr_flag ? ($resStorage[0] * $cStreamIrr[0]->(,,($i)))->sum * 1e-9 -
					  $compBal{Irr}{Stream_prev}[$i] : 0;
  } }
  return 1;
}

#######################################################################

sub compBalance_Soil
{
  return unless $compBalFlag;
  my ($waterIn, $flushWater, $evapotrans) = @_;
     # mm/day       m3		mm/day

  if ($compSwitch[2]) {
    $compBal{Prm}{WaterIn}[0]	 = ($waterIn	* $snowInFr	 * $MM_2_km3	)->sum;
    $compBal{Prm}{WaterIn}[0]	+= ($flushWater	* $snowInFr	 * 1e-9		)->sum;
    $compBal{Prm}{WaterIn}[1]	 = ($glMelt	* $dt		 * 1e-9		)->sum;
    $compBal{Prm}{WaterIn}[2]	 = ($waterIn	* $rainInFr	 * $MM_2_km3	)->sum;
    $compBal{Prm}{WaterIn}[2]	+= ($flushWater	* $rainInFr	 * 1e-9		)->sum;
    $compBal{Prm}{WaterIn}[3]	 =  0;

    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{EvapSoil}[$i] = ($evapotrans * $cSoilP(,,($i)) * $MM_2_km3)->sum;
  } }

  if ($compSwitch[3]) {
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
      $compBal{Irr}{EvapSoil}[$i] = ($evapotrans * $cSoilIrr(,,($i)) * $MM_2_km3)->sum;
  } }
  return 1;
}

#######################################################################

sub compBalance_EvapRiv
{
  return unless $compBalFlag;
  my ($netwkEvap, $sirEvap) = @_;
     #  mm/day     mm/day

  if ($compSwitch[2]) {
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{EvapRiv}[$i] = ($netwkEvap * $cStreamP[0]->	(,,($i)))->sum * $subDf * 1e-9;
      $compBal{Prm}{sirEvap}[$i] = ($sirEvap   * $cSIR_P	(,,($i)) * $MM_2_km3)->sum;
  } }

  if ($compSwitch[3]) {
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
      $compBal{Irr}{EvapRiv}[$i] = ($netwkEvap * $cStreamIrr[0]->(,,($i)))->sum * $subDf * 1e-9;
      $compBal{Irr}{sirEvap}[$i] = ($sirEvap   * $cSIR_Irr	 (,,($i)) * $MM_2_km3)->sum;
  } }
  return 1;
}

#######################################################################

sub compBalance_sinks
{
  return unless $compBalFlag;
  my ($snkVolume, $colRow) = @_;	# m3

  if ($compSwitch[2]) {
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{Sinks}[$i]	+= $snkVolume * $cStreamP[1]->at(@$colRow,$i) * 1e-9;
  } }

  if ($compSwitch[3]) {
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
      $compBal{Irr}{Sinks}[$i]	+= $snkVolume * $cStreamIrr[1]->at(@$colRow,$i) * 1e-9;
  } }
  return 1;
}

#######################################################################

sub compBalance_Use
{
  return unless $compBalFlag;
  my ($irrigationNet, $irrigationGross, $irrEvapIneff, $irrEvapDeliv, $ricePaddyWater, $pristineIrrComp) = @_;
	#  mm		mm/day		   mm/day	  mm/day	   mm		  [mm, m3]
  my $useExtra	= $domUseExtra + $indUseExtra + $stkUseExtra	if $compSwitch[2] || $compSwitch[3];

  if ($compSwitch[2]) {
    my $cExtra		= $zeroes_4->copy;	$cExtra(,,(3)).= 1;
    my $idlEvapComp	= $zeroes_4->copy;

    if ($runIO{WaterDemand}) {
      $idlEvapComp     .=	(1  - $domReturnFr) * (
	$DomUseStrgLoc*$cStreamP[0] + $DomUseStrgRmt*$cStreamP[0]->indexND($domUseFlowRID) +
	$DomUseFlowLoc*$cStreamP[1] + $DomUseFlowRmt*$cStreamP[1]->indexND($domUseFlowRID) +
	$domUseGrwt   *$cGrWaterP   + $domUseExtra  *$cExtra) +
				(1  - $indReturnFr) * (
	$IndUseStrgLoc*$cStreamP[0] + $IndUseStrgRmt*$cStreamP[0]->indexND($indUseFlowRID) +
	$IndUseFlowLoc*$cStreamP[1] + $IndUseFlowRmt*$cStreamP[1]->indexND($indUseFlowRID) +
	$indUseGrwt   *$cGrWaterP   + $indUseExtra  *$cExtra) +
				(1  - $stkReturnFr) * (
	$StkUseStrgLoc*$cStreamP[0] + $StkUseStrgRmt*$cStreamP[0]->indexND($stkUseFlowRID) +
	$StkUseFlowLoc*$cStreamP[1] + $StkUseFlowRmt*$cStreamP[1]->indexND($stkUseFlowRID) +
	$stkUseGrwt   *$cGrWaterP   + $stkUseExtra  *$cExtra);
		      # SIR
      $idlEvapComp += (1 - $stkReturnFr) * $stkUseSIR  * $cSIR_P_prev if $smResFlag;
		      # Aquifers
      foreach my $ID (@aqf_IDs) {
	$idlEvapComp  += ($aquiferID == $ID) * $cAqWaterP{$ID}  *
	      ((1-$domReturnFr) * $domUseAqf + (1-$indReturnFr) * $indUseAqf + (1-$stkReturnFr) * $stkUseAqf);
      }
    }
    map $compBal{Prm}{EvapDIL} [$_] = ($idlEvapComp(,,($_)) * $MM_2_km3)->sum,	(0 .. $cRunoffP->dim(2)-1);
    map $compBal{Prm}{EvapTech}[$_] =  0,					(0 .. $cRunoffP->dim(2)-1);
		### UGW water introduced to the system
    $compBal{Prm}{WaterIn}[3]	= (($useExtra + $irrUseExtra)*$MM_2_km3)->sum if $runIO{WaterDemand} || $runIO{Irrigation};
  }

  if ($compSwitch[3]) {
    my $cExtra		= $zeroes_N->copy;	$cExtra(,,(0)).= 1;
    my $idlUseComp	= $zeroes_N->copy;
    my $useAquifer	= $domUseAqf + $indUseAqf + $stkUseAqf;
    my $k		= $nIrr + 2;

		### Withdrawals for Industrial/Domestic/Livestock demands
    if ($runIO{WaterDemand}) {
      $idlUseComp      .=
	$DomUseStrgLoc*$cStreamIrr[0] + $DomUseStrgRmt*$cStreamIrr[0]->indexND($domUseFlowRID) +
	$DomUseFlowLoc*$cStreamIrr[1] + $DomUseFlowRmt*$cStreamIrr[1]->indexND($domUseFlowRID) +
		      $domUseGrwt * $cGrWaterIrr +
	$IndUseStrgLoc*$cStreamIrr[0] + $IndUseStrgRmt*$cStreamIrr[0]->indexND($indUseFlowRID) +
	$IndUseFlowLoc*$cStreamIrr[1] + $IndUseFlowRmt*$cStreamIrr[1]->indexND($indUseFlowRID) +
		      $indUseGrwt * $cGrWaterIrr +
	$StkUseStrgLoc*$cStreamIrr[0] + $StkUseStrgRmt*$cStreamIrr[0]->indexND($stkUseFlowRID) +
	$StkUseFlowLoc*$cStreamIrr[1] + $StkUseFlowRmt*$cStreamIrr[1]->indexND($stkUseFlowRID) +
		      $stkUseGrwt * $cGrWaterIrr + $useExtra * $cExtra;
			# SIR
      $idlUseComp  += $stkUseSIR  * $cSIR_Irr_prev if $smResFlag;
			# Groundwater
      map $idlUseComp += $useAquifer * $cAqWaterIrr{$_} * ($aquiferID == $_), @aqf_IDs;	# Aquifers
    }
		### Withdrawals for irrigation
    my  $irrUseComp =  $IrrUseStrgLoc*$cStreamIrr[0] + $IrrUseFlowLoc*$cStreamIrr[1]
		    +  $IrrUseStrgRmt*$cStreamIrr[0]->indexND($irrUseFlowRID)
		    +  $IrrUseFlowRmt*$cStreamIrr[1]->indexND($irrUseFlowRID)
		    +  $irrUseGrwt   *$cGrWaterIrr   + $irrUseExtra  * $cExtra
		    +  $irrUseSIR    *$cSIR_Irr_prev;
			# Reuse variables
    my  $irr_comp_Grwt		 = $irrUseGrwt * $cGrWaterIrr;
    map $$IrrReuseInGrossGrwt_irr[$_-3]	= $irr_comp_Grwt(,,($_)),	3..$k;
    my  $irr_comp_Flow		 = $irrUseComp - $irr_comp_Grwt;
    map $$IrrReuseInGrossFlow_irr[$_-3]	= $irr_comp_Flow(,,($_)),	3..$k;
			# Groundwater
    map $irrUseComp += $irrUseAqf    *$cAqWaterIrr{$_} * ($aquiferID == $_), @aqf_IDs;	# Aquifers
			# Reuse variables
    my  $irr_comp_Aqf		 = $irrUseComp - $irr_comp_Flow - $irr_comp_Grwt;
    map $$IrrReuseInGrossAqf_irr[$_-3]	= $irrUseComp(,,($_)),		3..$k;

    for (my $i=0; $i<=$k; $i++ ) {
      $compBal{Irr}{UsedDIL}[$i]	= ($idlUseComp(,,($i))    * $MM_2_km3)->sum;
      $compBal{Irr}{UsedIrr}[$i]	= ($irrUseComp(,,($i))    * $MM_2_km3)->sum;
      $compBal{Irr}{UsedIrrFlow}[$i]	= ($irr_comp_Flow(,,($i)) * $MM_2_km3)->sum;
      $compBal{Irr}{UsedIrrGrwt}[$i]	= ($irr_comp_Grwt(,,($i)) * $MM_2_km3)->sum;
      $compBal{Irr}{UsedIrrAqf}[$i]	= ($irr_comp_Aqf(,,($i))  * $MM_2_km3)->sum;

      if ($i > 2) {
		# Rasterized values for WBM output, mm/day
	$$UseReuseOutGross_irr [$i-3]	=  $idlUseComp(,,($i));		# Reuse of _irr by human use
	$$IrrReuseInGross_irr  [$i-3]	=  $irrUseComp(,,($i));		# Irrigation gross "In" to _irr balance
	$$IrrReuseOutNet_irr   [$i-3]	=  $irrUseComp(,,($i)) *	# Irrigtion net "Out" to _irr balance
		condition_slice($irrigationGross > 0, $irrigationNet / ($subDf*$irrigationGross), 0);	# Why "$subDf*"?
	$$IrrReuseOutNonben_irr[$i-3]	= $irrUseComp (,,($i)) *	# Irrigatoin nonbeneficial "Out" to _irr balance
		condition_slice($irrigationGross > 0,($irrEvapIneff + $irrEvapDeliv ) / $irrigationGross, 0);
	$$irrFracInGross       [$i-3]	=
		condition_slice($irrigationGross > 0, $irrUseComp(,,($i)) / $irrigationGross, 0);
      }
    }
		### Gross water use (introduced to the system)
    $compBal{Irr}{Gross}[1]   =  ($$pristineIrrComp[0] * $mm_2_km3)->sum + $$pristineIrrComp[1]->sum * 1e-9;
    $compBal{Irr}{Gross}[0]   = (($useExtra    + $irrUseExtra)		     *	$MM_2_km3)->sum;
    $compBal{Irr}{Gross}[2]   = (($domUseGross + $indUseGross + $stkUseGross)*	$MM_2_km3)->sum;
map $compBal{Irr}{Gross}[$_]  =   0, 3..$k;		map   { my $j = List::Util::max($_,3);
    $compBal{Irr}{Gross}[$j] += $compBal{Irr}{UsedIrr}[$_-1]; } reverse(1 .. $k);
    $compBal{Irr}{Gross}[$k] += $compBal{Irr}{UsedIrr}[$k];
		### Consumptively used
    my $irrNet		      =  ($irrigationNet - $ricePaddyWater) * $cVIS_Irr;
map $compBal{Irr}{Net}[$_]    =   0, 0..$k;
    $compBal{Irr}{Net}[2]     = (($domUseEvap + $indUseEvap + $stkUseEvap) *	$MM_2_km3)->sum;
map $compBal{Irr}{Net}[$_]    =  ($irrNet(,,($_))			   *	$MM_2_km3)->sum, 3..$k;
  }
  return 1;
}

#######################################################################

sub compBalance_EvapIrr
{
  return unless $compBalFlag;
  my ($irrEvapIneff, $irrEvapDeliv) = @_;	# mm/day

  if ($compSwitch[2]) {
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{EvapIrr}[$i] = (($irrEvapIneff + $irrEvapDeliv) * $cVIS_P(,,($i))  * $MM_2_km3)->sum;
  } }
  if ($compSwitch[3]) {
		### Non-beneficial use for Irrigation: non-efficient use for delivery and application evaporation
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
      $compBal{Irr}{EvapIrr}[$i] = (($irrEvapIneff + $irrEvapDeliv) * $cVIS_Irr(,,($i))* $MM_2_km3)->sum;
      $compBal{Irr}{EvapTech}[$i]= 0;
  } }
  return 1;
}

#######################################################################

sub compBalance_irrEvapRff
		### Non-beneficial evaporation from Irr retention pool with Irr technology
{
  return unless $compBalFlag;
  my $irrEvapRff = shift();			# mm/day

  if ($compSwitch[2]) {
    for (my $i=0; $i<$cRunoffP->dim(2); $i++ ) {
      $compBal{Prm}{EvapTech}[$i]    =($irrEvapRff * $cRffStgP(,,($i))	 * $MM_2_km3)->sum;
  } }

  if ($compSwitch[3]) {
    for (my $i=0; $i<$cRunoffIrr->dim(2); $i++ ) {
	$compBal{Irr}{EvapTech}[$i]    =($irrEvapRff * $cRffStgIrr(,,($i)) * $MM_2_km3)->sum; }
    map	$$IrrReuseOutNonben_irr[$_-3] += $irrEvapRff * $cRffStgIrr(,, ($_))* $irrFracInGross,	3..2+$nIrr;
  }
  return 1;
}

#######################################################################

sub compBalance_Final
{
  return unless $compBalFlag;
  my($flowUseDelta, $flow_out) = @_;

  my $rffStorage	= $surfRffStorage[1] + $irrRffStorage[1];	# Tracking components are shared between
									# surface and irrigation retention pools
  if ($compSwitch[2]) {
		### Calculate component balance variables
    for (my $i=0; $i<$cRunoffP->dim(2); $i++) {
      $compBal{Prm}{RffStg}[$i]	= ($rffStorage    * $cRffStgP	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Prm}{Soil}[$i]	= ($sMoist[1]	  * $cSoilP	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Prm}{VIS}[$i]	= ($irrVIS[1]	  * $cVIS_P	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Prm}{SIR}[$i]	= ($smResStrg[1]  * $cSIR_P	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Prm}{Stream}[$i]	= ($resStorage[1] * $cStreamP[1]->(,,($i)) * 1e-9 )->sum;	# Storage term
      $compBal{Prm}{GrWater}[$i]= ($grdWater[1]   * $cGrWaterP	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Prm}{AqWater}[$i]=  @aqf_IDs? 1e-9 * List::Util::sum(				# Storage term
				  map($$aqf_data{$_}{Storage} * $cAqWaterP{$_}->at(0,0,$i), @aqf_IDs)) : 0;
      $compBal{Prm}{Out}[$i]	= ($cStreamP[1]->(,,($i))->indexND($$outletIdx{ALL})*		# Flux    term
				   $flow_out->index($$outletIdx{all})  )->sum * $dt * 1e-9 +
				  (@aqf_IDs? 1e-9 * List::Util::sum(
				  map($$aqf_data{$_}{MF_out}  * $cAqWaterP{$_}->at(0,0,$i), @aqf_IDs)) : 0);
      $compBal{Prm}{StrDlt}[$i]+= ($flowUseDelta*$cStreamP[1]->(,,($i)))->sum * $dt * 1e-9;	# Flux    term
		### SIR and IBT canals evaporation contribution
      $compBal{Prm}{EvapRiv}[$i]+= $compBal{Prm}{sirEvap}[$i] + ($$connectivity[0]->dims ? (	# Flux    term
	  $cStreamP[1]->(,, ($i))->index2d($$connectivity[0]->((0),),$$connectivity[0]->((1),)) *
	  $$connectivity[1]->(6,)->flat)->sum * 1e-9 : 0);
    }
  }
  if ($compSwitch[3]) {
		### Calculate component balance variables
    my $dim = $cRunoffIrr->dim(2);
    for (my $i=0; $i<$dim; $i++) {
      $compBal{Irr}{RffStg}[$i]	= ($rffStorage    * $cRffStgIrr	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Irr}{Soil}[$i]	= ($sMoist[1]	  * $cSoilIrr	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Irr}{VIS}[$i]	= ($irrVIS[1]	  * $cVIS_Irr	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Irr}{SIR}[$i]	= ($smResStrg[1]  * $cSIR_Irr	(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Irr}{Stream}[$i]	= ($resStorage[1] * $cStreamIrr[1]->(,,($i)) * 1e-9 )->sum;	# Storage term
      $compBal{Irr}{GrWater}[$i]= ($grdWater[1]   * $cGrWaterIrr(,,($i)) * $mm_2_km3)->sum;	# Storage term
      $compBal{Irr}{AqWater}[$i]=  @aqf_IDs? 1e-9 * List::Util::sum(				# Storage term
				  map($$aqf_data{$_}{Storage} * $cAqWaterIrr{$_}->at(0,0,$i), @aqf_IDs)) : 0;
      $compBal{Irr}{Out}[$i]	= ($cStreamIrr[1]->(,,($i))->indexND($$outletIdx{ALL})*		# Flux    term
				   $flow_out->index($$outletIdx{all})    )->sum * $dt * 1e-9 +
				  (@aqf_IDs? 1e-9 * List::Util::sum(
				  map($$aqf_data{$_}{MF_out}  * $cAqWaterIrr{$_}->at(0,0,$i), @aqf_IDs)) : 0);
      $compBal{Irr}{StrDlt}[$i]+= ($flowUseDelta*$cStreamIrr[1]->(,,($i)))->sum * $dt * 1e-9;	# Flux    term
		### SIR and IBT canals evaporation contribution
      $compBal{Irr}{EvapRiv}[$i] +=$compBal{Irr}{sirEvap}[$i] + ($$connectivity[0]->dims ? (	# Flux    term
	  $cStreamIrr[1]->(,,($i))->index2d($$connectivity[0]->((0),),$$connectivity[0]->((1),)) *
	  $$connectivity[1]->(6, )->flat)->sum * 1e-9 : 0);
    }
		### Calculate totals of the irrigation cycles
    foreach my $key (keys %{$compBal{Irr}}) { for(my $i=3;$i<$dim;$i++) { $compBal{Irr}{$key}[$dim]  = 0; }}	# Reset then sum
    foreach my $key (keys %{$compBal{Irr}}) { for(my $i=3;$i<$dim;$i++) { $compBal{Irr}{$key}[$dim] += $compBal{Irr}{$key}[$i]; }}
  }
  return 1;
}

###############    End of Tracking Functions             ##############
#######################################################################

sub usage

{
  my $app_name = basename($0);
  print <<EOF;

Usage:
	$app_name [-h] [-v] [-bd|by|bm] [-test] [-noRun] [-rm] [-rmSpool] [-noOutput] [-noState] [-dState] [-t THREADS] [-tz THREAD_SIZE] [-sl] [-idump] [-saveDams] [-err] [-spoolDir SPOOL_DIR] [-stateDir STATE_DIR] RUN_ID

This is PDL implementation for the University of New Hampshire Water Balance Model (WBM). Version @{[WBM_VERSION]}
RUN_ID is the run ID from I/O spreadsheet (wbm_run_list.csv) or a parameter input file.

Options:

h		Display this help.
v		Verbose mode.
bd		Perform code benchmarking for daily  intervals.
by		Perform code benchmarking for yearly intervals.
bm		Perform code benchmark mapping. Pause using "bm.lock" file.
test		Test mode: no output, WBM runs one time step only.
noRun		No run mode: Initialization only, creates auxilary files
		   in the output directory.
rm		Remove and do not update existing ouput files.
rmSpool		Remove all this run input spool files. Manual removal
		   of selected files is recommened though.
noOutput	Do not write output files while running full TS of the model.
noState		Do not read or write run state files
dState		Write  daily run state files. Forces spinup "State_ID".
t		Number of threads to use. Default is 4.
tz		Minimum thread size. Default is 1 Mb.
sl		Use PDL slicing (faster when using many crops).
saveDams	Save subset of dams within this run spatial domain.
err		Print all GDAL relevant STDERR to screen for debugging.
spoolDir	Overwrite path to spool     directory defined in the init file.
stateDir	Overwrite path to run state directory defined in the init file.

Example:
$app_name -v -rm -by test

EOF
  exit;
}

#######################################################################
###################  PDL::PP Functions  ###############################

__DATA__
__Pdlpp__

#######################################################################

pp_def('routing', HandleBad => 1,
  Pars => 'int table(k,l); int rtMethod();

    double c0(n,m); double c1(n,m); double c2(n,m);
    double rnff(n,m);
    double rnffFr(n,m,cc);	double rnffFrPm(n,m,cp);	double rnffIrrFr(n,m,ci);
    double rnffTw(n,m);		double rnffMsk(n,m,mm);		double rnffDIN(n,m);
    double dschPrev(n,m);	double dschUsed(n,m);
    double waterAgePrev(n,m);
    double dschFrPrev(n,m,cc);	double dschFrPmPrev(n,m,cp);	double dschIrrFrPrev(n,m,ci);
    double dschTwPrev(n,m);	double dschMskPrev(n,m,mm);
    double dschDINPrev(n,m);	double consDINPrev(n,m);
    double flowOutPrev(l);	double dschMean(n,m);
    double resStoragePrev(n,m);	double minRsvStrg(n,m);
    int    resData(cRes,mRes);	double resParams(nRes,mRes);	int    resStack(nst);
    double dt();		int    j_date();		int    cSwitch(cNum);
    int connTable(ck,cl);	double connParms(pk,cl);	int    connStack(mst);
    double waterTe(n,m);	double waterExp(n,m);
    double DIN_coef(n,m);	double DIN_slope();		int    DIN_NoSzRes();
    double snkData( msk,nsk);   int    snkStack( nsk);
    double usgsData(mus,nus);   int    usgsStack(nus);

    double [o] dsch(n,m);	double [o] dsch_in(n,m);	double [o] waterAge(n,m);
    double [o] dschFr(n,m,cc);	double [o] dschFrPm(n,m,cp);	double [o] dschIrrFr(n,m,ci);
    double [o] dschTw(n,m);	double [o] dschMsk(n,m,mm);
    double [o] dschDIN(n,m);	double [o] consDIN(n,m);	double [o] DIN_Denit(n,m);
    double [o] resStorage(n,m);	double [o] flowOut(l);		double [o] routeDiversion(cl);',

  Code => '
  ////////  Muskingum/LRR Transport Model with Reservoirs and Connectivity  ////////

		// Dimension constants
    int k_size = $SIZE(k);	int c_size = $SIZE(cc);	int p_size = $SIZE(cp);
    int l_size = $SIZE(l);	int i_size = $SIZE(ci);	int mm_size= $SIZE(mm);

		// General internal/private variables
    int i,i_cell,j_route,i_in,i_out,j_in,j_out;
    int    subDh = $dt() / 3600;				// Sub-daily interval, hours
    double subDf = $dt() /(3600*24);				// Sub-daily interval, day
    double resRelease, denominator, waterVolume, wTemperature, DIN_R, HL;

		// Connectivity network variables and constants
    int iConnStackPos = 0; int nC;
    int routeI, ci_out, cj_out, c_methd;
    double c_parm1, c_parm2, c_parm3, diversion, diversionCell;

		// Connectivity canal width calculation parameters
    double tau     = 8.0;
    double phi     = 0.58;

		// Reservoir variables and constants
    int nResRemDays  = 100;				// Length of a dam removal period, days
    int iResStackPos = 0; int nR, damNum, resMethod;
    double resArea, resCapacity, regCapacity, resLevel, storageFlow;
    double alpha, beta, minLvl, optLvl, deltaDisch;
    double a_Par, b_Par, B_Par, c_Par, p_Par, Y0, Xe, Xb, dschObs;
    int iSnkStackPos = 0, iUsgsStackPos = 0;

  //	Initialization of the output arrays
    loop(n,m)    %{ if ($ISBAD($rnff())) {
				$SETBAD($dsch());
				$SETBAD($dsch_in());
				$SETBAD($resStorage());
				$SETBAD($DIN_Denit()); }
		    else {
				$dsch() = $dsch_in() = $resStorage() = $DIN_Denit() = 0; }
		    $waterAge()  = $resStoragePrev()*($waterAgePrev() + 1);
		    $dschTw()    = $resStoragePrev()* $dschTwPrev();
		    $dschDIN()   = $resStoragePrev()* $dschDINPrev();
		    $consDIN()   = $resStoragePrev()* $consDINPrev();   %}
    loop(n,m,cc) %{ $dschFr()    = $resStoragePrev()* $dschFrPrev();    %}
    loop(n,m,cp) %{ $dschFrPm()  = $resStoragePrev()* $dschFrPmPrev();  %}
    loop(n,m,ci) %{ $dschIrrFr() = $resStoragePrev()* $dschIrrFrPrev(); %}
    loop(n,m,mm) %{ $dschMsk()   = $resStoragePrev()* $dschMskPrev();   %}
    loop(l)      %{ $flowOut()   = 0; %}
    loop(cl)     %{ $routeDiversion() = 0; %}

    for (i=0; i<$SIZE(nsk); i++) { $snkData(msk=>2, nsk=>i) = 0; }	// Reset infiltration to sinks to zero

  ///////////////////////////////////////////////////////////////////////
  //////////////	Routing starts here	/////////////////////////

    for(i_cell=0; i_cell<l_size; i_cell++) {
      i_out = $table(k=>1,l=>i_cell);
      j_out = $table(k=>2,l=>i_cell);
      i_in  = $table(k=>3,l=>i_cell);
      j_in  = $table(k=>4,l=>i_cell);

  ////////  Reservoir Area, Capacity, and Method  ////////

      resCapacity = 0; regCapacity = 0; resArea = 0; resMethod = -1;
      if ($resStack(nst=>iResStackPos) == i_cell) {
	nR = $resStack(nst=>++iResStackPos);	// Number of dams in the pixel (ii,jj)
	for(i=0; i<nR; i++) {
	  damNum = $resStack(nst=>++iResStackPos);

		// Check reservoir dates and Add it to the cell Reservoir capacity
	  if ($j_date() >= $resData(cRes=>2,   mRes=>damNum) && $j_date() < $resData(cRes=>3,mRes=>damNum)+nResRemDays) {

		// Dam removal is accounted as:     capacity  * Dam_Removal_Factor (See 2 lines below)
	    resCapacity += $resParams(nRes=>0, mRes=>damNum)  * ($j_date() > $resData(cRes=>3,mRes=>damNum) ? 1 -
		($j_date() - $resData(cRes=>3, mRes=>damNum)) / nResRemDays : 1);

	    resArea     += $resParams(nRes=>1, mRes=>damNum);  // m2
					//// Variable regCapacity is here used for Spillway dams cumulatives
	    if($resData(cRes=>4,  mRes=>damNum) == 0) regCapacity += $resParams(nRes=>3,mRes=>damNum);
	    if(resMethod < $resData(cRes=>4,   mRes=>damNum)) {
	       resMethod = $resData(cRes=>4,   mRes=>damNum);
	      alpha	 = $resParams(nRes=>2, mRes=>damNum);	// Used for spill   dams
	      c_Par	 = $resParams(nRes=>2, mRes=>damNum);	// Used for managed dams
	      p_Par	 = $resParams(nRes=>3, mRes=>damNum);
	      Y0	 = $resParams(nRes=>4, mRes=>damNum);
	      Xe	 = $resParams(nRes=>6, mRes=>damNum);
	      Xb    = Xe - $resParams(nRes=>8, mRes=>damNum);	// Xb = Xe - d
	      a_Par	 = $resParams(nRes=>9, mRes=>damNum);
	      b_Par	 = $resParams(nRes=>10,mRes=>damNum);
	      B_Par	 = $resParams(nRes=>11,mRes=>damNum);
	      dschObs	 = $resParams(nRes=>13,mRes=>damNum);
	    }
	  }
	}
	if (resMethod == 0) {
	  minLvl = 0.01 * resArea;	// Minimum head cannot be below 1 cm
	  optLvl = regCapacity;
	}
	iResStackPos++;			// Advance stack pointer to the next cell

		// Skip too small aggregate dams and model them as regular rivers
	if (resCapacity < $minRsvStrg(n=>i_in,m=>j_in)) resCapacity = 0;
      }

  ////////  Discharge  ////////

  //	Add runoff to discharge
      if ( $ISBAD($rnff(n=>i_in,m=>j_in)) ) $rnff(n=>i_in,m=>j_in) = 0;

      $dsch_in(n=>i_in, m=>j_in)  = $dsch(n=>i_in, m=>j_in);
      $dsch   (n=>i_in, m=>j_in) += $rnff(n=>i_in, m=>j_in);

  //	Calculate component fractions of surface runoff water
      denominator = $resStoragePrev(n=>i_in,m=>j_in)+$dsch(n=>i_in,m=>j_in)*$dt(); // Trapezoidal would halve this, right?
      waterVolume = $rnff(n=>i_in,m=>j_in) * $dt();

		// Water age
      if ( $cSwitch(cNum=>0) ) {
	$waterAge(n=>i_in,m=>j_in) += waterVolume;
	$waterAge(n=>i_in,m=>j_in)  = denominator > 0 ? $waterAge(n=>i_in,m=>j_in)/denominator : 0;
      }
		// Water components by runoff source
      if ( $cSwitch(cNum=>1) ) {
	for(i=0; i<c_size; i++) {
	  $dschFr(n=>i_in,m=>j_in,cc=>i) += waterVolume   * $rnffFr(n=>i_in,m=>j_in,cc=>i);
	  $dschFr(n=>i_in,m=>j_in,cc=>i)  = denominator>0 ? $dschFr(n=>i_in,m=>j_in,cc=>i)/denominator : 0;
	}
      }
		// Water components by primary source
      if ( $cSwitch(cNum=>2) ) {
	for(i=0; i<p_size; i++) {
	  $dschFrPm(n=>i_in,m=>j_in,cp=>i) += waterVolume   * $rnffFrPm(n=>i_in,m=>j_in,cp=>i);
	  $dschFrPm(n=>i_in,m=>j_in,cp=>i)  = denominator>0 ? $dschFrPm(n=>i_in,m=>j_in,cp=>i)/denominator :
		i==p_size-1 ? 1 : 0;	}		// Make it all UGW, if storage (denominator) is empty
      }
		// Water components by irrigation use
      if ( $cSwitch(cNum=>3)) {
	for(i=0; i<i_size; i++) {
	  $dschIrrFr(n=>i_in,m=>j_in,ci=>i) += waterVolume   * $rnffIrrFr(n=>i_in,m=>j_in,ci=>i);
	  $dschIrrFr(n=>i_in,m=>j_in,ci=>i)  = denominator>0 ? $dschIrrFr(n=>i_in,m=>j_in,ci=>i)/denominator :
		i==0 ? 1 : 0;	}			// Make it all UGW, if storage (denominator) is empty
      }
		// Water temperature
      if ( $cSwitch(cNum=>4) ) {
	$dschTw(n=>i_in,m=>j_in) += waterVolume   * $rnffTw(n=>i_in,m=>j_in);
	$dschTw(n=>i_in,m=>j_in)  = denominator>0 ? $dschTw(n=>i_in,m=>j_in)/denominator : 0;

			/////////  Heating/Cooling of the stream by ambient air and solar radiation
	$dschTw(n=>i_in,m=>j_in) = $waterTe(n=>i_in,m=>j_in) +
		($dschTw(n=>i_in,m=>j_in) - $waterTe(n=>i_in,m=>j_in))*$waterExp(n=>i_in,m=>j_in);
	if ($dschTw(n=>i_in,m=>j_in) < 0) $dschTw(n=>i_in,m=>j_in) = 0;
      }
		// Water components by runoff mask
      if ( $cSwitch(cNum=>5) ) {
	for(i=0; i<mm_size; i++) {
	  $dschMsk(n=>i_in,m=>j_in,mm=>i) += waterVolume   * $rnffMsk(n=>i_in,m=>j_in,mm=>i);
	  $dschMsk(n=>i_in,m=>j_in,mm=>i)  = denominator>0 ? $dschMsk(n=>i_in,m=>j_in,mm=>i)/denominator : 0;
	}
      }
		// Dissolved Inorganic Nitrogen
      if ( $cSwitch(cNum=>6) ) {
	$dschDIN(n=>i_in,m=>j_in) += waterVolume   * $rnffDIN(n=>i_in,m=>j_in);
	$consDIN(n=>i_in,m=>j_in) += waterVolume   * $rnffDIN(n=>i_in,m=>j_in);			// Conservative DIN
	$dschDIN(n=>i_in,m=>j_in)  = denominator>0 ? $dschDIN(n=>i_in,m=>j_in)/denominator : 0;
	$consDIN(n=>i_in,m=>j_in)  = denominator>0 ? $consDIN(n=>i_in,m=>j_in)/denominator : 0;	// Conservative DIN


	if ( resCapacity == 0 || $DIN_NoSzRes() ) {
		/////////  Denitrification in the stream
	  DIN_R	= $dschDIN(n=>i_in,m=>j_in)>0 ?			// Nitrogen removal fraction
		  1 - exp(-$DIN_coef(n=>i_in,m=>j_in)*pow($dschDIN(n=>i_in,m=>j_in)*1e6,$DIN_slope())) : 0;
	}
	else {	////////  Denitrification in reservoirs
		     ///  Follows Seitzinger
	  HL = $dsch(n=>i_in, m=>j_in) * $dt()  * 365.256 / resArea;
	  HL = HL > 1. ? HL : 1.;
	  DIN_R = 0.88453 * pow( HL , -0.3677) * ( pow( 2 , (($dschTw(n=>i_in,m=>j_in) - 20) / 10)) * 1.4) ;
        }
        DIN_R = DIN_R > 1 - 1e-5 ? 1 - 1e-5 : DIN_R; // Needed to prevent zeroing out of DIN mass and divide by 0 errors
	$DIN_Denit(n=>i_in,m=>j_in) += DIN_R * $dschDIN(n=>i_in,m=>j_in)*denominator/subDf;	// kg/pixel/day
	$dschDIN  (n=>i_in,m=>j_in) *= 1 - DIN_R;						// g/L
      }

      if ( resCapacity == 0 ) {

  ////////  Muskingum Transport and Accumulation  ////////
	if ( $rtMethod() == 1 ) {
	  resRelease =
	    $c0(n=>i_in,m=>j_in) * $dsch(n=>i_in,m=>j_in)      +
	    $c1(n=>i_in,m=>j_in) * $dschPrev(n=>i_in, m=>j_in) +
	    $c2(n=>i_in,m=>j_in) * $flowOutPrev(l=>i_cell);
	  if (resRelease < 0) resRelease = 0;

	  $resStorage(n=>i_in,m=>j_in) =
	    $resStoragePrev(n=>i_in,m=>j_in) + ($dsch(n=>i_in,m=>j_in) - resRelease)*$dt();
	}

  ////////  LRR Transport and Accumulation  ////////
	else if ( $rtMethod() == 2 ) {
	  $resStorage(n=>i_in,m=>j_in) =
	    $resStoragePrev(n=>i_in,m=>j_in) + $dsch(n=>i_in,m=>j_in)*$dt();

	  resRelease = $resStorage(n=>i_in,m=>j_in)*$c0(n=>i_in,m=>j_in)/$dt();		// c0 is flowCoeff
	  $resStorage(n=>i_in,m=>j_in) *= 1 -       $c0(n=>i_in,m=>j_in);
	}

  ////////  Unknown Routing method  ////////
	else {
	  printf( "\nUnknown routing method in Pdlpp. Aborting...\n\n" );
	  exit(0xFF);		// It is exit with BAD status
	}

  ////////  Check for negative storage and fix if needed
	if ($resStorage(n=>i_in,m=>j_in) < 0) {
	  resRelease = $resStoragePrev(n=>i_in,m=>j_in)/$dt() + $dsch(n=>i_in, m=>j_in);
	  $resStorage(n=>i_in,m=>j_in) = 0;
	}
      }

  ////////  Reservoirs Storage and Release  ////////
      else {
				  ///// Observed outflow dams //
	  if ( dschObs ) {
	      resRelease = (dschObs < $resStoragePrev(n=>i_in, m=>j_in)  + $dsch(n=>i_in,m=>j_in)*$dt() ?
			    dschObs : $resStoragePrev(n=>i_in, m=>j_in)) / $dt();
	  } else {  // If observed discharge is present use that, unless there is not enough water in reservoir

				  ///// Spillway dams	/////
	      if (resMethod == 0) {				//// Crest is at the level of resCapacity
		  beta = 4 * alpha / resArea;			//// resLevel is head storage above crest
		  resLevel = $resStoragePrev(n=>i_in,m=>j_in) - resCapacity;
		  if (resLevel < minLvl) resLevel = minLvl;	//// Do not allow head to be below 1 cm
		  /// Iterative solution of Prusevich equation (2.7)
		  deltaDisch = ($dsch(n=>i_in,m=>j_in) - $dschPrev(n=>i_in,m=>j_in)) / subDh;
		  for(i=1; i<=subDh; i++) {
		      resRelease = $dschMean(n=>i_in,m=>j_in) *
			  pow((alpha == 0 ? resLevel/optLvl : (sqrt(1+beta*resLevel)-1)/(sqrt(1+beta*optLvl)-1)),1.6);
		      resLevel  += ($dschPrev(n=>i_in,m=>j_in)+deltaDisch*i - resRelease)*$dt()/subDh;
		      if (resLevel < minLvl) resLevel = minLvl;
		  }
		  resRelease = $dsch(n=>i_in,m=>j_in) - (resLevel+resCapacity-$resStoragePrev(n=>i_in,m=>j_in))/$dt();
		  if (resRelease < 0) resRelease = 0;
		  resCapacity = 1e15;				//// Spillway dams cannot be overfilled
	      }
				  ///// Regulated dams	/////
	      else {
		  regCapacity =  resCapacity / ($dschMean(n=>i_in,m=>j_in) * 365*$dt());
		  if (regCapacity < 0.1) regCapacity = 0.1;
		  if (regCapacity > 1.0) regCapacity = 1.0;
		  resLevel    = $resStoragePrev(n=>i_in,m=>j_in) / resCapacity;
		  deltaDisch  = ($dsch(n=>i_in,m=>j_in) - $dschPrev(n=>i_in,m=>j_in)) / subDh;

		  /// Iterative solution (needed for reservoirs with small regulatory capacity)
		  for(i=1; i<=subDh; i++) {
		      resRelease =  $dschMean(n=>i_in,m=>j_in) *
			  (resLevel < Xe ? Y0 + a_Par*log(1 + c_Par*resLevel) : B_Par + b_Par*pow(resLevel - Xb, p_Par));
		      resLevel  += ($dschPrev(n=>i_in,m=>j_in)+deltaDisch*i - resRelease)*$dt()/subDh / resCapacity;
		      if (resLevel > 1) resLevel = 1;
		      if (resLevel < 0) resLevel = 0;
		  }
	      }
	  } // Observed vs predicted reservoir release block
		 ///// Update reservoir storage

	  $resStorage(n=>i_in,m=>j_in) =
	  $resStoragePrev(n=>i_in,m=>j_in) + ($dsch(n=>i_in,m=>j_in) - resRelease)*$dt();
						 //// If overfilled
	  if ($resStorage(n=>i_in,m=>j_in) > resCapacity) {
	      resRelease = $dsch(n=>i_in,m=>j_in) + ($resStoragePrev(n=>i_in,m=>j_in) - resCapacity)/$dt();
	      $resStorage(n=>i_in,m=>j_in) = resCapacity;
	  }					//// If empty
	  else if ($resStorage(n=>i_in,m=>j_in) < 0) {
	      resRelease = $dsch(n=>i_in,m=>j_in) + $resStoragePrev(n=>i_in,m=>j_in)/$dt();
	      $resStorage(n=>i_in,m=>j_in) = 0;
	  }
      }

  ////////  Water Withdrawal from Flow (irrigation and other water uses)  ////////

      if ($dschUsed(n=>i_in,m=>j_in) > 0) {
	storageFlow	= $resStorage(n=>i_in,m=>j_in)/$dt();			// Flow to drain reservoir
	denominator	= resRelease + storageFlow;
	if ($dschUsed(n=>i_in,m=>j_in) > denominator) $dschUsed(n=>i_in,m=>j_in) = denominator;

			/// Case of proportional withdrawal
	if (denominator > 0) {
	  $resStorage(n=>i_in,m=>j_in)	-=($dschUsed(n=>i_in,m=>j_in) * storageFlow/denominator)*$dt();
	   resRelease			-= $dschUsed(n=>i_in,m=>j_in) * resRelease /denominator;
	}
      }

  ////////  Connectivity Routing  ////////

      if ($connStack(mst=>iConnStackPos) == i_cell) {
	nC = $connStack(mst=>++iConnStackPos);	// Number of connectivity routes in the pixel (ii,jj)
	diversionCell = 0;			// Total diversion from the cell
	storageFlow   = resCapacity ? $resStorage(n=>i_in,m=>j_in)/$dt() : 0;	// Flow to drain reservoir

	for(j_route=0; j_route<nC; j_route++) {
	  routeI = $connStack(mst=>++iConnStackPos);
	  $connParms(pk=>6,cl=>routeI) = 0;		// Evaporation from canal, reset it from previous time step

	if (($connTable(ck=>4,cl=>routeI) < $j_date()) && ($connTable(ck=>5,cl=>routeI) >= $j_date()) &&
	    (resRelease+storageFlow > 0)) {

	  ci_out  = $connTable(ck=>2,cl=>routeI);
	  cj_out  = $connTable(ck=>3,cl=>routeI);
	  c_parm1 = $connParms(pk=>0,cl=>routeI);	// PercentFlow parameter
	  c_parm2 = $connParms(pk=>1,cl=>routeI);	// MinFlow parameter
	  c_parm3 = $connParms(pk=>2,cl=>routeI);	// MaxFlow parameter

					////////// Connectivity universal rule (calculate diversion)-
	  diversion = (resRelease+storageFlow  <= c_parm2) ? 0 : (resRelease+storageFlow - c_parm2) * c_parm1/100;
	  if (diversion > c_parm3) diversion	= c_parm3;
	  $routeDiversion(cl=>routeI)		= diversion;

					////////// Connectivity Evaporation-
	  $connParms(pk=>4,cl=>routeI)  = tau * pow(diversion, phi);			// Canal Width, m
	  if ($connParms(pk=>4,cl=>routeI) < 0.01) $connParms(pk=>4,cl=>routeI) = 0;	// Cut out miniscules

	  $connParms(pk=>6,cl=>routeI)  = subDf *				// Evaporation from canal, m3
					  $connParms(pk=>3,cl=>routeI) *	// Canal length, km
					  $connParms(pk=>4,cl=>routeI) *	// Canal width, m
					  $connParms(pk=>5,cl=>routeI);		// Open water evaporation, mm/m2/day

	  if (diversion - $connParms(pk=>6,cl=>routeI)/$dt() > 0.001) {		// Cut out miniscules
	    $dsch(n=>ci_out,m=>cj_out)   += diversion - $connParms(pk=>6,cl=>routeI)/$dt();
	  }
	  else {
	    $connParms(pk=>6,cl=>routeI) = diversion*$dt();
	  }

		// Water components- age, origin, source, irrigation use, temperature, trace
	  waterVolume = diversion * $dt() - $connParms(pk=>6,cl=>routeI);	// m3
	  if ( $cSwitch(cNum=>0) )
	    $waterAge(   n=>ci_out,m=>cj_out)       += waterVolume * $waterAge( n=>i_in, m=>j_in);

	  if ( $cSwitch(cNum=>1) ) {
	    for(i=0; i<c_size; i++) {
	      $dschFr(   n=>ci_out,m=>cj_out,cc=>i) += waterVolume * $dschFr(   n=>i_in, m=>j_in,cc=>i);
	    }
	  }
	  if ( $cSwitch(cNum=>2) ) {
	    for(i=0; i<p_size; i++) {
	      $dschFrPm( n=>ci_out,m=>cj_out,cp=>i) += waterVolume * $dschFrPm( n=>i_in, m=>j_in,cp=>i);
	    }
	  }
	  if ( $cSwitch(cNum=>3) ) {
	    for(i=0; i<i_size; i++) {
	      $dschIrrFr(n=>ci_out,m=>cj_out,ci=>i) += waterVolume * $dschIrrFr(n=>i_in, m=>j_in,ci=>i);
	    }
	  }
			/////////  Heating/Cooling of the stream by ambient air and solar radiation
	  if ( $cSwitch(cNum=>4) && waterVolume*$connParms(pk=>4,cl=>routeI) > 0 ) {
	    wTemperature = $waterTe(n=>i_out,m=>j_out) +
		  ($dschTw(n=>i_in, m=>j_in) - $waterTe(n=>i_out,m=>j_out)) *
		  exp(-$connParms(pk=>7,cl=>routeI)*$connParms(pk=>3,cl=>routeI) /
		      (4186.8*(waterVolume/$connParms(pk=>4,cl=>routeI))));
	    if (wTemperature < 0) wTemperature = 0;
	    $dschTw(n=>ci_out,m=>cj_out) += waterVolume * wTemperature;
	  }
	  if ( $cSwitch(cNum=>5) ) {
	    for(i=0; i<mm_size; i++) {
	      $dschMsk(n=>ci_out,m=>cj_out,mm=>i)   += waterVolume * $dschMsk(n=>i_in, m=>j_in,mm=>i);
	    }
	  }
			/////////  No denitrification during the interbasin transfer
	  if ( $cSwitch(cNum=>6) ) {
	    $dschDIN(n=>ci_out,m=>cj_out) += waterVolume * $dschDIN(n=>i_in, m=>j_in);
	    $consDIN(n=>ci_out,m=>cj_out) += waterVolume * $consDIN(n=>i_in, m=>j_in);	// Conservative DIN
	  }
	  diversionCell += diversion;
	}}
		/// Subtract Diversion Withdrawal from Donor Flow and Reservoir Storage
	if (diversionCell > 0) {
			/// Case of proportional withdrawal (depreciated)
//	  $resStorage(n=>i_in,m=>j_in)	-=(diversionCell * storageFlow/(resRelease+storageFlow))*$dt();
//	  resRelease			-= diversionCell * resRelease /(resRelease+storageFlow);

			/// Case of flow-priority withdrawal
	  if (diversionCell > (resRelease-c_parm2)) {			// c_parm2 is MinFlow
	    $resStorage(n=>i_in,m=>j_in)-=(diversionCell - (resRelease-c_parm2))*$dt();
	    diversionCell		 = resRelease-c_parm2;		// Max from Flow
	  }
	  resRelease			-= diversionCell;
	}

	iConnStackPos++;			// Advance stack pointer to the next cell
      }

  ////////  Remove water to aquifer sinks  ////////

      if ( $snkStack(    nsk=>iSnkStackPos) == i_cell ) {
	$snkData(msk=>2, nsk=>iSnkStackPos) += resRelease * $snkData(msk=>0, nsk=>iSnkStackPos);   // Sink infiltration
	resRelease			    *= (1.0       - $snkData(msk=>0, nsk=>iSnkStackPos));  // Update discharge
	iSnkStackPos++;
      }

  ////////  USGS gauging data assimilation  ////////

      if ($usgsStack(	nus=>iUsgsStackPos ) == i_cell) {
	$usgsData(	mus=>1, nus=>iUsgsStackPos) =  resRelease;				// WBM discharge
	if ($usgsData(	mus=>0, nus=>iUsgsStackPos) >  0)					// Update Delta
	    $usgsData(	mus=>2, nus=>iUsgsStackPos) =  $usgsData(mus=>0, nus=>iUsgsStackPos) - resRelease;
	if ($usgsData(	mus=>2, nus=>iUsgsStackPos) < -resRelease)				// No negative discharge
	    $usgsData(	mus=>2, nus=>iUsgsStackPos) = -resRelease;
	resRelease				   +=  $usgsData(mus=>2, nus=>iUsgsStackPos);	// Add delta
		/// Shift USGS stack
	iUsgsStackPos++;
      }

  ////////  Update outDischarge pixels  ////////

      if (resRelease < 0) {				// It should not have been needed,
        $dschUsed(n=>i_in,m=>j_in) -= resRelease;	// but we had a problem with floating number ops
        resRelease = 0;
      }
      $flowOut(l=>i_cell) = resRelease;
      if ($table(k=>0,l=>i_cell) != 0) {
	$dsch(n=>i_out,m=>j_out) += resRelease;

		// Water components- age, origin, source, irrigation use, temperature, trace
        waterVolume = resRelease * $dt();
	if ( $cSwitch(cNum=>0) )
	  $waterAge(   n=>i_out,m=>j_out)       += waterVolume * $waterAge( n=>i_in, m=>j_in);

	if ( $cSwitch(cNum=>1) ) {
	  for(i=0; i<c_size; i++) {
	    $dschFr(   n=>i_out,m=>j_out,cc=>i) += waterVolume * $dschFr(   n=>i_in, m=>j_in,cc=>i);
	  }
	}
	if ( $cSwitch(cNum=>2) ) {
	  for(i=0; i<p_size; i++) {
	    $dschFrPm( n=>i_out,m=>j_out,cp=>i) += waterVolume * $dschFrPm( n=>i_in, m=>j_in,cp=>i);
	  }
	}
	if ( $cSwitch(cNum=>3) ) {
	  for(i=0; i<i_size; i++) {
	    $dschIrrFr(n=>i_out,m=>j_out,ci=>i) += waterVolume * $dschIrrFr(n=>i_in, m=>j_in,ci=>i);
	  }
	}
	if ( $cSwitch(cNum=>4) )
	  $dschTw(n=>i_out,m=>j_out)            += waterVolume * $dschTw(n=>i_in, m=>j_in);

	if ( $cSwitch(cNum=>5) ) {
	  for(i=0; i<mm_size; i++) {
	    $dschMsk(n=>i_out,m=>j_out,mm=>i)   += waterVolume * $dschMsk(n=>i_in, m=>j_in,mm=>i);
	  }
	}
	if ( $cSwitch(cNum=>6) ) {
	  $dschDIN(n=>i_out,m=>j_out)           += waterVolume * $dschDIN(n=>i_in, m=>j_in);
	  $consDIN(n=>i_out,m=>j_out)           += waterVolume * $consDIN(n=>i_in, m=>j_in);	// Conservative DIN
	}
      }
    }
');

#######################################################################
#######################################################################

pp_def('snow_routing', HandleBad => 1,
  Pars => 'int table(k,l);
    double snowPack(n,m);  double snowMask(n,m); double area(n,m); double maxDepth();
    double [o] gMelt(n,m); double [o] gPackChg(n,m);',

  Code => '
		// Dimension constants
    int k_size = $SIZE(k);	int l_size = $SIZE(l);

		// General internal/private variables
    int i_cell, i_in, i_out, j_in, j_out;
    double delta;

		// Initialization of the output arrays
    loop(n,m) %{ if ($ISBAD($snowPack())) { $SETBAD($gPackChg()); $SETBAD($gMelt()); }
		 else { $gPackChg() = $gMelt() = 0; } %}

		// Routing
    for(i_cell=0; i_cell<l_size; i_cell++) {
      i_out = $table(k=>1,l=>i_cell);
      j_out = $table(k=>2,l=>i_cell);
      i_in  = $table(k=>3,l=>i_cell);
      j_in  = $table(k=>4,l=>i_cell);

      if (	$snowPack(n=>i_in,  m=>j_in) <= $maxDepth() || $snowMask(n=>i_in,m=>j_in) == 0 ) continue;
	delta = $snowPack(n=>i_in,  m=>j_in)  - $maxDepth();
		$snowPack(n=>i_in,  m=>j_in)  = $maxDepth();
		$gPackChg(n=>i_in,  m=>j_in) -= delta;
      if ($table(k=>0,l=>i_cell) != 0) {			// Check if it is an outlet (river mouth)
	delta/= $area    (n=>i_out, m=>j_out) / $area(n=>i_in,m=>j_in);
		$gPackChg(n=>i_out, m=>j_out)+= delta;
		$snowPack(n=>i_out, m=>j_out)+= delta;
		$snowMask(n=>i_out, m=>j_out) = 1;		// This allows to push snow across equator
      } else {	$gMelt   (n=>i_out, m=>j_out) = delta; }
    }
');

#######################################################################
#######################################################################

pp_def('demandFromStreamGwater', HandleBad => 1, NoPthread => 1,
  Pars => 'double irrDemand(n,m);
    double groundWater(n,m);	double resStorage(n,m);    double sirStorage(n,m);	double discharge(n,m);
    int    aqfType();		int    aqf_ID(k1);	   double irrExtraCoeff(n,m);	double gwtUseC();
    int    aquiferID(n,m);	double aqfStorage(n,m);	   double Cfd_dQ(n,m);		double aqf_data(k0,k1);
    double mm_2_m3(n,m);	double SW_ratio(n,m);	   int    irrSearchDist(n,m);	double dt();

    double [o] irrStrgLoc(n,m);	double [o] irrFlowLoc(n,m);
    double [o] irrStrgRmt(n,m);	double [o] irrFlowRmt(n,m);	int    [o] irrFlowRID(nd,n,m);
    double [o] irrSIR(n,m);	double [o] irrGrwt(n,m);	double [o] irrAquifer(n,m);
    double [o] irrExtra(n,m);',

  RedoDimsCode => '$SIZE(nd) = 2;',

  Code => '
  ////////  Irrigation water withdrawal from stream storage (priority) and water storage  ////////

    int n_size  = $SIZE(n);	int i_sch, j_sch, indID;
    int m_size  = $SIZE(m);	int i, ii, j, jj;
  //int k0_size = $SIZE(k0);				// 0- Storage; 1- Delta
    int k1_size = $SIZE(k1);				// Number of Aquifer IDs
    int max_ID  = $aqf_ID(k1=>k1_size-1)+1;		// Max+1  of Aquifer IDs (last element of aqf_ID)

    double irrigation, irrDelta, gwtDelta, delta_m3, rStorage, denominator, flowStorage, flowFrac, strgFrac, aqfStrg;
    double maxFraction = 0.8;			// Maximum water take out from river flow & storage

    double *myDemand;	myDemand = malloc(k1_size*sizeof *myDemand);
    double *myRatio ;	myRatio  = malloc(k1_size*sizeof *myRatio );
    double *myAqfSum;	myAqfSum = malloc(k1_size*sizeof *myAqfSum);
    int    *index_ID;	index_ID = malloc(max_ID *sizeof *index_ID);

  ////////  Irrigation from River Storage  ////////

    for (jj = 0; jj < m_size; jj++) {
      for (ii = 0; ii < n_size; ii++) {
			//	Initialization of the output arrays
	if ($ISBAD($groundWater(n=>ii,m=>jj))) {
		$SETBAD($irrStrgLoc(n=>ii,m=>jj));
		$SETBAD($irrFlowLoc(n=>ii,m=>jj));
		$SETBAD($irrStrgRmt(n=>ii,m=>jj));
		$SETBAD($irrFlowRmt(n=>ii,m=>jj));
		$SETBAD($irrSIR    (n=>ii,m=>jj));
		$SETBAD($irrGrwt   (n=>ii,m=>jj));
		$SETBAD($irrAquifer(n=>ii,m=>jj));
		$SETBAD($irrExtra  (n=>ii,m=>jj)); }
	else {
		$irrStrgLoc(n=>ii,m=>jj) = $irrFlowLoc(n=>ii,m=>jj) =
		$irrStrgRmt(n=>ii,m=>jj) = $irrFlowRmt(n=>ii,m=>jj) = $irrSIR(n=>ii,m=>jj) =
		$irrGrwt   (n=>ii,m=>jj) = $irrAquifer(n=>ii,m=>jj) =
		$irrExtra  (n=>ii,m=>jj) = 0;
	}
	$irrFlowRID(nd=>0,n=>ii,m=>jj) = ii;
	$irrFlowRID(nd=>1,n=>ii,m=>jj) = jj;

	if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $irrDemand(n=>ii,m=>jj) == 0 ) continue;
	irrigation	= 0;
	irrDelta	= $irrDemand(n=>ii,m=>jj);

			// First, take irrigation water from Small Irrigation Reservoirs (SIR)
	if ($sirStorage(n=>ii,m=>jj) > 0) {
	  irrDelta = $irrDemand(n=>ii,m=>jj) <= $sirStorage(n=>ii,m=>jj) ? $irrDemand(n=>ii,m=>jj) : $sirStorage(n=>ii,m=>jj);
	  irrigation			+= irrDelta;
	  $irrSIR(n=>ii,m=>jj)		 = irrDelta;
	  $sirStorage(n=>ii,m=>jj)	-= irrDelta;
	  irrDelta			 = $irrDemand(n=>ii,m=>jj) - irrigation;
	}

			// Second, take irrigation water from ground water by SW-ratio (sustainable)
	if (irrDelta > 0) {
	  irrDelta *= (1 - $SW_ratio(n=>ii,m=>jj));
	  irrDelta  = irrDelta <= $groundWater(n=>ii,m=>jj) ? irrDelta : $groundWater(n=>ii,m=>jj);
	  irrigation			+= irrDelta;
	  $irrGrwt(n=>ii,m=>jj)		 = irrDelta;
	  $groundWater(n=>ii,m=>jj)	-= irrDelta;
	  irrDelta			 = $irrDemand(n=>ii,m=>jj) - irrigation;
	} else { continue; }

			// Third, take irrigation water from own river by SW-ratio
	if (irrDelta > 0) {
	  flowStorage	= $discharge( n=>ii,m=>jj) * $dt();
	  denominator	= $resStorage(n=>ii,m=>jj) + flowStorage;
	  if (denominator > 0) {
	    flowFrac	= flowStorage / denominator;
	    strgFrac	= 1 - flowFrac;
	    rStorage = maxFraction * denominator / $mm_2_m3(n=>ii,m=>jj);
	    irrDelta = irrDelta <= rStorage ? irrDelta : rStorage;
	    irrigation			+= irrDelta;
	    $irrFlowLoc(n=>ii,m=>jj)	 = irrDelta * flowFrac;
	    $irrStrgLoc(n=>ii,m=>jj)	 = irrDelta * strgFrac;

				/// Case of proportional withdrawal
	    $discharge( n=>ii,m=>jj)	-= irrDelta * flowFrac * $mm_2_m3(n=>ii,m=>jj) / $dt();
	    $resStorage(n=>ii,m=>jj)	-= irrDelta * strgFrac * $mm_2_m3(n=>ii,m=>jj);
	    irrDelta			 = $irrDemand(n=>ii,m=>jj) - irrigation;
	    if ($discharge(n=>ii,m=>jj) < 0) $discharge(n=>ii,m=>jj) = 0;	// Near zero float number problem
	  }
	} else { continue; }

			// Fourth, take irrigation water from ground water, if still needed
	if (irrDelta > 0) {
	  if ($groundWater(n=>ii,m=>jj) > 0) {
	    irrDelta = irrDelta <= $groundWater(n=>ii,m=>jj) ? irrDelta : $groundWater(n=>ii,m=>jj);
	    irrigation			+= irrDelta;
	    $irrGrwt(n=>ii,m=>jj)	+= irrDelta;
	    $groundWater(n=>ii,m=>jj)	-= irrDelta;
	    irrDelta			 = $irrDemand(n=>ii,m=>jj) - irrigation;
	  }
	} else { continue; }

			// Fifth, take irrigation water from largest stream around, if still needed
	if (irrDelta > 0) {
	  if ($irrSearchDist(n=>ii,m=>jj) > 0) {
	    i_sch = ii;	j_sch = jj;	rStorage = 0;

	    for (j = jj-$irrSearchDist(n=>ii,m=>jj); j <= jj+$irrSearchDist(n=>ii,m=>jj); j++) {
	      if (j < 0 || j >= m_size) continue;
	      for (i = ii-$irrSearchDist(n=>ii,m=>jj); i <= ii+$irrSearchDist(n=>ii,m=>jj); i++) {
		if ((i < 0 || i >= n_size) || $ISBAD($resStorage(n=>i,m=>j))) continue;
		denominator = $resStorage(n=>i,m=>j) + $discharge(n=>i,m=>j)*$dt();
		if (rStorage < denominator) {
		  i_sch = i; j_sch = j;  rStorage = denominator;
		}
	      }
	    }			// Take from found largest river around
	    flowStorage	= $discharge( n=>i_sch,m=>j_sch) * $dt();
	    denominator	= $resStorage(n=>i_sch,m=>j_sch) + flowStorage;
	    if (denominator > 0) {
	      flowFrac	= flowStorage / denominator;
	      strgFrac	= 1 - flowFrac;
	      rStorage	= maxFraction * denominator / $mm_2_m3(n=>ii,m=>jj);
	      irrDelta	= irrDelta <= rStorage ? irrDelta : rStorage;
	      irrigation		+= irrDelta;
	      $irrFlowRmt(n=>ii,m=>jj)	 = irrDelta * flowFrac;
	      $irrStrgRmt(n=>ii,m=>jj)	 = irrDelta * strgFrac;
	      $irrFlowRID(nd=>0,n=>ii,m=>jj) = i_sch;
	      $irrFlowRID(nd=>1,n=>ii,m=>jj) = j_sch;
					/// Case of proportional withdrawal
	      $discharge( n=>i_sch,m=>j_sch) -= irrDelta * flowFrac * $mm_2_m3(n=>ii,m=>jj) / $dt();
	      $resStorage(n=>i_sch,m=>j_sch) -= irrDelta * strgFrac * $mm_2_m3(n=>ii,m=>jj);
	      irrDelta = $irrDemand(n=>ii,m=>jj) - irrigation;
	      if ($discharge(n=>i_sch,m=>j_sch) < 0) $discharge(n=>i_sch,m=>j_sch) = 0;	// Near zero float number problem
	    }
	  }
	}
	$irrExtra(n=>ii,m=>jj) = irrDelta;		// Use irrExtra to keep temporarily the demand remainder
      }
    }

  ////////  Water use from aquifer  ////////		// Initializations. NB- myAqfSum is used to reduce number of operations
							// for cases when an aquifer storage is large (10^15 m3) that causes
    if ($aqfType()) {					// accumulated error of multiple substractions due to the number is
      for (i = 0; i < k1_size; i++) myAqfSum[i] = 0;	// at or near 16 significant digits that "double" type can take.
      for (i = 0; i < max_ID;  i++) index_ID[i]	= 0;
      for (i = 0; i < k1_size; i++) index_ID[$aqf_ID(k1=>i)] = i;		// Aquifer ID indices

  ////////  Check virtual/lumped aquifer water supply to meet demand  ////////

      if ($aqfType() < 3) {

	for ( i = 0;  i < k1_size; i++) myDemand[i] = myRatio[i]	= 0;		// Demand & Ratio for whole aquifer
        for (jj = 0; jj < m_size; jj++) {
	  for (ii = 0; ii < n_size; ii++) {
	    if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $irrExtra(n=>ii,m=>jj) == 0 ) continue;
	      indID		 = index_ID[$aquiferID(n=>ii,m=>jj)];
	      myDemand[indID]	+= $irrExtra(n=>ii,m=>jj) * $mm_2_m3(n=>ii,m=>jj);	// m3
	  }
	}
	for (i = 0; i < k1_size; i++) myRatio[i] = myDemand[i] > 0 && myDemand[i] > $aqf_data(k0=>0,k1=>i) ?
		$aqf_data(k0=>0,k1=>i)/myDemand[i] : 1;				// Set the ratio
      }

  ////////  Water use from aquifer source  ////////

      for (jj = 0; jj < m_size; jj++) {
	for (ii = 0; ii < n_size; ii++) {
	  if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $aquiferID(n=>ii,m=>jj) == 0 || $irrExtra(n=>ii,m=>jj) == 0 ) continue;

			// Sixth, take irrigation water from aquifer, if still needed
	    indID	   = index_ID[$aquiferID(n=>ii,m=>jj)];
	    if ($aqfType() == 3) {
		  aqfStrg  = $aqfStorage(n=>ii,m=>jj) + $Cfd_dQ(n=>ii,m=>jj);
		  irrDelta = $irrExtra(n=>ii,m=>jj)   * $mm_2_m3(n=>ii,m=>jj);
	      if (irrDelta > aqfStrg)     irrDelta    = aqfStrg;
		  $Cfd_dQ(n=>ii,m=>jj) -= irrDelta;
		  irrDelta*= $mm_2_m3(n=>ii,m=>jj) ? 1/$mm_2_m3(n=>ii,m=>jj) : 0; }
	    else{ irrDelta = $irrExtra(n=>ii,m=>jj)* myRatio[indID]; }
	    delta_m3       = irrDelta * $mm_2_m3(n=>ii,m=>jj);

	    $irrAquifer(n=>ii,m=>jj)	+= irrDelta;
	    $irrExtra(n=>ii,m=>jj)	-= irrDelta;
	    myAqfSum[indID]		-= delta_m3;
	    $aqf_data(k0=>1,k1=>indID)	-= delta_m3;
	}
      }

  ////////  Check virtual/lumped aquifer water supply to substitute groundwater  ////////

      if ($gwtUseC() < 1) {
	if ($aqfType() < 3) {

	  for ( i = 0;  i < k1_size; i++) myDemand[i] = myRatio[i] = 0;		// Demand & Ratio for whole aquifer
	  for (jj = 0; jj < m_size; jj++) {
	    for (ii = 0; ii < n_size; ii++) {
	      if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $irrGrwt(n=>ii,m=>jj) == 0 ) continue;
	      indID = index_ID[$aquiferID(n=>ii,m=>jj)];
	      myDemand[indID] += $irrGrwt(n=>ii,m=>jj) * (1 - $gwtUseC()) * $mm_2_m3(n=>ii,m=>jj);	// m3
	    }
	  }
	  for (i = 0; i < k1_size; i++) myRatio[i] = myDemand[i] > 0 && myDemand[i] > ($aqf_data(k0=>0,k1=>i)+myAqfSum[i]) ?
		($aqf_data(k0=>0,k1=>i)+myAqfSum[i])/ myDemand[i] : 1;				// Set the ratio
	}

  ////////  Substitute groundwater with aquifer source  ////////

	for (jj = 0; jj < m_size; jj++) {
	  for (ii = 0; ii < n_size; ii++) {
	    if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $aquiferID(n=>ii,m=>jj) == 0 || $irrGrwt(n=>ii,m=>jj) == 0 ) continue;

			// Seventh, substitute groundwater with aquifer source
	    indID	   = index_ID[$aquiferID(n=>ii,m=>jj)];
	    if ($aqfType() == 3) {
		  aqfStrg  = $aqfStorage(n=>ii,m=>jj) + $Cfd_dQ(n=>ii,m=>jj);
		  gwtDelta = $irrGrwt(n=>ii,m=>jj)   * $mm_2_m3(n=>ii,m=>jj) * (1 - $gwtUseC());
	      if (gwtDelta > aqfStrg)     gwtDelta   = aqfStrg;
		  $Cfd_dQ(n=>ii,m=>jj) -= gwtDelta;
		  gwtDelta*= $mm_2_m3(n=>ii,m=>jj) ? 1/$mm_2_m3(n=>ii,m=>jj) : 0; }
	    else{ gwtDelta = $irrGrwt(n=>ii,m=>jj) * myRatio[indID]          * (1 - $gwtUseC()); }
	    delta_m3       = gwtDelta * $mm_2_m3(n=>ii,m=>jj);

	    $irrAquifer(n=>ii,m=>jj)	+= gwtDelta;
	    $irrGrwt(n=>ii,m=>jj)	-= gwtDelta;
	    myAqfSum[indID]		-= delta_m3;
	    $aqf_data(k0=>1,k1=>indID)	-= delta_m3;
	    $groundWater(n=>ii,m=>jj)	+= gwtDelta;
	  }
	}
      }
      for (i = 0; i < k1_size; i++) $aqf_data(k0=>0,k1=>i) += myAqfSum[i];	// Finilizing
    }

			// Eighth, take irrigation water from unsustanable source, if still needed
    for (jj = 0; jj < m_size; jj++) {
      for (ii = 0; ii < n_size; ii++) {
	if ( $ISBAD($groundWater(n=>ii,m=>jj)) || $irrExtra(n=>ii,m=>jj) == 0 ) continue;

	$irrExtra(n=>ii,m=>jj) *= $irrExtraCoeff(n=>ii,m=>jj);
      }
    }
	// Free OS memory
    free(myDemand);
    free(myRatio );
    free(myAqfSum);
    free(index_ID);
');

#######################################################################
#######################################################################

pp_def('rmtOrig', HandleBad => 1, NoPthread => 1,
  Pars		=> '	double     flowRmt(n,m);	int irrFlowRID(nd,n,m);
			double [o] flowOrig(n,m);',
  Code		=> '
  ////////////////////////////////////////////////////////////////////////////////////////
  ////////  PDL version of locating remote water source (Origin) by its indicies  ////////
  ////////////////////////////////////////////////////////////////////////////////////////

    int n_size = $SIZE(n);	int m_size = $SIZE(m);
    int i, j, ii, jj;

	//	Initialization of the output arrays
    loop(n,m) %{
	if ($ISBAD($flowRmt())) {
			$SETBAD($flowOrig());	}
	else {			$flowOrig() = 0;}
    %}
	//	Accumulating output array
    for (j = 0; j < m_size; j++) {
      for (i = 0; i < n_size; i++) {
	if ( $ISBAD($flowRmt(n=>i,m=>j)) || $flowRmt(n=>i,m=>j) == 0 ) continue;

	ii = $irrFlowRID(nd=>0,n=>i,m=>j);
	jj = $irrFlowRID(nd=>1,n=>i,m=>j);

	$flowOrig(n=>ii,m=>jj) += $flowRmt(n=>i,m=>j);
      }
    }
');

#######################################################################
#######################################################################

pp_addhdr('
  #include <unistd.h>       /* we need defs of XXXX */
  #include <stdio.h>
  #include <math.h>

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

pp_def('addReservoirs', HandleBad => 1,
  Pars => 'int table(k,l);
    int resData(cRes,mRes); double resParams(nRes,mRes); int resStack(nst);
    double [o] resStorage(n,m);',
  OtherPars => '
    double fill; int j_date; int ns => n; int ms => m;',
  Code => '
    int i_cell, i, ii, jj, nR, damNum;
    int iResStackPos = 0;
    int l_size = $SIZE(l);
    int jdate  = $COMP(j_date); double rFill = $COMP(fill);

	//	Initialization of the output arrays
    loop(n,m) %{ $resStorage() = 0; %}

	//	Building initial reservoir storage

    for(i_cell=0; i_cell<l_size; i_cell++) {
      if ($resStack(nst=>iResStackPos) == i_cell) {
	ii  = $table(k=>3,l=>i_cell);
	jj  = $table(k=>4,l=>i_cell);

	nR = $resStack(nst=>++iResStackPos);	// Number of dams in the pixel (ii,jj)
	for(i=0; i<nR; i++) {
	  damNum = $resStack(nst=>++iResStackPos);

		// Check reservoir dates and add:	 60 % of reservoir capacity or
		//					100 % in case of spillway dams
	  if (jdate >= $resData(cRes=>2,mRes=>damNum) && jdate < $resData(cRes=>3,mRes=>damNum))
	    $resStorage(n=>ii,m=>jj) += ($resData(cRes=>4,mRes=>damNum) ? rFill : 1) * $resParams(nRes=>0,mRes=>damNum);
	}
	iResStackPos++;			// Advance stack pointer to the next dam
      }
    }
');

#######################################################################

pp_def('resArea', HandleBad => 1,
  Pars => 'int table(k,l);
    int resData(cRes,mRes); double resParams(nRes,mRes); int resStack(nst);
    double [o] rsrvrArea(n,m);',
  OtherPars => '
    int j_date; int ns => n; int ms => m;',
  Code => '
    int i_cell, i, ii, jj, nR, damNum;
    int iResStackPos = 0;
    int l_size = $SIZE(l);
    int jdate  = $COMP(j_date);

	//	Initialization of the output arrays
    loop(n,m) %{ $rsrvrArea() = 0; %}

	//	Building Reservoir/Lake Surface Area

    for(i_cell=0; i_cell<l_size; i_cell++) {
      if ($resStack(nst=>iResStackPos) == i_cell) {
	ii  = $table(k=>3,l=>i_cell);
	jj  = $table(k=>4,l=>i_cell);

	nR = $resStack(nst=>++iResStackPos);	// Number of dams in the pixel (ii,jj)
	for(i=0; i<nR; i++) {
	  damNum = $resStack(nst=>++iResStackPos);

		// Add Reservoir Area
	  if (jdate >= $resData(cRes=>2,mRes=>damNum) && jdate < $resData(cRes=>3,mRes=>damNum))
	    $rsrvrArea(n=>ii,m=>jj) += $resParams(nRes=>1,mRes=>damNum);
	}
	iResStackPos++;			// Advance stack pointer to the next dam
      }
    }
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

pp_done();
