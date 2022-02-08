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
#	Some General I/O functions used in PDL WBM script.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	January, 2011
#	Last Modified-	See Main model for details.
#
#	Version-	(YY.M.#)	# Number is zero-based
my $version =		'22.1.0';	# Version. Note- update version variable below
#
#######################################################################

package RIMS::WBM;

use strict;
use Benchmark;
use Cwd qw/abs_path/;
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Temp;
use Geo::GDAL;
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
use Storable qw/dclone/;
use Sys::Hostname;
use Time::JulianDay;
use Time::DaysInMonth;
use RIMS qw(get_file_path date_now htm_template read_attrib read_table read_table_hash get_calendar make_date_list check_MT_date set_default prepare_dir crs_to_Obj transform_point);

require Exporter;

#######################################################################

our $VERSION	= $version;
our @ISA	= qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use RIMS::WBM ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
get_conf copy_path set_err wbm_params wbm_reqParams spinup_param_init dump_init attrib isInitFile check_defaults read_init read_param_str init_DSet isNumber make_runAttr make_list_out make_GW_BW_output make_dataCube uniq_list GDAL_FileCheck isDataSet_TS read_Layer read_dateLayer read_climate_bias read_GDAL read_raster get_extent lonLat cell_area area_layers write_cell_area colRow write_gridascii write_tif write_nc GDAL_test unpdl_data unpdl_scalars flow_to check_circularityC build_cell_table cell_table find_outlets saveEndoMask grid_to_table table_to_grid add_connectivity diversion_yr_total aquifer_summary compBalance_save add_reservoirs a_dam_equaton b_dam_equaton B_dam_equaton d_root_solver add_DIN_WWPT springs_init usgs_init update_usgs rehash_stack make_stack gl_scale aquifer_init aqf_ID_mask Aqf_Storage_2_Head Aqf_Well_Storage ModFlow_EFDM condition condition_slice set_to_zero checkZero checkOnes hash_to_arr hash_to_keys arr_to_hash change_meta add_spinup_dates readState fileBaseName time_used post_processing make_build_spool bMarkMap make_URL
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT    = ( @{ $EXPORT_TAGS{'all'} } );

#######################################################################

use vars qw(*NEWERR *OLDERR);	# To avoid silly warning messages from GDAL, such as
open NEWERR, ">/dev/null";	# "No UNIDATA NC_GLOBAL:Conventions attribute"
open OLDERR, ">&STDERR";

no if $] >= 5.017011, warnings => 'experimental::smartmatch';

my $conf_file	= abs_path(dirname($0)).'/WBM.conf';		# Try conf file in the user directory
   $conf_file	= '/etc/WBM.conf' unless -e $conf_file;		# Default conf
my %path	= read_init($conf_file);
my $PATH	= get_file_path();

sub get_conf  { return %path; }
sub copy_path { %path = @_;   }
sub set_err   { open NEWERR, ">&STDERR" }

######################  Functions  ####################################

sub wbm_params
{
  return [ qw(ID Comment Project MT_Code_Name Spinup Output_vars MT_Precip precipFraction MT_airT Network Projection Routing wbmParam ConnectivityNetwork Reservoirs soilAWCapacity FieldCap WiltingPoint rootingDepth PET MT_windU MT_windV MT_cloudFr MT_humidity MT_albedo airT_yc openWaterEvap Glaciers Aquifers Springs snowBands lapseDownscale DIN Runoff_mask Impervious openWater canopyHt MT_LAI WaterDemand landCollapse LandCover Irrigation WM_mod_precip WM_obs_precip WM_mod_airT WM_obs_airT PostProcessing WaterRights USGS) ];
}

sub wbm_reqParams
{
  return [ qw(ID MT_Code_Name Output_vars MT_Precip MT_airT Network Projection Routing rootingDepth) ];
}

sub MT_params
{
  return [ qw(Code_Name Data_Cube Project Time_Series Start_Date End_Date Legend_File Legend_Img Name Param_Name Var_Name Units Var_Scale Var_Offset Orig_Units Bands Shade_factor Shade_Default Processing Projection Round MetadataLink File_Path Data_Attrib Point_Data Polygon_Aggregation DownloadLink) ];
}

sub MT_reqParams
{
  return [ qw(Code_Name Time_Series Start_Date End_Date Var_Name Bands Projection File_Path) ];
}

#######################################################################

sub spinup_param_init
{
  my ($runIO, $flag) = @_;
  die "\nObsolete spinup parameters  input. Use \"Spinup\" block  instead. Aborting...\n\n"
	if grep m/^spinup_/i, keys %$runIO;

	### Read spinup parameters
  my %spinup	= read_param_str($runIO, 'Spinup');
  map { die "\nWrong key \"$_\" in the \"Spinup\" input block. See \"WBM_Usage_and_Input_Reference.xlsx\". Aborting...\n\n"
		     unless m/^(Start|End|Loops|State_ID|Force_ID_Date)$/; } keys %spinup;
  $spinup{Loops}= 0  unless exists $spinup{Loops} && isNumber($spinup{Loops});	map
 {$spinup{$_}	= '' unless exists $spinup{$_}; } qw(State_ID Start End Force_ID_Date);

	### Check spinup parameters
  map {	die "\nWrong format of Spinup=>$_ input. Should be YYYY-MM-DD. Aborting...\n\n"
		if  $spinup{$_} && $spinup{$_} !~ m/\d{4}-\d{2}-\d{2}/; } qw(Start End Force_ID_Date);
	die "\nInput Spinup=>Start/End dates are required with Loops > 0. Aborting...\n\n"
		if  $spinup{Loops} && !($spinup{Start} && $spinup{End});
	die "\nInput Spinup=>State_ID is required with the use of -dState flag. Aborting...\n\n"
		if !$spinup{State_ID} && $flag;
	die "\nCannot apply Spinup=>Force_ID_Date to undefined Spinup=>Force_ID state in the input. Aborting...\n\n"
		if  $spinup{Force_ID_Date} && !$spinup{State_ID};

  return \%spinup;
}

#######################################################################

sub dump_init
	### Dump initialization parameters to init file
{
  my ($file, $set, $options) = (shift, dclone(shift), shift);
  		### Options
  my $all_MT	= set_default( $$options{ALL_MT},	undef);
		### Make directory
  prepare_dir($file);
		### Make list of parameters (keys)
  my $isMT = exists($$set{Code_Name});
  my @list =  $all_MT ? @{MT_params()} :
	      $isMT   ?
		### MT parameters
    qw(Code_Name Time_Series Start_Date End_Date Name Var_Name Units Var_Scale Var_Offset Orig_Units Bands Processing Projection File_Path) :
		### WBM parameters
    @{wbm_params()};
		### Check and re-format block parameters
  unless ($isMT) {
    foreach my $param (@list) {
      if (ref $$set{$param}) {
	my $dumper = Data::Dumper->new([$$set{$param}]);
	$dumper->Purity(1)->Deepcopy(1)->Quotekeys(0)->Sortkeys(1);
	$$set{$param} = $dumper->Dump;		$$set{$param} =~ s/.+=//;	$$set{$param} =~ s/;\n$//;
	$$set{$param} =~ s/\n\s*\n/\n/g;	# Remove empty lines
  } } }
		### Make parameter string
  my $str = "{\n";					# Opening barcket of the hash string
  map { my $q	= isNumber($$set{$_})	? '' : $$set{$_}=~m/^\s*{/ ? '' : "'";
	my $tab	= length($_) > 7	? '' : "\t";
	$str   .= sprintf "%s\t$tab=> $q%s$q,\n", $_, $$set{$_}; } @list;
  $str  =~ s/( \w{1,5}) =>/$1\t\t=>/g;
  $str  =~ s/( \w+) =>/$1\t=>/g;
  $str  =~ s/,\n$/\n}\n/;				# Closing barcket of the hash string
		### Save parameters to init file
  open (FILE,">$file") or die "Couldn't open $file, $!";
    print FILE $str;
  close FILE;
}

#######################################################################

sub attrib
{			### Function to read RIMS dataSet attributes from MT or init file.
  my($id, $MT, $key)	=  @_;					# Optional parameter $key is for error reporting only:
	  $MT		= [$MT,''] if ref($MT) ne 'ARRAY';	#   Recommended to be use over top level WBM init keys
  die "\nEmpty dataset entry".($key?" for \"$key\"":'').". Aborting...\n\n" unless $id;

		### Read MT attributes
  my $file_flag	= isInitFile($id);
  my %attrib	= (ref($id) eq 'HASH') ? %$id : ($file_flag ? read_init($id) : read_attrib($$MT[0],$id,'Code_Name'));

		### Report error, if %attrib hash is not for RIMS dataSet
  die(	"\nRIMS dataSet or possibly (?) WBM initialization key \"$key\" has wrong format.\nSee WBM documentation.\n",
	"It is supposed to be a MT dataset ID or a path to a RIMS dataSet init file that exists, but it is-\n",
	"   $key => {\n", join(",\n", map(sprintf("\t$_\t=> $attrib{$_}"),keys(%attrib))), " },\n",
	"Aborting...\n\n")	if defined $key && !$attrib{Code_Name};

		### Check required MT attributes
  map { die("MT parameter \"$_\" is required in this MT ".($file_flag?'init file':'entry').":\n\t$id\nAborting...\n\n")
				unless exists $attrib{$_} }	@{MT_reqParams()};
  map { $attrib{$_} = ''	unless exists $attrib{$_} }	@{MT_params()};

		### Convert tileindex shape file to vrt (the former files have to retire and be replaces with vrt)
  tileindex_2_vrt(\%attrib)
	if ($attrib{File_Path}  =~ m/\.shp$/i || $attrib{File_Path} =~ m/\.shp\W/i) &&
	    $attrib{Processing} !~ m/polygon/i;

		### Check/verify if "Processing" has "polygon" instruction for ESRI shape files
  die "\nDataset ID: \"$attrib{Code_Name}\". " .
      "It appears to use shape file(s), but \"Processing\" does not have \"polygon\" instruction. Aborting...\n\n"
	if ($attrib{File_Path}  =~ m/\.shp$/i || $attrib{File_Path} =~ m/\.shp\W/i) &&
	    $attrib{Processing} !~ m/polygon/i;

		### Read dates from file, if needed
  check_MT_date($attrib{Start_Date});
  check_MT_date($attrib{End_Date});
		### Fill up blanks
  $attrib{Var_Scale}  = 1 unless $attrib{Var_Scale};
  $attrib{Var_Offset} = 0 unless $attrib{Var_Offset};
		### Other cleanups
  $attrib{Processing} =~ s/\s//g;		# Remove blank spaces in processing

  get_calendar(    \%attrib);			# Find the dataset calendar format
  write_MT($$MT[1],\%attrib) if $$MT[1];	# Save attributes to the Local MT file

  return \%attrib;
}

sub isInitFile
{			### Helper function to determine if it is an init file
  my $str = shift();
  return $str=~m/\/|\\|\.init$/i  || -T $str;
}

#######################################################################

sub tileindex_2_vrt
{
  my $attrib	= shift();
  my @files	= split m/;/, $$attrib{File_Path};

  foreach my $file_shp (@files) {
    $file_shp	=~ s/\s//g;		# Trim all white spaces
    $file_shp	=~ s/\(.+\):(.+)/$1/;	# Remove resolution scales
    next unless
   (my $file_vrt= $file_shp) =~ s/\.shp$/.vrt/;
    if (-e $file_vrt) {
	$$attrib{File_Path} =~ s/$file_shp/$file_vrt/; }
    else {
		### Perform the convertion from index shape file to VRT file
      open STDERR, ">&NEWERR";
	my $junk	= `$$PATH{gdalbuildvrt} -q $file_vrt $file_shp`;
      open STDERR, ">&OLDERR";
      if (-e $file_vrt) {
	$$attrib{File_Path} =~ s/$file_shp/$file_vrt/; }
      else {
	die "\nConvertion of tileindex shape file\n\t$file_shp\nto VRT file in the dataset \"$$attrib{Code_Name}\" has" .
	    " failed.\n  The likely problem can be permission for you to write in this file directory. Aborting...\n\n";
  } } }
}

#######################################################################

sub check_defaults
	### Replace "DEFAULT" keys with actual datasets
{
  my ($runIO, $MT) = @_;
			# Add closing slash to the path string
  $path{data_dir} .= '/' if $path{data_dir} && $path{data_dir} !~ m/\/$/;
			# Walk through the input hash
  foreach my $key_1 (keys %$runIO) {
    if (ref $$runIO{$key_1} eq 'HASH') {
      foreach my $key_2 (keys %{$$runIO{$key_1}}) {
		replace_default($runIO, $MT, $$runIO{$key_1}{$key_2},[$key_1,$key_2]);}}
    else {	replace_default($runIO, $MT, $$runIO{$key_1},        [$key_1       ]); }
  }
}

sub replace_default
{
  my($runIO, $MT, $val, $keys) = @_;
  return  if $val !~ m/^\s*default\s*/i;

  my $input_str	= '"' . join('->', @$keys) . '"';
  die "\nThis WBM run has default dataset for the $input_str input, \nbut default data directory " .
      "is not provided in the 'wbm_path.init' file. Aborting...\n\n" unless $path{data_dir};
  my $dir	= $path{data_dir} . join('/', 'defaults', @$keys) . '/';

	### Processing default directory for the requested dataset
  if (-d $dir) {
    opendir( DIR_HANDLE, $dir ) || die "failed to open directory handle to $dir in 'check_defaults' function: $!";
		#   Add full path     # Exclude *.aux.xml  # Filter only files (not directories)
      my @files	= map $dir.$_, sort grep(!/\.aux\.xml$/, grep(-f $dir.$_, readdir(DIR_HANDLE)));
    closedir DIR_HANDLE;
		### Process *.init files
    my @init_files = grep m/\.init$/i, @files;
    if (@init_files) {
      die "\nToo many *.init files for DEFAULT dataset in the $input_str input.\n" .
	  "Check this directory: $dir\nAborting...\n\n" if @init_files > 1;
      $val = {read_init( $init_files[0] )};			# Line below- Replace _DIR_ tag with its value
      $$val{File_Path}=~s/^\s*(\((.+?),(.+?)\):)*\s*_DIR_\//($1?$1:'').$path{data_dir}/ei;
      write_MT($$MT[1], $val) if ref($MT) && $$MT[1];		# Save attributes to the Local MT file

      if (scalar(@$keys) == 1)	{ $$runIO{$$keys[0]}		= $val; }
      else			{ $$runIO{$$keys[0]}{$$keys[1]}	= $val; }	return;
    }
		### Process GDAL files
    die "\nToo many default files for DEFAULT dataset in the $input_str input.\n" .
	"Check this directory: $dir\nAborting...\n\n" if @files > 1;
    if (scalar(@$keys) == 1)	{ $$runIO{$$keys[0]}		= $files[0]; }
    else			{ $$runIO{$$keys[0]}{$$keys[1]}	= $files[0]; }	return;
  }
  die "\nDefault data for $input_str input does not exist. Aborting...\n\n";
}

#######################################################################

sub write_MT
{
  my($file, $attr) = @_;
  my $header = MT_params();

		### Check if MT file has already this attributes entry
  return if -s $file && read_attrib($file, $$attr{Code_Name}, 'Code_Name', {CHECK_EXISTS=>1});

		### Write MT attributes to file
  open (FILE,">>$file") or die "Couldn't open $file, $!";
    print FILE join("\t", @$header ),"\n" unless -s $file;
    print FILE join("\t",map(exists($$attr{$_}) ? defined($$attr{$_})?$$attr{$_}:$$attr{$_} : '', @$header)),"\n";
  close FILE;
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
  die "\nError in read_init 'eval': $@\tFile = $file\n" .
	($str=~m/[^[:ascii:]]/?"It has non-ASCII characters. Fix it using \"dos2unix\".\n\n" : "\n") if $@;
  die "\nFailed to read attributes in read_init... File =\n   $file\n\n" unless ref($att);
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

sub read_param_str
{
  my ($HASH, $key)	= @_;			# Use of this pair instead of resulting value is done for
  my  $hash_or_file	= $$HASH{$key};		#   better error reporting
  return if !$hash_or_file;
  return( isInitFile($hash_or_file) ? read_init($hash_or_file) : ref($hash_or_file) eq 'HASH' ? %$hash_or_file :
	die(	"\nWBM initialization key \"$key\" has wrong format (see WBM documentation).\n",
		"It is supposed to be a hash block statement or a path to an init file that exists,\nbut it is-\n",
		"   $key => '$hash_or_file',\n\tAborting...\n\n"));
}

#######################################################################

sub init_DSet
	### Initialization of "DataSet" that can be a number, file, of Magic Table dataset
{
  my($ext, $meta, $str, $MT, $options) = @_;
			### Options
  my $resample	= set_default( $$options{RESAMPLE},	1);	# 1- bi-linear interpolation sampling
  my $patch_val	= set_default( $$options{PATCH_VALUE},	undef);
  my $dSetOpt	= set_default( $$options{DATASET_OPT},	undef);

		### Number
  return $str if isNumber($str);
		### GDAL file
  return read_GDAL($ext,$meta,$resample,$str,1,$patch_val) if GDAL_FileCheck($str);
		### RIMS DataSet
  return new RIMS::DataSet(attrib($str,$MT), $dSetOpt);
}

sub isNumber
{
  my $num = shift();
  return defined($num) ? $num =~ m/^[-+]?\d+(?:\.\d*)?(?:[eE][-+]?\d+(?:\.\d*)?)?$/ : 0;
}

#######################################################################

sub make_runAttr
	### Makes a Magic table entry for WBM output dataset (runoff)
{
  my ($runIO, $MT) = @_;

  die "\nWrong format of \"Calendar\" parameter in the input. Aborting...\n\n"
	if defined($$runIO{MT_Code_Name}{Calendar}) && $$runIO{MT_Code_Name}{Calendar}!~m/standard|366|365|360/i;
  my %attrib	= map(( $_	=> '' ), @{MT_params()});
  my %TS_suff	= ('hourly'	=> '_h',	'daily'	=> '_d',	'monthly' => '_m');
  my %units	= ('hourly'	=> 'mm/day',	'daily'	=> 'mm/day',	'monthly' => 'mm/month');
  my %scale	= ('hourly'	=> '',		'daily'	=> '',		'monthly' => 30);
  my $bands_d	= isNumber($$runIO{MT_Code_Name}{Calendar}) ? $$runIO{MT_Code_Name}{Calendar} : 366;
  my %bands	= ('daily_year' => $bands_d,	'monthly_year'  => 12,
		   'daily_month'=> 31,		'monthly_month' => 1,	'daily_day' => 1);
  my %files	= ('year'	=> 'wbm__YEAR_.nc',
		   'month'	=> '_YEAR_/wbm__YEAR_-_MONTH_.nc',
		   'day'	=> '_YEAR_/wbm__YEAR_-_MONTH_-_DAY_.nc');

  if (ref $$runIO{MT_Code_Name}) {
	### Make MT metadata for runoff using runIO data
    my %MT_param	  = %{ dclone($$runIO{MT_Code_Name}) };	$MT_param{Output_dir} =~ s/\/$//;
       $MT_param{MT_ID}	  = $$runIO{ID}		unless  defined	$MT_param{MT_ID};
       $MT_param{MT_ID}	 .= '_runoff'		unless		$MT_param{MT_ID} =~ m/_runoff$/;
       $$runIO{Project}	  = 'My project'	unless  defined	$$runIO{Project};

    die "\nIllegal value of \"Time_Series\" key \"$MT_param{Time_Series}\" in \"MT_Code_Name block\". Aborting...\n\n"
	if $MT_param{Time_Series} && $MT_param{Time_Series} !~ m/daily(\{\d+\})*|monthly/;

    my $start_date	  = $MT_param{Run_Start}   ||  $$runIO{Spinup}{Start} || '';
    my $time_series	  = $MT_param{Time_Series} || ($start_date =~ m/-00$/ ? 'monthly' : 'daily');
    my $TIME_SERIES	  = $time_series =~ m/daily\{\d+\}/ ? 'hourly' : $time_series;
    my $file_size	  = $TIME_SERIES eq 'hourly'  ? 'day' : set_default($MT_param{File_Size}, 'year');
    $MT_param{Run_Title}  = $MT_param{Run_Title}   ? $MT_param{Run_Title} . ", Runoff, $TIME_SERIES" :
			'WBM-TrANS, '.($$runIO{Project} || 'Simulation')  . ", Runoff, $TIME_SERIES" ;

    die "\nCannot make output daily file size for monthly WBM simulations. Aborting...\n\n"
	if $file_size eq 'day' && $time_series eq 'monthly';
    die "\n\"File_Size\" attribute in \"MT_Code_Name\" block must be 'year', 'month' or 'day'. Aborting...\n\n"
	if $file_size !~ m/^(year|month|day)$/;
    die "\n\"Run_Start\" attribute in \"MT_Code_Name\" block, or \"Start\" in \"Spinup\" block are not given:\n",
	"    Cannot determine this run start date. Aborting...\n\n" unless $start_date;

    $attrib{Start_Date}	  = $start_date;
    $attrib{End_Date}	  = $MT_param{Run_End} || attrib($$runIO{MT_airT}{Primary}, $MT)->{End_Date};
    $attrib{Time_Series}  = $time_series;
    $attrib{Code_Name}	  = $MT_param{MT_ID} . $TS_suff{$TIME_SERIES};
    $attrib{Data_Cube}	  = $MT_param{MT_ID};
    $attrib{Project}	  = $$runIO{Project};
    $attrib{Legend_File}  = 'DarkBlue(0:20)';
    $attrib{Name}	  = $MT_param{Run_Title};
    $attrib{Param_Name}	  = 'Runoff';
    $attrib{Var_Name}	  = 'runoff';
    $attrib{Units}	  = $units{$TIME_SERIES};
    $attrib{Var_Scale}	  = $scale{$TIME_SERIES};
    $attrib{Orig_Units}	  = 'mm/day';
    $attrib{Bands}	  = $time_series =~ m/daily\{(\d+)\}/  ? $1 : $bands{$TIME_SERIES.'_'.$file_size};
    $attrib{Shade_Default}= '5:0';
    $attrib{Processing}	  = defined $MT_param{Calendar} ? 'calendar='.$MT_param{Calendar} : '';
    $attrib{Projection}	  = $$runIO{Projection};
    $attrib{Round}	  = '%.2f';
    $attrib{MetadataLink} = 'no_metadata.htm';
    $attrib{File_Path}	  = "(1e20,1e5):$MT_param{Output_dir}/$TIME_SERIES/$files{$file_size};";
  } else {
	### Read MT metadata for runoff from Global Magic Table or init file
    %attrib	= %{ attrib($$runIO{MT_Code_Name}, $MT) };
  }
  check_MT_date($attrib{Start_Date});	# It can be a dynamic date from the file
  check_MT_date($attrib{End_Date});	# It can be a dynamic date from the file

	### Check format of "Start_Date" and "End_Date" attributes
  die "\nRun attribute \"Start_Date\" ($attrib{Start_Date}) is in a wrong format (must be YYYY-MM-DD). Aborting...\n\n"
	if $attrib{Start_Date} !~ m/\d{4}-\d{2}-\d{2}/;
  die "\nRun attribute \"End_Date\"   ($attrib{End_Date})   is in a wrong format (must be YYYY-MM-DD). Aborting...\n\n"
	if $attrib{End_Date}   !~ m/\d{4}-\d{2}-\d{2}/;

  return \%attrib;
}

#######################################################################

sub make_list_out
{
  my ($runIO)	= @_;
  my  @list		= grep m/\w+/, split(m/\s+/,$$runIO{Output_vars});	# "grep" removes blank vars
  my  @wbm_list		= get_wbm_var_list();
  my  %par		= read_hashes($path{WBM_varAttr});	### Read hashes for WBM output variables

  foreach my $var (@list) {
		### Exceptions for cheking output variables- Do not check these...
    next if $var =~ m/(_irrigation|_irrDemand|_soilMoistFrac|_PET|_AET|_netIrrigation|_BW_ET|_GW_ET)$/;
		### Check list of requested output variables
    die "\nRequested output variable \"$var\" does not exist in the WBM output list. Aborting...\n\n"
	unless $var ~~ @wbm_list;
		### Check attributes for the output variables
    die "\nProblem: Variable \"$var\" is not found in the WBM variable table...\n".
	"Please, add it to-\n\t$path{WBM_varAttr}\n\n" unless exists $par{$var};
  }

  return @list;
}

#######################################################################

sub get_wbm_var_list
{
  my @var_list;

  open (FILE,"<$0") or die "Couldn't open $0, $!";
    my $check	= 0;
    foreach (<FILE>) {
      $check = 1 if m/my \%data_out\s*=/;
      if ($check) {
	push(@var_list, $1) if m/['"](\w+)(_\$id|_\$_)*['"]\s*=>\s*\[\s*\$/;
	last if m/### End of \%data_out/;
    } }
  close FILE;

  return @var_list
}

#######################################################################

sub make_GW_BW_output		### Green/Blue water output aggregations
{
  my ($sMoistGr, $sMoistGrET, $sMoist, $sMoistET, $landFrac, $list_out) = @_;
  my ($sMoistGR, $sMoistBL,   $GW_ET,  $BW_ET);

  if (m/(sMoistGr|sMoistBl)$/ ~~ @$list_out) {
    foreach my $lnd (sort keys %$landFrac) { foreach my $tp (sort keys %{$$landFrac{$lnd}}) {	### ADDED SORT-NEED!
      next if $lnd =~ m/slc/i;		# $lnd keys are supposed to be from %land hash, not from %landFrac
      $sMoistGR	+= $$sMoistGr{$lnd}{$tp} * $$landFrac{$lnd}{$tp};
      $sMoistBL	+=($$sMoist  {$lnd}{$tp} - $$sMoistGr{$lnd}{$tp}) * $$landFrac{$lnd}{$tp};
  }}}
  if (m/(GW_ET|BW_ET)$/ ~~ @$list_out) {
    foreach my $lnd (sort keys %$landFrac) { foreach my $tp (sort keys %{$$landFrac{$lnd}}) {	### ADDED SORT-NEED!
      next if $lnd =~ m/slc/i;		# $lnd keys are supposed to be from %land hash, not from %landFrac
      $GW_ET	+= $$sMoistGrET{$lnd}{$tp} * $$landFrac{$lnd}{$tp};
      $BW_ET	+=($$sMoistET  {$lnd}{$tp} - $$sMoistGrET{$lnd}{$tp}) * $$landFrac{$lnd}{$tp};
  }}}

  return $sMoistGR, $sMoistBL, $GW_ET, $BW_ET;
}

#######################################################################
#######################################################################

sub make_dataCube
	###   Expand WBM output variables to a Data Cube
{
  my ($runIO, $attrib, $varAdd, $rnffID) = @_;

		### Check input MT metadata (runoff)
 ($$attrib{Data_Cube}	= $$attrib{Code_Name}) =~ s/(_h|_d|_m)$//;
  die "\nMT metadata ID does not contain word 'runoff' at the end in the 'MT_Code_Name' input field. Aborting...\n\n"
	unless $$attrib{Data_Cube} =~ m/runoff$/;

		### Split Local MT
  my($outVars, $inVars, $n)	= ('','',0);
  if (defined $varAdd) {
    ($outVars, $inVars)	= split m/\n\n/, htm_template($$runIO{Output_MT});
    open (FILE,">$$runIO{Output_MT}") or die "Couldn't open $$runIO{Output_MT}, $!";
      print FILE "$outVars\n";
    close FILE;
  }
		### Data Cube components
  my $subDi	= $$attrib{Time_Series} =~ m/daily\{(\d+)}/ ? $1 : 1;	# Sub-daily intervals in a day
  my $ts_org	= $$attrib{Time_Series};
  my @suff	= qw/_h _d _m _y _dc _mc _yc/;
  my @ts	= qw/daily{} daily monthly yearly daily_clim monthly_clim yearly_clim/;	$ts[0] =~ s/\{\}/{$subDi}/;
  my @ts_full	=('hourly','daily','monthly','yearly','daily climatology','monthly climatology','yearly climatology');
  my @s_date	= dc_dates($$attrib{Start_Date},0);
  my @e_date	= dc_dates($$attrib{End_Date},  1);
  my @bands	= ($subDi,366,12,1,365,12,1);
  my $hourly	= $subDi > 1;
  my $daily	= $$attrib{Time_Series} eq 'daily';
  my $monthly	= $$attrib{Time_Series} eq 'monthly';
     $bands[1]	= 1			if $hourly;
  my @path	= dc_path($$attrib{File_Path},$hourly,$daily,$monthly);
  my %par	= read_hashes($path{WBM_varAttr});	### Read hashes for WBM output variables
  my $pol_str	= $$runIO{PostProcessing}{Polygon_Aggregation}{IDs}
	if ref(   $$runIO{PostProcessing}) && ref($$runIO{PostProcessing}{Polygon_Aggregation});
  my @pol_str	= split m/\s+/,$pol_str if $pol_str;
  my @pol_ID	= map -e $_ ? read_init($_)->{Code_Name} : $_, @pol_str;

		### Process and write to file Magic Table entries
  foreach my $var (defined($varAdd) ? @$varAdd : grep(m/\w+/, split(m/\s+/, $$runIO{Output_vars}))) {
    next if  $var =~ m/Msk$/;					# Skip  Spatial Masks base variables

    if ($var =~ m/^(\w+Msk)_/) {			### Case of Spatial Masks
      $par{$var}  = dclone($par{$1});				# Clone Spatial Masks base attributes
      my ($id, $name)	= @{$$rnffID[$n++%($#$rnffID+1)]};	# Fill up "Full_name"
      $par{$var}{Full_name} =~ s/_ID_/$id/;
      $par{$var}{Full_name} =~ s/_NAME_/$name/;
    }
    if ($var =~ m/^(\w+_irr)_(\d+)$/) {			### Case of Irrigation cycling variables
      $par{$var}	    = dclone($par{$1});			# Clone Spatial Masks base attributes
      $par{$var}{Full_name}.= ", cycle $2";
    }
    die "\nProblem: Variable \"$var\" is not found in the WBM variable table...\n".
	"Please, add it to-\n\t$path{WBM_varAttr}\n\n" unless exists $par{$var};

   (my $data_cube = $$attrib{Data_Cube}) =~ s/runoff$/$var/;

    for (my $i=0; $i<=6; $i++ ) {
      next if !$hourly  &&  $i==0;		### hourly  WBM data
      next if  $monthly && ($i==1 || $i==4);	### monthly WBM data
      my $ind  = $i==0?0: ($i -1) % 3;
      my %meta = %$attrib;	# Clone it

      $meta{Code_Name}		= $data_cube.$suff[$i];
      $meta{Data_Cube}		= $data_cube;
      $meta{Time_Series}	= $ts[$i];
      $meta{Start_Date}		= $s_date[$i];
      $meta{End_Date}		= $e_date[$i];
      $meta{Name}		=~s/Runoff/$par{$var}{Full_name}/i;
      $meta{Name}		=~s/$ts_org/$ts_full[$i]/i;
      $meta{Var_Name}		= $var;
      $meta{Param_Name}		= $par{$var}{Par_name};
      $meta{Round}		= $par{$var}{Round};
      $meta{Shade_Default}	= $par{$var}{Shading};
      $meta{Legend_File}	= $par{$var}{Legend}	[$ind];
      $meta{Units}		= $par{$var}{Units}	[$ind];
      $meta{Orig_Units}		= $par{$var}{Org_units}	[$ind];
      $meta{Var_Scale}		= $par{$var}{Scale}	[$ind];
      $meta{Var_Scale}		= 1	if $monthly && ($i==2 || $i==5) && $meta{Orig_Units} =~ m/\/month$/;
      $meta{Var_Scale}		= 12	if $monthly && ($i==3 || $i==6) && $meta{Orig_Units} =~ m/\/month$/;
      $meta{Bands}		= $bands[$i] if $hourly || ($daily && $i>1) || ($monthly && $i>2);
      $meta{Processing}		= $$attrib{Processing}	if  $i==1  && $$attrib {Processing};
      $meta{Processing}	       .=($meta   {Processing}  ? ':':'')  .  $par{$var}{Processing};
     ($meta{File_Path}		= $path[$i]) =~ s/_VAR_/$var/;
      $meta{Polygon_Aggregation}= join ' ',
	map("$_:$$runIO{Output_dir}/spatial_agg/$meta{Code_Name}.$_.nc;", @pol_ID);

		### Write attributes to MT file
      write_MT($$runIO{Output_MT}, \%meta);
    }
  }
		### Write empty line to MT file to separate output from input
  open (FILE,">>$$runIO{Output_MT}") or die "Couldn't open $$runIO{Output_MT}, $!";
    print FILE "\n",$inVars;
  close FILE;
		### Write polygon mask attributes to MT file
  map attrib($_,[$$runIO{Input_MT},$$runIO{Output_MT}]), @pol_str;
}

sub dc_dates
{
  my($dt,$dr) = @_;

  my @date  = split m/-/, $dt;
  my @dir   = $dr ? (-1,12,31) : (1,1,1);

  my @dates = (	sprintf("%04d-%02d-%02d",@date[0..2]),
		sprintf("%04d-%02d-%02d",@date[0..2]), sprintf("%04d-%02d-00",@date[0,1]), sprintf("%04d-00-00",$date[0]),
		sprintf("0000-%02d-%02d",@dir [1..2]), sprintf("0000-%02d-00",$dir[1]),    "0000-00-00");
			### Case of partial year
  $dates[3] =	sprintf("%04d-00-00",$date[0]+$dir[0]) if $date[1] != $dir[1];

  return @dates;
}

sub dc_path
{
  my @path = ((shift) x 7);
  my($hour, $day, $mnth) =  @_;
  my $p = $1 if $path[0] =~ m/(.+\/)/;
     # $p =~ s/(daily(\/_YEAR_)*|monthly(\/_YEAR_)*)\/$//;
     $p =~ s/((hourly|daily|monthly)(\/_YEAR_)*)\/$//;

  $path[1] = $p.'daily/'.($hour?'_VAR_/_YEAR_/wbm__YEAR_-_MONTH_-_DAY_.nc;' : 'wbm__YEAR_.nc;')	unless $day;
  $path[2] = $p.'monthly/_VAR_/wbm__YEAR_.nc;'							unless $mnth;
  $path[3] = $p.'yearly/_VAR_/wbm__YEAR_.nc;';
  $path[4] = $p.'climatology/wbm__VAR__dc.nc;';
  $path[5] = $p.'climatology/wbm__VAR__mc.nc;';
  $path[6] = $p.'climatology/wbm__VAR__yc.nc;';

  return @path;
}

sub read_hashes
{
  my ($hdr, @data) = read_table(shift);
  my %hash;
		### Populate parameter hash for the variables
  my $vCol = delete $$hdr{Var};
  foreach my $row (@data) {
    $hash{$$row[$vCol]} = {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
  }
		### Evaluate arrays
  foreach my $var (keys %hash) {
    foreach my $key (keys %{$hash{$var}}) {
      $hash{$var}{$key} = eval($hash{$var}{$key}) if $hash{$var}{$key} =~ m/\[.+]/;
    }
  }

  return %hash;
}

sub uniq_list { my %seen; grep !$seen{$_}++, @_; }

#######################################################################
#######################################################################

sub GDAL_FileCheck
{			### Check if it is a GDAL readable file
  my $file  = shift;
  my $check = -e ($file =~ m/:(.+):/ ? $1 : $file) && eval {
      open STDERR, ">&NEWERR";
	$_ = Geo::GDAL::Open($file);
      open STDERR, ">&OLDERR";
  };
  open STDERR, ">&OLDERR";	# Must be reset after eval, if eval fails.
  return $check;
}

#######################################################################

sub isDataSet_TS
{			### Check if it is a DataSet with actual Time Series data
  my ($str, $MT) = @_;
		### Existing RIMS DataSet
  return $str->climatology && $str->yearly ? 0 : 1 if ref($str) =~ m/DataSet/;
		### Number
  return 0 if isNumber($str);
		### GDAL file
  return 0 if GDAL_FileCheck($str);
		### RIMS DataSet
  my $dSet = new RIMS::DataSet(attrib($str,$MT));
  return $dSet->climatology && $dSet->yearly ? 0 : 1;
}

#######################################################################

sub read_Layer
	### Reads a single data layer
{
  my($ext, $meta, $str, $MT, $options) = @_;
			### Options
  my $resample	= set_default( $$options{RESAMPLE},	1);	# 1- bi-linear interpolation sampling
  my $patch_val	= set_default( $$options{PATCH_VALUE},	undef);
  my $dSetOpt	= set_default( $$options{DATASET_OPT},	undef);
  my $inputKey	= set_default( $$options{KEY},		undef);	# Optional input key to report input error

		### Number
  return $str if isNumber($str);
		### GDAL file
  return read_GDAL($ext,$meta,$resample,$str,1,$patch_val) if GDAL_FileCheck($str);
		### RIMS DataSet
  my $dSet	= new RIMS::DataSet(attrib($str,$MT,$inputKey),	$dSetOpt);
  return read_dateLayer($dSet, '2001-01-01', $ext, 0,		$options);
}

#######################################################################

sub read_dateLayer
		### Reads a data layer by given date
{
  my($dataSet, $date, $ext, $spool_dir, $options) = @_;
			### Options
  my $resample	= set_default( $$options{RESAMPLE},	1);	# 1- bi-linear interpolation sampling
  my $patch_val	= set_default( $$options{PATCH_VALUE},	undef);	# Nodata patch value. Can be DataSet, PDL layer, or number
  my $PATCH_val	= set_default( $$options{PPATCH_VALUE},	undef);	# Primary patch value for patch DataSet above
  my $noCaching	= set_default( $$options{NO_CACHING},	0);	# Disable caching of monthly/yearly data
  my $DtSrchOpt	= set_default( $$options{DATE_SEARCH_OPT}, undef);	# Dataset date search options
			### Other initializations
  my $ref	= ref($dataSet);
  my $patchFlag	= ref($patch_val) =~ m/DataSet/;	# Flag to patch with secondary dataset
     $patch_val	= $PATCH_val if defined($PATCH_val) && !defined($patch_val);

	########### Dataset is a number (scalar)  #####################
  if (!$ref)		{ return $dataSet; }

	########### Dataset is a static layer     #####################
  if ($ref=~m/PDL/)	{ return $dataSet->copy; }

	########### Dataset is a time-series (DataSet object)  ########
  if ($ref!~m/DataSet/)	{ die "Unknown DataSet type...\n"; }

			### Previously read data for monthly and yearly TS
  unless ( $dataSet->daily || $noCaching) {
    return $dataSet->{data}	if defined($dataSet->{'data'})  &&
      (($dataSet->monthly && $date !~ m/-01$/)  || ($dataSet->yearly  && $date !~ m/-01-01$/));
  }
			### Read data layer
  my $data;
  if ($spool_dir) {
    my $dir  = $spool_dir.$dataSet->{ID}.($patchFlag ? '_'.$patch_val->{MT_attrib}{Code_Name} : '');
    my $file = sprintf "$dir/%s.dat",$dataSet->searchDate($date,$DtSrchOpt);
    unless (-e $dir) {
      mkpath($dir,0,0775) or die "Cannot make directory...\n$dir\n";
    }
    if (-e $file) {
      $data = readflex($file);
    } else {
      $data =	read_GDAL($ext,$dataSet  ->{MT_attrib},$resample,$dataSet  ->dateLayer($date,$DtSrchOpt), $patchFlag ?
		read_GDAL($ext,$patch_val->{MT_attrib},$resample,$patch_val->dateLayer($date,$DtSrchOpt), $PATCH_val):
												 $patch_val);
      writeflex($file, $data);		$$ext{spool_writes}++;
    }
  }
  else {		### See note above about read_GDAL...
      $data =	read_GDAL($ext,$dataSet  ->{MT_attrib},$resample,$dataSet  ->dateLayer($date,$DtSrchOpt), $patchFlag ?
		read_GDAL($ext,$patch_val->{MT_attrib},$resample,$patch_val->dateLayer($date,$DtSrchOpt), $PATCH_val):
												 $patch_val);
  }
  $dataSet->{data} = $data;

  return $data;
}

#######################################################################

sub read_climate_bias
			### Must be monthly or daily climatology time series
{
  my ($cData, $mod, $obs, $runSet, $MT)	= @_;
  my ($runIO, $meta,$extent)		= @$runSet;
  return () unless $mod and $obs;

  my $dataSet_mod = new RIMS::DataSet(attrib($mod,$MT,'WM_mod_precip or WM_mod_airT'));
  my $dataSet_obs = new RIMS::DataSet(attrib($obs,$MT,'WM_obs_precip or WM_obs_airT'));

		### Check for time series resolution
			### Daily
  if	($cData->daily) {
    die 'Climate bias correction datasets for "' . $cData->{'ID'} . "\" must be daily climatology. Aborting...\n\n"
	unless $dataSet_obs->daily && $dataSet_obs->climatology && $dataSet_mod->daily && $dataSet_mod->climatology;
  }			### Monthly
  elsif	($cData->monthly) {
    die 'Climate bias correction datasets for "' . $cData->{'ID'} . "\" must be monthly climatology. Aborting...\n\n"
	unless $dataSet_obs->monthly && $dataSet_obs->climatology && $dataSet_mod->monthly && $dataSet_mod->climatology;
  }			### Others
  else { die 'Climate bias correction is not available for temporal resolution (' . $cData->{MT_attrib}{Time_Series} .
	') of the "' . $cData->{'ID'} . "\" dataset. Aborting...\n\n";
  }
		### Calculate bias and write it to spool
  my $spool_dir	= $$runIO{spool}.$dataSet_mod->{ID}.'_'.$dataSet_obs->{ID};
	unless (-e $spool_dir) { mkpath($spool_dir,0,0775) or die "Cannot create-\n$spool_dir\n"; }
  my @bias;
  foreach my $day (1 .. 365) {
    my @date	= inverse_julian_day(2451910 + $day);
    my $date	= sprintf("0000-%02d-%02d", $date[1], $cData->daily ? $date[2] : 0);

    my $file	= sprintf "$spool_dir/%03d.dat", $day;
       $file	= $bias[-1] if $cData->monthly && $date[2] > 1;
    push @bias, $file;
    next if -e  $file;

    my $delta	= read_GDAL($extent,$dataSet_mod->{MT_attrib},1,$dataSet_mod->dateLayer($date,0)) -
		  read_GDAL($extent,$dataSet_obs->{MT_attrib},1,$dataSet_obs->dateLayer($date,0));
       $delta->where(($$extent{mask}->isgood - $delta->isgood)==1) .= 0;
    writeflex($file, $delta);		$$extent{spool_writes}++;
  }

  printf("Climate bias is read for %s\n",$dataSet_mod->{ID});
  return @bias;
}

#######################################################################

sub read_GDAL

{
  my ($extent,$meta,$resample,$file,$b,$patch_value) = @_;
 (my  $file_s = $file)	=~ s/.+:(.+):.+/$1/;

		### Set default metadata values, e.g. it can be empty hash
  $$meta{Var_Scale}	= 1		unless $$meta{Var_Scale};
  $$meta{Var_Offset}	= 0		unless $$meta{Var_Offset};
  $$meta{Projection}	= 'epsg:4326'	unless $$meta{Projection};
  $$meta{Processing}	= ''		unless $$meta{Processing};

		### Check input file string
  die "\nNo file path to read is given (in read_GDAL). Aborting...\n\n"			unless length	$file_s;
  die "\nRequested file (in read_GDAL)-\n  $file_s\ndoes not exist. Aborting...\n\n"	unless -e	$file_s;
  die "\nRequested file (in read_GDAL)-\n  $file_s\nhas extension of a shape file," .
	" but Processing metadata does not indicate\"polygon\" flag. Aborting...\n\n"
		if $file_s =~ m/\.shp/i && $$meta{Processing} !~ /polygon/i;

		### Make tmp file string
  my $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm');
  my $tmp_file	= $tmp_obj->filename;	# Do not combine with the line above otherwise it will fail in forks

		### Build extent string
  my  $extents	= sprintf "%f %f %f %f", $$extent{xllcorner}, $$extent{yllcorner},
	$$extent{xllcorner}+$$extent{cellsizeX}*$$extent{ncols},
	$$extent{yllcorner}+$$extent{cellsizeY}*$$extent{nrows};

		### Build resample string
		#    0      1       2        3         4       5     6    7   8   9  10 11
  my @resample = qw/near bilinear cubic cubicspline lanczos average mode max min med Q1 Q3/;

		### Populate metadata if not provided in the Magic Table, e.g. case of non-MagicTable files
  my $varName		= $$meta{Var_Name}   ? $$meta{Var_Name}   : $file =~ m/.+:.+:(.+)/ ? $1 : '';
  my $processing	= $$meta{Processing} ? $$meta{Processing} : $file =~ m/(.+):.+:.+/ ? $1 : '';

		### Get NetCDF data compression and NODATA
  my($netcdf_scale, $netcdf_offset, $netcdf_nodata) =
	 get_NetCDF_compression($file_s, $varName, $processing);
  my $nodataStr	=
	($processing=~m/nodata=([-+e\.\d]+)/i)	? "-srcnodata $1 -dstnodata -9999" :
	 defined( $netcdf_nodata )		? "-srcnodata $netcdf_nodata -dstnodata $netcdf_nodata" : '';
				# The line above is not needed for GDAL v. 2.2.2 (released 2017/09/15) and later
		### Get Polygon Attribute Name (shape file dataset)
  my $pol_var	= $varName =~ m/_YEAR_|_MONTH_|_DAY_|_HOUR_|_MIN_/ ? $b : $varName	if $processing=~m/polygon/i;

		### Perform GDAL warping
  my $bounds	= "-q -ts $$extent{ncols} $$extent{nrows} -te $extents";
  my($l_name, $poly_format, $suf) = $processing !~ m/polygon/i ? undef :
	($file_s =~ m/\.json$/i ? (basename($file_s,".json"),	'GeoJSON',	  'json') :	# Add more formats if needed
				  (basename($tmp_file),	'ESRI Shapefile', 'shp'));
  my $gdal_warp	= ($processing=~m/polygon/i) ?
			### Shape  Files
	"$$PATH{ogr2ogr} -f \"$poly_format\" -s_srs \"$$meta{Projection}\" -t_srs \"$$extent{projection}\" -skipfailures $tmp_file.$suf $file_s;".
	"$$PATH{gdal_rasterize} -of gtiff $bounds -a_nodata -9999 -a $pol_var -l $l_name $tmp_file.$suf $tmp_file.tif" :
			### Raster Files
	"$$PATH{gdal_translate} -of VRT -b $b $file $tmp_file.vrt;".
	(($processing=~m/flip/i) ? "$$PATH{flip_nc}  $tmp_file.vrt;" : '').
	(($processing=~m/pm0/i)  ? "$$PATH{shift_pm} $tmp_file.vrt;" : '').
	"$$PATH{gdalwarp} -ot Float32 -wt Float32 -of gtiff -multi -s_srs \"$$meta{Projection}\" -t_srs \"$$extent{projection}\" $bounds -r $resample[$resample] $nodataStr $tmp_file.vrt $tmp_file.tif";

#   warn $gdal_warp;			### In case it fails...
  open STDERR, ">$tmp_file.err";
    my $junk = `$gdal_warp`;		### It does it all
  open STDERR, ">&OLDERR";

		### Report debugging info and errors, if $gdal_warp fails... and quit
  unless (-e "$tmp_file.tif") {	open  ERRFILE, "$tmp_file.err";
    my @errors = <ERRFILE>;	close ERRFILE;
    print STDERR  "\nFailed to warp $file\n\tFork code   = $?\n\tFork string = $gdal_warp\n\tErrors      = ",
	@errors ? "\n@errors\n" :	"No errors reported. Most likely the system is out of memory...\n\n";
    unlink <$tmp_file.*>;
    exit;
  }		### Check for GDAL errors that still result in output file with all zeros (default patch values)
  if (open ERRFILE, "$tmp_file.err") {
    my @err_lines = grep  m/ERROR/i, <ERRFILE>;
       @err_lines = grep !m/writing to NetCDF/,			@err_lines;	# Avoid bug in GDAL v1.92
       @err_lines = grep !m/Invalid dfSouthLatitudeDeg/,	@err_lines;	# Avoid bug in GDAL v3
    close ERRFILE;
    if (@err_lines) {
      unlink <$tmp_file.*>;
      die "ERROR  in warp $file\nFork code   = $?\nFork string = $gdal_warp\nFork errors =\n@err_lines\n";
    }
  }
		### Read Raster and change units
  my $pdl = read_raster("$tmp_file.tif", 1, [$netcdf_scale, $netcdf_offset]);
     $pdl = $pdl*$$meta{Var_Scale} + $$meta{Var_Offset};	### Change Units
  unlink <$tmp_file.*>;

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
  die "   Failed reading GDAL file as all values are NODATA:\n$file_s\n\n" unless $nPDL;
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

  open STDERR, ">&NEWERR";	# Reversed back to OLDERR about 15 lines below (suppresses warnings)
    my $geotiff_data = Geo::GDAL::Open( $file, 'ReadOnly' );

#     my $geoTransform = $geotiff_data->GetGeoTransform;
#     my $order = ($$geoTransform[5]>0) ? 1 : -1;
#     print "Order = $order\n";
    my $band	= $geotiff_data->GetRasterBand($b);

    my $x_size	= $band->{XSize};
    my $y_size	= $band->{YSize};
	# Warning: TIFFReadDirectoryCheckOrder:Invalid TIFF directory; tags are not sorted in ascending order
    my $bin_data= $band->ReadRaster(0, 0, $x_size, $y_size);
    my $nodata	= $band->GetNoDataValue;
       $nodata	= -9999 unless defined $nodata;
  open STDERR, ">&OLDERR";

		### Data types needed for PDL
  my @data_type = (undef,'byte','ushort','short',undef,'long','float','double',
	'short','long','float','double');
  die sprintf("Data type problem in \"read_raster\": %s (%d) is not defined...\n",
    Geo::GDAL::GetDataTypeName($band->{DataType}),$band->{DataType})
	unless defined $data_type[$band->{DataType}];

  open(my $FH,"<",\$bin_data) or die "Couldn't open filehabdle to binary data stream, $!";
  my $pdl = readflex($FH,
	[{NDims=>2, Dims=>[$x_size,$y_size], Type=>$data_type[$band->{DataType}]}]);
  close $FH;
		### Set bad and uncompress data
  $pdl	= $pdl->setvaltobad($nodata)*$$compr[0]+$$compr[1];

  return $pdl;
}

#######################################################################

sub get_extent

{
  my ($file, $projection, $options) = @_;
  if (ref($projection) eq 'HASH') {			# Shift secondary arguments, if projection is not present
    $options	= $projection;
    $projection	= undef;
  }
		### Options
  my $mask_opt	= set_default($$options{MASK},	1);	# Option to read mask layer

 (my  $file_s	= $file) =~ s/.+:(.+):.+/$1/;
  $projection	= 'epsg:4326' unless defined $projection;
  die "Requested file (in get_extent)-\n$file_s\ndoes not exist...\n" unless -e $file_s;

  my($size,$gT)	= get_geo_transform($file);
  my $igT	= Geo::GDAL::InvGeoTransform($gT);
  my @llcorner	= Geo::GDAL::ApplyGeoTransform($gT, 0, $$size[1]);
  my @urcorner	= Geo::GDAL::ApplyGeoTransform($gT, $$size[0], 0);
  my %extent	= (	'file'		=> $file_s,
			'ncols'		=> $$size[0],
			'nrows'		=> $$size[1],
			'xllcorner'	=> $llcorner[0],
			'yllcorner'	=> $llcorner[1],
			'xurcorner'	=> $urcorner[0],
			'yurcorner'	=> $urcorner[1],
			'cellsize'	=> $$gT[1],
			'cellsizeX'	=> $$gT[1],
			'cellsizeY'	=>-$$gT[5],
			'projection'	=> $projection,
			'gTransform'	=> $gT,
			'igTransform'	=> $igT,
			'mask'		=> $mask_opt?read_raster($file):undef);

#   die "Non-Square pixels in $file..." if $$gT[1] != -$$gT[5];
  return \%extent;
}

sub fill_extent
{
  my $ext = shift;
		# Fill extent with maks that does not have nodata values
  my %extent = ( 'mask' => ones($$ext{ncols}, $$ext{nrows}) );
  foreach my $key (keys %$ext) {
    next  if $key eq 'mask';
    $extent{$key} = $$ext{$key};
  }
  return \%extent;
}

#######################################################################

sub get_geo_transform

{
  my $file = shift;
  open STDERR, ">&NEWERR";
    my $geotiff_data = Geo::GDAL::Open($file, 'ReadOnly');
  open STDERR, ">&OLDERR";

  my $geoTransform = $geotiff_data->GetGeoTransform;
#   die "Incompatible GeoTransform in $file : ".join(' ',map("[$_] $$geoTransform[$_]",0..5))
# 	if !$$geoTransform[2] && !$$geoTransform[4] && abs($$geoTransform[1]+$$geoTransform[5]) > 1e-6;

  my $band = $geotiff_data->GetRasterBand(1);
  my $x_size	= $band->{XSize};
  my $y_size	= $band->{YSize};

  return [$x_size,$y_size],$geoTransform;
}

#######################################################################

sub get_bad_value

{
  my $file = shift;
  open STDERR, ">&NEWERR";
    my $geotiff_data = Geo::GDAL::Open("$file", 'ReadOnly');
  open STDERR, ">&OLDERR";

  my $band	= $geotiff_data->GetRasterBand(1);
  my $nodata	= $band->GetNoDataValue;
     $nodata	= -9999 unless defined $nodata;

  return $nodata;
}

#######################################################################

sub lonLat
{
  my $extent = shift;

  my $lon	= zeroes($$extent{ncols})->xlinvals(
	$$extent{xllcorner}+$$extent{cellsizeX}/2,
	$$extent{xllcorner}+$$extent{cellsizeX}*$$extent{ncols}-$$extent{cellsizeX}/2);
  my $lat	= zeroes($$extent{nrows})->xlinvals(
	$$extent{yllcorner}+$$extent{cellsizeY}*$$extent{nrows}-$$extent{cellsizeY}/2,
	$$extent{yllcorner}+$$extent{cellsizeY}/2);

  return $lon, $lat;
}

#######################################################################

sub cell_area
{
  my ($lon, $lat, $extent, $options) = @_;

		### Options
  my $force	= set_default($$options{FORCE},	0);	# Option to assume equal area cells.
							# Value is unit conversion to meters.

  my $cell_area	=				### Cell area in km^2
	($$extent{projection} =~ m/epsg:4326/i) ?
		(12387.69*$$extent{cellsizeY}*$$extent{cellsizeX}*cos($lat/180*pi))->dummy(0,$$extent{ncols}):
	($$extent{projection} =~ m/epsg:3408/i) ?
		1e-6*$$extent{cellsize}**2 + zeroes(double,$$extent{ncols},$$extent{nrows}):
	 $force ?    $force *
		1e-6*$$extent{cellsize}**2 + zeroes(double,$$extent{ncols},$$extent{nrows}):
	 die "\nUnknown method to calculate cell area for Network data...\n\n";
  return $cell_area;
}

#######################################################################

sub area_layers
{
  my ($cellArea, $glAreaFr, $rmAreaFr, $lakeMsk) = @_;

  my $cell_area = $cellArea * (1 - $glAreaFr);				# Active area, km2
  my $soil_area = $cellArea * (1 - $glAreaFr - $rmAreaFr - $lakeMsk);	# Soil   area, km2
  die "\nNegative soil area. Aborting...\n\n" if whichND($soil_area < 0)->nelem;

  return		$cell_area,  $soil_area,
			$cell_area * 1e3*1e-9,			# conversion: mm -> km3 over active area
			$soil_area * 1e3*1e-9,			# conversion: mm -> km3 over soil   area
			$soil_area * 1e3,			# conversion: mm ->  m3 over soil   area
	condition_slice($soil_area,  1e-3 / $soil_area, 0);	# conversion: m3 -> mm  over soil   area
}

#######################################################################

sub write_cell_area
{
  my ($c_area, $a_area, $s_area, $flag, $runIO, $grid, $credits,  $year) = @_;
      $year	= substr($$runIO{Run_Start}, 0, 4) unless defined $year;
  my  $FILE	= $$runIO{Area_dir}.'full_cell_area.nc';
  my  $file	= $$runIO{Area_dir}. 'sub_cell_area.nc';

  if ($flag) {		### Write active and soil area to annual time series NetCDF file

    my $BAND	= get_nc_dim($file, 'time');					# Band number in the NC file
    my $band	= $year - substr($$runIO{Run_Start}, 0, 4) + 1;			# Band to write
    unless ($BAND && $BAND == $band) {
      my $time	= julian_day(1900, 1, 1) - julian_day($year, 6, 30);		# NC time
      write_nc($file, $band, $time, $grid,
		{active_cell_area => [$a_area,'Active area in a grid cell', 'km2'],
		   soil_cell_area => [$s_area,  'Soil area in a grid cell', 'km2']},
		{TYPE => \&float,  CREDITS => $credits, TS_RESOLUTION => 'yearly'});		# Options
  } }
  else {		### Write active and soil area NetCDF file (static layer)
    unless (-e $file) {
      write_nc($file, 1, 37070, $grid,
		{active_cell_area => [$a_area,'Active area in a grid cell', 'km2'],
		   soil_cell_area => [$s_area,  'Soil area in a grid cell', 'km2']},
		{TYPE => \&float,  CREDITS => $credits, TS_RESOLUTION => 'yearly_clim'});	# Options
  } }
			### Write full grid cell area
  unless (-e $FILE) {
    write_nc($FILE, 1, 37070, $grid,{cell_area=>[$c_area,'Full grid cell area','km2']},
		{TYPE => \&float,  CREDITS => $credits, TS_RESOLUTION => 'yearly_clim'});	# Options
  }
}

#######################################################################

sub colRow
{
  my ($extent,$lon,$lat) = @_;
  return map(int, Geo::GDAL::ApplyGeoTransform($$extent{igTransform},$lon,$lat));	# (col,row)
}

#######################################################################

sub cellGeometry
	### Calculate cell width and length, in meters
{
  my $extent = shift;
  die "\n\"cellGeometry\" function is presently done for the 'epsg:4326' projection only. Aborting...\n\n"
	unless $$extent{projection} =~ m/epsg:4326/i;

  my($lon,$lat)	= lonLat($extent);
  my $cell_X	= (111300*$$extent{cellsize}*cos($lat/180*pi))->dummy(0,$$extent{ncols});	# m
  my $cell_Y	= (111300*$$extent{cellsize}*ones($lat->dims))->dummy(0,$$extent{ncols});	# m

  return $cell_X, $cell_Y;
}

#######################################################################

sub write_gridascii
{
  my ($file, $pdl, $extent, $options) = @_;

		### Options
  my $format	= set_default($$options{FORMAT},	'%f');
  my $nodata	= set_default($$options{NODATA},	-9999);

		### Extent fromat check
  die "Pixels are not square in \"write_gridascii()\"...\n" if abs(1-abs($$extent{cellsizeX}/$$extent{cellsizeY})) > 1e-6;

		### Convert PDL to Perl array
  my @data	= unpdl_data($pdl->setbadtoval($nodata));
  my $header	= <<END;
ncols          $$extent{ncols}
nrows          $$extent{nrows}
xllcorner      $$extent{xllcorner}
yllcorner      $$extent{yllcorner}
cellsize       $$extent{cellsize}
NODATA_value   $nodata
END
		### Write File
  prepare_dir( $file );			# Prepare output directory
  open (FILE,">$file") or die "Couldn't open $file, $!";
    print FILE $header;
    foreach my $row (@data) {
      print FILE join(' ',map(sprintf($format,$_),@$row)),"\n";
    }
  close FILE;
}

#######################################################################

sub write_tif
{
  use File::Copy;

  my ($extent, $ot, $file, $data, $options) = @_;
  my %pack = (	'Float32'	=> 'f*',
		'Float64'	=> 'd*',
		'Int16'		=> 's*',
		'Int32'		=> 'i*');
  die "Unknown pack format in \"write_tif\" function. Aborting...\n\n" unless defined $pack{$ot};
			### Options.
	# See https://gdal.org/drivers/raster/gtiff.html for possible "-co" Creation Options. Add more as needed
  my $NODATA	= defined $$options{NODATA} ? "-a_nodata $$options{NODATA}" : '';
  my $CO = ''; if ( defined $$options{CO}) {
     $CO = join(' ', map("-co $_=$$options{CO}{$_}",		keys(%{$$options{CO}}))); }
  my $MO = ''; if ( defined $$options{MO}) {
     $MO = join(' ', map("-mo $_=\"$$options{MO}{$_}\"",	keys(%{$$options{MO}}))); }

			### Convert extent file to GeoTiff
  prepare_dir( $file );			# Prepare output directory
  open STDERR, ">&NEWERR";
    my $junk	= `$$PATH{gdal_translate} -q -ot $ot $NODATA $CO -a_srs "$$extent{projection}" $$extent{file} $file`;

			### Add custom metadata tags
    if ($MO) {	# Custom tags go to XML, if "PROFILE=..." is used. Must do it below
      unlink $file.'.aux.xml';			# Remove XML with all junk metadata
      $CO =~ s/-co PROFILE=\S+//i;		# Stop creating XML
	$junk	= `$$PATH{gdal_translate} -q $NODATA $MO $CO $file $file.tmp`;	# Add custom tags and repeat compression
      move("$file.tmp", $file);			# Move temporary file to its final copy
    }
			### Replace extent data with the data to write
    my $geotiff_data = Geo::GDAL::Open($file, 'Update');
    my $band	= $geotiff_data->GetRasterBand(1);
    my $nodata	= $band->GetNoDataValue;
       $nodata	= -9999 unless defined $nodata;
       $band->WriteRaster(0,0,$$extent{ncols},$$extent{nrows},pack($pack{$ot},$data->setbadtoval($nodata)->list));
  open STDERR, ">&OLDERR";
  unlink $file.'.aux.xml';
}

#######################################################################

sub write_nc
		# Data Notes- Data structure ($data is a reference to a hash) for each variable in the hash:
		#	Variable name			- $$data{key} (hash key is the variable name)
		#	Variable data			- $$data{key}[0]
		#	Variable attribute "long_name"	- $$data{key}[1]
		#	Variable attribute "units"	- $$data{key}[2]
# Optional	#	Variable custom attributes	- $$data{key}[3] (reference to a hash)
# Optional	#	Variable sort order		- $$data{key}[4] (integer of float number for sorting)
{
	# Subroutine input:
  my (	$file,		# Output file path (directory will be created, if it does not exists)
	$band,		# Band number for the "unlimited dimension" to start writng data to
	$time,		# "time" variable to write to the output NetCDF file
	$grid,		# Reference to an array containg pointers to- [longitude, latitude, geotransform, projection]
	$data,		# See "Data Notes" above
	$options) = @_;	# See inline notes below

			### Check for version compatibility
  die "\"write_nc\" function requires WBM I/O v.17.12.0 or later. Aborting...\n\n" unless ref($grid) eq 'ARRAY';

			### Options
  my $credits	= set_default($$options{CREDITS},	[(getpwuid($<))[0,6],'email unknown','unknown']);
  my $calendar	= set_default($$options{CALENDAR},	366);
  my $ts_res	= set_default($$options{TS_RESOLUTION},	'daily');
  my $attrib	= set_default($$options{ATTRIB},	{});	# Global attributes. Can be array or hash
  my $deflate	= set_default($$options{DEFLATE},	1);	# NetCDF4 deflate (compression) parameter
  my $shuffle	= set_default($$options{SHUFFLE},	0);	# NetCDF4 shuffle parameter
  my $nodata	= set_default($$options{NODATA},	-9999);	# Nodata value
  my $nc4	= set_default($$options{NC4},		1);	# Use NetCDF v.4 format, otherwise "classic"
  my $type	= $$options{TYPE};	# Use each var own type unless this option is given.
								# Example- {TYPE => \&float}

	###############################################################
	###	Update existing NetCDF file/dataset

  if (-e $file) {
			### Open NetCDF file and write data
    my $nc = new PDL::NetCDF($file, {MODE => O_RDWR, REVERSE_DIMS => 1});
       $nc->putslice('time', [], [], [$band-1], [long($time)->dim(0)], long($time)) if defined $time;

    foreach my $var (keys %$data) {
       $nc->putslice($var,[], [], [0,0,$band-1], [map($$data{$var}[0]->dim($_),0..2)],
		$$data{$var}[0]->setbadtoval($nodata)->slice(':,-1:0'), {SHUFFLE => $shuffle, DEFLATE => $deflate});
    }
    $nc->close();
    return;
  }
	###############################################################
	###	Create new NetCDF file/dataset

  die "\nNot coded to create NetCDF file with BAND # gt 1 for file-\n\t$file\nIt is $band...\n\n" if $band != 1;
			### Calendar metadata string
  my %calendar_str = (366 => 'standard', 365 => '365_day', 360 => '360_day');
			### Prepare output directory
  prepare_dir($file);
			###  Prepare georeferencing
  my($lon, $lat, $gT, $proj)	= @$grid;
  my $proj_crs	= crs_to_Obj($proj);
  my $wkt	= $proj_crs->ExportToWkt;		### Convert to Wkt fromat
  my($xVr,$yVr)	= $proj_crs->IsGeographic ? qw(lon lat) : qw(x y);

			###  Prepare variables
  my @var_name	= sort {$$data{$a}[4] <=> $$data{$b}[4] if defined $$data{$a}[4]} keys %$data;
  my @data_dims	= ([$lon->dims, $lat->dims, PDL::NetCDF::NC_UNLIMITED()], [0,0,0],
	[map($$data{$var_name[0]}[0]->dim($_),0..2)]);

			###  Create NetCDF attributes
  my @var_att;
  foreach my $var (@var_name) {
    my %hash = (
	'grid_mapping'	=> 'crs',
	'missing_value'	=> $type ? &$type([$nodata]) : pdl($$data{$var}[0]->type, [$nodata]),
	'long_name'	=> $$data{$var}[1] ,
	'units'		=> $$data{$var}[2]);
    if (ref($$data{$var}[3]) eq 'HASH') {		# Add custom attributes
      delete $$data{$var}[3]{_FillValue};
      %hash = ( %hash,	 %{$$data{$var}[3]} ); }
    push @var_att, \%hash;
  }

  my %time_att	= (
	'units'		=> $ts_res eq 'hourly' ? 'hours since 1900-01-01T00:00' : 'days since 1900-01-01',
	'long_name'	=> 'Time',
	'resolution'	=> $ts_res,
	'calendar'	=> $calendar_str{$calendar}
  );

  my %crs_att	= (
	'srs_ref'	=> $proj,
	'srs_wkt'	=> $wkt,		### Attribute name convention used by CF
	'spatial_ref'	=> $wkt,		### Attribute name convention used by GDAL
	'GeoTransform'	=> join(' ', @$gT)
  );

  my %lon_att	= $proj_crs->IsGeographic ? (
	'units'		=> 'degrees_east',	'long_name'	=> 'longitude',	'standard_name' => 'longitude') : (
	'units'		=> 'meters',		'long_name'	=> 'x coordinate of projection',
	'standard_name' => 'projection_x_coordinate' );

  my %lat_att	= $proj_crs->IsGeographic ? (
	'units'		=> 'degrees_north',	'long_name'	=> 'latitude',	'standard_name' => 'latitude' ) : (
	'units'		=> 'meters',		'long_name'	=> 'y coordinate of projection',
	'standard_name' => 'projection_y_coordinate' );

  my @global_att= (
	['Conventions',		'CF-1.6'],
	['history',		'Created on '.date_now()." by $$credits[1] ($$credits[2])"],
	['NetCDF_driver',	'NetCDF.' . $PDL::NetCDF::VERSION],
	['institution',		 $$credits[3]],
	['FilePath',		 $file]
  );
  if    (ref($attrib) eq 'HASH')  { map(push(@global_att,[$_,$$attrib{$_}]), keys(%$attrib)) }
  elsif (ref($attrib) eq 'ARRAY') {     push @global_att,    @$attrib }
  else { die "Unknown type of global attributes in \"write_nc\"...\n" }

			###  NetCDF File definitions
  my $nc = new PDL::NetCDF($file, {MODE => O_CREAT, REVERSE_DIMS => 1,
	NC_FORMAT => $nc4 ? PDL::NetCDF::NC_FORMAT_NETCDF4 : PDL::NetCDF::NC_FORMAT_CLASSIC});
  my $slice_opt = {_FillValue => $nodata};	# This key works only for fixed 4.21 version of PDL::NetCDF module
    $$slice_opt{SHUFFLE} = $shuffle if $nc4;	# Remove _FillValue=> key here for the 4.21 MCPAN version (no att _FillValue)
    $$slice_opt{DEFLATE} = $deflate if $nc4;	# Can check version by presence of _FillValue in man pages, e.g.
						# >>perldoc PDL::NetCDF | grep _FillValue | wc -l
			###  Write Dimensions
  $nc->putslice('time',['time'],[PDL::NetCDF::NC_UNLIMITED()],[0],[long($time)->dim(0)], long($time));
  $nc->putslice( $yVr, [$yVr], [$lat->dims], [0], [$lat->dim(0)], double($lat->slice('-1:0')));
  $nc->putslice( $xVr, [$xVr], [$lon->dims], [0], [$lon->dim(0)], double($lon));
  $nc->putslice('crs', [],     [],           [0], [1],            PDL::Char->new(''));

			###  Write Data
  foreach my $var (@var_name) {
    $nc->putslice($var,[$xVr, $yVr, 'time'], @data_dims,				# Must use clone($slice_opt)
	($type ? $$data{$var}[0]->$type : $$data{$var}[0])->setbadtoval($nodata)->slice(':,-1:0'), dclone($slice_opt));
  }
			###  Write Attributes
  foreach my $key (@global_att)    { $nc->putatt ($$key[1], $$key[0]); }
  foreach my $key (keys %time_att) { $nc->putatt ($time_att{$key},   $key, 'time');}
  foreach my $key (keys %lat_att)  { $nc->putatt ($lat_att{$key},    $key,  $yVr); }
  foreach my $key (keys %lon_att)  { $nc->putatt ($lon_att{$key},    $key,  $xVr); }
  foreach my $key (keys %crs_att)  { $nc->putatt ($crs_att{$key},    $key, 'crs'); }

  for (my $i=0; $i<=$#var_name; $i++) {
    foreach my $key (sort keys %{$var_att[$i]}){
	$nc->putatt ($var_att[$i]{$key}, $key, $var_name[$i]); }
  }

  $nc->close();
}

sub get_nc_dim		### Get dimension size by dimension name
{
  my ($file, $dimnm) = @_;
  return 0 unless -e $file;

  my  $ncobj = PDL::NetCDF->new ($file, {MODE => O_RDONLY});
  return $ncobj->dimsize($dimnm);
}

#######################################################################

# sub shift_file_list
#
# {
#   my $file = $_[0][0][0];
#   my $band = shift @{$_[0][0][1]};
#   shift @{$_[0]} unless @{$_[0][0][1]};
#
#   return $file,$band;
# }

#######################################################################

sub get_NetCDF_compression

{
  my ($file,$var_name,$processing) = @_;
  my ($scale,$offset,$nodata) = (1,0,undef);
  return ($scale,$offset,$nodata) unless $file =~ m/\.nc$/i && $var_name;

  my $ncobj	= PDL::NetCDF->new ($file, {MODE => O_RDONLY});
		### Check that variable exists
  unless ($var_name ~~ @{$ncobj->getvariablenames()}) {
  (my  $file_s = $file) =~ s/.+:(.+):.+/$1/;
    die "\nRequested variable \"$var_name\" is not found in the NetCDF file:\n   $file_s\n\tAborting...\n\n";
  }
		### Get variable Attributes
  my %att	= map(($_ => $ncobj->getatt($_,$var_name)), @{$ncobj->getattributenames($var_name)});
  $ncobj->close();

  $scale  = $att{scale_factor} ->at(0)	if defined $att{scale_factor};
  $offset = $att{add_offset}   ->at(0)	if defined $att{add_offset};
		### Find Nodata in the NEtCDF file or from "Processing" attribute
  if (defined($processing) && $processing =~ m/nodata=([-+e\.\d]+)/i) {
    $nodata = $1;
  } else {
    $nodata = $att{_FillValue}   ->at(0)	if defined $att{_FillValue};
    $nodata = $att{missing_value}->at(0)	if defined $att{missing_value} && !defined($att{_FillValue});
  }

  die "\nNodata value is not found for $var_name variable in the NetCDF file:\n\t$file\n".
	"It can be set/forced using:\n\t".
	"1. NetCDF attribute editor, e.g. \"ncatted  -h -O -a _FillValue,$var_name,a,f,-9999 FILE\"\n\t".
	"2. Using \"nodata\" key in \"Processing\" attribute for this dataset in the MT DB\n\tAborting...\n\n"
		unless defined $nodata;

  return ($scale,$offset,$nodata);
}

#######################################################################

sub GDAL_test
{
  my $test_dir	= shift;
     $test_dir	=~ s/\/$//;

	##########     Initializations      ###########################

  return 0 unless $test_dir && -e $test_dir;
  my $result	= 0;
  my $epsg_8001	= '+proj=lcc +ellps=sphere +a=6370000.0 +b=6370000.0 +lat_1=41 +lat_0=41 +lon_0=-75 +k_0=1 +x_0=891000.92 +y_0=486000.15 +units=m +no_defs';
  my @coord	= (-71.0, 47.5);
			# Add closing slash to the path string
  $test_dir    .= '/' if $test_dir && $test_dir !~ m/\/$/;

  my $file_g1	= "NETCDF:$test_dir".'MERRA.prod.assim.tavg1_2d_slv_Nx.19790101.SUB.nc:t2m';	# Good # 1
  my $file_b1	= "NETCDF:$test_dir".'wrfout_d03_T2_2006-02-01.nc:T2';				# Bad  # 1
  my $file_b2	= "NETCDF:$test_dir".'air.2m.1948.nc:air';					# Bad  # 2
  my $file_g2	=	  $test_dir .'aqThick_05min_clip.tif';					# Good # 2
		### Temporary file in shared memory
  my $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm');
  my $tmp_file	= $tmp_obj->filename.'.tif';	# Do not combine with the line above otherwise it will fail in forks

			### Generic dataset metadata
  my $meta_g1	= {'Var_Scale' => 1, 'Var_Offset' => -273.15,'Processing' => '', 'Projection' => 'epsg:4326'};
  my $meta_b1	= {'Var_Scale' => 1, 'Var_Offset' => 0,      'Processing' => '', 'Projection' => $epsg_8001 };
  my $meta_b2	= {'Var_Scale' => 1, 'Var_Offset' => -273.15,'Processing' => '', 'Projection' => 'epsg:4326'};

	##########     Check GDAL warping    ##########################

  my $gdal_warp	= "$$PATH{gdalwarp} -r bilinear -ts 30 30 $file_g2 $tmp_file";
  my $junk	= `$gdal_warp`;
  my $raster	= read_raster($tmp_file);
  unlink $tmp_file;
  die  "\n   GDAL test failed: GDAL cannot warp correctly test file-\n   $file_g2\n   Aborting...\n\n"
	unless ($raster > 0)->sum == 323;

	##########     Check NetCDF-4       ###########################

  open STDERR, ">&NEWERR";
  { my $geotiff_data	= Geo::GDAL::Open( $file_b1, 'ReadOnly' );
    my $band		= $geotiff_data->GetRasterBand(1);
    my $x_size		= $band->{XSize};
    my $y_size		= $band->{YSize};
    eval { my $bin_data	= $band->ReadRaster(0, 0, $x_size, $y_size); };
    open STDERR, ">&OLDERR";
    die  "\n   GDAL test failed: GDAL cannot read NetCDF-4 file-\n   $file_b1\n   Aborting...\n\n" if $@;
  }
	##########     Read Input Data      ###########################

  my @coordB1	= transform_point('epsg:4326', $epsg_8001, @coord);

  my $extent_g1	= get_extent($file_g1, $$meta_g1{Projection});
  my $extent_b1	= get_extent($file_b1, $$meta_b1{Projection});
  my $extent_b2	= get_extent($file_b2, $$meta_b2{Projection});

  my @colRow_g1	= colRow($extent_g1, @coord);
  my @colRow_b1	= colRow($extent_b1, @coordB1);
  my @colRow_b2	= colRow($extent_b2, @coord);

			### Read raster
  my $rData_g1	= (read_raster($file_g1)		   + $$meta_g1{Var_Offset})->setbadtoval(-100);
  my $rData_b1	= (read_raster($file_b1))					   ->setbadtoval(-100);
  my $rData_b2	= (read_raster($file_b2,1, [0.01, 477.65]) + $$meta_b2{Var_Offset})->setbadtoval(-100);

			### Read GDAL
  my $gData_g1	= read_GDAL($extent_g1, $meta_g1, 0, $file_g1, 1, -100)->setbadtoval(-100);
  my $gData_b1	= read_GDAL($extent_b1, $meta_b1, 0, $file_b1, 1, -100)->setbadtoval(-100);
  my $gData_b2	= read_GDAL($extent_b2, $meta_b2, 0, $file_b2, 1, -100)->setbadtoval(-100);

	##########     Report Results      ############################

  $result	= 1 if
	sprintf('%.2f',$rData_g1->at(@colRow_g1)) ==  -1.33 &&
	sprintf('%.2f',$rData_b1->at(@colRow_b1)) == -20.44 &&
	sprintf('%.2f',$rData_b2->at(@colRow_b2)) == -19.70 &&

	sprintf('%.2f',$gData_g1->at(@colRow_g1)) ==  -1.33 &&
	sprintf('%.2f',$gData_b1->at(@colRow_b1)) == -20.44 &&
	sprintf('%.2f',$gData_b2->at(@colRow_b2)) == -19.70;

  die "\n\tGDAL test failed. Aborting...\nCheck with $test_dir/test_gdal.pl\n\n" unless $result;
  return $result;
}

#######################################################################

sub unpdl_data
	### Based on repack_data routine. Note- the alternative $pdl->unpdl function is MUCH INEFFICIENT!!!
{
  my ($pdl, $options) = @_;
  my  $want_arr    = set_default( $$options{ARR}, undef);
  my  $want_ref    = set_default( $$options{REF}, undef);
  my ($data,$dims) = ([$pdl->list], [reverse($pdl->dims)]);

  for (my $d=$#$dims; $d>0; $d--) {
    my $slab = 1; map $slab*=$_,@$dims[0..$d-1];
    $data = [map [@$data[$_*$$dims[$d]..($_+1)*$$dims[$d]-1]],0..$slab-1];
  }
  return $want_arr ? @$data : $want_ref ? $data : wantarray ? @$data : $data;
}

#######################################################################

sub unpdl_scalars
	### Convert PDL sums/avg/etc to perl var type (in new PDL v.2.063)
{
  my @arr;
  foreach my $var (@_) {
    push @arr, (ref($var) eq 'PDL' ? $var->at(0) : $var);
  }

  return @arr;
}

#######################################################################

sub flow_to
	### Sets flow direction to zero if it flows-
{	### (a) outside of the domainor flows; and (b) to Nodata cells
  my $dir   = shift;
  my $col   = sequence(long,$dir->dim(0))->dummy(1,$dir->dim(1));
  my $row   = sequence(long,$dir->dim(1))->dummy(0,$dir->dim(0));
  my $colTo = $col->copy;
  my $rowTo = $row->copy;

		### Fix flow to the outside of the extent
  $dir->where(($colTo== 0              ) & (($dir== 8) | ($dir==16) | ($dir== 32))) .= 0; # Left
  $dir->where(($colTo==($dir->dim(0)-1)) & (($dir== 1) | ($dir== 2) | ($dir==128))) .= 0; # Right
  $dir->where(($rowTo== 0              ) & (($dir==32) | ($dir==64) | ($dir==128))) .= 0; # Top
  $dir->where(($rowTo==($dir->dim(1)-1)) & (($dir== 2) | ($dir== 4) | ($dir==  8))) .= 0; # Bottom

		### Find Flow-to indices
  $colTo->where(($dir== 8) | ($dir==16) | ($dir== 32)) -= 1;	# Left
  $colTo->where(($dir== 1) | ($dir== 2) | ($dir==128)) += 1;	# Right
  $rowTo->where(($dir==32) | ($dir==64) | ($dir==128)) -= 1;	# Top
  $rowTo->where(($dir== 2) | ($dir== 4) | ($dir==  8)) += 1;	# Bottom

		### Fix flow to endorheic cells
  $dir->where($dir == -1) .= 0;

		### Fix flow to the Nodata cells
  my $ind  = $colTo->flat + $rowTo->flat*$dir->dim(0);
  $dir *= ($dir->flat->index($ind) >= 0)->setbadtoval(0)->reshape($dir->dims);

		### Find fixed Flow-to indices
  $colTo = $col->copy;
  $rowTo = $row->copy;
  $colTo->where(($dir== 8) | ($dir==16) | ($dir== 32)) -= 1;	# Left
  $colTo->where(($dir== 1) | ($dir== 2) | ($dir==128)) += 1;	# Right
  $rowTo->where(($dir==32) | ($dir==64) | ($dir==128)) -= 1;	# Top
  $rowTo->where(($dir== 2) | ($dir== 4) | ($dir==  8)) += 1;	# Bottom

  return $colTo,$rowTo;
}

#######################################################################

sub check_circularityC

{
  my ($flowDir, $gT) = @_;

	### Check/Search Circularity
  my ($circX, $circY)	= $flowDir->checkCircularity();

	### Report Circularity, if found
  if ($circX != -1) {
    my @direction	= ([0,1,1,0,-1,-1,-1,0,1],[0,0,1,1,1,0,-1,-1,-1]);
    my %log2		= (0=>0,1=>1,2=>2,4=>3,8=>4,16=>5,32=>6,64=>7,128=>8);
    my @route		=([$circX, $circY, $flowDir->at($circX, $circY)]);

    do {
      my $dir		= $log2{$flowDir->at($route[-1][0],$route[-1][1])};
      push @route, [$route[-1][0]+$direction[0][$dir], $route[-1][1]+$direction[1][$dir], $dir];
      $route[-1][-1]	= $flowDir->at($route[-1][0],  $route[-1][1]);
    } until $route[0][0] == $route[-1][0] && $route[0][1] == $route[-1][1];

    my  $route		= join("\n\t",map(join("\t",@$_),@route));
    my ($lon,$lat)	= Geo::GDAL::ApplyGeoTransform( $gT, $route[0][0]+0.5, $route[0][1]+0.5 );

    die <<END;

	Network circularity is detected at-
	(col,row) = ($route[-1][0], $route[-1][1])
	(lon,lat) = ($lon, $lat)

	Circular route-
	Col	Row	Dir
	$route

	Aborting...\n
END
  }
}

#######################################################################

sub check_circularity

{
  my ($flowDir, $colTo, $rowTo, $gT) = @_;
  my $dims = [$flowDir->dims];
  my $nD   = -9999;
  my $flow_dir	= unpdl_data($flowDir->setbadtoval($nD));

  my @mask	= map [(0) x $$dims[0]],1..$$dims[1];
  my @direction	= ([0,1,1,0,-1,-1,-1,0,1],[0,0,1,1,1,0,-1,-1,-1]);
  my %log2	= (0=>0,1=>1,2=>2,4=>3,8=>4,16=>5,32=>6,64=>7,128=>8);

  for (my $row=0; $row<$$dims[1]; $row++) {
    CC_LOOP:
    for (my $col=0; $col<$$dims[0]; $col++) {
      next CC_LOOP if $$flow_dir[$row][$col]==$nD || $mask[$row][$col];
      $mask[$row][$col]	= 1;

      my $col_to	= $col+$direction[0][$log2{$$flow_dir[$row][$col]}];
      my $row_to	= $row+$direction[1][$log2{$$flow_dir[$row][$col]}];

      next CC_LOOP unless $$flow_dir[$row][$col];

      my $route_to	= $col_to.'_'.$row_to;
      my %route		= ($col.'_'.$row => [$col,$row]);
      my @flow		= ([$col,   $row,   $$flow_dir[$row]   [$col]],
			   [$col_to,$row_to,$$flow_dir[$row_to][$col_to]]);
      do {
	if (defined $route{$route_to}) {		### Circularity check
	  shift @flow while ($flow[0][0].'_'.$flow[0][1] ne $route_to);
	  my $route	= join("\n\t",map(join("\t",@$_),@flow[ grep {$_ & 1} 1..$#flow ],$flow[-1]));
	  my ($lon,$lat)= Geo::GDAL::ApplyGeoTransform($gT,$col_to+0.5,$row_to+0.5);

	  die <<END;

	Network circularity is detected at-
	(col,row) = ($col_to, $row_to)
	(lon,lat) = ($lon, $lat)

	Circular route-
	Col	Row	Dir
	$route

	Aborting...\n
END
	}
	$route{$route_to}	= [$col_to,$row_to];
	my ($col_fr,$row_fr)	= ($col_to,$row_to);
	next CC_LOOP if $mask[$row_fr][$col_fr];
	$mask[$row_fr][$col_fr]	= 1;
	push @flow, [$col_fr,$row_fr,$$flow_dir[$row_fr][$col_fr]];

	$col_to		= $col_fr+$direction[0][$log2{$$flow_dir[$row_fr][$col_fr]}];
	$row_to		= $row_fr+$direction[1][$log2{$$flow_dir[$row_fr][$col_fr]}];
	$route_to	= $col_to.'_'.$row_to;
	push @flow, [$col_to,$row_to,$$flow_dir[$row_to][$col_to]];
      } while ($$flow_dir[$row_to][$col_to]);
    }
  }
}

#######################################################################

sub build_cell_table

{
  my ($runIO, $extent, $cell_area) = @_;

  print "Building Cell Table-\n";
  my $flowDir		= $$extent{mask};
  my ($colTo,$rowTo)	= flow_to($flowDir);		# Modifies $flowDir too
		### Check curcularity of the Network
  print "\tChecking curcularity of the Network...\n";
  check_circularityC($flowDir, $$extent{gTransform});			# C-coded version
# check_circularity($flowDir, $colTo, $rowTo, $$extent{gTransform});	# Plain Perl version
		### Calculate upstream area
  print "\tCalculating upstream area...\n";
  my $up_area	= byte($flowDir)->upstrAccumAll($cell_area);
		### Build Cell table
  print "\tWriting Cell table...\n";
  my ($area,$cell_table) = cell_table($up_area,$flowDir,$colTo,$rowTo);
		### Write Cell table if needed
  writeflex($$runIO{Cell_Table}, $up_area, $area, $cell_table)
	if  $$runIO{Cell_Table};

  return $up_area, $area, $cell_table;
}

#######################################################################

sub cell_table

{
  my ($up_area,$flowDir,$colTo,$rowTo) = @_;

  my $col   = sequence(long,$flowDir->dim(0))->dummy(1,$flowDir->dim(1))->flat;
  my $row   = sequence(long,$flowDir->dim(1))->dummy(0,$flowDir->dim(0))->flat;
  my $table = zeroes(long,5,$flowDir->nelem);
#      $table((0),) .= $up_area->flat;			### Upstream area
     $table((0),) .= $flowDir->flat;			### Flow direction
     $table((1),) .= $colTo->flat;			### To Cell X
     $table((2),) .= $rowTo->flat;			### To Cell Y
     $table((3),) .= $col;				### Cell X
     $table((4),) .= $row;				### Cell Y

  my $len   = $flowDir->flat->ngoodover - 1;
  my $area  = $up_area->flat->qsort->(:$len)->copy;
     $table = $table->dice('X',$up_area->flat->qsorti)->(,:$len)->copy;

  return $area, $table;
}

#######################################################################

sub find_outlets
	### Find indices of endorheic and exorheic outlets in the cell table
{
  my ($CT,    $cell_area, $runSet) =  @_;
  my ($runIO, $meta, $extent, $MT) = @$runSet;
  my  %param	= read_param_str($runIO, 'wbmParam');		# Custom WBM parameters
  my  $Net	= $$extent{mask};
  my  $mthMask	= $Net < 1;					# This is mask of river outlets (mouths)

  my  $EnR_Dir	= set_default($param{endorheicDir},    0);	# Flow direction (-1) in Network for endorheic outlets
  my  $segment	= set_default($param{exorheicCoast},   1);	# Segmentation of contineous coastal river outlets
  my  $buffer	= set_default($param{endorheicBuffer}, 0);	# Buffer around network to exclude endorheic basins
  my  $oceanMsk	= set_default($param{oceanMask_file}, '');	# Ocean mask file to use to exclude endorheic outlets
  my  $maskStr	= set_default($param{endorheicMask},   0);	# Mask (val > 0) for endorheic basins to keep
  my  $endoMask	= $EnR_Dir ? read_raster($$extent{file}) == -1 :
		  $maskStr ? read_Layer(  $extent,$meta,$maskStr,$MT,{PATCH_VALUE=>0}) > 0 : $mthMask;

	### Coastal segmentaiton mask of outlets
  if ($segment && !$EnR_Dir) {
    my $rim	= $mthMask->isgood->conv2d(ones(3,3),{Boundary => 'Truncate'})->copybad($mthMask) < 9;
    my $segment	= $mthMask->setbadtoval(0)->cc8compt;	# Connected 8-component labeling (segmentation)
    my $rimPnts	= whichND($rim);			# Must make $rimPnts otherwhise it will cause whichND list warning
       $endoMask->where($segment->mask_union($rimPnts)) .= 0; # WARNING: ccNcompt does not handle bad values
  }

  my  %mouth	= (all => which($CT((0),) == 0));	# Indices if all outlets
  my (@endorheic,  @exorheic, @ALL, @ENDORHEIC, @EXORHEIC);
      $buffer	= floor($buffer/sqrt($cell_area))->lclip(1)->long;	# Convert buffer distance to pixels
  my  $ocean	=  $oceanMsk  ? read_GDAL($extent, {}, 0, $oceanMsk, 1, 1) : zeroes($Net->dims);
  my  $land	=(($ocean==0) & $Net->isgood)->setvaltobad(0)->long;

  foreach my $row ($mouth{all}->list) {			# Row in Cell Table for the outlet
    my $xy	= $CT(3:4, $row) ->reshape(2);
    my $bd	= $buffer->at($xy->list);
    push @ALL, $xy;
    if (!$endoMask($xy->list) || (!$EnR_Dir && $land->range($xy-$bd,2*$bd+1,'truncate')->nbad)) { # Check for outlet type
      push @exorheic,  $row;
      push @EXORHEIC,  $xy;
    } else {
      push @endorheic, $row;
      push @ENDORHEIC, $xy;
    }
    $Net($xy->list)    .= 0;	# Just in case, set flow direction to zero for the outlet
  }
  $mouth{endorheic}	= long(\@endorheic);
  $mouth{exorheic}	= long(\@exorheic);
  $mouth{ALL}		= long(\@ALL);
  $mouth{ENDORHEIC}	= long(\@ENDORHEIC);
  $mouth{EXORHEIC}	= long(\@EXORHEIC);

  return \%mouth;
}

sub saveEndoMask {
  my ($mouth, $runSet)	= @_;
  my ($runIO, $meta, $extent)	= @$runSet;
  return if $$runIO{noOutput};
		### Initialize endorheic mask grid
  my  $grid	= zeroes($$extent{ncols},$$extent{nrows})->copybad($$extent{mask});
		### Populate endorheic mask grid
  map $grid->where($$extent{mask}->upstreamMask($$mouth{ENDORHEIC}->(,$_)->list)).=1, 0..$$mouth{ENDORHEIC}->dim(1)-1;
		### Save it to a file
  write_tif($extent, 'Int16', $$runIO{Output_dir}.'/endorheic_mask.tif', $grid);
}

#######################################################################

sub grid_to_table
	### Converts a gridded field to a network table
{
  my	($grid,		$CT) = @_;	# References to pdl and to cell table
  return $grid->indexND($CT(3:4,));
}

sub table_to_grid
	### Converts a network table to a gridded field
{
  my ($table, $CT, $z) = @_;

  my  $grid = $z->copy;
      $grid->indexND($CT(3:4,)) .= $table;

  return $grid;
}

#######################################################################

# sub makeStack
	### Makes stack of a feature IDs (e.g. dams) to be used in "routing' function
# {
#   my
# }

#######################################################################

sub add_connectivity
	### Read interbasin connectivity table + pre-processing
	### It builds array of connectivity routes and updates the Cell Table
{
  my ($runSet, $cell_area)	= @_;
  my ($runIO,  $meta, $extent)	= @$runSet;

	############################################
		###  Read or Build Cell Table
  my ($up_area,$area,$cell_table) = (-e $$runIO{Cell_Table}) ?
	readflex($$runIO{Cell_Table}) : build_cell_table($runIO, $extent, $cell_area);

	#########################################################
		###  Add Connectivity and Re-Build Cell Table

  my %input = read_param_str($runIO, 'ConnectivityNetwork');
  return (0,[pdl(),pdl(),pdl()],$cell_table,$up_area) unless %input;

  my  $igT		= $$extent{igTransform};
  die "Connectivity Database file does not exist-\n$input{File}\n\tAborting...\n\n"    unless -e $input{File};
  die "\nConnectivity data input block seems to be in a wrong format. Aborting...\n\n" unless
		defined($input{File}) &&  defined($input{SkipLines});
  my ($header,@table)   = read_table($input{File},$input{SkipLines});
  return (0,[pdl(),pdl(),pdl()],$cell_table,$up_area) unless scalar(@table);

				### Convert PDL cell table to Perl array
  my @cell_table	= unpdl_data($cell_table);
  my @area		= $area->list;
  map splice(@{$cell_table[$_]},3,0,$area[$_]),0..$#cell_table;		# Add area to col=4

  my @route;			### Connectivity data
  my @missingYear_list;
  my @lockSkipYear_list;
  my $lock_year		= isNumber($input{Lock_Year}) ? julian_day($input{Lock_Year},7,1) : 0;
  foreach my $row (@table)
  {
    next unless $$row[$$header{$input{Use}}];
    next if     $$row[$$header{Speculative}] && !$input{Speculative};
			### Check empty coords
    map {next unless $$row[$$header{$_}]} ($input{FromLon},$input{FromLat},$input{ToLon},$input{ToLat});
			### Coords
    my @coord_from  = ($$row[$$header{$input{FromLon}}], $$row[$$header{$input{FromLat}}]);	# From Lon/Lat
    my @coord_to    = ($$row[$$header{$input{ToLon}}],   $$row[$$header{$input{ToLat}}]);	# To   Lon/Lat
    my @colRow_from = map(POSIX::floor($_), Geo::GDAL::ApplyGeoTransform($igT,@coord_from));
    my @colRow_to   = map(POSIX::floor($_), Geo::GDAL::ApplyGeoTransform($igT,@coord_to));

					# Check if coords are in bounds
    next if $colRow_from[0] < 0 || $colRow_from[0] >= $$extent{ncols} ||
	    $colRow_to[0]   < 0 || $colRow_to[0]   >= $$extent{ncols} ||
	    $colRow_from[1] < 0 || $colRow_from[1] >= $$extent{nrows} ||
	    $colRow_to[1]   < 0 || $colRow_to[1]   >= $$extent{nrows};

			### Diversion parameters
						# Canal Length (km)
    my $connLength  = $$row[$$header{$input{TransferLen}}]	|| arcLength(@coord_from,@coord_to);
    my $PercentFlow = $$row[$$header{$input{PercentFlow}}]	|| 0;
    my $MinFlow     = $$row[$$header{$input{MinFlow}}]		|| 0;
    my $MaxFlow     = $$row[$$header{$input{MaxFlow}}]		|| 0;		# Default EndYear = 3000-12-31
    my $StartYear   = $$row[$$header{$input{StartYear}}] =~ m/(\d{4}-\d{2}-\d{2})/ ? julian_day(split m/-/,$1) : 0;
    my $EndYear     = $$row[$$header{$input{EndYear}}]   =~ m/(\d{4}-\d{2}-\d{2})/ ? julian_day(split m/-/,$1) : 0;
		### Use routes that are active on the Lock Year for all dates in the run, if requested
    ($StartYear, $EndYear) = $StartYear<$lock_year && $EndYear>$lock_year ? (1, 1e8) : (-1, -1) if $lock_year;
    if ($StartYear==0 || $EndYear==0) {
      push @missingYear_list, $$row[$$header{$input{ID}}];
      next;
    }
    if ($StartYear<0  || $EndYear<0) {
      push @lockSkipYear_list, $$row[$$header{$input{ID}}];
      next;
    }
				# Connectivity method and its parameter(s)
    my @connPars    =($PercentFlow,$MinFlow,$MaxFlow,$StartYear,$EndYear,$connLength,0,0,0,0);
			### All together
    push @route,[
	@colRow_from,			# (col,row) - [0,1]
	@colRow_to,			# (col,row) - [2,3]
	$$row[$$header{$input{ID}}],	# ID        - [4]
	@connPars];			# Parameters- [5..7]; Dates - [8,9]; Length/Width [10,11]
  }
				### Prepare cell table hash
  my %cell_ind = map(($cell_table[$_][4].'_'.$cell_table[$_][5] => $_), 0..$#cell_table);

		### Check Circularity and find downstream flow from recipient cell
  my @circ_list;
  my %flow;		# Receiving flow
  for (my $j=0; $j<=$#route; $j++) {
    my $route_from = $route[$j][0].'_'.$route[$j][1];
    my $route_to   = $route[$j][2].'_'.$route[$j][3];
    if ($route_from eq $route_to) {
      push @circ_list, [$j,0];				# Add "Same Cell"
      next;
    }
    unless (defined $cell_ind{$route_from} && defined $cell_ind{$route_to}) {
      push @circ_list, [$j,1];				# Add "Out of Network"
      next;
    }

    my @flow;
    my $mouth	= 0;
    my $cell	= $route_to;

    while (defined $cell_ind{$cell}) {
      if ($cell eq $route_from) {
	push @circ_list, [$j,2];			# Add "Circularity"
	@flow = ();
	last;
      }
      push @flow, $cell_ind{$cell};
      last unless $cell_table[$cell_ind{$cell}][0];	# River mouth
      $cell	= $cell_table[$cell_ind{$cell}][1].'_'.$cell_table[$cell_ind{$cell}][2];
    }
    next unless @flow; #- it is circularity!
							# Save flow data
    my $ctRow  = $cell_ind{$route_from};
       $ctRow += 0.01 while defined $flow{$ctRow};  # Work around split donor cells
    $flow{$ctRow} = [$route[$j][4],@flow];
  }

		### Update cell table with connectivity upstream area
  my ($addedArea,$nLoops) = (0,0);
  do {
    $addedArea = 0;
    foreach my $rte (sort keys %flow) {
      my @flow		= @{$flow{$rte}};		# The first element of @flow is route ID
      my $area_from	= $cell_table[$rte][3];
      my $area_to	= $cell_table[$flow[1]][3];
      my $add		= ($area_from < $area_to) ? 0 : $area_from + 1 - $area_to;
      map $cell_table[$_][3]+=$add,@flow[1..$#flow] if $add;	# Forcing the recepient cell to be larger
      $addedArea += $add;
    }
    die "Cannot update cell table with connectivity upstream area...\n" if $nLoops++ > 1e3;
  } until $addedArea == 0;

		### Sort cell table
  @cell_table = sort {$$a[3] <=> $$b[3]} @cell_table;
  my @cell_table_pdl= map [@$_[0,1,2,4,5]],@cell_table;

  map $cell_ind{$cell_table[$_][4].'_'.$cell_table[$_][5]}=$_ , 0..$#cell_table;

		### Report connectivity status

  my @code = ('Same Cell','Out of Network','Circularity');
  print  "\nConnectivity initialization summary:\n";
  printf "   Use Speculative Routes   = %s\n",$input{Speculative}? 'Yes'	     : 'No';
  printf "   Use Lock Year            = %s\n",$input{Lock_Year}  ? $input{Lock_Year} : 'No';
  print  "   Upstream Area Loops      = $nLoops\n";
  printf "   Good Connectivity Routes = %d\n",scalar(@route) - scalar(@circ_list);
  printf "   Bad  Connectivity Routes = %d\n\n",scalar(@circ_list) + scalar(@missingYear_list) + scalar(@lockSkipYear_list);
  print  "   Details:\n",join("\n",map(sprintf(
	   "\tBad Connectivity for ID = %-7s%s", $route[$$_[0]][4],$code[$$_[1]]),@circ_list)),"\n" if @circ_list;
  print  "   Details:\n",join("\n",map(
	   "\tMissing Start/End date(s) for ID = $_",@missingYear_list )),"\n" if @missingYear_list;
  print  "   Details:\n",join("\n",map(
	   "\tSkipped by Lock Year date for ID = $_",@lockSkipYear_list)),"\n" if @lockSkipYear_list;

		### Remove bad connectivity
  map splice(@route,$$_[0],1),reverse(@circ_list);
  return (0,[pdl(),pdl(),pdl()],$cell_table,$up_area) unless scalar(@route);

			### Build Connectivity Stack hash
  my %connStack;
  my @connStack = (-1);	# First route does not exist (-1 row in the Cell Table)
  for (my $j=0; $j<=$#route; $j++) {
    my $row = $cell_ind{$route[$j][0].'_'.$route[$j][1]};
    push @{$connStack{$row}}, $j;
  }
  pop @connStack if %connStack;		# remove row = -1

			### Build Connectivity Stack array and count Split donors
	my %split_donor;
	my $count = 0;		# Count for split donors
  foreach my $cell (sort {$a <=> $b} keys %connStack) {
    my @arr	= @{$connStack{$cell}};
    my $size	= scalar @arr;
    push @connStack,$cell,$size,@arr;
					# Add routes to "split donor" hash
	if ($size > 1) {
	  $split_donor{$cell} = [map $route[$_],@arr];
	  $count = $size if $size > $count;
	}
  }
  push @connStack,-1;			# add row = -1 at the end of the stack

		### Report Identical routes
  my (%identical_routes, %route);
  foreach my $rte (@route) {
    my $key = join '-', @$rte[0..3];
    if (defined $route{$key}) {
      if (defined $identical_routes{$key}) { push(@{$identical_routes{$key}},$$rte[4]); }
      else {      $identical_routes{$key} = [$route{$key},$$rte[4]]; }}
    else { $route{$key} = $$rte[4]; }
  }
  if (%identical_routes) {
    my $check = 0;
    foreach my $key (sort keys %identical_routes) {
      printf "%s   Identical Route IDs = %s\n", $check++?'':"\n", join(' ',@{$identical_routes{$key}});
    }
  }
		### Report split donors and check diversion rules
  if ($count > 1) {
    my ($check,$n) = (0,0);
    foreach my $row (sort keys %split_donor) {
      my @rte = @{$split_donor{$row}};
			### Check if percent flow > 100 % in split donors
      my $sum = List::Util::sum(map($$_[5],@rte));
      my $percent_flag = $sum > 100 ? "\n\tWarning: Total percent = $sum %" : '';
      map $_*=100/$sum,map($$_[5],@rte) if $sum > 100;
			### Set min flow in split donors
      my $min = List::Util::max(map($$_[6],@rte));	# Use maximum of $MinFlow in split donors
      map $_=$min,map($$_[6],@rte);

      printf "%s   Split Donor Cell (%d-way): Route IDs = %s%s\n",
      $check++?'':"\n", scalar(@rte), join(' ',map($$_[4],@rte)), $percent_flag;
    }
  }
  print "\n";
		### Route components: Col/Row from, Col/Row to,  Start/End dates
		### Rule  components: PercentFlow,  Min/MaxFlow, Length/Width, PET, Evap, waterEc
  my $route	= long  ([map([@$_[0..3,8,9]],   @route)]); # ID is excluded as it can be a string
  my $rule_pars	= double([map([@$_[5..7,10..14]],@route)]);
  return \@route, [$route,$rule_pars,long(\@connStack)], long(\@cell_table_pdl), $up_area;
}

#######################################################################

sub arcLength
		### Great-circle distance between two Lon/Lat points
		### From: http://en.m.wikipedia.org/wiki/Great-circle_distance
{
  my ($L1,$P1,$L2,$P2)	= map $_*pi/180, @_;

  return 0 if $L1==$L2 && $P1==$P2;	# Same location of the two points

  my ($sinP1,  $sinP2,  $cosP1,  $cosP2,   $sinDL,      $cosDL) =
    (sin($P1),sin($P2),cos($P1),cos($P2), sin($L1-$L2),cos($L1-$L2));

  return 6371 *		# Earth radius in WGS84 (km)
  	atan2(	sqrt(($cosP2*$sinDL)**2 + ($cosP1*$sinP2-$sinP1*$cosP2*$cosDL)**2),
		$sinP1*$sinP2 + $cosP1*$cosP2*$cosDL);
}

#######################################################################

sub rmtOrigPrl
		### Perl version of locating remote water source (Origin) by its indicies
{
  my($useFlowRmt, $useFlowRID) = @_;
  my $useFlow	= zeroes($useFlowRmt->dims)->copybad($useFlowRmt);

  foreach   my $i (0 .. $useFlow->dim(0)-1) {
    foreach my $j (0 .. $useFlow->dim(1)-1) {
      $useFlow($useFlowRID(,$i,$j)->list) += $useFlowRmt($i,$j);
  } }
  return $useFlow;
}

#######################################################################

sub diversion_yr_total

{
  my ($diversion,$rtDiv,$date,$runIO,$route,$dt) = @_;
  return  unless $$runIO{ConnectivityNetwork};

		### Create file directory
  my $dir_out	= $$runIO{Output_dir} . '/diversion';
  my $file	= "$dir_out/$$runIO{ID}.csv";
  unless (-e $dir_out) { mkpath($dir_out,0,0775) or die "Cannot create-\n$dir_out\n"; }

		### Create file header
  unless (-e $file) {
    open (FILE,">$file") or die "Couldn't open $file, $!";
      printf FILE "Year\t%s\n",join("\t",map($$_[4],@$route));
    close FILE;
  }
		### Initialize diversion data
  map(push(@$diversion,0),0..$#$route) unless @$diversion;
		### Accumulate diversion
  map {$$diversion[$_] += $rtDiv->at($_)*$dt} 0..$#$route;

		### Save diversion data
  my $endDay = $$runIO{calendar}==360 ? 30 : 31;
  if ($$date[1]==12 && $$date[2]==$endDay) {
    open (FILE,">>$file") or die "Couldn't open $file, $!";
      printf FILE "$$date[0]\t%s\n",join("\t",map(sprintf("%.2f",$_),@$diversion));
    close FILE;
    @$diversion = ();
  }
}

#######################################################################

sub aquifer_summary

{
  my ($aqfType, $aqf_data, $date, $runIO, $compP, $compIrr) = @_;
  return unless $aqfType;
		# Removed from public domain
}

#######################################################################

sub compBalance_save
{
  my ($compBal, $compSwitch, $date, $runIO, $nIrr) = @_;
		### Create file directory
  my $dir_out	= $$runIO{Output_dir} . '/tracking_balance';
  unless (-e $dir_out) { mkpath($dir_out,0,0775) or die "Cannot create-\n$dir_out\n"; }

	### Processing output for each bucket
  if ($$compSwitch[2]) {
    my @list	= scalar(@{$$compBal{Prm}{Soil}}) == 4 ? qw(Snow Glacial Rain Relict) :
							 qw(Snow GlacIce GlacNonIce Rain Relict);
    my @file	= map "$dir_out/PrimarySource_$_.csv", @list;
		### Spreadsheet Header matching data hash keys
    my  @hdr = qw(Soil Soil_prev Soil_delta VIS VIS_prev VIS_delta RffStg RffStg_prev RffStg_delta SIR SIR_prev SIR_delta Stream Stream_prev Stream_delta GrWater GrWater_prev GrWater_delta AqWater AqWater_prev AqWater_delta WaterIn EvapSoil EvapDIL EvapIrr EvapTech EvapRiv Out StrDlt Balance);

		### Write data to files
    my @storages = qw(Soil VIS RffStg SIR Stream GrWater AqWater);
    my @fluxes   = qw(WaterIn EvapSoil EvapDIL EvapIrr EvapTech EvapRiv Out StrDlt);
    my @fluxDir  =   (  -1,      1,       1,     1,       1,       1,    1,   -1  );

    write_balance_files(\@file, \@hdr, \@storages, \@fluxes, \@fluxDir, $$compBal{Prm}, $date);
  }

  if ($$compSwitch[3]) {
    my @file	= map "$dir_out/Irrigation_$_.csv", (qw(Relict Pristine DomIndLsk), ($nIrr==1?():
		  map("Irrigation_$_",1..$nIrr)), 'Irrigation');
		### Spreadsheet Header matching data hash keys
    my  @hdr = qw(Soil Soil_prev Soil_delta VIS VIS_prev VIS_delta RffStg RffStg_prev RffStg_delta SIR SIR_prev SIR_delta Stream Stream_prev Stream_delta GrWater GrWater_prev GrWater_delta AqWater AqWater_prev AqWater_delta Gross EvapSoil Net UsedDIL UsedIrr UsedIrrFlow UsedIrrGrwt UsedIrrAqf EvapIrr EvapTech EvapRiv Out StrDlt Balance);

		### Write data to files
    my @storages = qw(Soil VIS RffStg SIR Stream GrWater AqWater);
    my @fluxes   = qw(Gross EvapSoil Net UsedDIL UsedIrr UsedIrrFlow UsedIrrGrwt UsedIrrAqf EvapIrr EvapTech EvapRiv Out StrDlt);
    my @fluxDir  =   ( -1,      1,    1,    1,       1,      0,          0,          0,         1,      1,      1,    1,   -1  );

    write_balance_files(\@file, \@hdr, \@storages, \@fluxes, \@fluxDir, $$compBal{Irr}, $date);
  }

	########################################################
	###	Write README file	########################
  my $fileRM	=     "$dir_out/README.txt";
  unless (-e $fileRM) {
    open (FILE,">$fileRM") or die "Couldn't open $fileRM, $!";
    print FILE <<END;
Component balance formulations and terms-

=====================================================

   sum(Storage_changes) = sum(Fluxes)*dt

for each tracking component in

A. 'Primary Source' ensemble-
   1. Snow melt water
   2. Glacial melt water
   3. Rain water
   4. Relict and Unsustainable water

B. 'Irrigation' ensemble-
   1. Relict and Unsustainable water
   2. Pristine water
   3. Water used in Domestic, Industrial, Livestock demands
   4. Water used in Irrigation, Cycle # 1
   5. Water used in Irrigation, Cycle # 2
   6. etc. Number of cycles is defined by parameter "nIrr"

   sum(fraction(component(i))) = 1

where i is a component listed above

=====================================================
A. 'Primary source' ensemble-

= = = = = = = = = = = = = = = = = = = = = = = = = = =
Storages      Comment
Soil          Soil storage
VIS           Virtual Irrigation Storage;
RffStg        Runoff Retention pool storage
SIR           Small Irrigation Reservoirs
Stream        Rivers and reservoirs
GrWater       Ground water storage
AqWater       Aquifer water storage

= = = = = = = = = = = = = = = = = = = = = = = = = = =
Fluxes       Comment                        Direction
WaterIn      Added to the system            +In
EvapSoil     Soil evaporation               -Out
EvapDIL      Dom/Ind/Lsk evaporation        -Out
EvapIrr      Irr consumptive evap.          -Out
EvapTech     Irr. tech.  evaporation        -Out
EvapRiv      Open water  evaporation        -Out
Out          Discharged at mouth            -Out
StrDlt       Flow use underestimation       +In
             plus Land rotation             +In/-Out

=====================================================
B. 'Irrigation' ensemble-

= = = = = = = = = = = = = = = = = = = = = = = = = = =
Storages      Comment
Soil          Soil storage
VIS           Virtual Irrigation Storage;
RffStg        Runoff Retention pool storage
SIR           Small Irrigation Reservoirs
Stream        Rivers and reservoirs
GrWater       Ground water storage
AqWater       Aquifer water storage

= = = = = = = = = = = = = = = = = = = = = = = = = = =
Fluxes       Comment                        Direction
Gross        Gross or added to the system   +In
EvapSoil     Soil evaporation               -Out
Net          Net or removed                 -Out/0*
UsedDIL      Taken for Dom/Ind/Lsk          -Out
UsedIrr      Taken for Irrigation           -Out
UsedIrrFlow  Flow        portion in UsedIrr  0
UsedIrrGrwt  Groundwater portion in UsedIrr  0
UsedIrrAqf   Aquifer     portion in UsedIrr  0
EvapIrr      Irr consumptive evap.          -Out
EvapTech     Irr. tech.  evaporation        -Out
EvapRiv      Open water  evaporation        -Out
Out          Discharged at mouth            -Out
StrDlt       Flow use underestimation       +In
             plus Land rotation             +In/-Out
* Irrigation Net is applied to Soil, and so
  not removed from the system (Direction = 0)

=====================================================
Notes:
   1. Units, m3
   2. Only active storages with in/out fluxes outside of the hydrological cycle are listed above
   3. 'Land rotation' refers to changes in open water or inprevious surface area
END
    close FILE;
  }

  return 1;
}

#######################################################################

sub write_balance_files
{
  my ($files, $hdr, $storages, $fluxes, $fluxDir, $bal, $date)	= @_;
  my  %fluxDir	= map(($$fluxes[$_]=>$$fluxDir[$_]), 0..$#$fluxes);

		### Create/initiate files and headers
  unless (-e $$files[0]) {
    foreach my $file (@$files) {
      open (FILE,">$file") or die "Couldn't open $file, $!";
      print FILE join("\t", 'Date',@$hdr), "\n";
      close FILE;
  } }
		### Write data to files
  for (my $i=0; $i<scalar(@$files); $i++) {
    my  $irr_Flag	= $$files[$i]=~m/_Irrigation(_\d+)*\.csv$/ ? 1 : 0;
    map $$bal{$_.'_delta'}[$i] = $$bal{$_}[$i] - $$bal{$_.'_prev' }[$i], @$storages;
    $$bal{Balance}[$i]	= List::Util::sum(map($$bal{$_.'_delta'}[$i], @$storages),
	map($$bal{$_}[$i]*($_ eq 'Net' && $irr_Flag ? 0 : $fluxDir{$_}), @$fluxes));

    open (FILE,">>$$files[$i]") or die "Couldn't open $$files[$i], $!";
    print FILE join("\t", $date,map(sprintf("%.1f", $$bal{$_}[$i]*1e9), @$hdr)), "\n";
    close FILE;
  }
}

#######################################################################

sub add_reservoirs

{
  my ($runSet, $meta, $cell_area, $cell_ind, $save) = @_;
  my ($runIO,  $junk, $extent) =  @$runSet;

  my(@reservoir, %resStack, $damID);
  my @resStack		= (-1);	# First dam does not exist (-1 row in the Cell Table)
  # resStack is indexed by Cell_Index in routing ... index -1 is never encountered
  return (\@reservoir, [pdl(), pdl(), long(\@resStack)]) unless $$runIO{Reservoirs};
  print "Initialization of datasets for reservoirs and dams...\n";

  my  @report	= ((0) x 10);			# Summary report
  my  $startDate= julian_day(split m/-/,$$meta{Start_Date});
  my  $endDate	= julian_day(split m/-/,$$meta{End_Date});

	############################################
		###  Read or Build Cell Table
  my ($up_area,$area,$cell_table) = (-e $$runIO{Cell_Table}) ?
	readflex($$runIO{Cell_Table}) : build_cell_table($runIO, $extent, $cell_area);
      $up_area->inplace->copybad($$extent{mask});

	#########################################################
		###  Add Reservoirs
  my  %input		= read_param_str($runIO, 'Reservoirs');
  my  $igT		= $$extent{igTransform};
		### Read raw Dam Database
  die "\nReservoirs dam DB file does not exist:\n$input{File}\n\tAborting...\n\n"   unless -e $input{File};
  die "\nReservoir data input block seems to be in a wrong format. Aborting...\n\n" unless
		defined($input{File}) &&  defined($input{SkipLines});
  my ($header,@table)	= read_table($input{File},$input{SkipLines});
  die "\nNo Data in the Reservoirs dam DB file:\n\t$input{File}\nAborting...\n\n" unless @table;
		### Read dam operating parameters
  my ($hdr,@parTable)	= read_table($input{damUseParams}) if $input{damUseParams};
  if (@parTable) {
    map splice(@$_,0,2), @parTable;				# Remove first two columns of the damUseParams,
    my $rows = scalar @parTable;				# assuming Use_ID(s) are in correct order
    my $cols = scalar @{$parTable[0]};
    die "Wrong dimentions of the \"damUseParams\" data. Aborting...\n\n" unless $rows==6 && ($cols==8 || $cols==9);
  } else {
		### Default damUseParams
    @parTable	= (				# (Min/Max capacities are in m3)
  #	MinCap	MaxCap	c	p	Y0	Y1	Xe	Ye  # "c" is used as "alpha" for lakes/spill dams
  #	0	1	2	3	4	5	6	7   #	Water release method in increasing PRIORITY ORDER:
	[0,	1e+9,	0.3,	0,	0,	0,	0,	0  ],	# 0 - Unregulated (Lakes and Spill dams)
	[1e+9,	undef,	4,	6,	0.2,	5,	0.8,	1  ],	# 1 - Generic
	[1e+9,	undef,	100,	170,	0.2,	5,	0.2,	1  ],	# 2 - Flood control
	[2e+8,	undef,	200,	6,	0.2,	5,	0.85,	1  ],	# 3 - Hydropower
	[1e+9,	undef,	200,	6,	0.2,	5,	0.85,	1  ],	# 4 - Irrigation
	[1e+9,	undef,	1,	6,	0.1,	5,	0.7,	0.1]);	# 5 - Water Supply
  }
  my @irrScale	= map { $#$_==7 ? 1 : pop @$_ } @parTable;		# Temporarily remove "Irr_Scale" parameter
  map { $_=1 unless isNumber($_) } @irrScale;				# Fill "Irr_Scale" with 1 if not defined

		### Calculate additional parameters to the @parTable
  map push(@{$parTable[$_]}, $_ ? d_root_solver([@{$parTable[$_]}[2.. 7]]) : 0), 0..$#parTable;	# "d" parameter [8]
  map push(@{$parTable[$_]}, $_ ? a_dam_equaton([@{$parTable[$_]}[2.. 7]]) : 0), 0..$#parTable;	# "a" parameter [9]
  map push(@{$parTable[$_]}, $_ ? b_dam_equaton([@{$parTable[$_]}[2.. 8]]) : 0), 0..$#parTable;	# "b" parameter [10]
  map push(@{$parTable[$_]}, $_ ? B_dam_equaton([@{$parTable[$_]}[2..10]]) : 0), 0..$#parTable;	# "B" parameter [11]
  map push(@{$parTable[$_]}, $irrScale[$_]),   0..$#parTable;				# "Irr_scale" parameter [12]

		### Read custom dam operating parameters
  my ($hdc,@customPar)	= read_table($input{damCusParams}) if $input{damCusParams};
  map $customPar[$$hdc{Dam_ID}] =~ s/^\s+|\s+$//g, @customPar;		# Trim Dam_ID string
  die "Wrong format of \"damCusParams\" file: $input{damCusParams}\n\n" if @customPar && !exists($$hdc{Dam_ID});
		### Add @parTable to %input hash
  $input{parTable} = \@parTable;

  my  $min_capacity	= List::Util::min(map($$_[0],@parTable));
  my  @purpose_count	= ((0) x 8);

                ################################
                ### Read in dam outflow
  my  %outflow;
  if ($input{damObsOutflowDir}) {
      print "Reading observed dam outflow ... \n";
      my @files;
      my $dir = $input{damObsOutflowDir};
      if (-d $dir) {
	opendir( DIR_HANDLE, $dir ) || die "failed to create directory handle to $dir: $!";
	  @files = sort grep(m/\.csv$/i, readdir(DIR_HANDLE));
	closedir DIR_HANDLE;
      }
      else { die "Directory for dam observed outflows: $dir does not exist!\n\n"; }

      foreach my $file (@files) {
	 (my  $ID = $file) =~ s/.csv$//;
	  my ($hof,@obs)   = read_table("$dir/$file") if grep($ID, @table[$$header{NIDID}]);
		### Check date format: american (1) or universal (2)
	  unless (defined $$hof{DateTime}) {
	    printf "\tSkipped observed reservoir outflow file due to wrong format: $file\n";
	    next;
	  }
	  my  $dateFormat  = $obs[0][$$hof{DateTime}] =~ m/\d{1,2}\/\d{1,2}\/\d{4}/ ? 1 :
			     $obs[0][$$hof{DateTime}] =~ m/\d{4}-\d{2}-\d{2}/       ? 2 : 0;
	  die "Unknown date string format in file-\n\t$dir/$file\nAborting...\n\n" unless $dateFormat;

		### Create a hash between date and flow
	  my %obs_flows;
	  foreach my $ROW (@obs) {
	    my $fdate	= $dateFormat == 2 ?			$$ROW[$$hof{DateTime}] :
			  sprintf('%04d-%02d-%02d', (split('/',	$$ROW[$$hof{DateTime}]))[2,0,1]);
	    $obs_flows{$fdate} = $$ROW[$$hof{Q_out_m3d}];
	  }
	  $outflow{$ID}	= \%obs_flows;
  }   }

		################################
		### Process the Dam Database

  my $damSubsetFile_F	= $$runIO{Output_dir}."/damSubset_filtered.csv";	# Filtered   dam DB subset
  my $damSubsetFile_U	= $$runIO{Output_dir}."/damSubset_unfiltered.csv";	# Unfiltered dam DB subset
  my $damSubsetFile_D	= $$runIO{Output_dir}."/damSubset_discarded.csv";	# Discarded  dam DB subset
  if ($save) {
    my $hdr		= join "\t", sort({$$header{$a} <=> $$header{$b}} keys(%$header));
			# Filtered   dam DB subset
    open (FILE_F,">$damSubsetFile_F") or die "Couldn't open $damSubsetFile_F, $!";
    print FILE_F   $hdr,"\tOverlap_Method:IDs\tNet_Lon\tNet_Lat".		# Print header
		 "\tSnappedTo_Lon\tSnappedTo_Lat\tSnappedTo_Upstr\n";
			# Unfiltered dam DB subset
    open (FILE_U,">$damSubsetFile_U") or die "Couldn't open $damSubsetFile_U, $!";
    print FILE_U   $hdr,"\n";							# Print header
			# Discarded  dam DB subset
    open (FILE_D,">$damSubsetFile_D") or die "Couldn't open $damSubsetFile_D, $!";
    print FILE_D   $hdr,"\tFilter\tDB_value\n";					# Print header
  }
  my  $lock_year	= isNumber($input{Lock_Year}) ? julian_day($input{Lock_Year},7,1) : 0;
  my  $search_mtch	= set_default($input{search_tolerance},	0.4);	# Tolerance to catchment mismatch
  my  $search_dist	= set_default($input{search_dist},	5.0);	# Default is 5 km radius search
  my  $search_pix	= floor($search_dist / sqrt($cell_area))->long->lclip(1);
								### Convert search radius to pixels (1 pix min)
  foreach my $ROW (@table) {
			### Data
      ($damID	= $$ROW[$$header{$input{ID}}]) =~ s/^\s+|\s+$//g;		# Dam ID (trim string)
    my @coord	=($$ROW[$$header{$input{Lon}}], $$ROW[$$header{$input{Lat}}]);	# Lon/Lat
				# Check if coords are in bounds
    my ($col,$row) = map(POSIX::floor($_), Geo::GDAL::ApplyGeoTransform($igT,@coord));
    if ($col < 0 || $col >= $$extent{ncols} ||
	$row < 0 || $row >= $$extent{nrows} || $up_area($col,$row)->isbad->sum)
    {$report[0]++; next}
    print FILE_U join("\t",@$ROW),"\n" if $save;		# Print unfiltered dam DB entry used in this run
				# Check records
    map { if (!(isNumber($$ROW[$$header{$_}])) || $$ROW[$$header{$_}] == -99) {
	    $report[1]++;
	    print FILE_D join("\t",@$ROW),"\tIncomplete data\t$_\n" if $save;
	    next;
    }} ($input{CatchArea}, $input{Capacity}, $input{Area});

    my $upstr	= $$ROW[$$header{$input{CatchArea}}];			# Upstream area, km2
    my $capcty	= $$ROW[$$header{$input{Capacity}}] * 1e6;		# Capacity, m3
    my $sf_area	= $$ROW[$$header{$input{Area}}]     * 1e6;		# Surface area, m2
    my $dateStr	= julian_day($$ROW[$$header{$input{StartYear}}],7,1);	# Start date
    my $dateEnd	= defined $input{EndYear} && $$ROW[$$header{$input{EndYear}}] > 0 ?
       julian_day($$ROW[$$header{$input{EndYear}}],1,1) : 2816788;	# End date. Default (3000-01-01)
    my $purpose	= $$ROW[$$header{$input{Purpose}}] || 'Undefined';	# Perpose/Use of the dam
    if($capcty <= $min_capacity) {
      $report[2]++;
      print FILE_D join("\t",@$ROW),"\tBelow min capacity\t$capcty <= $min_capacity\n" if $save;
      next;
    }		### Use dams that are active on the Lock Year for all dates in the run, if requested
    ($dateStr, $dateEnd) = $dateStr<$lock_year && $dateEnd>$lock_year ? (1, 1e8) : (0, 0) if $lock_year;

    my($method,$pars) = dam_method($damID,$purpose,$capcty,\@parTable,\@customPar);	# Water release method:
									   # 0 - Unregulated	3 - Hydropower
				# Check if dates are in temporal bounds	   # 1 - Generic	4 - Irrigation
    if($dateEnd < $startDate || $dateStr > $endDate) {			   # 2 - Flood control	5 - Water Supply
      $report[3]++;
      my $endYear	= !$dateEnd || $dateEnd >= 2816788 ? 'NoEndYear' : $$ROW[$$header{$input{EndYear}}];
      print FILE_D join("\t",@$ROW),"\tOutside model dates\t" .
		"DamYears = [$$ROW[$$header{$input{StartYear}}] .. $endYear]\n" if $save;
      next;
    }		####################################################################
		### Match coordinates to Network by upstream area (search by NxN box)
    my @snappedTo	= ('','','');

			### Case of forced snapping point by the Input
    if (exists $$header{ForceTo_Lon}  && exists $$header{ForceTo_Lat} &&
	 $$ROW[$$header{ForceTo_Lon}] &&  $$ROW[$$header{ForceTo_Lat}]) {
      @coord		= ($$ROW[$$header{ForceTo_Lon}], $$ROW[$$header{ForceTo_Lat}]);	# Lon/Lat
     ($col,$row)	= map(POSIX::floor($_), Geo::GDAL::ApplyGeoTransform($igT,@coord));
      die "ForceTo location for dam ID=\"$damID\" (Lon,Lat)=($coord[0],$coord[1]) is outside of the Network domain.\n" .
	  "Aborting...\n\n"	if ($col < 0 || $col >= $$extent{ncols} ||
				    $row < 0 || $row >= $$extent{nrows} || $up_area($col,$row)->isbad->sum);
      @snappedTo	=(@coord, sprintf("%.1f",$up_area->at($col,$row)), 'Forced by Input');
    }
    else {
			### Case of searched snapping point, if needed
      my $UPSTR = pdl([$upstr, $cell_area($col,$row)])->max;
      if (abs($UPSTR-$up_area($col,$row)) > $search_mtch*$UPSTR) {	### Check if search is needed
	my $radius	= $search_pix->at($col,$row);
	my $box_dim	= $radius*2 + 1;
	my $box		= pdl([$box_dim,$box_dim]);		# Search box of NxN size
	my @search	= abs($up_area->range(pdl([$col,$row])-$radius,$box,'t')-$UPSTR)->flat->minmaximum;
	my $r		= int $search[2]/$box_dim;		### $search[2] is minimum
	my $c		= $search[2] -$r*$box_dim;
	  ($col,$row)	=($col+$c-$radius, $row+$r-$radius);
	  @coord		= Geo::GDAL::ApplyGeoTransform($$extent{gTransform},$col,$row);
	  @snappedTo	=(@coord, sprintf("%.1f",$up_area->at($col,$row)));
			### Check if search is successful
	if($search[0] > $search_mtch*$UPSTR) {			### Allow NN % difference
	  $report[4]++;
	  print FILE_D join("\t",@$ROW),"\tCatchment mismatch\t", sprintf('Searched min Difference is at ' .
	    "Network ColRow(%d,%d) LonLat(%.4f,%.4f) = %.1f km2 must be < 40 %% of DB Catchment = %.1f km2\n",
	    $col, $row, @snappedTo, $UPSTR)	 if $save;
	  next;
    }}}
			### Check/Fix zero capacity and/or surface area (many in NID)
    if    (!$capcty && !$sf_area) { ($capcty,   $sf_area) = (1e4, 1e4);	}
    elsif (!$capcty &&  $sf_area) {  $capcty  = $sf_area;		}
    elsif ( $capcty && !$sf_area) {  $sf_area = $capcty ;		}

			### Yield outflow data if present
	# Check if multiple observed outflow dams are in the same cell.
	# If Yes, do not allow use of observed outflow data
    if (exists($outflow{$damID}) && exists($resStack{$$cell_ind{"$col\_$row"}})) {
      foreach my $i (@{$resStack{$$cell_ind{"$col\_$row"}}}) {
        if ($reservoir[$i][22]) {
	    $reservoir[$i][22] = 0;
	    delete $outflow{$damID};
    } } }
    my $qout	= exists $outflow{$damID} ? $outflow{$damID} : 0;
       $method	= 7 if $qout;

			### All together
    push @reservoir,[			#     Index in:  @reservoir	$resv_data	$resv_pars
	$damID,				# ID		- [0]
	@coord,				# (lon,lat))	- [1,2]
	$col,$row,			# (col,row)	- [3,4]		[0,1]
	$dateStr,$dateEnd,		# Dates		- [5,6]		[2,3]
	$method,			# Method	- [7]		[4]
	$capcty,			# Capacity	- [8]				[0]
	$sf_area,			# Srfc area	- [9]				[1]
	@$pars, 			# Mthd pars	- [10..20]			[2..12]
	0,				# Outflow	- [21]				[13]
	$qout];				# Outflow Data	- [22]

    # resStack HASH: resStack={ "Cell_ID" => [ resID1, resID2, ...] }
    push @{$resStack{$$cell_ind{"$col\_$row"}}}, $#reservoir;

    $report[7]++;			# Total reservoirs
    $dateStr <= $startDate ? $report[8]++ : $report[9]++; # Start/Future
    $purpose_count[$method]++;		# Reservoir purpose count

    print FILE_F join("\t",@$ROW,'',@coord,@snappedTo),"\n" if $save;	# Print filtered dam DB entry used in this run
  }
  if ($save) {
    close FILE_F; close FILE_U; close FILE_D;
    save_damOverlap($damSubsetFile_F, $$header{$input{ID}}, scalar(keys(%$header)), \@reservoir, \%resStack);
  }
			### Check if reservoir data has been initialized
  die "\nNo Reservoirs data has been initialized. Tip: Remove \"Reservoirs\" entry or fix DB.\nAborting...\n\n"
	unless @reservoir;

			### Build Reservoir Stack
  pop @resStack if %resStack;		# remove row = -1 (we'll fill then add again to the end)
  foreach my $cell (sort {$a <=> $b} keys %resStack) {
    my @arr	= @{$resStack{$cell}};
    my $size	= scalar @arr;
    # Array resStack: [Cell_Index,Num_reservoirs_here,[resID1,resID2,...]]
    push @resStack,$cell,$size,@arr;

    if ($size > 1) {
      $report[5]++;				# Overlapping cells
      $report[6] = $size if $report[6] < $size;	# Maximum overlap
    }
  }
  push @resStack,-1;			# Add row = -1 at the end of the stack
  splice  @report,7,0,@purpose_count;	# Add dam purpose count to the report
  unshift @report, $input{smReservoirFile} ? 'Yes' : 'No';
  unshift @report, $input{Lock_Year} ? $input{Lock_Year} : 'No';

			### Summary Report
  printf "   Reservoir initialization summary:
        Use Lock Year-          %s
	Small irr. reservoirs-	%s
	Outside spatial domain-	%d
	Incomplete data-	%d
	Below min capacity-	%d
	Outside model dates-	%d
	Catchment mismatch-	%d
	Overlapping cells-	%d
	Maximum overlap-	%d
	Purpose: Spillway-	%d
		 Generic-	%d
		 Flood Control-	%d
		 Hydroelectric-	%d
		 Irrigation-	%d
		 Water Supply-	%d
		 Custom-	%d
		 Obs.Discharge-	%d
	Total, (start/future)-	%d (%d / %d)
	Total capacity =	%.2f km3\n\n", @report, List::Util::sum(map($$_[8],@reservoir))*1e-9;

  my $resv_data	= long  ([map([@$_[3.. 7]], @reservoir)]);	# Reservoir Col, Row, Dates
  my $resv_pars	= double([map([@$_[8..21]], @reservoir)]);	# Rule parameters

  save_ReservoirStats(\@reservoir, $extent, $endDate, $$runIO{spool}.fileBaseName($input{File})) unless $$runIO{noOutput};

  return \@reservoir, [$resv_data,$resv_pars,long(\@resStack)], \%input;
}

	#####################################################
	### Dam Use Method and Operating Parameters
sub dam_method {
  my ($id,$use,$cap,$pars,$cusPars) = @_;
		### Custom dam operating parameters by dam ID
  my ($row)	= grep {$$cusPars[$_][0] eq $id} 0..$#$cusPars;
  if (defined $row) {
    my @params	= @{$$cusPars[$row]}[2..7];
    my $d	= d_root_solver([@params]);		# "d" parameter
    my $a	= a_dam_equaton([@params,$d]);		# "a" parameter
    my $b	= b_dam_equaton([@params,$d,$a]);	# "b" parameter
    my $B	= B_dam_equaton([@params,$d,$a,$b]);	# "B" parameter
    my $IrrScl	= $$cusPars[$row][8];		# "Irr_Scale" parameter
       $IrrScl	= 1 unless isNumber($IrrScl);
    return $use =~ m/Flood/i		? 2 :		# Method ID
	   $use =~ m/Hydroelectric/i	? 3 :
	   $use =~ m/Irrigation/i	? 4 :
	   $use =~ m/Supply/i		? 5 :
	   die("Custom parameters cannot be used for dam ID \"$id\". Unapplicable dam purpose \"$use\". Aborting...\n\n"),
	[@params,$d,$a,$b,$B,$IrrScl];			# Parameters
  }
		### Dam operating parameters by dam use method
					# Water release method (in increasing PRIORITY ORDER):
  my $method = 0 if	$cap > $$pars[0][0] && $cap <= $$pars[0][1];		# 0 - Unregulated
     $method = 1 if	$cap > $$pars[1][0];					# 1 - Generic
     $method = 2 if $use =~ m/Flood/i		&& $cap > $$pars[2][0];		# 2 - Flood Control
     $method = 3 if $use =~ m/Hydroelectric/i	&& $cap > $$pars[3][0];		# 3 - Hydroelectric
     $method = 4 if $use =~ m/Irrigation/i	&& $cap > $$pars[4][0];		# 4 - Irrigation
     $method = 5 if $use =~ m/Supply/i		&& $cap > $$pars[5][0];		# 5 - Water Supply
										# 6 - N/A: reserved for future use
										# 7 - Observed discharge
     $method = 0 if $use =~ m/Lake/i;	# Lakes are always use spill method

  return $method, [@{$$pars[$method]}[2..$#{$$pars[$method]}]];
}

	#####################################################
	### Equation for "a" parameter for managed dams
sub a_dam_equaton {
  my $par	= shift;
  return (($$par[5] - $$par[2]) / log(1+$$par[0]*$$par[4]));
}

	#####################################################
	### Equation for "b" parameter for managed dams
sub b_dam_equaton {
  my $par	= shift;
  return (($$par[3] - $$par[5]) / ((1-$$par[4]+$$par[6])**$$par[1] - $$par[6]**$$par[1]));
}

	#####################################################
	### Equation for "B" parameter for managed dams
sub B_dam_equaton {
  my $par	= shift;
  return ($$par[5] - $$par[8]*$$par[6]**$$par[1]);
}

	###################################################################
	### Equation for "d" parameter for managed dams (implicit equation)
sub d_root_solver {
  my $par	= shift;
  my $a		= a_dam_equaton($par);
  my $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm');
  my $tmp_file	= $tmp_obj->filename.'.err';	# Do not combine with the line above otherwise it will fail in forks

  no warnings qw(redefine);
    eval "sub my_func {
      my(\$x)  = \@_;
      my \$y   = zeroes(\$x);
      my \$x0  = \$x->slice(0);
      my \$y0  = \$y->slice(0);
	 \$y0 .= \$x0**(\$\$par[1]-1) / (\$x0**\$\$par[1] - (1-\$\$par[4]+\$x0)**\$\$par[1]) + \$a*\$\$par[0]/(1+\$\$par[0]*\$\$par[4]) / \$\$par[1] / (\$\$par[3]-\$\$par[5]);
      return \$y; }";
  use warnings qw(redefine);

  for (my $i=5; $i<=50; $i+=5) {
    open STDERR, ">$tmp_file";
      my $init	= pdl($$par[1] / $i);
      my $d	= gslmroot_fsolver($init, \&my_func, {Method => 0, EpsAbs => 1e-6});
		### Check for error in root solver conversion
      open ERRFILE, "$tmp_file";
	my @err_lines = grep  m/Final status/i, <ERRFILE>;
      close ERRFILE;	unlink "$tmp_file";
    open STDERR, ">&OLDERR";

    return $d unless @err_lines;
  }
  die "Cannot solve \"d_root_solver\". Aborting...\n\n";
}

#######################################################################

sub save_damOverlap
{
  my ($file, $col_ID, $col_Ov, $reservoir, $stack)	= @_;
  my  %overlap;
		### Find overlap dams
  foreach my $cell (sort {$a <=> $b} keys %$stack) {
    my @arr	= @{$$stack{$cell}};
    my $size	= scalar @arr;
    next if $size < 2;			# Overlap is where more than one dam in a cell

    my @IDs	= map $$reservoir[$_][0], @arr;
    my $method	= List::Util::max(map($$reservoir[$_][7], @arr));	# Find priority dam method
    my $str	= join(':', $method,	@IDs);				# Make dam overlap string
    map $overlap{$_} = $str,		@IDs;
  }
		### Save dam overlap information to the file
  my ($hdr, @dams)	= read_table($file);
  my  $header		= join "\t", sort({$$hdr{$a} <=> $$hdr{$b}} keys(%$hdr));
  open (FILE,">$file") or die "Couldn't open $file, $!";
    print FILE $header,"\n";
    foreach my $dam (@dams) {
      $$dam[$col_Ov] = $overlap{$$dam[$col_ID]} if exists $overlap{$$dam[$col_ID]};
      print FILE join("\t",@$dam),"\n";
    }
  close FILE;
}

#######################################################################

sub save_ReservoirStats
{
  my ($damData, $extent, $jdate, $file)	= @_;
  my  @date = inverse_julian_day($jdate);
  $file    .= "_$date[0]";
  return if -e $file.'_count.tif';

  my ($rMethod, $rCount, $rCap, $rArea) = cumulative_Reservoir($damData, $jdate,
	zeroes($$extent{ncols},$$extent{nrows})->copybad($$extent{mask}));
  write_tif($extent, 'Int16',   $file.'_count.tif',    $rCount);
  write_tif($extent, 'Float32', $file.'_capacity.tif', $rCap);
  write_tif($extent, 'Float32', $file.'_area.tif',     $rArea);
  write_tif($extent, 'Int16',   $file.'_method.tif',   $rMethod);
}

#######################################################################

sub cumulative_Reservoir
	### Cumulative reservoir count, area, capacity, and method
{
  my ($damData, $jdate, $zeroes)	= @_;
  my ($rMethod, $rCount, $rCap, $rArea)	= map $zeroes->copy, 1..4;
      $rMethod -= 1;

  foreach my $res (@$damData) {
    if ($jdate >= $$res[5] && $jdate < $$res[6]) {
      $rCount (@$res[3,4]) += 1;
      $rMethod(@$res[3,4]) .= $$res[7] if $rMethod(@$res[3,4]) < $$res[7];
      $rCap   (@$res[3,4]) += $$res[8];
      $rArea  (@$res[3,4]) += $$res[9];
    }
  }
  return $rMethod, $rCount, $rCap, $rArea;
}

#######################################################################

sub add_DIN_WWPT
	### Add Dissolved Inorganic Nitrogen (DIN) from Waste Water Treatment Plants (WWTP)
{
  my ($runSet, $param, $up_area, $search_pix) = @_;
  my ($runIO,  $junk,  $extent)= @$runSet;
  my ($input, $removal)	= ($up_area*0, $up_area*0);

  my ($hdr,@table)	= read_table($$param{WWTP_File});
  my  $N_Load		= set_default($$param{perCapitaNLoad}, 0.019);	# kg TN per person per day
  my  %N_Removal	= ( 1 => 0.1, 2 => 0.5, 3=> 0.8 );		# Removal by technology level
  my  @report	= ((0) x 3);			# Summary report

  foreach my $ROW (@table) {
			### Data
    my @coord	=($$ROW[$$hdr{$$param{Lon}}], $$ROW[$$hdr{$$param{Lat}}]);	# Lon/Lat
    my $popYear	= $$ROW[$$hdr{$$param{popYear}}];				# Served population in reference year
    my $t_level	= $$ROW[$$hdr{$$param{t_level}}];				# WWTP treatment technology level
    if ($popYear < 0) {$report[1]++; next}
				# Check if coords are in bounds
    my ($col,$row) = map(POSIX::floor($_), Geo::GDAL::ApplyGeoTransform($$extent{igTransform},@coord));
    if ($col < 0 || $col >= $$extent{ncols} ||
	$row < 0 || $row >= $$extent{nrows} || $up_area($col,$row)->isbad->sum)
    {$report[0]++; next}	### Report-0: Out of bounds

			### Search for largest upstream area within search distance
    my $radius	= $search_pix->at($col,$row);
    my $box_dim	= $radius*2 + 1;
    my $box	= pdl([$box_dim,$box_dim]);		# Search box of NxN size
    my @search	= $up_area->range(pdl([$col,$row])-$radius,$box,'t')->flat->minmaximum;
    my $r	= int $search[3]/$box_dim;		# $search[3] is maximum
    my $c	= $search[3] -$r*$box_dim;
    my @colRow	= ($col+$c-$radius, $row+$r-$radius);

			### Build DIN data
    my $in		 = $popYear * $N_Load;		# Kg per pixel
    $input  (@colRow)	+= $in;
    $removal(@colRow)	+= $in * $N_Removal{$t_level};

    $report[2]++;			# Total of WWTP plants
  }
			### Summary Report
  printf "   Waste Water Treatment Plants summary:
	Outside spatial domain-	%d
	Invalid WWTP data-	%d
	Total WWTP plants used-	%d\n\n", @report;

  return $input, $removal, $input-$removal;		# Kg per pixel
}

#######################################################################

sub springs_init

{
  my($runSet, $aqf_type, $aqf_data)	= @_;
  my(%spr_data, %snk_data);

	### Initializations
  return \%spr_data, \%snk_data, 0;	# Removed from public domain
}

#######################################################################

sub usgs_init

{
	### Initializations
  return undef;		# Removed from public domain
}

#######################################################################

sub update_usgs
		### Update USGS data hash for this time step
{
			# Removed from public domain
}

#######################################################################

sub rehash_stack
{
			# Removed from public domain
}

#######################################################################

sub make_stack
		### Create data stack for passing to routing
{
  my($data, $data_keys, $cell_ind)	= @_;
  my @dataArray	= ([map 0,@$data_keys]);
  my @dataStack	= (-1);

			# Removed from public domain

  return double(\@dataArray), long(\@dataStack);
}

#######################################################################

sub gl_scale
		### Calculate glacier melt scaling factor due to difference resolutions of source and Network grids
{
  my ($src_dataset, $hr_dataset, $glArea, $cell_area, $date, $runSet) = @_;
  my ($runIO, $meta, $extent)	= @$runSet;
  my  $tmp_obj	= new File::Temp(TEMPLATE => 'file'.('X' x 10), DIR => '/dev/shm');
  my  $tmp_file	= $tmp_obj->filename.'.vrt';	# Do not combine with the line above otherwise it will fail in forks

	### Read average glacier area fraction in the coarse grid cells
  open STDERR, ">&NEWERR";
    my $file_in	= $src_dataset->{fileList}[0][0];
    my $junk	= `$$PATH{gdal_translate} -of VRT -b 1 $file_in $tmp_file`;
  open STDERR, ">&OLDERR";

	### Check grid resolutions (if same, no need for scaling)
  my($size,$gT)	= get_geo_transform($tmp_file);
  return 1 if	abs($$size[0] - $$extent{cellsizeX}) < 0.0001 * abs($$size[0]) &&
		abs($$size[1] - $$extent{cellsizeY}) < 0.0001 * abs($$size[1]);

	### Calculate glacier melt scaling
  my $spool_file = $$runIO{spool} . $src_dataset->{ID} . '.glAreaFrac.tif';
  unless (-e $spool_file) {
    my $src_extent = get_extent($tmp_file);
    my $src_glArea = cell_area(lonLat($src_extent), $src_extent) * # Use first TS layer of the simulation
	read_dateLayer($hr_dataset,$date,$src_extent,0,{DATE_SEARCH_OPT=>{YR_UP=>1}, RESAMPLE=>5})->clip(0, 0.99);
		# Save it to temporary file
    write_tif($src_extent, 'Float32', $spool_file, $src_glArea);
  }
	### Calculate the scaling ratio
  my $src_glArea= read_GDAL($extent,$meta,0,$spool_file,1,0);
  my $scale	= condition_slice($src_glArea > 0, $cell_area * $glArea / $src_glArea, 0);

  unlink $tmp_file;
  return $scale;
}

#######################################################################
#####	Groundwater Flow block of functions
#######################################################################

sub aquifer_init

{
  return (0,{},{gwtUseC=>0},0);
	### Read Aquifer data		# Removed from public domain
}

#######################################################################
#######################################################################

sub condition
	### Conditional operator (a?b:c) implementation for PDL objects
	### Use 'sub condition_slice' if some NAN or BAD values can result
{
  return ($_[0]!=0)*$_[1] + ($_[0]==0)*$_[2];
}

#######################################################################

sub condition_slice
	### Conditional operator (a?b:c) implementation for PDL objects using slices.
	### Useed for cases when NAN or BAD values can result in unused A : B sides.
{
  my $left	= isNumber($_[1]) ? $_[1] * ones($_[0]->dims) : $_[1];
  my $rght	= isNumber($_[2]) ? $_[2] * ones($_[0]->dims) : $_[2];
  my $result	= zeroes(  $left->type, $_[0]->dims)->copybad($_[0]);
  my $true	= whichND( $_[0] != 0);
  my $false	= whichND( $_[0] == 0);

  $result->indexND($true) .= $left->indexND($true);
  $result->indexND($false).= $rght->indexND($false);

  return $result;
}

########################################################################

sub set_to_zero
{
  if (ref $_[0] eq 'PDL') { $_[0] .= 0; }		# This significantly improves performance
  else			  { $_[0]  = $_[1]->copy; }	# as compared to creating a copy of zeros
}

########################################################################

sub checkZero
	### Return TRUE if check for all elements in the piddle to be zero fails
{
  my $pdl = shift;
  return 0 unless defined $pdl;

  my @list = $pdl->uniq->list;
  return scalar(@list) == 1 && !$list[0] ? 0 : 1;
}

######################################################################

sub checkOnes
	### Return TRUE if check for all elements in the piddle to be one fails
{
  my $pdl = shift;
  return 0 unless defined $pdl;

  my @list = $pdl->uniq->list;
  return scalar(@list) == 1 && $list[0] == 1 ? 0 : 1;
}

######################################################################

sub hash_to_arr
{
  my $hash = shift();
  return ref($hash) ne 'HASH' ? $hash : [map hash_to_arr($$hash{$_}), sort keys %$hash];
}

sub hash_to_keys
{
  my %hash = %{shift()};
  my @keys = ([sort keys %hash]);
  my $ref  = $hash{$keys[-1][-1]};
	### Make array of sorted keys
  while (ref($ref) eq 'HASH') {
    push @keys, [sort keys %{$ref}];
    $ref = $$ref{$keys[-1][-1]};
  }
  return \@keys;
}

sub arr_to_hash
{
  my ($arr, $keys, $hash, $row) = @_;
#     $hash = {} unless defined $hash;	# Uncomment if want overloading this function
#     $row  = -1 unless defined $row;	# Uncomment if want overloading this function
      $row++;
  foreach my $i (0 .. $#$arr) {
    if (ref($$arr[$i])) {
	   $$hash{$$keys[$row][$i]} = {} unless exists $$hash{$$keys[$row][$i]};
	   arr_to_hash($$arr[$i],$keys,$$hash{$$keys[$row][$i]},$row); }
    else { $$hash{$$keys[$row][$i]} = $$arr[$i]; }
  } return  $hash;
}

######################################################################

sub change_meta
	### Changes WBM output metadata for daily and monthly time series
{
  my ($flag,$meta) = (shift,dclone(shift));

  die "Unknown Time Series to run. Aborting...\n\n"
	unless $$meta{Time_Series} =~ m/^(daily|monthly)/i;

		### Daily TS
  if ($flag == 0 && $$meta{Time_Series} =~ s/monthly$/daily/i) {
    $$meta{Start_Date}	=~ s/00$/01/;
    $$meta{End_Date}	=~ s/00$/days_in(split(m%-%,substr($$meta{End_Date},0,7)))/e;
  }		### Monthly TS
  if ($flag == 1 && $$meta{Time_Series} =~ s/daily$/monthly/i) {
    $$meta{Start_Date}	=~ s/01$/00/;
    $$meta{End_Date}	=~ s/\d{2}$/00/;
  }

  return $meta;
}

#######################################################################

sub add_spinup_dates
	### Adds spinup loops to the date lists
{
  my ($j_date_list, $date_list, $runIO, $metaDaily) = @_;
  return 0 unless $$runIO{Spinup}{Loops};

	### Make spinup dates for one cycle
  my @start_SP	= split(m/-/,$$runIO{Spinup}{Start}); $start_SP[2] = 1;
  my @end_SP  	= split(m/-/,$$runIO{Spinup}{End});   $end_SP[2]   = days_in(@end_SP[0,1]);

  my $start_date= sprintf "%04d-%02d-%02d",@start_SP;		# Daily
  my $end_date	= sprintf "%04d-%02d-%02d",@end_SP;		# Daily

  my ($j_date_SP,$date_SP) = make_date_list($start_date,$end_date,
	{'Time_Series'=>$$metaDaily{Time_Series},'Processing'=>'calendar='.$$runIO{calendar}});
  my $n_SP	= scalar @$date_SP;

	### Add spinup cycles to the date list
  map unshift(@$j_date_list, @$j_date_SP),1..$$runIO{Spinup}{Loops};	# Daily
  map unshift(  @$date_list,   @$date_SP),1..$$runIO{Spinup}{Loops};

  return $n_SP;
}

#######################################################################

sub readState
	### It makes it easer to add new vars to be saved to the state files
	### at the end of the list without rebuilding the existing state files
{
  my  @vars  = readflex(shift());
  map $_[$_] = $vars[$_], 0..$#vars;
}

#######################################################################

sub fileBaseName
{
  my $file = shift;
     $file =~ s/.+:(.+):.+/$1/;
     $file = basename($file);
     $file =~ s/\.\w+$//;
  return $file;
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

sub post_processing
	### WBM output post processing script
{
  my ($runIO, $rffAttr, $path, $list) = @_;
     $$runIO{TA_depth}= $$rffAttr{Time_Series} =~ m/daily\{/ ? [1,0,0] : [0,0,0]; # Flag for runtime temporal aggregations
     $$runIO{agg_vars}= $list;
  return 0 unless $$runIO{PostProcessing};

	### Check this function input
  my %ppParam = read_param_str($runIO, 'PostProcessing');
  die "\nPP_Perl_path is missing or wrong. Aborting...\n\n" unless $$path{PP_Perl_path} && -e $$path{PP_Perl_path};
  die "\nPostProcessing TA_depth must be (day|month|year). Aborting...\n\n\n"
	if defined($ppParam{TA_depth}) && lc($ppParam{TA_depth}) !~ m/^(day|month|year)$/;

	### Read post processing init parameters
     $ppParam{Runtime_Aggregation} = set_default($ppParam{Runtime_Aggregation}, 1);
     $ppParam{TemporalAgg_flag}	= $ppParam{Temporal_Aggregation}	? 1 : 0;
     $ppParam{SpatialAgg_flag}	= $ppParam{Polygon_Aggregation}		? 1 : 0;
     $ppParam{hourly}		= $$rffAttr{Time_Series} =~ m/daily\{/	? 1 : 0;
     $ppParam{monthly}		= $$runIO{Output_TS} eq 'monthly'	? 1 : 0;
     $ppParam{NC3}		=!$$runIO{NC4}				? 1 : 0;
     $ppParam{Trim}		= $ppParam{Trim}			? 1 : 0;
     $ppParam{Var_list}		= $ppParam{Var_list}	? [grep(m/\w+/, split(m/\s+/, $ppParam{Var_list}))] : 0;
     $ppParam{TA_depth}		= $ppParam{TA_depth}	?  lc($ppParam{TA_depth}) : 'year';
     $$runIO {TA_depth}[1]	= $ppParam{Runtime_Aggregation} && $ppParam{TA_depth} =~ m/year|month/	? 1 : 0;
     $$runIO {TA_depth}[2]	= $ppParam{Runtime_Aggregation} && $ppParam{TA_depth} =~ m/year/	? 1 : 0;
     $$runIO {TA_depth}[0,1]	= (0,0) if $$rffAttr{Time_Series} =~ m/monthly/;
     $ppParam{Clim_flag}	= int set_default($ppParam{Climatology},  1);
  my $forks			= int set_default($ppParam{Forks},	  8);
  die "\nNumber of forks in post-processing cannot be less than one. Aborting...\n\n" if $forks < 1;

	### Make list of Polygon Aggregation IDs
  my ($PA_ID_list, $tr) = ('','');
  if ($ppParam{SpatialAgg_flag}) {
    die "\"IDs\" key in the Polygon_Aggregation block of the Post Processing must be present...\n"
		unless $ppParam{Polygon_Aggregation}{IDs};
    while ($ppParam{Polygon_Aggregation}{IDs} =~ s/^(\S+)\s*//) {
      my $attrib   = attrib($1,[$$runIO{Input_MT},$$runIO{Output_MT}]);
      $PA_ID_list .= ($PA_ID_list ? ' ' : '') . $$attrib{Code_Name};
      if ($$attrib{Processing} =~ m/polygon/i) {
	die "\"Rasterization_Res\" key in the Polygon_Aggregation block of the Post Processing must be present \n".
	    "\tsince you are using vector polygons for the aggregation by \"$$attrib{Code_Name}\"...\n"
		unless	$ppParam{Polygon_Aggregation}{Rasterization_Res};
	$tr = '-tr ' .	$ppParam{Polygon_Aggregation}{Rasterization_Res};
      }
    }
    $ppParam{Polygon_Aggregation}{IDs} = $PA_ID_list;
  }

	### Make list of Data Cube variable IDs
  $$runIO{agg_vars} = [];
  my $var_list;
  foreach my $var (@$list) {
    next if  $var =~ m/Msk_/	&& !($$runIO{Runoff_mask}{PostProcessing});
    next if  $ppParam{Var_list}	&& !($var  ~~ @{$ppParam{Var_list}});
   (my $data_cube = $$rffAttr{Data_Cube})  =~ s/runoff$/$var/;
    $var_list	 .= "\t$data_cube\n";
    push @{$$runIO{agg_vars}}, $var;
  }
  die "\nNo requested variables are found for post-processing. Aborting...\n\n" unless $var_list;

	### Make list of multi-year aggregation dates
  my  $year_list = '';
  if ($ppParam{TA_multi_year_start} && $ppParam{TA_multi_year_timestep}) {
    my ($MY_yr,   $MY_step) = ($ppParam{TA_multi_year_start}, $ppParam{TA_multi_year_timestep});
    my ($first_yr,$last_yr) = map substr($_,0,4), $$rffAttr{Start_Date},$$rffAttr{End_Date};
    my  $last_day = $$runIO{calendar} == 360 ? 30 : 31;
        $last_yr-- unless $$rffAttr{End_Date} =~ m/-12-($last_day|00)/;
    die "Aggregation multi-year start is outside of the run dates...\n" if $MY_yr < $first_yr || $MY_yr > $last_yr;

    for (my $yr=$MY_yr; $yr<=$last_yr-$MY_step; $yr+=$MY_step) {
      $year_list .= sprintf "\t[%d,%d],\n", $yr,$yr+$MY_step-1;
    }
    $year_list	=~ s/,\n$/\n/;
  }

	### Make Perl script to do post processing
  my $script	= <<END;
#!/usr/bin/perl

#######################################################################
#
#	WBM output data post processing:
#	Temporal and spatial aggregation. Adopted from data_cube.pl
#
#	Written by Dr. A. Prusevich (alex.proussevitch\@unh.edu)
#
#	October 2016	Adopted from stand alone script
#	April 2020	Updated with var list, TA depth, etc.
#	July 2020	Updated with pre-creating output directories
#
#######################################################################

use strict;
use File::Path;
use Parallel::ForkManager;
use RIMS;			### WSAG UNH module

my \$perl_path = '$$path{PP_Perl_path}';
my \$forks     = $forks;
my \$pm        = new Parallel::ForkManager(\$forks);	# Parallel Processing object

my \@param = qw(
$var_list);

my \@multi_year_dates = (
$year_list);

#################################################################
#		Options for processing				#
#################################################################

my \$hourly       = $ppParam{hourly};			# Set if the source dataset is hourly ts
my \$monthly      = $ppParam{monthly};			# Set if the source dataset is monthly ts
my \$temporal_agg = $ppParam{TemporalAgg_flag};
my \$spatial_agg  = $ppParam{SpatialAgg_flag};
my \$climatology  = $ppParam{Clim_flag};
my \$TA_depth     ='$ppParam{TA_depth}';		# Temporal aggregation depth
my \$v            = 0 ? '-v'    : '';	# Verbose for aggregations (not for data_cube.pl)
my \$rm           = 0 ? '-rm'   : '';
my \$trim         = $ppParam{Trim} ? '-trim' : '';
my \$nc3          = $ppParam{NC3} ? '-nc3'  : '';
my \$MT           = '$$runIO{Output_MT}';
my \$output_dir   = '$$runIO{Output_dir}';
my \@PA_ID_list   = qw($PA_ID_list);

#################################################################
#		Pre-create output directories
#		to avoid fork conflicts
#################################################################

my   \@dir_list;				if(\$temporal_agg) {
push \@dir_list,\$output_dir.'/daily'		if \$hourly;
push \@dir_list,\$output_dir.'/monthly'		if \$TA_depth !~ m/day/;
push \@dir_list,\$output_dir.'/yearly'		if \$TA_depth !~ m/day|month/;
push \@dir_list,\$output_dir.'/climatology'	if \$climatology; }
push \@dir_list,\$output_dir.'/spatial_agg'	if \$spatial_agg;

foreach my \$dir \(\@dir_list) {
  unless (-e \$dir) { mkpath(\$dir,0,0775) or die "Cannot create-\n\$dir\n"; }
}

#################################################################

my \@suff  = qw/_d _m _y _dc _mc _yc/;
my \$count = 0;
\$pm->run_on_finish(sub{
  my (\$pid,\$exit_code,\$ident,\$exit_signal,\$core_dump,\$data) = \@_;
  printf "Done (\%d)- \$\$data[0]\\n",++\$count;
});
printf "\\nDataCube aggregation started for \%d WBM output variables (\%d forks):\\n\\n",
	scalar(\@param), \$forks;

#################################################################
#		Temporal Aggregation				#
#################################################################

foreach my \$param \(\@param) {
  \$pm->start and next;			### Run fork processes/net/nfs/zero/home/scripts/perl/

  if (\$temporal_agg) {
	# Hourly -> Daily
    system "\$perl_path/temporal_aggregation.pl \$v \$rm \$trim \$nc3 -mt \$MT \$param\\_h \$param\\_d" if \$hourly;

	# Daily -> Monthly
    unless (\$TA_depth eq 'day') {
    system "\$perl_path/temporal_aggregation.pl \$v \$rm \$trim \$nc3 -mt \$MT \$param\\_d \$param\\_m" unless \$monthly;

	# Daily/Monthly -> Yearly
    unless (\$TA_depth eq 'month') {
    my \$suf = \$monthly ? '_m' : '_d';
    system "\$perl_path/temporal_aggregation.pl \$v \$rm \$trim \$nc3 -mt \$MT \$param\$suf \$param\\_y";
    }}
    if (\$climatology) {
	# Daily -> Daily Climatology
      system "\$perl_path/temporal_aggregation.pl \$v -rm \$nc3 -mt \$MT \$param\\_d \$param\\_dc" unless \$monthly;
      next if \$TA_depth eq 'day';

	# Monthly -> Monthly Climatology
      system "\$perl_path/temporal_aggregation.pl \$v -rm \$nc3 -mt \$MT \$param\\_m \$param\\_mc";
      next if \$TA_depth eq 'month';

	# Yearly -> Yearly Climatology (Multi-Year)
      foreach my \$range \(\@multi_year_dates) {
	my \$dates   = sprintf "-sd \%d-00-00 -ed \%d-00-00", \@\$range;
	my \$yc_file = file_pyramid({read_attrib(\$MT, \$param.'_yc', 'Code_Name')}->{File_Path});
       (my \$my_file = \$yc_file) =~ s/(_yc.nc)\$/_\$\$range[0]-\$\$range[1]\$1/;

	system "\$perl_path/temporal_aggregation.pl \$v -rm \$nc3 \$dates -mt \$MT \$param\\_y \$param\\_yc";
	rename \$yc_file, \$my_file;
      }
	# Yearly -> Yearly Climatology
      system "\$perl_path/temporal_aggregation.pl \$v -rm \$nc3 -mt \$MT \$param\\_y \$param\\_yc";
    }
  }
#################################################################
#		Spatial Aggregation				#
#################################################################

  if (\$spatial_agg) {
    foreach my \$suff \(\@suff) {
      next if \$monthly             && (\$suff=~m/_(h|d)\$/);
      next if \$TA_depth eq 'day'   && (\$suff=~m/_(m|y)/);
      next if \$TA_depth eq 'month' && (\$suff=~m/_y/);
      next if !\$temporal_agg && ((\$hourly && \$suff ne '_h') || (!\$hourly && \$suff ne '_d'));
      my \$flag  = \$rm;
      if (\$suff =~ m/c\$/) {
	next unless \$climatology;
	\$flag   = '-rm';
      }

      my \%dataAttr	= read_attrib(\$MT, \$param.\$suff, 'Code_Name');

      foreach my \$pol_ID \(\@PA_ID_list) {
	my \$file_nc  = \"$$runIO{Output_dir}/spatial_agg/\$param\$suff.\$pol_ID.nc\";
	system "\$perl_path/spatial_aggregation.pl \$v \$flag $tr -mt \$MT \$param\$suff \$pol_ID \$file_nc";
	  # Convert NetCDF to scv table
       (my \$file_csv = \$file_nc) =~ s/nc\$/csv/;
	system "\$perl_path/nc_aggr_read.pl \$dataAttr{Var_Name} \$file_nc > \$file_csv";
      }
    }
  }
  \$pm->finish(0,[\$param]);
}
\$pm->wait_all_children;

print "\\nAll Done!\\n\\n";

exit;

END
	### Save post processing Perl script to a file
  open (FILE,">$$runIO{PP_script}") or die "Couldn't open $$runIO{PP_script}, $!";
    print FILE $script;
  close FILE;

  return exists($ppParam{Run}) ? $ppParam{Run} : 0;
}

#######################################################################

sub make_build_spool
	### WBM build spool files script
{
  my ($runIO, $dates) = @_;
  return if $$runIO{SB_script} eq 'None';

  die "\nPP_Perl_path is missing or wrong. Aborting...\n\n" unless $path{PP_Perl_path} && -e $path{PP_Perl_path};
  my $cmd	= "$path{PP_Perl_path}/build_spool.pl";

	### Make build spool command list
  my @list;
  my $spool_list = delete $$runIO{spool_list};		# Must be deleted from runIO for idump to work later
  foreach my $rec (@$spool_list){
      $$rec[1]{PATCH_VALUE}	= $$rec[1]{PATCH_VALUE}->{ID} if ref($$rec[1]{PATCH_VALUE});
    my $date	 = $$rec[0]->climatology ? sprintf("-sd %s -ed %s",$$rec[0]{dateList}[0],$$rec[0]{dateList}[-1]) :
					     "-sd $$dates[0] -ed $$dates[-1]";
       $date	 =~ s/(-\d{2})-\d{2}/$1-00/g	if $$rec[0]->monthly;
       $date	 =~ s/-\d{2}-\d{2}/-00-00/g	if $$rec[0]->yearly;
    my $resample = exists($$rec[1]{RESAMPLE})     ? "-r $$rec[1]{RESAMPLE}"     : '';
    my $patch	 = length($$rec[1]{PATCH_VALUE})  ? "-p $$rec[1]{PATCH_VALUE}"  : '';
    my $PATCH	 = exists($$rec[1]{PPATCH_VALUE}) ?"-pp $$rec[1]{PPATCH_VALUE}" : '';
    my $spl_MT	 = "-spDir $path{spool_dir} -mt $$runIO{Output_MT}";
    push @list, "   \"$cmd -v -f _FORKS_ $date $resample $patch $PATCH $spl_MT $$runIO{Network} $$rec[0]{ID}\"";
  }
  my $list = join ",\n", @list;

	### Make Perl script to do post processing
  my $script	= <<END;
#!/usr/bin/perl

#######################################################################
#
#	WBM spool files processing:
#
#	Written by Dr. A. Prusevich (alex.proussevitch\@unh.edu)
#
#	July 2021	Adopted from a stand alone script
#
#######################################################################

use strict;
use File::Basename;
use Getopt::Long;

#######################################################################
#############   Process and check command line inputs   ###############

my (\$help,\$quiet,\$nc,\$forks) = (0,0,0,8);
			# Get command line options
usage() if !GetOptions( 'h'=>\\\$help, 'q'=>\\\$quiet, 'nc'=>\\\$nc, 'f=i'=>\\\$forks ) or \$help;

#######################################################################
#############   Build Spool   #########################################

my \@list = (
$list
);
map(s/_FORKS_/\$forks/,	\@list);		# Set forks
map(s/ -v / -v  -nc /,	\@list) if \$nc;	# Add nc output,  if needed
map(s/ -v //,		\@list) if \$quiet;	# Remove verbose, if needed
map(s/\\s+/ /g,		\@list);		# Remove extra spaces

print "\\n" unless \$quiet;

foreach my \$cmd \(\@list) {
  system "\$cmd";
}
print "\\nAll Done!\\n\\n" unless \$quiet;

exit;

#######################################################################
######################  Functions  ####################################

sub usage
{
  my \$app_name = basename(\$0);
  print <<EOF;

Usage:
	\$app_name [-h] [-q] [-nc] [-f FORKS]

This code pre-builds spool files for a WBM run.

Options:

h	Display this help.
q	Quiet mode.
f	Number of forks to use. Default is \$forks.
nc	Build additional NetCDF copy of spool binary data.

Example:
\$app_name -q -f 24

EOF
  exit;
}

END
	### Save post processing Perl script to a file
  open (FILE,">$$runIO{SB_script}") or die "Couldn't open $$runIO{SB_script}, $!";
    print FILE $script;
  close FILE;

  return 0;
}

#######################################################################

sub bMarkMap		# Benchmark mapping
{
  my ($level,	$rec, $tag, $txt) = @_;
  return if ref($rec) ne 'HASH';

	### Check for pause locking file
  if ($level < 0) { $$rec{pause} = -e $$rec{dir}.'/bm.lock'; return; }
  if ($$rec{pause}) {
    if ($level == 0) {			# Reset yearly benchmark in lock mode anyway
      $$rec{data}{0}{start}	= Benchmark->new;
      $$rec{data}{0}{tag}	= $tag; }
    return;
  }
  my $end	= Benchmark->new;
  my $tab	= 80;			# Line position of timing string
     $txt	= defined $txt ? " $txt:" : '';

	### Close this and all higher level tags
  my $levMax	= (sort keys %{$$rec{data}})[-1] || 0;
  for (my $l	= $levMax; $l>=$level; $l--) {		  next unless exists $$rec{data}{$l};
    my $str	= "L$l End  : $$rec{data}{$l}{tag}:".($l==$level?$txt:'');
    my $spacer	= "\t" x int(($tab - $l*8 - length($str))/8 + 0.99);	# Make ceil() from int()
    printf {$$rec{FH}} n_tab($l)."$str%s%s\n", $spacer, timestr(timediff($end,$$rec{data}{$l}{start}));
    delete $$rec{data}{$l};
  }
	### Open Level tag
  if ($tag) {				# $tag is also used as a flag to time this level benchmark
    print {$$rec{FH}} n_tab($level)."L$level Start: $tag\n";
    $$rec{data}{$level}{start}	= $end;
    $$rec{data}{$level}{tag}	= $tag;
  }
}
sub n_tab { return "\t" x shift(); }

#######################################################################

sub make_URL		# EarthAtlas URL
{						# Can add var list to make URLs (do we need it?)
  my ($runIO, $meta, $extent)	= @{shift()};	# Presently, the list is hard coded as qw(runoff discharge)
  if ($$extent{projection}	=~ m/epsg:4326/i) {
    my ($hdr, @MT_data)	= read_table($$runIO{Output_MT});
    my $dataDB	= $$runIO{Output_MT};
    my $res	= List::Util::min(	1200/($$extent{xurcorner}-$$extent{xllcorner})*360/256,
					600 /($$extent{yurcorner}-$$extent{yllcorner})*360/256);
    my $zoom	= POSIX::log($res)/POSIX::log(2)*0.995;
    my @center	= (($$extent{xurcorner}+$$extent{xllcorner})/2, ($$extent{yurcorner}+$$extent{yllcorner})/2);
    foreach my $var (qw(runoff discharge)) {
      my $idx	= find_var_in_MT($var, \@MT_data);	next if $idx < 0;	# Find index of $var in MT
      my $dataID= $MT_data[$idx][$$hdr{Code_Name}];
      my $date	= $MT_data[$idx][$$hdr{End_Date}];
      my $url	= "[InternetShortcut]\nURL=https://earthatlas.sr.unh.edu/maps/?center=" . url_encode(
	"[$center[0],$center[1]]") . "&zoom=$zoom&data_ID=$dataID&data_DB=" . url_encode($dataDB) .
	"&data_Date=$date&data_Mthd=nearest&overlays=" . url_encode('["ol_countries","ol_rivers","ol_STN_rivers"]');

	### Save URL to a file
      my $file	= "$$runIO{Output_dir}/EarthAtlas_$var.url";
      open (FILE,">$file") or die "Couldn't open $file, $!";
	print FILE $url;
      close FILE;
  } }
  else { print "EarthAtlas URL cannot be created for this Net projection...\n"; }
}
sub find_var_in_MT {
  my ($var, $MT_data)	= @_;
  my  $run_TS		= substr($$MT_data[0][0],length($$MT_data[0][0])-2);
  my  $idx		= -1;
  foreach my $i (0 .. $#$MT_data) { if ($$MT_data[$i][0] =~ m/$var$run_TS$/) { $idx = $i; last; } }
  return $idx;
}
sub url_encode {	# Taken from here- https://www.perlmonks.org/?node_id=1179436
  my $rv = shift;
  $rv =~ s/([^a-z\d\Q.-_~ \E])/sprintf("%%%2.2X", ord($1))/geix;
  $rv =~ tr/ /+/;
  return $rv;
}

#######################################################################

return 1;
close NEWERR; close OLDERR;

#######################################################################
__END__

# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

RIMS::WBM - Perl extension for RIMS::WBM

=head1 SYNOPSIS

  use RIMS::WBM;

=head1 DESCRIPTION

Stub documentation for RIMS::WBM.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Alexander Prusevich
alex.proussevitch@unh.edu

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2017-2021 by UNH

=cut
