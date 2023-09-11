
#######################################################################
#
#       All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      C University of New Hampshire and Water Systems Analysis Group 2008-2023
#
#######################################################################
#######################################################################
#
#	Some General functions.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	October, 2002
#		Development-
#	Aug 2017	Added- Dataset metadata *.init file processing as alternative to the MT.
#	Oct 2019	Unknown number of changes in the past 2 years
#	Apr 2020	Repacing sub "hour_min" to "jday_hour_min"
#	Aug 2020	Added draw_xy_graph function
#	Apr 2021	Added few calendar functions
#	Jul 2021	Added write_csv prepare_dir functions
#	Aug 2021	Proj4 dependency is removed. Math::VecStat is replaced with List::Util (faster)
#	Apr 2022	Updated make_date_list function for multi-day time series
#	Jun 2022	Switching from GDAL swig to FFI interface. Added band mapping for dates from file
#	Jun 2023	Added check_ascii_file function
#
#	NB 1 - Only significant modifications are indicated in the revision/version history above.
#	NB 2 - Old significant versions are saved in "old_versions/" folder off development path.
#
#	Version-	(YY.M.#)	# Number is zero-based
my $version =		'23.8.0';	# Version. Note- update version variable below
#
#######################################################################

package RIMS;

use v5.8.8;
use warnings;

use Cwd qw/abs_path/;
use CGI qw/header param/;
use Fcntl;
use File::Basename;
use File::Path;
use Geo::GDAL::FFI;
use List::Util;
use Time::JulianDay;
use Time::DaysInMonth;
use POSIX qw/log10 floor ceil/;
use Storable qw/dclone/;
use Sys::Hostname;

require Exporter;

our $VERSION	= $version;
our @ISA	= qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Rims ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw( get_file_path date_now date_time
htm_template read_attrib read_table read_table_hash headerCSV save_img reply ts_name
read_legend draw_error draw_graph draw_xy_graph axis_scale y_scale axis_scale_aaa rgb put_comma
file_pyramid shp_file_list get_netcdf_compression get_column_location load_column timecheck
band_mapping rm_duplicate_options split_date get_calendar calendar360_day calendar360_inverse_day
calendar365_DaysInMonth calendar360_DaysInMonth calendar365_is_leap calendar360_is_leap
make_date_list make_file_list make_date_layers jday_hour_min trim_dates chk_date_list band_count
check_MT_date STD_date check_file_type check_sigma polygon_files dumperror set_default write_csv
prepare_dir crs_to_Obj transform_point transform_points check_ascii_file
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT    = ( @{ $EXPORT_TAGS{'all'} } );

#######################################################################
######################  Utilities  ####################################

my $conf_file	= abs_path(dirname($0)).'/RIMS.conf';		# Try conf file in the user directory
   $conf_file	= '/etc/RIMS.conf' unless -e $conf_file;	# Default conf
my $path	= get_file_path();

use vars qw(*OLDERR);		### A switch to control STDERR output
open OLDERR, ">&STDERR";	*OLDERR if 0; # Prevent spurious warning

no if $] >= 5.017011, warnings => 'experimental::smartmatch';

######################  Functions  ####################################

sub get_file_path
{
  my $file = $conf_file;

  my $hash = eval htm_template($file);
  die "Error in RIMS get_file_path 'eval':\nFile = $file\n   $@" if $@;

  return $hash;
}

#######################################################################

sub date_now
	# Local date string
{
  my $delta = shift;
  $delta = 0 unless defined $delta;

  my @loc_time_0 = localtime(time+$delta);
  $loc_time_0[5] += 1900; $loc_time_0[4]++;
  return sprintf("%04d-%02d-%02d",reverse(@loc_time_0[3..5]));
}

#######################################################################

sub date_time
	# Local date and time
{
  my @loc_time_0 = localtime(time());
  $loc_time_0[5] += 1900; $loc_time_0[4]++;
  return reverse(@loc_time_0[0..5]);
}

#######################################################################

sub htm_template		### Read HTML Template

{
  my $file = shift;

  open (FILE,"<$file") or die "Couldn't open $file, $!";
    my @lines = <FILE>;
  close FILE;
  map s/\r//,@lines;		### Windows second end of the line char

  return join('',@lines);
}

#########################################################################

sub read_attrib         ### Read ascii table (Attributes)

{
  my ($file,$value,$field_name, $options) = @_;

  my $eval_keys	= set_default( $$options{EVAL_KEYS},	undef);
  my $check_att	= set_default( $$options{CHECK_EXISTS},	undef);
  my (@data,%attrib,$col);

  open (FILE,"<$file") or die "Couldn't open $file, $!";

  my $line	= <FILE>;
     $line	=~ s/\r//;	### Windows second end of the line char
  chomp $line;
  my $sep	= $line =~ m/\t/ ? "\t" : ',';
  my $re	= qr/(?:^|$sep)(?:"([^"]*)"|([^$sep]*))/;
  my @field	= split_csv_line($line,$re);
  for ($col=0; $col<=$#field; $col++) {last if $field[$col] eq $field_name}
  die "Field $field_name does not exist in $file\n" if $col == scalar(@field);

  while (my $line = <FILE>)
  {
    $line	=~ s/\r//;	### Windows second end of the line char
    chomp $line;
    @data = split_csv_line($line,$re);
    if (defined $data[$col] && $data[$col] eq $value)
    {
      while ($#data < $#field) {push @data,'';}
      last;
    }
  }
  close FILE;

  return($data[$col] eq $value ? 1 : 0) if $check_att;		### Check if the attribute exists
  die "Attributes for $value are not found in $file or dataset init file is not used. Aborting...\n"
	unless $data[$col] eq $value;

  for (my $i=0; $i<=$#field; $i++)
  {
    if ($eval_keys && $data[$i]=~m/^\s*(\{.+\}|\[.+\])\s*$/) {
      $data[$i] = eval($data[$i]);
      die "\nError in read_attrib 'eval' = $@\nFile = $file\nID   = $value\nKey  = $field[$i]\n\n" if $@;
    }
    $attrib{$field[$i]} = $data[$i];
  }

  return %attrib;
}

#########################################################################

sub read_table                  ### Read ascii table (Attributes)

{
  my $file	= shift;	# Skip some number of lines from the file
  my $options	= shift || 0;	# Options or $skip (backward compartibility)
  my $sep	= shift;	# Separator, e.g. '\s+'
  my $asc	= shift;	# Check file for non-ASCII characters
  my(@data, %field, $records, $skip);

		### Get Options (Added on Jun 6, 2023
  if (ref($options)) {
    $skip  = set_default($$options{SKIP},	0    );	# If $skip is negative: read table without header
    $sep   = set_default($$options{SEPARATOR},	undef);	# Separator, e.g. '\s+'
    $asc   = set_default($$options{CHECK_ASCII},1    );	# Check file for non-ASCII characters
  } else {
    $skip  = $options;
  }
		### Check non-ASCII characters in the file (Added on Jun 6, 2023
  check_ascii_file($file) if $asc;

  open (FILE,"<$file") or die "Couldn't open $file, $!";
    my @lines	= <FILE>;
  close FILE;
  while ($#lines>=0 && $lines[-1] !~ m/\S/) { pop @lines };	# Remove bottom empty lines
  return(\%field,@data) if $#lines < 0;				# Case of empty file
  
		### Skip top lines, if requested
  if ($skip > 0) {
    while ($skip > 0) { shift @lines; $skip--; }
  }
  die "File- $file does not have data records in it...\n" unless scalar(@lines);

  map(s/^ +//,	@lines);	# Trim blanks on the left side of file lines
  map(s/\r//,	@lines);	# Trim Windows second end-of-the-line char
  chomp(	@lines);	# Trim end-of-the-line char

		### Determine the record separator
     $sep = $lines[0] =~ m/\t/ ? "\t" : ',' unless defined $sep;
  my $re  = qr/(?:^|$sep)(?:"([^"]*)"|([^$sep]*))/;

		### Read Header, unless requested do not
  if ($skip == 0) {
    my $line	= shift @lines;
    my @line	= split_csv_line($line,$re);

    for (my $i=0; $i<=$#line; $i++) {
		### Check for duplicate header names
      die "\nDuplicate header name \"$line[$i]\" in the CSV/TXT file-\n$file\n\tAborting...\n\n"
	if exists $field{$line[$i]};
      $field{$line[$i]}	= $i;		# Populate header hash
    }
    $records	= scalar keys %field;
  }

  foreach my $line (@lines) {
    my @line	= split_csv_line($line,$re);

    if ($records) {
      while (scalar(@line) < $records) { push @line,'' }
    }
    push @data, \@line;
  }

  return \%field,@data;
}

sub split_csv_line
{
  my ($line,$re) = @_;
  my @cells;

  while($line =~ /$re/g) {
    my $value = defined $1 ? $1 : $2;
    push @cells, (defined $value ? $value : '');
  }
  return @cells;
}

sub headerCSV
{
  my ($hash, $option)	= @_;
		### Options are: STRING ARRAY REFERENCE
  my $ouput	= defined($$option{OUTPUT}) ? $$option{OUTPUT} : 'STRING';

  my @header	= sort { $$hash{$a} <=> $$hash{$b} } keys %$hash;
  return $ouput =~ m/^ARRAY$/i ? @header : $ouput =~ m/^REFERENCE$/i ? \@header : join("\t",@header);
}

#######################################################################

sub read_table_hash
{
  my ($file,$id)	= (shift, shift);
  my $skip		=  shift || 0;		# If $skip is negative: read table without header
  my $sep		=  shift;		# Separator, e.g. '\s+'
  my ($hdr,@data)	= read_table($file, $skip, $sep);
  my %hash;

  my $cCol = delete $$hdr{$id};
  foreach my $row (@data) {
    $hash{$$row[$cCol]}	= {map(($_ => $$row[$$hdr{$_}]), keys(%$hdr))};
  }

  return %hash;
}

#######################################################################

sub save_img

{
  my ($file,$img) = @_;
  open (FILE,">$file") or die "Couldn't open $file, $!";
    binmode FILE;
    print FILE $img;
  close FILE;
}

#######################################################################

sub reply

{
  my $image = shift;

  print CGI::header(-type=>'image/png');
  binmode STDOUT;
  print STDOUT $image;

  exit;
}

#########################################################################

sub write_csv
{
  my ($file, $header, $table) = @_;

  prepare_dir( $file );
  open (FILE,">$file") or die "Couldn't open $file, $!";
    print FILE join("\t",@$header),"\n";
    foreach my $row (@$table) {
      print FILE join("\t",@$row),"\n";
    }
  close FILE;
}

#######################################################################

sub prepare_dir
{
  my $dir_out = dirname(shift());
  unless (-e $dir_out) {
    mkpath($dir_out,0,0775) or die "Cannot make directory...\n$dir_out\n";
  }
}

#######################################################################

sub ts_name

{
  my $ts = ucfirst(shift);

  $ts =~ s/Daily\{\d+\}/Hourly/;
  $ts =~ s/\W.+//;
  $ts =~ s/_clim/ Climatology/;
  return $ts;
}

#######################################################################

sub read_legend

{
  ######### Read input data  #########
  my $file = shift;

  ######### Read Legend  #########

  if (open (FILE,"<$file"))
  {
    my @data;
    my $line = <FILE>;				### Skip header
    while (<FILE>)
    {
      s/\r//;	### Windows second end of the line char
      chomp;
      push @data,[split m/\t/];
    }
    close FILE;
    return @data;
  }

  $file =~ m/legends\/(\w+)\.txt/;
  my $image = draw_error("Legend for $1 is not found...");
  reply($image);
  exit;
}

#######################################################################

sub draw_error

{
							# Read arguments
  my $message = shift;

  ######### End of input data  #######
						# Create image objects
  my @image_size = (640,480);
  my $img = new GD::Image(@image_size);
						# Create colors
  my $white = $img->colorAllocate(255,255,255);
  my $grey = $img->colorAllocate(115,115,115);
  my $red = $img->colorAllocate(255,100,0);

  my $brush_r = new GD::Image(2,2);		# Create a Red  Brush
  $brush_r->colorAllocate(255,100,0);


						# Draw the Message
  $img->setBrush($brush_r);
  my @line = split m/\n/,$message;
  for (my $i=0; $i<=$#line; $i++)
  {
    $img->string(GD::gdGiantFont,$image_size[0]/2-4*length($line[$i]),$image_size[0]/3+20*$i,$line[$i],GD::gdBrushed);
  }

  reply($img->png);
}

#######################################################################
##################      Graphing Functions       ######################
#######################################################################

sub y_scale

{
  my ($min,$max,$scale,$lock) = @_;
  return($max-$min,$min,$max) if $lock;

  my @y_scale = ($scale*POSIX::floor($min/$scale),$scale*POSIX::ceil($max/$scale));
  return $y_scale[1]-$y_scale[0],@y_scale;
}

#######################################################################


sub axis_scale

{
  my ($min_y,$max_y,$lock) = @_;
      $lock = 0 unless defined $lock;
  my ($scale,$y_range,@y_scale) = (1,1,0,1);
  my ($count,@fraction) = (0,0.5,0.8,0.5);


  if ($max_y == $min_y)
  {
    if    ($max_y > 0)	{$min_y=0}
    elsif ($max_y == 0)	{$max_y=1}
    else		{$max_y=0}
  }
							# Find Y-Scale
  $scale = 10**(POSIX::floor(POSIX::log10($max_y-$min_y)));
  ($y_range,@y_scale) = y_scale($min_y,$max_y,$scale,$lock);
  if ($y_range > 10*$scale)
  {
    $scale *= 10;
    ($y_range,@y_scale) = y_scale($min_y,$max_y,$scale,$lock);
  }
					### The factor used to be 0.2 (0.1 seems better(?))
  while ($count<3 &&
      (($min_y > $y_scale[0]+$y_range*0.1) || ($max_y < $y_scale[1]-$y_range*0.1)))
  {
    $scale *= $fraction[$count++];
    ($y_range,@y_scale) = y_scale($min_y,$max_y,$scale,$lock);
  }
							# Find major ticks
  my $n_ticks = sprintf("%.0f",$y_range/$scale);
  while ($n_ticks >= 10) {$n_ticks /= 10;}
  $n_ticks =
	($n_ticks<2)?5:($n_ticks==2)?4:($n_ticks==3)?
	((length($y_range/3)<length($y_range/6))?3:6):($n_ticks==8)?4:$n_ticks;

							# Find minor ticks
  my $delta = $y_range/$n_ticks;
  my $m_ticks = (length($delta/4) <= length($delta/5)) ? 4 : 5;


  return $n_ticks,$m_ticks,@y_scale;
}

#######################################################################


sub draw_graph

{
  my ($data_x,$data_y,$sigma,$nodata,$time,$frmt,$title,$calendar) = @_;
  my  @data_x = @$data_x;
  my  @data_y = @$data_y;

  my $add = ($time=~m/daily(?!{)/) ? 0.5 : 0;
  for (my $i=0; $i<=$#data_x; $i++) {$data_x[$i] += $add}

		### Get calendar and link calendar functions
  $calendar = 366 unless   $calendar;
  my	     $julian_day = $calendar==360 ? \&calendar360_day         : \&Time::JulianDay::julian_day;
  my $inverse_julian_day = $calendar==360 ? \&calendar360_inverse_day : \&Time::JulianDay::inverse_julian_day;

  ###################  Make Graph  ####################

  my $img = new GD::Image(480,360);
  my $white	= $img->colorAllocate(255, 255, 255);
  my $red	= $img->colorAllocate(255, 100,   0);
  my $blue	= $img->colorAllocate(0,   100, 255);
  my $yellow	= $img->colorAllocate(255, 205,   0);
  my $lt_grey	= $img->colorAllocate(180, 180, 180);
  my $grey	= $img->colorAllocate(115, 115, 115);

  my $brush_b = new GD::Image(2,2);			# Create a Blue Brush
  $brush_b->colorAllocate(0,100,255);
  my $brush_r = new GD::Image(2,2);			# Create a Red  Brush
  $brush_r->colorAllocate(255,100,0);
  my $brush_g = new GD::Image(2,2);			# Create a Grey Brush
  $brush_g->colorAllocate(115,115,115);
  $img->setBrush($brush_g);

  my ($x0,$y0) = (79,322);
  my ($dx,$dy) = (330,279);

								# Draw title
  $img->string(GD::gdLargeFont,$x0+$dx/2-4*length($$title[0]),$y0-$dy-25,$$title[0],GD::gdBrushed);

  $img->rectangle($x0,$y0-$dy,$x0+$dx,$y0,GD::gdBrushed);		# Draw frame

  ###################  Make X ticks and labels  ###################

  my ($x_ticks,$x_m_ticks,@x_scale,$x_range);
  if (($data_x[-1]-$data_x[0]) < 62)
  {
    @x_scale = (POSIX::floor($data_x[0]),POSIX::ceil($data_x[-1]));
    $x_range = $x_scale[1] - $x_scale[0];
    $x_ticks = 1;
    $x_m_ticks = $x_range;

    my @used;
    for (my $i=0; $i<=$x_m_ticks; $i++)				# Draw major X-labels
    {
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my $label = sprintf("%04d-%02d-%02d", &$inverse_julian_day(sprintf("%.0f",$x_scale[0]+$x_range/$x_m_ticks*$i)));
  open STDERR, ">&OLDERR";
      next unless $label =~ s/(.+)-01$/$1/;
      $label =~ s/\d{4}/0000/ if $time=~m/clim/;

      my $x_pos = $x0+$dx/$x_m_ticks*$i;
      my $x_posS = $x_pos-3*length($label);
      my $x_posF = $x_pos+3*length($label);
      $img->string(GD::gdMediumBoldFont,$x_posS,$y0+4,$label,$grey);
      $img->line($x_pos,$y0-$dy+9,$x_pos,$y0-9,$lt_grey) if ($i%$x_m_ticks);
      $img->line($x_pos,$y0-8,$x_pos,$y0,GD::gdBrushed);
      $img->line($x_pos,$y0-$dy,$x_pos,$y0-$dy+8,GD::gdBrushed);
      push @used,[$x_posS-10,$x_posF+10];
    }
    D_LOOP:
    for (my $i=0; $i<=$x_m_ticks; $i++)				# Draw X-labels
    {
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my @date = &$inverse_julian_day(sprintf("%.0f",$x_scale[0]+$x_range/$x_m_ticks*$i));
  open STDERR, ">&OLDERR";
      my $label = sprintf("%04d-%02d-%02d",@date);
      $label =~ s/\d{4}/0000/ if $time=~m/clim/;
      if (scalar(@used))
      {
        $label = substr($label,8,2);
        next if ($label+0)%5 || ($x_range>=20 && ($label+0)==30);
      }

      my $x_pos = $x0+$dx/$x_m_ticks*$i;
      my $x_posS = $x_pos-3*length($label);
      my $x_posF = $x_pos+3*length($label);

      $img->line($x_pos,$y0-$dy+9,$x_pos,$y0-9,$lt_grey) if ($i%$x_m_ticks);
      $img->line($x_pos,$y0-8,$x_pos,$y0,GD::gdBrushed);
      $img->line($x_pos,$y0-$dy,$x_pos,$y0-$dy+8,GD::gdBrushed);
      for (my $j=0; $j<=$#used; $j++)
      {
        next D_LOOP if (($x_posS>$used[$j][0] && $x_posS<$used[$j][1]) ||
		($x_posF>$used[$j][0] && $x_posF<$used[$j][1]));
      }
      $img->string(GD::gdMediumBoldFont,$x_posS,$y0+4,$label,$grey);
      push @used,[$x_posS-10,$x_posF+10];
    }
  }
  elsif (($data_x[-1]-$data_x[0]) < 732)
  {
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
    my @end_date = &$inverse_julian_day($data_x[-1]);
    @x_scale = (
	&$julian_day((&$inverse_julian_day($data_x[0]))[0,1],1),
	&$julian_day($end_date[0],$end_date[1]+1,1));
  open STDERR, ">&OLDERR";
    $x_range = $x_scale[1] - $x_scale[0];
    $x_ticks = sprintf("%.0f",$x_range/30)+0;
    $x_m_ticks = 4;

    my @used;
    for (my $i=0; $i<=$x_ticks; $i++)				# Draw X-labels
    {								# and grid for Years
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my $label = sprintf("%04d-%02d",(&$inverse_julian_day(
	sprintf("%.0f",$x_scale[0]+$x_range/$x_ticks*$i)+10))[0,1]);
  open STDERR, ">&OLDERR";
      next unless $label =~ s/(.+)-01/$1/;
      $label =~ s/\d{4}/0000/ if $time=~m/clim/;

      my $x_pos = $x0+$dx/$x_ticks*$i;
      my $x_posS = $x_pos-3*length($label);
      my $x_posF = $x_pos+3*length($label);
      $img->string(GD::gdMediumBoldFont,$x_posS,$y0+4,$label,$grey);
      push @used,[$x_posS-10,$x_posF+10];
    }
    M_LOOP:
    for (my $i=0; $i<=$x_ticks; $i++)				# Draw X-labels
    {								# and grid for Months
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my @date = &$inverse_julian_day(sprintf("%.0f",$x_scale[0]+$x_range/$x_ticks*$i)+10);
  open STDERR, ">&OLDERR";
      my $label = (scalar(@used)) ? sprintf("%02d",$date[1]) :
	sprintf("%04d-%02d",@date[0,1]);
      $label =~ s/\d{4}/0000/ if $time=~m/clim/;

      my $x_pos = $x0+$dx/$x_ticks*$i;
      my $x_posS = $x_pos-3*length($label);
      my $x_posF = $x_pos+3*length($label);
      $img->line($x_pos,$y0-$dy+9,$x_pos,$y0-9,$lt_grey) if ($i%$x_ticks);
      for (my $j=0; $j<=$#used; $j++)
      {
        next M_LOOP if (($x_posS>$used[$j][0] && $x_posS<$used[$j][1]) ||
		($x_posF>$used[$j][0] && $x_posF<$used[$j][1]));
      }

      $img->string(GD::gdMediumBoldFont,$x_posS,$y0+4,$label,$grey);
      push @used,[$x_posS-10,$x_posF+10];
    }
  }
  else
  {
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
    @x_scale = (
	&$julian_day((&$inverse_julian_day($data_x[0]))[0],1,1),
	&$julian_day((&$inverse_julian_day($data_x[-1]))[0]+1,1,1));
  open STDERR, ">&OLDERR";
    $x_range = $x_scale[1] - $x_scale[0];
    $x_ticks = sprintf("%.0f",$x_range/365)+0;
    $x_m_ticks = ($x_ticks<10)?6:(($x_ticks<20)?3:1);

    my $x_posF = 0;
    for (my $i=0; $i<=$x_ticks; $i++)				# Draw X-labels
    {								# and grid
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my $label = (&$inverse_julian_day(
	sprintf("%.0f",$x_scale[0]+$x_range/$x_ticks*$i)+10))[0];
  open STDERR, ">&OLDERR";

      my $x_pos = $x0+$dx/$x_ticks*$i;
      my $x_posS = $x_pos-3*length($label);
      next if $x_posS < $x_posF+10;
      $x_posF = $x_pos+3*length($label);
      $img->string(GD::gdMediumBoldFont,$x_posS,$y0+4,$label,$grey);
      $img->line($x_pos,$y0-$dy+1,$x_pos,$y0-1,$lt_grey) if ($i%$x_ticks);
    }
  }

								# Draw X-ticks
  if ($x_range < 36526)		# skip minor tick for range over 100 yrs
  {
    for (my $i=1; $i<$x_ticks*$x_m_ticks; $i++)
    {
      my $x_pos = $x0+$dx/$x_ticks/$x_m_ticks*$i;
      my $t_len = ($i%$x_m_ticks) ? 4 : 8;
      $img->line($x_pos,$y0-$t_len,$x_pos,$y0,GD::gdBrushed);
      $img->line($x_pos,$y0-$dy,$x_pos,$y0-$dy+$t_len,GD::gdBrushed);
								# X-grid line
      $img->line($x_pos,$y0-$dy+5,$x_pos,$y0-5,$lt_grey) if $x_range>1 && $x_range<20;
    }
  }

								# Draw X-title
  $img->string(GD::gdLargeFont,$x0+$dx/2-4*length($$title[1]),$y0+20,$$title[1],GD::gdBrushed);

  ###################  Make Y ticks and labels  ###################

								# Find Y-ticks
  my @test_data_min; my @test_data_max;
  map {
    if ($data_y[$_]!=$nodata) {
      push @test_data_min,($sigma?$data_y[$_]-$$sigma[$_]:$data_y[$_]);
      push @test_data_max,($sigma?$data_y[$_]+$$sigma[$_]:$data_y[$_]);
    }
  } 0..$#data_y;
  my $min_y = List::Util::min(@test_data_min);
  my $max_y = List::Util::max(@test_data_max);
  my ($y_ticks,$y_m_ticks,@y_scale) = axis_scale($min_y,$max_y);
  my $y_range = $y_scale[1]-$y_scale[0];

  for (my $i=1; $i<$y_ticks*$y_m_ticks; $i++)			# Draw Y-ticks
  {
    my $y_pos = $y0-$dy/$y_ticks/$y_m_ticks*$i;
    my $t_len = ($i%$y_m_ticks) ? 4 : 8;
    $img->line($x0,$y_pos,$x0+$t_len,$y_pos,GD::gdBrushed);
    $img->line($x0+$dx-$t_len,$y_pos,$x0+$dx,$y_pos,GD::gdBrushed);
   }
  for (my $i=1; $i < $y_ticks; $i++)				# Draw Y-labels
  {								# and grid
    my $label = sprintf $frmt,$y_scale[0]+$y_range/$y_ticks*$i;
    my $y_pos = $y0-$dy/$y_ticks*$i;
    my $x_pos = $x0-7*(4/7+length($label));
    $img->string(GD::gdMediumBoldFont,$x_pos,$y_pos-7,$label,$grey);
    $img->line($x0+9,$y_pos,$x0+$dx-9,$y_pos,$lt_grey);
  }
  my $lbl = sprintf $frmt,$y_scale[0];
  $img->string(GD::gdMediumBoldFont,
	$x0-7*(4/7+length($lbl)),$y0-12,$lbl,$grey);
     $lbl = sprintf $frmt,$y_scale[1];
  $img->string(GD::gdMediumBoldFont,
	$x0-7*(4/7+length($lbl)),$y0-$dy-3,$lbl,$grey);

								# Draw Y-title
  $img->stringUp(GD::gdLargeFont,$x0-75,$y0-$dy/2+4*length($$title[2]),$$title[2],GD::gdBrushed);

  ###################  Draw Data  ###################

  $img->setBrush($brush_b);
  my @pos0 = ($x0+$dx*($data_x[0]-$x_scale[0])/$x_range,
	      $y0-$dy*($data_y[0]-$y_scale[0])/$y_range);
  for (my $i=1; $i<=$#data_y; $i++)
  {
    my @pos1 = ($x0+$dx*($data_x[$i]-$x_scale[0])/$x_range,
	        $y0-$dy*($data_y[$i]-$y_scale[0])/$y_range);

    @pos0 = @pos1 if $data_y[$i-1]==$nodata && $data_y[$i]!=$nodata;
    @pos1 = @pos0 if $data_y[$i-1]!=$nodata && $data_y[$i]==$nodata;

    $img->line(@pos0,@pos1,GD::gdBrushed)
		  if $data_y[$i-1]!=$nodata || $data_y[$i]!=$nodata;
    @pos0 = @pos1;
  }

  ###################  Draw Sigma  ###################

  if ($sigma) {
    my @pos0 = ($x0+$dx*($data_x[0]-$x_scale[0])/$x_range,
	      $y0-$dy*($data_y[0]-$y_scale[0])/$y_range);
    my $dlt0 = $dy*$$sigma[0]/$y_range;
    for (my $i=1; $i<=$#data_y; $i++)
    {
      my $dlt1 = $dy*$$sigma[$i]/$y_range;
      my @pos1 = ($x0+$dx*($data_x[$i]-$x_scale[0])/$x_range,
	        $y0-$dy*($data_y[$i]-$y_scale[0])/$y_range);

      @pos0 = @pos1 if $data_y[$i-1]==$nodata && $data_y[$i]!=$nodata;
      @pos1 = @pos0 if $data_y[$i-1]!=$nodata && $data_y[$i]==$nodata;

      if ($data_y[$i-1]!=$nodata || $data_y[$i]!=$nodata) {
	$img->line($pos0[0],$pos0[1]+$dlt0,$pos1[0],$pos1[1]+$dlt1,$yellow);
	$img->line($pos0[0],$pos0[1]-$dlt0,$pos1[0],$pos1[1]-$dlt1,$yellow);
      }
      @pos0 = @pos1;
      $dlt0 = $dlt1;
    }
  }

  return $img->png;
}

#######################################################################

sub draw_xy_graph
{
  my ($data_x,$data_y,$nodata,$title,$image_size) = @_;
  my  @data_x = @$data_x;
  my  @data_y = @$data_y;

  ###################  Make Graph  ####################

  my $img = new GD::Image(@{$image_size});
  my $white	= $img->colorAllocate(255, 255, 255);
  my $blue	= $img->colorAllocate(0,   100, 255);
  my $lt_grey	= $img->colorAllocate(180, 180, 180);
  my $grey	= $img->colorAllocate(115, 115, 115);

  my $brush_b = new GD::Image(2,2);			# Create a Blue Brush
  $brush_b->colorAllocate(0,100,255);
  my $brush_g = new GD::Image(2,2);			# Create a Grey Brush
  $brush_g->colorAllocate(115,115,115);

  my ($x0,$y0) = ($$image_size[0]*80/480, $$image_size[1]*320/360);
  my ($dx,$dy) = ($$image_size[0]*330/480,$$image_size[1]*280/360);

  ### 								# Find X-ticks
  my $min_x = List::Util::min(@$data_x);
  my $max_x = List::Util::max(@$data_x);
  my ($x_ticks,$x_m_ticks,@x_scale) =  axis_scale($min_x,$max_x);
  my $x_range = $x_scale[1]-$x_scale[0];
								# Find Y-ticks
  my   @test_data_y;	map {
  push(@test_data_y,$data_y[$_]) if $data_y[$_]!=$nodata; } 0..$#data_y;
  my $min_y = scalar(@test_data_y) ? List::Util::min(@test_data_y) : 0;
  my $max_y = scalar(@test_data_y) ? List::Util::max(@test_data_y) : 1;
  my($y_ticks,$y_m_ticks,@y_scale) = axis_scale($min_y,$max_y);
  my $y_range = $y_scale[1]-$y_scale[0];

  ###################  Draw Data  ###################

  $img->setBrush($brush_b);
  my @pos0 = ($x0+$dx*($data_x[0]-$x_scale[0])/$x_range,
	      $y0-$dy*($data_y[0]-$y_scale[0])/$y_range);
  for (my $i=1; $i<=$#data_y; $i++)
  {
    my @pos1 = ($x0+$dx*($data_x[$i]-$x_scale[0])/$x_range,
	        $y0-$dy*($data_y[$i]-$y_scale[0])/$y_range);

    @pos0 = @pos1 if $data_y[$i-1]==$nodata && $data_y[$i]!=$nodata;
    @pos1 = @pos0 if $data_y[$i-1]!=$nodata && $data_y[$i]==$nodata;

    $img->line(@pos0,@pos1,GD::gdBrushed)
		  if $data_y[$i-1]!=$nodata || $data_y[$i]!=$nodata;
    @pos0 = @pos1;
  }

  ###################  Draw Decorations  ####################

  $img->setBrush($brush_g);
								# Draw title
  $img->string(GD::gdLargeFont,$x0+$dx/2-4*length($$title[0]),$y0-$dy-25,$$title[0],GD::gdBrushed);

  $img->rectangle($x0,$y0-$dy,$x0+$dx,$y0,GD::gdBrushed);		# Draw frame

  ###################  Make X ticks and labels  ###################

  for (my $i=0; $i<$x_ticks*$x_m_ticks+1; $i++)			# Draw X-ticks
  {
    my $x_pos = $x0+$dx/$x_ticks/$x_m_ticks*$i;
    my $t_len = ($i%$x_m_ticks) ? 4 : 8;

    $img->line($x_pos,$y0,$x_pos,$y0-$t_len,GD::gdBrushed);
    $img->line($x_pos,$y0-$dy,$x_pos,$y0-$dy+$t_len,GD::gdBrushed);
  }
  for (my $i=0; $i <= $x_ticks; $i++)				# Draw X-labels
  {								# and Grid
    my $label = $x_scale[0]+$x_range/$x_ticks*$i;
    my $x_pos = $x0+$dx/$x_ticks*$i;

    $img->string(GD::gdMediumBoldFont,$x_pos-length($label)/2,$y0+7,$label,$grey);
    $img->line($x_pos,$y0-9,$x_pos,$y0-$dy+9,$lt_grey);
  }
								# Draw X-title
  my $min = List::Util::min($y0+35,$$image_size[1]-16);
  $img->string(GD::gdLargeFont,$x0+$dx/2-4*length($$title[1]),$min,$$title[1],GD::gdBrushed);

  ###################  Make Y ticks and labels  ###################

  for (my $i=1; $i<$y_ticks*$y_m_ticks; $i++)			# Draw Y-ticks on the Left
  {
    my $y_pos = $y0-$dy/$y_ticks/$y_m_ticks*$i;
    my $t_len = ($i%$y_m_ticks) ? 4 : 8;
    $img->line($x0,$y_pos,$x0+$t_len,$y_pos,GD::gdBrushed);
    $img->line($x0+$dx-$t_len,$y_pos,$x0+$dx,$y_pos,GD::gdBrushed);
   }
  for (my $i=0; $i <= $y_ticks; $i++)				# Draw Y-labels on the Left
  {								# and grid
    my $label = $y_scale[0]+$y_range/$y_ticks*$i;
    my $y_pos = $y0-$dy/$y_ticks*$i;
    my $x_pos = $x0-7*(1+length($label));
    $img->string(GD::gdMediumBoldFont,$x_pos,$y_pos-7,$label,$grey);
    $img->line($x0+9,$y_pos,$x0+$dx-9,$y_pos,$lt_grey);
  }
								# Draw Y-titles
  $img->stringUp(GD::gdLargeFont,$x0-70,$y0-$dy/2+4*length($$title[2]),$$title[2],GD::gdBrushed);

  ###################  Return the Graph  ##################

  return $img->png;
}

#######################################################################

sub axis_scale_aaa

{
  my ($min_y,$max_y) = @_;
  my ($scale,@y_scale) = (1.,0.,1.);
								# Find Y-Scale
  ($min_y,$max_y)=($max_y)?($min_y*0.9,$min_y*1.1):(-1,1) if $max_y == $min_y;

  $scale = 10.**(POSIX::floor(POSIX::log10($max_y-$min_y)));
  @y_scale = ($scale*POSIX::floor($min_y/$scale),$scale*POSIX::ceil($max_y/$scale));
								# Find ticks
  my $n_ticks = sprintf("%.0f",($y_scale[1]-$y_scale[0])/$scale) + 0;
  while ($n_ticks >= 10.) {$n_ticks /= 10.;}
  SWITCH:
  {
     if ($n_ticks <= 2.) {$n_ticks = 5; last SWITCH;}
     if ($n_ticks == 3.) {$n_ticks = 6; last SWITCH;}
     if ($n_ticks == 8.) {$n_ticks = 4; last SWITCH;}
     if ($n_ticks == 9.)
     {
	$n_ticks = 5;
	$y_scale[1] = $y_scale[0] + 10./9.*($y_scale[1]-$y_scale[0]);
	last SWITCH;
     }
  }
  my $y_range = $y_scale[1]-$y_scale[0];		# Find minor ticks
  my $m_ticks = (sprintf("%.0f",$y_range/$n_ticks/
	10.**POSIX::floor(POSIX::log10($y_range/$n_ticks))) % 2) ? 5 : 4;

  return $n_ticks,$m_ticks,@y_scale;
}

###################   End of Graph Functions   ########################
#######################################################################

sub rgb

{
  my $file = shift;
  my ($n_field,@colors);

  if (-e $file)
  {
    ($n_field,@colors) = read_table($file);
  }
  else
  {
    my $steps = 100;

    foreach my $step (0..$steps-1)
    {
      my $index = int((1 - $step/($steps-1)) * 255);
      push @colors, [$index,$index,$index];
    }
  }

  return @colors;
}

#######################################################################

sub put_comma

{
  my $number = shift;

  return 'N/A' if $number==-9999;
  my $len = length(($number =~ m/\./) ? $` : $number) - ($number =~ m/^-/);
  while (($len-=3) > 0) {$number =~ s/(\d{$len})/$1,/}

  return $number;
}

#######################################################################

sub file_pyramid

{
  my $pyramid	= shift	|| return '';
     $pyramid	=~ s/\s*;\s*$//;	# Remove last scale range delimiter
  my $scale	= shift	|| 1e10;	# Set to default scale if not defined
  my $file	= 'no_image';

  my @files	= split m/;/, $pyramid;
  foreach my $FILE (@files) {
	$FILE	=~ s/^\s+|\s+$//g;	# Trim leading and trailing white spaces
    if ($FILE	=~ m/\((.+?),(.+?)\):(.+)/) {
	$file	= $3 if $scale < $1 && $scale >= $2;
    }
    elsif (scalar(@files)==1) { $file = $FILE; }
  }

  return $file;
}

#######################################################################

sub shp_file_list

{
  my ($file,$bounds,$src_projection,$map_projection) = @_;

  return $file unless $file =~ m/\.shp$/i;
  my $file_list = '';

  $bounds = [split m/ /,$bounds] if ($bounds =~ m/ /);
  $bounds = [split m/_/,$bounds] if ($bounds =~ m/_/);

  my @points = ([$$bounds[0],$$bounds[1]], [$$bounds[2],$$bounds[3]], [$$bounds[0],$$bounds[3]], [$$bounds[2],$$bounds[1]]);

  my @src_points = transform_points($map_projection,$src_projection,\@points);

  my $min_x = List::Util::min(map($$_[0],@src_points));
  my $min_y = List::Util::min(map($$_[1],@src_points));
  my $max_x = List::Util::max(map($$_[0],@src_points));
  my $max_y = List::Util::max(map($$_[1],@src_points));
  $bounds = join ' ',$min_x,$min_y,$max_x,$max_y;

  my @lines = split(m/^/,`$$path{ogrinfo} -q -ro -al -spat $bounds $file`);
  map { $file_list .= " $1" if m/location.+= (\S+)/i } @lines;

  unless ($file_list)
  {
    @lines = split(m/^/,`$$path{ogrinfo} -q -ro -al $file`);
    foreach (@lines) { if (m/location.+= (\S+)/i) { $file_list .= " $1"; last; }}
  }
#warn "LIST ($src_projection   $bounds) = $file_list\n";

  return $file_list;
}

#######################################################################
###########	Geo::GDAL::FFI functions	#######################

sub crs_to_Obj
{
  my $proj	= shift;
  my $obj	= new Geo::GDAL::FFI::SpatialReference(
	$proj	=~ m/epsg:(\d+)/i ? (EPSG  => $1)    :
	$proj	=~ m/proj=/i      ? (Proj4 => $proj) :
	$proj	=~ m/GEOGCS/      ?           $proj  :	# WKT
	die "Unknown projection format <$proj>...\n");	# Can add more formats listed in OSR.pm
  Geo::GDAL::FFI::OSRSetAxisMappingStrategy($$obj,0) if !defined($ENV{GDAL_VERSION_NUM}) or $ENV{GDAL_VERSION_NUM} >= 3;

  return $obj;
}

#######################################################################

sub transform_point				# Returns array or array ref
{
  my($x,$y) =	map \$_, @{dclone([@_[2,3]])};
  my $obj   =	Geo::GDAL::FFI::OCTNewCoordinateTransformation(${crs_to_Obj($_[0])}, ${crs_to_Obj($_[1])});
  my $out   = 	Geo::GDAL::FFI::OCTTransform($obj, 1, $x, $y);
		Geo::GDAL::FFI::OCTDestroyCoordinateTransformation($obj);

  return wantarray ? ($$x, $$y) : [$$x, $$y];
}

sub transform_points
{
  my($proj_from, $proj_to, $points) = @_;
  my $Points =	dclone( $points );
  my $obj    =	Geo::GDAL::FFI::OCTNewCoordinateTransformation(${crs_to_Obj($proj_from)}, ${crs_to_Obj($proj_to)});
  map		Geo::GDAL::FFI::OCTTransform($obj, 1, \$$_[0], \$$_[1]), @$Points;
		Geo::GDAL::FFI::OCTDestroyCoordinateTransformation($obj);
  return wantarray ? @$Points : $Points;	# Returns array or array ref
}

###########	End of Geo::GDAL::FFI functions		###############
#######################################################################

sub get_netcdf_compression
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
    die "\nNodata value 'NaN' defined in the NetCDF metadata for $var_name variable in the NetCDF file:\n".
        "\t$file\nis not valid.\n\t".
	"It can be set/forced using:\n\t".
	"1. NetCDF attribute editor, e.g. \"ncatted  -h -O -a _FillValue,$var_name,m,f,9.96921e+36 FILE\"\n\t".
	"2. Using \"nodata\" key in \"Processing\" attribute for this dataset in the MT DB\n\tAborting...\n\n"
		if defined($nodata) && $nodata =~ m/NaN/i;
  }

  die "\nNodata value is not found for $var_name variable in the NetCDF file:\n\t$file\n".
	"It can be set/forced using:\n\t".
	"1. NetCDF attribute editor, e.g. \"ncatted  -h -O -a _FillValue,$var_name,a,f,-9999 FILE\"\n\t".
	"2. Using \"nodata\" key in \"Processing\" attribute for this dataset in the MT DB\n\tAborting...\n\n"
		unless defined $nodata;

  return ($scale,$offset,$nodata);
}

#######################################################################

sub get_column_location {

   my ($filename,$colname) = @_;

   open(INFILE,"$filename");

   my $header = <INFILE>;
   chomp($header);

   my @colheadings = split("\t",$header);

   my $arraysize = scalar(@colheadings);

   #find column location
   my $location = -1;

   for(my $z=0;$z<$arraysize;$z++) {
      if($colheadings[$z] eq $colname){
         $location = $z;
      }
   }

   return ($location);

}

#######################################################################

sub load_column {   ### Returns the column with given header name.

my ($array,$filename,$colname) = @_;

   my $location = get_column_location($filename,$colname);

   open(INFILE,"$filename");
   my $header = <INFILE>;

   while(my $line = <INFILE>) {
      chomp($line);
      my @tmp = split("\t",$line);
      my $val = $tmp[$location];
      push(@$array,$val);
   }

   close (INFILE);

}

#######################################################################

sub timecheck {     ### Returns true if file1 is more recent.
   my($file1, $file2) = (shift,shift);
   my $d1 = (stat($file1))[9];
   my $d2 = (stat($file2))[9];

   return($d1 > $d2 ? 1:0);
}

#######################################################################

sub band_mapping {
  my ($names, $date, $daytime)	 = @_;
  my ($nc_bands,$ts,$start_date) = ($$names{Bands},$$names{Time_Series},$$names{Start_Date});
      $daytime = [0,0]	unless defined $daytime;
  return 1 unless $nc_bands;

		### Get calendar and link calendar functions
  my			   $calendar = get_calendar($names);
  my	     $julian_day = $calendar==360 ? \&calendar360_day         : \&Time::JulianDay::julian_day;

		############################################

  my @start_date= split m/-/,$start_date;
    $$date[0]	= 2001 if $ts=~m/clim/ || $nc_bands==365;	# No Leap year
  my $date_str	= sprintf("%04d-%02d-%02d", @$date);
  my $multiDay	= $ts=~m/daily\[/ ? 1 : 0;			# Custom daily dates

  my $rec_num = 1;					# Default

	############   Dates from file   ############
	###	NB- Works for one data file only
  my $dates_file = ($ts =~ m/:(\/.+)/) ? $1 : '';
  if (-e $dates_file) {
    $date_str =~ s/-\d{2}$/-00/ if $ts =~ m/monthly/i;
    my ($field,@data) = read_table($dates_file);
    foreach  $rec_num (0..$#data) {
      return($rec_num+1) if $data[$rec_num][$$field{Date}] eq $date_str;
  }   return 1; }

	############   Hourly   ############
  if	($ts=~m/daily\{(\d+)}/) {
		my $step = 24/$1;
	# Case one file per date
	if ($nc_bands == 1) {
		$rec_num = 1; }
	# Case one file per day
	elsif ($nc_bands == 24/$step) {
		$rec_num = $$daytime[0]/$step + 1; }
	# Case one file per month
	elsif ($nc_bands == 31*24/$step) {
		$rec_num = $$daytime[0]/$step + 1 + ($$date[2]-1)*24/$step; }
	else { die "Undefined sub-daily band mapping. Aborting...\n"; }
  }
	############   Daily    ############
  elsif	($ts=~m/daily/) {
    if ($nc_bands==360 || $nc_bands==365 || $nc_bands==366) {		# 1 year per file
		$rec_num = $multiDay ? multiDay_band($$date[0].'-01-01', $date_str, $names) :
			   &$julian_day(@$date) - &$julian_day($$date[0],1,0);
    }
    elsif ($nc_bands==30 || $nc_bands==31) {				# 1 month per file
		$rec_num = $multiDay ? multiDay_band(sprintf("%04d-%02d-%02d",@$date[0,1],1), $date_str, $names) :
			   &$julian_day(@$date) - &$julian_day(@$date[0,1],0);
    }
    elsif ($nc_bands > 366) {				# > 1 year per file (single large file)
      if ($calendar == 366) {				# With leap years
		$rec_num = $multiDay ? multiDay_band($start_date, $date_str, $names) :
			   &$julian_day(@$date) - &$julian_day(@start_date) + 1;
      }
      elsif ($calendar ==  365) {			# Without leap years
		$rec_num = $multiDay ? multiDay_band($start_date, $date_str, $names) :
			   365*($$date[0]-$start_date[0])+&$julian_day(2001,@$date[1,2])-2451910;
      }
      elsif ($calendar ==  360) {
                $rec_num = $multiDay ? multiDay_band($start_date, $date_str, $names) :
			  &$julian_day(@$date) - &$julian_day(@start_date) + 1;
      }
      else { die "Unknown calendar format to get band # (in RIMS.pm). Aborting...\n"; }
    }
  }
	############   Monthly  ############
  elsif	($ts=~m/monthly/) {				# 1 year per file
    if ($nc_bands==12) {
		$rec_num = $$date[1];
    }
    elsif($nc_bands>1) {				# > 1 year per file
		$rec_num = 12*($$date[0]-$start_date[0])+($$date[1]-$start_date[1])+1;
    }
  }
	############   Yearly   ############
  elsif	($ts=~m/yearly/ && $nc_bands>1) {
		$rec_num = $$date[0]-$start_date[0]+1;
  }
	############   Decadal   ############
  elsif	($ts=~m/decadal/ && $nc_bands>1) {
		$rec_num =($$date[0]-$start_date[0])/10+1;
  }

  return int $rec_num;
}

#######################################################################

sub multiDay_band
{
  my ($start, $date, $names)	= @_;

  my ($junk,$list) = make_date_list($start, $date, $names);
  die "Date $date is not part of the time series for $$names{Time_Series}. Aborting...\n" if $$list[-1] ne $date;

  return scalar(@$list);
}

#######################################################################

sub rm_duplicate_options

{
  my ($gdalwarp, $option) = (shift, shift);

  while ($gdalwarp =~ m/$option=(\d+).+$option=(\d+)/)
  {
    my $remove = List::Util::min($1,$2);
    $gdalwarp =~ s/ -wo $option=$remove//;
  }

  return $gdalwarp;
}

#######################################################################

sub split_date

{
  my ($date,$ts) = @_;

		### Get calendar and link calendar functions
  my			   $calendar = get_calendar();
  my	     $julian_day = $calendar==360 ? \&calendar360_day         : \&Time::JulianDay::julian_day;

		############################################

  my @date = split m/-/,$date;
     @date = ($date[0],'',sprintf("%03d", int((&$julian_day(@date)-&$julian_day($date[0],1,1))/8)*8 + 1))
	if $ts =~ m/8day/;
     @date = ($date[0],'',sprintf("%03d",      &$julian_day(@date)-&$julian_day($date[0],1,0)))
	if $ts =~ m/jday/;

  return @date;
}

#######################################################################

sub calendar360_day
{
  my ($year, $month, $day) = @_;
  my  $YEAR  = 1900;

  my  $j_day = 360*($year-$YEAR) + 30*($month-1) + $day;
  die sprintf("Bad date: %04d-%02d-%02d\n", $year,$month,$day) .
	"360-day calendar starts from year $YEAR. Aborting (RIMS.pm function \"calendar360_day\")...\n"
	if $j_day < 1;

  return $j_day;
}

#######################################################################

sub calendar360_inverse_day
{
  my $j_day = shift;
  my  $YEAR  = 1900;

  my $year  = POSIX::floor(($j_day-1)		/ 360);
  my $month = POSIX::floor(($j_day-1-360*$year)	/  30);
  my $day   = $j_day - 360*$year - 30*$month;

  return $year+$YEAR, $month+1, $day;
}

#######################################################################

sub calendar365_DaysInMonth { return Time::DaysInMonth::days_in(2001,$_[1]); }
sub calendar360_DaysInMonth { return 30; }
sub calendar365_is_leap     { return  0; }
sub calendar360_is_leap     { return  0; }

#######################################################################

sub get_calendar
{
  my $names	= shift;
  my $calendar	= 366;

  if	(defined $names) {
    return	$$names{calendar}	 if	defined $$names{calendar};
    my $bands = $$names{Bands} || 366;
		$$names{Processing} = '' unless	defined $$names{Processing};
		$$names{Processing} =~ s/calendar=standard/calendar=366/i;
    $calendar =	$$names{Processing} =~ m/calendar=(\d+)/i ? $1 : $bands==365 ? 365 : 366;
		$$names{calendar} = $calendar;
  }
  elsif	(defined $CALENDAR) {
    return	 $CALENDAR; }

  return $calendar;
}

#######################################################################

sub unzero_dates
{
  my ($date,$ts)= @_;
  my  $clim_yr	= 2001;

  if	($ts =~ m/clim/)		{ $date =~ s/^\d{4}/$clim_yr/;		}
  if	($ts =~ m/8day|jday/)		{ $date =~ s/(\d{4})-(\d{3})/$1-01-$2/;	}
  elsif	($ts =~ m/monthly/)		{ $date =~ s/\d{2}$/15/;		}
  elsif ($ts =~ m/yearly|decadal/)	{ $date =~ s/\d{2}-\d{2}$/07-01/;	}

  return $date;
}

#######################################################################

sub make_date_list

{
  my ($date_start,$date_end,$names) = @_;
  my  $ts_type	= $$names{Time_Series};
  my  $clim_yr	= 2001;

		### Get calendar and link calendar functions
  my			   $calendar = get_calendar($names);
  my	     $julian_day = $calendar==360 ? \&calendar360_day         : \&Time::JulianDay::julian_day;
  my $inverse_julian_day = $calendar==360 ? \&calendar360_inverse_day : \&Time::JulianDay::inverse_julian_day;
  my $days_in		 = $calendar==360 ? \&calendar360_DaysInMonth :
			   $calendar==365 ? \&calendar365_DaysInMonth : \&Time::DaysInMonth::days_in;
  my $is_leap		 = $calendar==360 ? \&calendar360_is_leap     :
			   $calendar==365 ? \&calendar365_is_leap     : \&Time::DaysInMonth::is_leap;

		### Check input errors
  if ($ts_type) {
    die "Wrong format for the time series start date (ID = $$names{Code_Name}) in RIMS.pm. Aborting...\n"
	if $date_start !~ m/^\d{4}-(\d{2}-\d{2}|\d{3})(T.+)*$/;
    die "Wrong format for the time series   end date (ID = $$names{Code_Name}) in RIMS.pm. Aborting...\n"
	if $date_end   !~ m/^\d{4}-(\d{2}-\d{2}|\d{3})(T.+)*$/;
  }

  ################################################
  #####   Get dates from file if available   #####

  my $dates_file = ($ts_type =~ m/:(\/.+)/) ? $1 : '';
  if (-e $dates_file) {
    my @idx;
	#####   Convert start and end dates to Julian Day format   #####
    my ($j_date_start, $j_date_end) =
	(&$julian_day(split m/-/,unzero_dates($date_start,$ts_type)),
	 &$julian_day(split m/-/,unzero_dates($date_end,  $ts_type)));

    my ($field,@data) = read_table($dates_file);
    my @date   = map $$_[$$field{Date}],@data;
    my @j_date = map $$_[$$field{Julian_date}],@data;
    map {push @idx,$_ if $j_date[$_] >= $j_date_start && $j_date[$_] <= $j_date_end} 0..$#date;

    return [@j_date[@idx]],[@date[@idx]];
  }

  ############################################################
  #####   Return empty list if data is not time series   #####

  return [&$julian_day($clim_yr,1,1)],[$clim_yr.'-01-01'] unless $ts_type;

  ############################################################
  ##################   Create date list   ####################

  #####   Separate daytime from date string   #####
  my $daytime_start = ($date_start    =~ s/(T\d{2}:\d{2})//) ? $1 : '';
  my $daytime_end   = ($date_end      =~ s/(T\d{2}:\d{2})//) ? $1 : '';
  my $dtime_start   = ($daytime_start =~ m/(\d{2}):(\d{2})/) ? ($1/24+$2/24/60) : 0;
  my $dtime_end     = ($daytime_end   =~ m/(\d{2}):(\d{2})/) ? ($1/24+$2/24/60) : 0;

  #####   Prepare date string to be used for Julian Day format   #####
  $date_start = unzero_dates($date_start,$ts_type);
  $date_end   = unzero_dates($date_end,  $ts_type);

  #####   Convert start and end dates to Julian Day format   #####
  $date_start =~ s/(\d{4})-(\d{3})/$1-01-$2/ if $ts_type =~ m/8day|jday/;
  $date_end   =~ s/(\d{4})-(\d{3})/$1-01-$2/ if $ts_type =~ m/8day|jday/;
  my ($j_date_start,$j_date_end) =
	(&$julian_day(split m/-/,$date_start)+$dtime_start, &$julian_day(split m/-/,$date_end)+$dtime_end);

  #####   Add time fraction for hourly data   #####
  my $time_int = ($ts_type =~ m/daily(_clim)*{(\d+)}/) ? $2 : 1;
  $j_date_end += 1 - 1/$time_int unless $daytime_end;

  #####   Build list of Julian dates   #####
  my @j_date = ( $j_date_start);			# Julian day
  my @J_DATE = ([$j_date_start,0,0]);			# Julian day split to integers: [day, hour, minute]

  while ($j_date[-1] < $j_date_end-0.001)		###  0.001 account for day time accuracy
  {
    open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my $step =
	($ts_type =~ m/monthly/) ?       &$days_in(&$inverse_julian_day($j_date[-1])) :
	($ts_type =~ m/yearly/)  ? 365 + &$is_leap(&$inverse_julian_day($j_date[-1]+365)) :
	($ts_type =~ m/decadal/) ? 3653  : 1/$time_int;		### Daily
    open STDERR, ">&OLDERR";

    push @j_date, $j_date[-1]+$step;
    push @J_DATE,[$j_date[-1], 0,0];
    if ($time_int > 1) {
      $J_DATE[-1] = [jday_hour_min($j_date[-1])];
      $j_date[-1] = $J_DATE[-1][0] + $J_DATE[-1][1]/24 + $J_DATE[-1][2]/1440;
    }
  }

  #####   Build list of formatted dates   #####
  open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
    my @date = ($time_int==1) ?
	map(sprintf("%04d-%02d-%02d",		&$inverse_julian_day($_)),		@j_date) :
	map(sprintf("%04d-%02d-%02dT%02d:%02d",	&$inverse_julian_day($$_[0]),@$_[1,2]), @J_DATE) ;
  open STDERR, ">&OLDERR";

  if ($ts_type =~ m/yearly|decadal/)	{ map s/\d{2}-\d{2}$/00-00/,@date; }
  if ($ts_type =~ m/monthly/)		{ map s/\d{2}$/00/,         @date; }
  if ($ts_type =~ m/clim/)		{ map s/^\d{4}/0000/,       @date; }

  #####   Trim leap year dates   #####
  if ($ts_type =~ m/daily(\[)*/ && $calendar==365)
  {
    my @list;
    map {push @list,$_ if substr($date[$_],5) ne '02-29'} 0..$#date;

    @date   = @date[@list];			### Trim lists
    @j_date = @j_date[@list];
  }

  #####   Trim custom daily dates   #####
  if ($ts_type =~ m/(daily)\[([\d,]+)(lastDay)*\]/)
  {
    my  $last = $3;
    my  @days = split m/,/, $2;
    map $_    = sprintf("%02d",$_),@days;	### Format days to be zero-padded
    my  $days = join '|', @days;		### Make search string
    my  @list;					### Make match list
    foreach my $i (0 .. $#date) {
      my @date	= split m/-/, $date[$i];
      my $add	= $last ? '|'.&$days_in(@date[0,1]) : '';
      push(@list, $i) if $date[2] =~ m/$days$add/;
    }
    @date   = @date[@list];			### Trim lists
    @j_date = @j_date[@list];
  }

  #####   Trim 8-day dates   #####
  if ($ts_type =~ m/8day/)
  {
    my @list;					### Make match list
    map {push @list,$_ unless ($j_date[$_]-&$julian_day((split(m/-/,$date[$_]))[0],1,1))%8;} 0..$#date;
    push @list,0 unless @list;

    @date   = @date[@list];			### Trim lists
    @j_date = @j_date[@list];
  }

  return \@j_date,\@date;
}

#######################################################################

sub make_file_list

{
  my ($dates, $names, $scale, $point) = @_;
  $scale	 = 1e10  unless defined $scale;
  $point	 = '0_0' unless defined $point;
  $$names{Bands} = 1 unless $$names{Bands};
				### Get the last path from the pyramid
  my $file = 'no_image';
  while (length($file) && $file =~ m/no_image/ && $scale < 1e30)
  {
    $file = file_pyramid($$names{File_Path},$scale);
    $scale *= 10;
  }
		### Get calendar and link calendar functions
  my			   $calendar = get_calendar($names);
  my $inverse_julian_day = $calendar==360 ? \&calendar360_inverse_day : \&Time::JulianDay::inverse_julian_day;

				### Make file and band list
  my @file_list = (['',[0]]);
  foreach my $date (@$dates)
  {
    my @daytime	= jday_hour_min($date);
    open STDERR, ">/dev/null";	### Warning for "pre-dates British use of Gregorian calendar" < yr.1760
      my @date	=(split_date(sprintf("%04d-%02d-%02d",&$inverse_julian_day($daytime[0])),$$names{Time_Series}),
			map( sprintf("%02d",$_), @daytime[1,2]));
    open STDERR, ">&OLDERR";
    my $data_file = $file;

    $data_file =~ s/_YEAR_/$date[0]/g;
    $data_file =~ s/_MONTH_/$date[1]/g;
    $data_file =~ s/_DAY_/$date[2]/g;
    $data_file =~ s/_HOUR_/$date[3]/g;
    $data_file =~ s/_MIN_/$date[4]/g;

    my ($var_name,$pol_ts) = ($$names{Var_Name},0);
    if ($$names{Processing}=~m/polygon/i) {		### Polygon Data Layer Format
      $pol_ts += $var_name =~ s/_YEAR_/$date[0]/g;
      $pol_ts += $var_name =~ s/_MONTH_/$date[1]/g;
      $pol_ts += $var_name =~ s/_DAY_/$date[2]/g;		### This treats time series in shape files
      $pol_ts += $var_name =~ s/_HOUR_/$date[3]/g;
      $pol_ts += $var_name =~ s/_MIN_/$date[4]/g;
    }
					### Extract single file from shape file list
    $data_file = shp_file_list($data_file,$point.'_'.$point,$$names{Projection},'epsg:4326')
	if ($data_file =~ m/\.shp$/i && $$names{Processing} !~ m/polygon/i);
    $data_file =~ s/ //g;		### Remove spaces from shp_file_list() returns

					### NeetCDF data file name
    $data_file = "NETCDF:$data_file:$$names{Var_Name}" if $data_file =~ m/\.nc$/i;

    my $band = $pol_ts ? $var_name : ($$names{Processing}=~m/band=(\d+)/) ? $1 :
	band_mapping($names, [@date[0..2]], [@date[3,4]]);
		# Populate the list
    if ($data_file eq $file_list[-1][0]) { push @{$file_list[-1][1]},$band; }
    else				 { push @file_list,[$data_file,[$band]];}
  }
  shift @file_list;

  return \@file_list;
}

#######################################################################

sub make_date_layers
{
  my ($fileList,$dateList) = @_;
  my %dateLayer;
  my ($f,$b) = (0,0);

  foreach my $date (@$dateList) {
    my $file = $$fileList[$f][0];
    my $band = $$fileList[$f][1][$b];
    $dateLayer{$date} = [$file,$band];

    if ($b++ == $#{$$fileList[$f][1]}) {
      $f++;
      $b = 0;
    }
  }

  return \%dateLayer;
}

#######################################################################

sub chk_date_list
{
  my ($j_dates,$dates,$names,$scale,$point) = @_;
  my (@j_dates,@dates);
  $$names{Bands} = 1 unless $$names{Bands};
				### Get the last path from the pyramid
  my $file = 'no_image';
  while (length($file) && $file =~ m/no_image/ && $scale < 1e30)
  {
    $file = file_pyramid($$names{File_Path},$scale);
    $scale *= 10;
  }

				### Make list of actually existing dates
  my ($last_file,$n_bands) = ('',1);
  for (my $i=0; $i<=$#$dates; $i++)
  {
    my $date		= $$dates[$i];
    my $daytime	= ($date=~s/T(\d{2}:\d{2})//) ? $1 : '';
    my @date		= split_date($date,$$names{Time_Series});
    my $data_file	= $file;

    $data_file =~ s/_YEAR_/$date[0]/g;
    $data_file =~ s/_MONTH_/$date[1]/g;
    $data_file =~ s/_DAY_/$date[2]/g;

    last unless -e $data_file;
    $data_file = shp_file_list($data_file,$point.'_'.$point,$$names{Projection},'epsg:4326')
	if $data_file =~ m/\.shp$/i;	### Extract single file from shape file list
    $data_file =~ s/ //g;		### Remove spaces from shp_file_list() returns

					### NeetCDF data file name
    $data_file = "NETCDF:$data_file:$$names{Var_Name}" if $data_file =~ m/\.nc$/i;
    my $band   = band_mapping($names,\@date);

		# Check band
    if ($last_file ne $data_file)
    {
      $last_file = $data_file if $band > 1;
      $n_bands   = ($band==1) ? 1 : band_count($data_file);
    }

    if ($band <= $n_bands) {
      push @j_dates,$$j_dates[$i];
      push @dates,$$dates[$i];
    }
    else { last; }
  }

  return \@j_dates,\@dates;
}

#######################################################################

sub band_count
{
  my $file = shift;

  open STDERR, ">/dev/null";
    my $dataset = Geo::GDAL::FFI::Open($file, {Flags => ['READONLY']});
  open STDERR, ">&OLDERR";

  return Geo::GDAL::FFI::GDALGetRasterCount($$dataset);
}

#######################################################################

sub jday_hour_min

{
  my $frac = shift;
  my $day  = int($frac);
  my $min  = sprintf "%.0f",1440*($frac - $day);
  if($min == 1440) {
     $day += 1;
     $min  = 0;
  } 
  my $hour = int $min / 60;
     $min  =     $min % 60;

  return $day,$hour,$min;
}

#######################################################################

sub trim_dates

{
  my ($step,$offset) = (pop,pop);
  my @data = @_;
  my $len  = scalar(@{$data[0]});
  return @data if ($offset==0 && $step<=1) || $offset>=$len;

  my @idx = ($offset);
  while ($idx[-1]+$step < $len) {
    push @idx,$idx[-1]+$step;
  }

  return map([@$_[@idx]],@data);
}

#######################################################################

sub check_MT_date
			### Reads dates from file (first occurance in the file)
{
  if (-e $_[0]) {
    my $content = htm_template($_[0]);
    $_[0] = ($content =~ m/(\d{4}-\d{2}-\d{2})/) ? $1 : '';
  }
  return(!length($_[0]) ? 0 : $_[0] =~ m/\d{4}-\d{2}-\d{2}/ ? 0 : 1);
}

#######################################################################

sub STD_date
			### Reads dates from file (first occurance in the file)
{
		### Get calendar and link calendar functions
  my			   $calendar = get_calendar();
  my	     $julian_day = $calendar==360 ? \&calendar360_day         : \&Time::JulianDay::julian_day;
  my $inverse_julian_day = $calendar==360 ? \&calendar360_inverse_day : \&Time::JulianDay::inverse_julian_day;

  my @date   = split m/-/,$_[0];
  $_[0] = sprintf "%04d-%02d-%02d", &$inverse_julian_day(&$julian_day($date[0],1,$date[1]));
}

#######################################################################

sub check_file_type
			### Makes GDAL compartible data file name
{
  my $fileExt = ($_[0] =~ m/\.(\w+)$/) ? lc($1) : 'none';
  my %fileDrv = (
	'nc'	=> 'NETCDF',
	'hdf'	=> 'HDF4_SDS',
	'he5'	=> 'HDF5');

  $_[0] = "$fileDrv{$fileExt}:$_[0]:$_[1]" if defined($fileDrv{$fileExt});
}

#######################################################################

sub check_sigma

{
  my $file = shift;

  if ($file =~ s/(NETCDF:.+:.+)/$1\_sigma/) {
    open STDERR, ">/dev/null";
      my $dataset = eval { Geo::GDAL::FFI::Open($file, {Flags => ['READONLY']}) };
    open STDERR, ">&OLDERR";
    return defined($dataset) ? 1 : 0;
  }
  else { return 0 }
}
#######################################################################

sub polygon_files
				### Added on April 26, 2013
{
  my $names	= shift;
  my %list;
  my $PA	= $$names{Polygon_Aggregation};
 (my $dir	= $$path{polygon_dir} . "$$names{Project}/$$names{Code_Name}")=~s/ /_/g;

  $list{$1} = $2		while ($PA =~ s/^\s*(\S+?):(.+?);\s*//);
  $list{$1} = "$dir.$1.nc"	while ($PA =~ s/^(\S+)\s*//);

  return \%list;
}

#######################################################################

sub dumperror {
   my $message = shift;
   open(ERR,">>/tmp/errorlog.txt");
   print ERR ("$message\n");
   close (ERR);
}

#######################################################################

sub set_default
{
  my ($value, $default)	= @_;
  return defined($value) && length($value) ? $value : $default;
}

#######################################################################

sub check_ascii_file		### Check ASCII file for non-ASCII characters
{				#   Added on Jun 5, 2023
  my ($file, $i) = (shift(), 0);

  open (FILE,"<$file") or die "Couldn't open $file, $!";
    while (<FILE>) { $i++;
      die sprintf("\nError in reading ASCII file:\n\t$file\n" .
		"It has non-ASCII character \\x%X at column %d of line %d. " .
		"Fix it, e.g. using \"dos2unix\".\n\tAborting...\n\n", ord($1), $+[0], $i)
	if m/([^[:ascii:]])/;
    }
  close FILE;
}

#######################################################################

1;

#######################################################################
__END__

# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Rims - Perl extension for RIMS

=head1 SYNOPSIS

  use RIMS;

=head1 DESCRIPTION

Stub documentation for Rims.

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

