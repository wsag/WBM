#!/usr/bin/perl

#######################################################################
#
#       All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      (C) University of New Hampshire and Water Systems Analysis Group 2008-2016
#
#######################################################################

#######################################################################
#
#	This perl code flips row order in NetCDF files.
#
#	NB- this is done specifically for GDAL processing.
#	Unfortunately GDAL ignores ascending or descending order in
#	Y-dimention variable, so flipping Y in GDAL is not possible
#	by just re-ordering the Y-dimension variable...
#	So we have to fool around with the VRT file to flip the order.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	August, 2016
#	Last Modified
#
#######################################################################

use strict;
use warnings;
		### Open and read file
my $file = shift;
# my $file = '/net/nfs/warf/data3/uncompressed/2040/dom3_output/test_2039-12-18.one.vrt';
my $vrt  = htm_template($file);

		### Makes changes to VRT file text

			# Read GeoTransform
my $ySize	  = 		$1 if $vrt =~ m/rasterYSize="(\d+)"/;
my @GeoTransform  = split m/,/,	$1 if $vrt =~ m/<GeoTransform>(.+)<\/GeoTransform>/i;

			# Change GeoTransform
$GeoTransform[2] *= -1;				# Flips Y-dimension order
$GeoTransform[5] *= -1;				# Flips Y-dimension order
$GeoTransform[3] -= $GeoTransform[5] * $ySize;	# Flips Y-dimention origin

			# Change GeoTransform text string in the VRT text
my $GeoTransform  = join ",", map(sprintf('%24.16e',$_), @GeoTransform);
   $vrt		  =~ s/<GeoTransform>(.+)<\/GeoTransform>/<GeoTransform>$GeoTransform<\/GeoTransform>/i;
warn $GeoTransform;

		### Save updated VRT file
# warn $vrt;
open (FILE,">$file") or die "Couldn't open $file, $!";
  print FILE $vrt;
close FILE;

exit;

#######################################################################
######################  Functions  ####################################

sub htm_template		### Read HTML Template

{
  my $file = shift;

  open (FILE,"<$file") or die "Couldn't open $file, $!";
  my @lines = <FILE>;
  close FILE;

  return join('',@lines);
}

#########################################################################
