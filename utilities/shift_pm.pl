#!/usr/bin/perl

#######################################################################
#
#       All code contained within this document is for viewing only and it is an
#      intellectual property of the copyright holder.
#      Any partial or complete reproduction, redistribution or modification
#      without approval of the authors is strictly prohibited.
#      (C) University of New Hampshire and Water Systems Analysis Group 2008-2013
#
#######################################################################

#######################################################################
#
#	This perl code shifts/wraps raster datasets symmetrically around
#	Prime meridian in VRT files.
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu)
#
#	August, 2012
#	Last Modified
#
#######################################################################

use strict;
use warnings;
		### Open and read file
my $file = shift;
# my $file = '/net/home/cv/alexp/file.vrt';
my $vrt  = htm_template($file);

		### Makes changes to VRT file text
my $SimpleSource1 = $1   if $vrt =~ m/\n(\s+<SimpleSource.+SimpleSource>\n)/s;
my $xSize	  = $1/2 if $SimpleSource1 =~ s/ xSize="(\d+)"/' xSize="'.(0.5*$1).'"'/ge;
my $SimpleSource2 = $SimpleSource1;
my $longitude	  = $vrt =~ m/<GeoTransform>\s+([-+e\.\d]+)/ ? $1-180 : -180;

$SimpleSource1	=~ s/DstRect xOff="0"/DstRect xOff="$xSize"/;
$SimpleSource2	=~ s/SrcRect xOff="0"/SrcRect xOff="$xSize"/;
$vrt		=~ s/(<GeoTransform>\s+).+?,/$1$longitude,/;
$vrt		=~ s/\s+<SimpleSource.+SimpleSource>\n/$SimpleSource1$SimpleSource2/s;

		### Save updated VRT file
# die $vrt;
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
