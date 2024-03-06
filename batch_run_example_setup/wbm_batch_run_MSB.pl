#!/usr/bin/perl

#######################################################################
#
#	WBM batch run:
#	Adopted from data_cube.pl
#
#	Written by Dr. A. Prusevich (alex.proussevitch@unh.edu) 
#       and Dr. D.S. Grogan (danielle.grogan@unh.edu)
#
#	June 2018
#
#######################################################################

use strict;
use Parallel::ForkManager;

my $forks     = 8;


my $pm        = new Parallel::ForkManager($forks);	# Parallel Processing object

my @rcp   = qw(historical rcp45 rcp85);
my @model = qw( ACCESS1-0
		ACCESS1-3
		bcc-csm1-1              
 		bcc-csm1-1-m
		CanESM2
		CCSM4
		CESM1-BGC
		CESM1-CAM5
		CMCC-CM                 
		CMCC-CMS
		CNRM-CM5
		CSIRO-Mk3-6-0           
		FGOALS-g2
		GFDL-CM3
		GFDL-ESM2G              
		GFDL-ESM2M
		HadGEM2-AO
		HadGEM2-CC              
		HadGEM2-ES
		inmcm4
		IPSL-CM5A-LR            
		IPSL-CM5A-MR
		MIROC5
		MIROC-ESM-CHEM        
		MIROC-ESM
		MPI-ESM-LR
		MPI-ESM-MR              
		MRI-CGCM3
		NorESM1-M  
		);
		
my $template_file = "/net/home/eos/dgrogan/MSB/wbm_init/LOCA_drought/USNE_drought_cmip5_TEMPLATE_RCP.init";
my $verbose      = 0;

#################################################################

my $count = 0;
$pm->run_on_finish(sub{
  my ($pid,$exit_code,$ident,$exit_signal,$core_dump,$data) = @_;
  printf "Done (%d)- $$data[0]\n",++$count;
});
printf "\nWBM runs started for %d RCPs and %d models (%d forks):\n\n",
	scalar(@rcp), scalar(@model), $forks;

#################################################################
#		Loop through all models and rcps		#
#################################################################

foreach my $rcp (@rcp) {
  foreach my $model (@model) {
    $pm->start and next;			### Run fork processes/net/nfs/zero/home/scripts/perl/

    (my $file_str = $template_file) =~ s/TEMPLATE/$model/;
    (   $file_str = $file_str)      =~ s/RCP/$rcp/;
    system "/net/home/eos/dgrogan/wbm_repos/wbm_stable/wbm.pl -rm -idump -by $file_str";
    
    $pm->finish(0,["$rcp for $model"]);
  }
  $pm->wait_all_children;
}

print "\nAll Done!\n\n";

exit;

#################################################################
