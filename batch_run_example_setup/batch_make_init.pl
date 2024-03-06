#!/usr/bin/perl

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
		
		
##### historical #############

my $template_file = "/net/home/eos/dgrogan/MSB/wbm_init/LOCA_drought/USNE_drought_cmip5_TEMPLATE_historical.init";

  foreach my $model (@model) {
  
   (my $file_new = $template_file) =~ s/TEMPLATE/$model/;
   
   print "$file_new\n";
 
   open my $in,  '<',  $template_file or die "Can't read old file: $!";
   open my $out, '>', "$file_new"     or die "Can't write new file: $!";

    while( <$in> ){
      s/TEMPLATE/$model/g;

      print $out $_;
    }
   close $out;
   
  }

############# rcps ##########
my @rcp   = qw(rcp45 rcp85);
my $template_file = "/net/home/eos/dgrogan/MSB/wbm_init/LOCA_drought/USNE_drought_cmip5_TEMPLATE_RCP.init";

foreach my $rcp (@rcp) {
  foreach my $model (@model) {
  
   (my $file_new = $template_file) =~ s/TEMPLATE/$model/;
   (   $file_new = $file_new)      =~ s/RCP/$rcp/;
   
   print "$file_new\n";
 
   open my $in,  '<',  $template_file or die "Can't read old file: $!";
   open my $out, '>', "$file_new"     or die "Can't write new file: $!";

    while( <$in> ){
      s/TEMPLATE/$model/g;
      s/RCP/$rcp/g;

      print $out $_;
    }
   close $out;
  }
   
 }
