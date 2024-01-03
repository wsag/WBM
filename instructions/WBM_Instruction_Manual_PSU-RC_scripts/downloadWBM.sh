##########################################################################
##########################################################################
## Script Name: downloadWBM.R
## Purpose of Script: A quick script to download and prepare the WBM from
##  https://wbm.unh.edu/.
##
## Special Requirements: Singularity >= 3.7.1
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 8/25/2022
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 8/25/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################

#!/bin/bash

##download the WBM container and readme file - read only version
wget https://wbm.unh.edu/v1.0.0/wbm_opensource_v1.0.0.sif
wget https://wbm.unh.edu/v1.0.0/wbm_opensource_v1.0.0_Readme.txt

##download the WBM container and readme file - read-write version
##both links causing 404 Not Found errors
#wget https://wbm.unh.edu/v1.0.0/wbm_opensource_rw_v1.0.0.sif
#wget https://wbm.unh.edu/v1.0.0/wbm_opensource_rw_v1.0.0_Readme.txt

##download the WBM storage directory structure and readme file
wget https://wbm.unh.edu/v1.0.0/wbm_storage_v1.0.0.tar.gz
wget https://wbm.unh.edu/v1.0.0/wbm_storage_v1.0.0_Readme.txt
tar -xvzf ./wbm_storage_v1.0.0.tar.gz

##download the WBM storage directory structure and readme file
cd ./wbm_storage_v1.0.0/data
wget https://wbm.unh.edu/v1.0.0/WBMAncillaryData_v1.0.0.tar.gz
##copy of the readme file within the .tar.gz
#wget https://wbm.unh.edu/v1.0.0/WBMAncillaryData_v1.0.0_Readme.txt
tar -xvzf ./WBMAncillaryData_v1.0.0.tar.gz
##return to original directory
cd ../..

##run the setup test for WBM, page 6 of
##https://github.com/wsag/WBM/blob/main/instructions/WBM_Instruction_Manual.docx
singularity exec \
  -B ./wbm_storage_v1.0.0/data:/wbm/data \
  -B ./wbm_storage_v1.0.0/data_init:/wbm/data_init \
  -B ./wbm_storage_v1.0.0/spool:/wbm/spool \
  -B ./wbm_storage_v1.0.0/wbm_output:/wbm/wbm_output \
  -B ./wbm_storage_v1.0.0/WBM_run_state:/wbm/WBM_run_state \
  -B ./wbm_storage_v1.0.0/wbm_init:/wbm/wbm_init \
  -B ./wbm_storage_v1.0.0/model:/wbm/model \
  -B ./wbm_storage_v1.0.0/utilities:/wbm/utilities \
  -B ./wbm_storage_v1.0.0/gdal_test_files:/wbm/gdal_test_files \
  ./wbm_opensource_v1.0.0.sif /wbm/model/wbm.pl -v /wbm/wbm_init/test.init
