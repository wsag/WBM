##########################################################################
##########################################################################
## Script Name: downloadWBMExampleData.R
## Purpose of Script:
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

##assumed being ran from same location as container
cd ./wbm_storage_v1.0.0/data

##download and extract the river network data
mkdir ./network
cd ./network
wget --user-hydrography --password=rivernetwork http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/distribute/v1.0/dir_n30w120.tar
tar -xvf ./dir_n30w120.tar
cd ..

##download and extract the dam and reservoir data
mkdir ./dams
cd ./dams
wget https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/5YBWWI/17ad3cd2895-4ca246ca093d.orig?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27HydConDams_Global_2021_v2.0.csv
