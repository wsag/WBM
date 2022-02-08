# WBM
The University of New Hampshire Water Balance Model

This repository contains the open source release of the University of New Hampshire Water Balance Model

Authors and contact info:
Richard Lammers: richard.lammers@unh.edu
Danielle Grogan: danielle.grogan@unh.edu
Shan Zuidema: shan.zuidema@unh.edu
Alex Prusevich: alex.prusevich@unh.edu
Stanley Glidden: stanley.glidden@unh.edu

## Files
### model/
This directory contains:
wbm.pl: main executable model code
WBM.conf: configuration file, contains directory and file paths to other inputs
RIMS.pm: custom perl library required by WBM 
RIMS.conf: configuration file for RIMS.pm
build_spool.pl: script called by WBM to generate binary input files from primary input data
build_static_spool.pl: same as build_spool.pl, but for data that are not time series
WBM_dataCube_expand.csv: list of WBM output variables, and info required for temporal aggregation by utilites/temporal_aggregation.pl
RIMS/: directory containing RIMS perl library code

### utilities/
This directory contains:
networkTools.pl: perl code to extract subsets of global digital river networks
networkTools_manual.init: instruction manual for how to use networkTools.pl
spatial_aggregation.pl: spatial aggregation of WBM output. Can be called by wbm.pl
temporal_aggregation.pl: temporal aggregation of WBM output. Can be called by wbm.pl

## Code release notes
This version of WBM is being released by the [Water Systems Analysis Group] (https://wsag.unh.edu/) at the University of New Hampshire. Other research groups use unique development versions of WBM that may differ from the code provided here. A publication describing this WBM code is in preparation. The published paper will be posted here once it is available. Until then, the references for this model are:
[Wisser et al. 2010](https://hess.copernicus.org/articles/14/1/2010/) and Appendix A from [Grogan 2016](https://scholars.unh.edu/dissertation/2260/):

1. Wisser, D., Fekete, B. M., Vörösmarty, C. J., and Schumann, A. H.: Reconstructing 20th century global hydrography: a contribution to the Global Terrestrial Network- Hydrology (GTN-H), Hydrol. Earth Syst. Sci., 14, 1–24, https://doi.org/10.5194/hess-14-1-2010, 2010. 

2. Grogan, Danielle Sarah, "Global and regional assessments of unsustainable groundwater use in irrigated agriculture" (2016). Doctoral Dissertations. 2260.
https://scholars.unh.edu/dissertation/2260 
