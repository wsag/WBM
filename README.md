# WBM
The University of New Hampshire Water Balance Model

This repository contains the open source release of the University of New Hampshire Water Balance Model

Authors and contact info:<br/>
Richard Lammers: richard.lammers@unh.edu<br/>
Danielle Grogan: danielle.grogan@unh.edu<br/>
Shan Zuidema: shan.zuidema@unh.edu<br/>
Alex Prusevich: alex.prusevich@unh.edu<br/>
Stanley Glidden: stanley.glidden@unh.edu<br/>

General questions can be sent to: unh.wbm@unh.edu <br/>

## Files
### model/
This directory contains:  
**wbm.pl**: main executable model code. <br/>
**WBM.conf**: configuration file, contains directory and file paths to other inputs. <br/>
**RIMS.pm**: custom perl library required by WBM   <br/>
**RIMS.conf**: configuration file for RIMS.pm. <br/>
**build_spool.pl**: script called by WBM to generate binary input files from primary input data. <br/>
**WBM_dataCube_expand.csv**: list of WBM output variables, and info required for temporal aggregation by utilites/temporal_aggregation.pl. <br/>
**RIMS/**: directory containing RIMS perl library code. <br/>

### utilities/
This directory contains:  
**networkTools.pl**: perl code to extract subsets of global digital river networks. <br/>
**networkTools_manual.init**: instruction manual for how to use networkTools.pl. <br/>
**spatial_aggregation.pl**: spatial aggregation of WBM output. Can be called by wbm.pl. <br/>
**temporal_aggregation.pl**: temporal aggregation of WBM output. Can be called by wbm.pl. <br/>

## Code release notes
This version of WBM is being released by the [Water Systems Analysis Group](https://wsag.unh.edu/) at the [University of New Hampshire](https://www.unh.edu/). Other research groups use unique development versions of WBM that may differ from the code provided here. A publication describing this WBM code is in review at the journal Geoscientific Model Development.  The [preprint](https://gmd.copernicus.org/preprints/gmd-2022-59/#discussion) can be cited as:     <br/>

Grogan, D.S., Zuidema, S., Prusevich, A., Wollheim, W.M., Glidden, S., and Lammers, R.B. WBM: A scalable gridded global hydrologic model with water tracking functionality, Geoscientific Model Development Discussions, https://doi.org/10.5194/gmd-2022-59, 2022. 

The published paper will be posted here once it is available. Until then, the references for this model are:
[Wisser et al. 2010](https://hess.copernicus.org/articles/14/1/2010/) and Appendix A from [Grogan 2016](https://scholars.unh.edu/dissertation/2260/):

1. Wisser, D., Fekete, B. M., V??r??smarty, C. J., and Schumann, A. H.: Reconstructing 20th century global hydrography: a contribution to the Global Terrestrial Network- Hydrology (GTN-H), Hydrol. Earth Syst. Sci., 14, 1???24, https://doi.org/10.5194/hess-14-1-2010, 2010. 

2. Grogan, Danielle Sarah, "Global and regional assessments of unsustainable groundwater use in irrigated agriculture" (2016). Doctoral Dissertations. 2260.
https://scholars.unh.edu/dissertation/2260. 

## Input data
Model input data and a Singularity container with the required operating system and software dependencies can be found here: https://wbm.unh.edu/ (https://dx.doi.org/10.34051/d/2022.2)
