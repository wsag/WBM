# Introduction  
This document is a quick-start guide which focuses on the technical steps needed to install and begin making minor changes to WBM. Once completed, the user will have installed WBM, verified that the install was preformed correctly, and completed the execution of two example WBM models. The second example will include the beginning steps of how to modify the input files for WBM. For more details on the modeling framework and a comprehensive list of options, please see the full WBM instructions.

The steps outlined here were written with the assumption that the user is working on PSU's ROAR HPC. However, other than any ROAR specific path names, the instructions should work on any Linux system. These instructions may have some application on Mac and Windows machines, but you are responsible for making the necessary edits in order to work on those systems.

## What is WBM?  
From the full WBM instructions:
_"The open source WBM code described here is composed of three main files:
wbm.pl, which is the main model script; WBM.pm, a module providing WBM
specific functionality; and RIMS.pm, a module providing geospatial and
temporal transformation utilities. The entire modeling framework is
dependent upon other software: perl, PDL, gdal, ogr, and NetCDF. The
model input data repository (<https://wbm.unh.edu/>) also includes a
Singularity container which has pre-installed the required operating
system and software dependencies for ease of model use by the research
community."_

For emphasis, packaging WBM and its run environment as a Singularity container is a very effective means of distributing the model. For anyone unfamiliar with containers, they allow for the packaging of an independent OS and software and can operate very much like a Virtual Machine (VM). One of the many benefits of this is that the model authors have already constructed the computing environment in which WBM is meant to be ran. This means that we, the model users, can focus on the data and model output instead of spending significant portions of time setting up the WBM software dependencies. If Singularity is installed correctly, the container may be accessed from any machine (including Mac and Windows), although the OS of the contain will remain Linux.

If following this guide on a machine other than ROAR, make sure your Singularity version is \>= 3.7.1. The container may work on earlier versions of Singularity, but neither the model authors nor myself can guarantee that it will. Singularity 3.7.1 is the version in which WBM was developed and tested with.
<br>
<br>

# Installing WBM
As the WBM run environment is already set within the Singularity container, there is not much installing to be done by a user. However, the container and its supporting files need to be downloaded and stored in a local directory. Due to the potential size of the input datasets, downloading these files to your HOME directory on ROAR is not suggested. My best suggestion is to download everything to an available subdirectory in `/gpfs`, if one is available to you. **Once a directory has been selected or created, navigate (cd) to that location.**

The following is a step-by-step process of how to collect the WBM container, some example data, and finally how to correctly extract the data to locations in which the WBM container will most easily find it. You may follow though each step individually, which is highly recommended the first time, or run the included `WBM_localPSUguide/scripts/downloadWBM.sh` script which utilizes the the following code to automatically set up WBM and its data files.

The first step to set up WBM is to download the WBM container as well as the container's README file. It is noteworthy that UNH is expecting to provide two distinct versions of WBM soon: a read-only version, and a read/write version. The read/write version will allow the user to adjusted the container as well as to the underlying model code for WBM, providing greater flexibility. However, the cost of greater model flexibility is the chance of a user breaking their copy of the container. Additionally, the current link to the read/write container is broken. We will proceed with the read-only version of WBM, which will allow us to change the input data but not change how the model variables interact with each other. **To download the WBM container, run:**
```shell
##Download the WBM container and its readme file - read-only version
##This code can be ran from any directory on your Linux machine. The
##container will be installed in your current directory.

wget https://wbm.unh.edu/v1.0.0/wbm_opensource_v1.0.0.sif
wget https://wbm.unh.edu/v1.0.0/wbm_opensource_v1.0.0_Readme.txt
```

**Next, download the WBM Storage Data.** The Storage Data contains a couple of small datasets and is in part a pre-constructed directory structure used for mounting the Singularity container to local directories. However, it primarily includes copies of the initialization files for WBM variables. The initialization files will be discussed at more length later on, as well as how to begin editing them. More on directory mounting will also be discussed later; for now know that mounting is one of the methods used to allow the Singularity container access to locally stored directories and files.
```shell
##Download the WBM storage directory structure and readme file
##This example is done at the root directory

wget https://wbm.unh.edu/v1.0.0/wbm_storage_v1.0.0.tar.gz
wget https://wbm.unh.edu/v1.0.0/wbm_storage_v1.0.0_Readme.txt
```

Once you have the tar file downloaded, it will need to be uncompressed to be able to use the directory structure contained within. To do so, **type the following commands:**
```shell
tar -xvzf ./wbm_storage_v1.0.0.tar.gz
```
A listing of the directory should look like this:
```shell
ls ./wbm_storage_v1.0.0

\> data/ data_init/ gdal_test_files/ model/ spool/ utilities/ wbm_init/
wbm_output/ WBM_run_state/
```

**The final step in setting up WBM is do download and uncompress the Ancillary Data.** The Ancillary Data is a pre-selected data collection provided by UNH to test the installation of WBM, as well as to provide some data in which to run their examples. The entire contents of the Ancillary Data should be stored in the `/data` subdirectory of the Storage Data structure. To make sure the data is put in the correct location, it will be downloaded to the location WBM is expecting it to be in.
```shell
##Download the WBM Ancillary Data

cd ./wbm_storage_v1.0.0/data
wget https://wbm.unh.edu/v1.0.0/WBMAncillaryData_v1.0.0.tar.gz

##You may download the readme file if you would like, however a copy
of the file is already provided within the above collected tar ball
#wget https://wbm.unh.edu/v1.0.0/WBMAncillaryData_v1.0.0_Readme.txt

##Extract the contents of the tar ball
tar -xvzf ./WBMAncillaryData_v1.0.0.tar.gz

##return to original directory
cd ../..
```
WBM should now be properly set up. Before jumping into examples, there is some code that can be ran to verify the set up will work.

_Note:_ If you have looked at the full instructions, you may have noticed that they discuss the downloading of another tar ball of prepackaged open source data. However, there are no links to this data provided on the UNH website for this file.
<br>
<br>

# Opening and Verifying WMB Installation
WBM comes packages with a verification script (`./wbm_storage_v1.0.0/wbm_init/test.init`). However, before the WBM installation can be verified, you first need to learn how to access the WBM container. The UNH full instructions includes a process of creating an instance object to access the container. The purpose of the instance object is to save how the container is being accessed to a file, which makes sure that the container is being accessed with the same settings each time it is opened. This is of particular importance with the WBM contain as it has a significant number of directories to mount, any of which may cause an issue if they are not correctly mapped in subsequent container sessions.

However, the creation of the instance object did not work for me in my initial attempts. More specifically, the instance object would be created, but would not allow access to the WBM container. This is a good lesson for anyone not as familiar with the modeling/coding process - it is always a benefit to keep in mind multiple solutions to the same problem. Over time we all developed our own "flavor" of coding, including our own preferred methods of solving various issues. However, it is almost inevitable that there will come a time that your preferred method will not work and for those occasions it is good to have some alternatives already in mind to fall back on. Due to this a copy of the instance creation method will be provided here, as well as two alternative means of accessing the WBM container. The two alternative methods are: through executable commands, and through shell access.

In the below code you will see the majority of lines begin with `-B` followed by two directories separated by a `:`. This is the mounting process previously mentioned. For those unfamiliar with the mounting concept, these lines are instructing the WBM container to view local directories as though they are directories within the container. Showcasing the first mounting, the local directory of `./wbm_storage_v1.0.0/data` will be referred to as `/wbm/data` when the container is open. This allows the container to have access to all files within `./wbm_storage_v1.0.0/data`. It also means that files the container writes to `/wbm/data` will be also written to `./wbm_storage_v1.0.0/data` and will be available to the local system once the container is closed.

**Now, review the below options for accessing the container, and choose one to proceed with.**
```shell
##Creating a Singularity Instance of the WBM Container
##The instance object is called "wbm_os_instance1"
singularity instance start \
  -B ./wbm_storage_v1.0.0/data:/wbm/data \
  -B ./wbm_storage_v1.0.0/data_init:/wbm/data_init \
  -B ./wbm_storage_v1.0.0/spool:/wbm/spool \
  -B ./wbm_storage_v1.0.0/wbm_output:/wbm/wbm_output \
  -B ./wbm_storage_v1.0.0/WBM_run_state:/wbm/WBM_run_state \
  -B ./wbm_storage_v1.0.0/wbm_init:/wbm/wbm_init \
  -B ./wbm_storage_v1.0.0/model:/wbm/model \
  -B ./wbm_storage_v1.0.0/utilities:/wbm/utilities \
  -B ./wbm_storage_v1.0.0/gdal_test_files:/wbm/gdal_test_files \
  ./wbm_opensource_v1.0.0.sif wbm_os_instance1

  ##Accessing the instance
  singularity shell instance://wbm_os_instance1
```

```shell
##Opening an Interactive Shell with the WBM Container
singularity shell \
  -B ./wbm_storage_v1.0.0/data:/wbm/data \
  -B ./wbm_storage_v1.0.0/data_init:/wbm/data_init \
  -B ./wbm_storage_v1.0.0/spool:/wbm/spool \
  -B ./wbm_storage_v1.0.0/wbm_output:/wbm/wbm_output \
  -B ./wbm_storage_v1.0.0/WBM_run_state:/wbm/WBM_run_state \
  -B ./wbm_storage_v1.0.0/wbm_init:/wbm/wbm_init \
  -B ./wbm_storage_v1.0.0/model:/wbm/model \
  -B ./wbm_storage_v1.0.0/utilities:/wbm/utilities \
  -B ./wbm_storage_v1.0.0/gdal_test_files:/wbm/gdal_test_files \
  ./wbm_opensource_v1.0.0.sif
```
```shell
##Passing a Command to the WBM Container
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
```

At this point, you may have noticed a couple quick notes. One is that accessing the Singularity Instance version of WBM is essentially the same opening an interactive shell. Again, this is the purpose of the instance object- to be able to access the shell at later dates without inviting errors by re-typing the `-B` lines each time. My hope is that any created instance objects will work for you.

The second note is that the third example, passing of a command to the WBM container, includes a call to WBM. Part of the note with this example is to show that commands can be passed directly to the container, which will open the container, run the passed commands, and then close the container. This is most useful if you know your code is already set and just need it to run.

The other part of the second note is that this is our first look specifically at a command invoking WBM. The first portion of the command, `/wbm/model/wbm.pl`, calls the WBM code file. `/wbm/wbm_init/test.init` is the instruction being passed and functions much like any other shell arguments. This line is saying: "run WBM with the `/wbm/wbm_init/test.init` file, and print out console text verbosely (`-v`)". On the surface this may seem simple, but as we will soon learn the correct set up of a initalization (`.init`) file is perhaps the most significant portion of the WBM modeling process.  

WBM works on a series of `.init` files. There is one file which defines the model, `/wbm/wbm_init/test.init` in this case, and a series of `.init` files for almost all model variables. The `.init` files act as a combination of instructions to WBM, model archive, and metadata storage for the model and input data. We will look more in depth at the `.init` once our WBM install has been varified.

**For now, if you have not accessed the WBM container by the passing command example, make sure you have a WBM interactive shell open and please run:**
```shell
/wbm/model/wbm.pl -v /wbm/wbm_init/test.ini
```

Once the model has finished processing, near the end of the printed lines you should see a block of text that looks like this:
```shell
Summary for Year 2000:
Rff-Gl/Precip  Yr = (8.24 / 15.84), 52.03 %
Runoff-Dischg  Yr = (8.24 - 8.21), 0.03 km3
ET Non/Irr/Rfd Yr = (7.60 / 0.00 / 0.00) km3
Surface W Storage =  0.03 km3
Grndwater Storage =  0.65 km3

Y=2000 Spool files added = 0
    Run state is saved.

```
If for some reason you have missed the lines printed in the console or would like to check your verification values in another session, they can be found within this file: `/wbm/wbm_output/WBM_test/test.log`.

If your `Rff-Gl/Precip  Yr`, `Runoff-Dischg  Yr`, `ET Non/Irr/Rfd Yr`, `Surface W Storage`, and/or `Grndwater Storage` values do not matched the above values, something is wrong with your installation. Double check the steps you have taken, and if you are still unable to identify a problem, please contact me at mdl5548@psu.edu.

It is worth noting that, after a successful run, you will receive the following message should you attempt to run the model again:
```shell
Initializtion of Land Cover (including crops) parameters-
Landcover/Cropland collapse method  : none
Init files are saved to:    /wbm/wmb_output/WBM_test/init_files/
Destination dataset is up to date! Nothing to do...
```
This means that the model detected that all of the output files have already been created for the `.init` file provided and are already written to the output directory. In a similar manner, WBM keeps track of where it is in processing, so that should the run be interrupted, the model will pick up where it left off when called again. For this example, the output directory is `/wbm/wmb_output/WBM_test/init_files/`, from the container's perspective.
<br>
<br>

# WBM Example 1: Ipswich River
The Ipswich River is a small river in Massachusetts and flows into the Plume Island Estuary. The lab in which the UNH authors work with have been conducting research at the Plum Island Estuary Long Term Ecosystem Research site for almost 20 years. Due to their familiarity with the area and the relatively small geographic scope, this makes the Ipswich River an excellent example to run as a first full WBM model. We will run a minimalist, least number of variables possible, version of this model.

**The is one important note to be made about ROAR before proceeding!** Up until this point, the time the system has spent running commands have been short. However, with this and the last example, the run times will become much longer - on the magnitude to two to six hours. Due to administrative settings of ROAR, users are limited to about two hours of run time in a `SSH` session for a single command that has not been submitted by a script to the job que. This means that simply `SSH`-ing into ROAR and typing in the commands below will cause walltime errors on ROAR.

There are two solutions for how to submit the commands to ROAR without violating the administrative rules. The first option has already been mentioned, and that is to submit the commands in a script to the job que. This tends to be the most effective method for running finalized code, and you should be able to copy the lines presented here into a script which can be submitted as a job. However, submitting the code as part of a script removes the interactivity of a shell session which is often more beneficial when learning/experimenting with code.

The other solution is to log into ROAR through the ROAR Open OnDemand portal (https://portal2.aci.ics.psu.edu) and launch an interactive desktop session (ACI RHEL7 Interactive Desktop). The interactive desktop provides a GUI for ROAR but does include a shell app for running bash commands. Commands submitted to ROAR through the interactive desktop do tend to run slower than ones submitted though a job script. However, the interactivity and ability to quickly fix mistakes or experiment with your own ideas is a great benefit when learning a new modeling technique.

Before running the model, the data needs to be collected from the UNH FTP site. **If you have just worked though the previous exercise, please close the container.** For ease of reference, it will be assumed the Ipswich example data will be downloaded to the same directory as the WBM `.sif` file. As done with the downloading of the example data for the WBM install, a tar ball will be downloaded and extracted:
```shell
##Download the Ipswich example data
wget --user=ftp ftp://merrimack.sr.unh.edu/wbm_public/ipswich_test.tar.gz

##Extract the contents of the tar ball
tar -xvzf ./ipswich_test.tar.gz
```

Next the WBM container will be accessed. Due to the data presumably being in a different location, remember that the bindings need to be different. If the root directory for both datasets is the same for you, the only change needed would be to replace `/wbm_storage_v1.0.0/` with `/ipswich_test/` in the binding examples presented in the WBM verification section above.

The Ipswich River example adds a couple of steps to the model running process. These steps are optional but highly beneficial to do when setting up a WBM. These steps allow for some error catching before running the WBM and handles several set-up steps so that the overall model runtime is minimalized. The first of these steps is an initialization of the model directory to prepare it to received output.

```shell
##Once the WBM has been started with the new bindings, preform a model initalization
/wbm/model/wbm.pl -v -noRun /wbm/wbm_init/ipswich.init
```
The second step is to precalculate the binary and spool data for the WBM model. To explain briefly, the spool data is a collection of the data that has been prepared for WBM. This process includes automatic spatial clipping, rasterization, resampling, and any needed unit conversions.

If the input files are large or your system is constrained _the spooling process can take a significant amount of time._ Due to this, one of the arguments of the spool script is the number of forks to use, denoted by the `-f` switch. For those unfamiliar with forking, it is a method of multicore processing that is available to Linux systems. To run the code below, please modify `X` with the number of forks (cores) you would like to use during the spooling process. If you are unsure of how many cores you have access to, 3 should work for the typical ROAR log-in.

```shell
##Precalculate binary and spool data,
##fill X with your own value or number of cores to use
/wbm/wbm_output/ipswich_test/build_spool_batch.pl -f X -nc
```

Due to certain adjustments made within the spooling process, it is very likely the data that has been spooled will be slightly different than the input. This is very common when modifying raster data, particularly when those modifications affect cell size and spatial referencing. In the example of Ipswich River, the input data has been minimally affected by the spooling process. **If you would like to see the difference between the original data and the spooled data, run the following code.**
```shell
##Uses GDAL to calculate the difference between original data and spooled data
gdal_calc.py -A netcdf:data/metdata_air.2m_d/1999.nc:air_temperature -B netcdf:spool/Merit_30sec_Ipswich/metdata_air.2m_d/1999.nc:air_temperature --outfile="test.nc" --format="netcdf" --A_band=50 --B_band=50 --calc="A-B"

##Print to console a summary of the resulting GDAL calculation
gdalinfo test.nc -stats
```
The closer your values are to zero, the better. In this case, differences around +/- 1e^6 are expected.

The Ipswich River WBM example is not ready to be ran.  _The WBM modeling process is also likely to take a significant amount of time._
```shell
##Run the WBM
/wbm/model/wbm.pl -v /wbm/wbm_init/ipswich.init
```
Once the model is finished, the result print out / what can be found in the model log file should look something like this:

```shell
Summary for Year 2020:
Rff-Gl/Precip  Yr = (0.31 / 0.45), 68.57 %
Runoff-Dischg  Yr = (0.31 - 0.30), 0.01 km3
ET Non/Irr/Rfd Yr = (0.15 / 0.00 / 0.00) km3
Surface W Storage =  0.00 km3
Grndwater Storage =  0.03 km3

Y=2000 Spool files added = 0
    Run state is saved.
```
You have now ran a full WBM model.
<br>
<br>

# WBM Example 2: Wyoming
The WBM Ancillary Data was originally intended to include two running examples - one the state of Wyoming, and one for a global model. While some files for the intended global model can still be found within the Ancillary Data, too many are missing for it to easily used as a learning example. However, most of the Wyoming data is available within the Ancillary Data  Data package. And the data that was not included is easily obtainable.

However, if you were to just run the Wyoming WBM, it will fail. Beyond the currently missing data, some of the model files have various errors which will prevent WBM from completing. The good news is that this presents a wonderful learning opportunity for us to begin looking at the structure of a WBM model more closely and to begin making some minor changes to make the Wyoming example work.

Before rushing into opening and editing files, the structure of the WBM input files should be described. As mentioned above, the input data for WBM is provided through a series of initiation (`.init`) files. There is one `.init` for the model as a whole, and individual variables may or may not need/have one. The model `.init` file can be thought of as the "master" file, as it contains the overall model parameters as well the connections to the variables.

Variable `.init` files contain a variety of important information about the variable they are associated with. This obviously includes where to find the input data, but also if/how that input data should be subset, whether there should be any unit conversions, as well as variable metadata that WBM uses to create output `NetCDF` files.

### Modifying the _.init_ files
In order to make the required edits, please check your access to an appropriate text editor. My suggestion when on ROAR is Nano, which will be the editor the following instructions assume you are using. If you have a strong preference for another editor, please feel free to use it. However, including instructions for all the possible text editors is not feasible and you will be responsible for any needed code translations.

The majority of edits the Wyoming Example needs are small and often involve making sure that what the various `.init` files are passing along the correct files and/or directories. However, there are a couple more advanced edits that will be made as well in order to begin getting a feel of what make more extensive changes would be like. As one of the edits does include modifying mountings of the Singularity container, **if you have the container open from a previous exercise, please close it now.**

As the model `.init` file acts as the "master file" when WBM is ran, the following editing instructions will be made in the order the items are presented in that file. Edits will occure in the model `.init` file and various individual variable  `.init` files. Due to these two reasons, you may find it beneficial to keep the model file open for the entire process. The model `.init` file is `./wbm_storage_v1.0.0/wbm_init/Wyoming_tracking.init`. All of the variable `.init` files are found in various subdirectories of `./wbm_storage_v1.0.0/data_init/`, for which more details will be provided as needed. To begin, **open the model `.init` file:**
```shell
##Opening the Wyoming WBM example in Nano in order to reference and make changes
nano ./wbm_storage_v1.0.0/wbm_init/Wyoming_tracking.init
```

In exploring the model `.init` file, you may notice that it has some superficial similarities to XML files. Specifically in that both are structured in the _variable_ and _assigned value_ format. From here, many important model level variables are set, such as the model output directory, the spool directory, which variables to create output for, and where / how to find the input variable data.

It may be of interest to know that the model `.init` file is the only one that is needed for WBM to run. The information held within individual variable `.init` files can be copied into the model level file and those files deleted. However, for most models splitting the input over multiple files is recommended from a human readability standpoint.

Feel free to take a few moments to become familiar with the model `.init` file and how it is structured. Once you are ready, proceed with the following steps required for the Wyoming WBM example to run.

**Climate Data**  
Climate data may be among one of the first sets of variables one imagines when thinking about what would be needed to model the movement of water on a landscape. Specifically in the amount of precipitation that may fall. The choice of which climate product to use with WBM will have a significant impact on your results and be among the most important decisions made in this modeling process. It is not the purpose of this document to provide pros and cons of choosing one climate product over another, but keep in mind what can be gained or lost in WBM by choosing from the climate datasets (also known as Climate Drivers by the WBM developers) that are available.

Looking at the Wyoming `.init` file, it can be seen that there are two distinct variables related to climate: `MT_Precip` and `MT_airT`, which are precipitation and air temperature respectively. Each of these variables have their own `.init` file. Beginning with the precipitation `.init` file, we can see that a climate driver has already been selected to be used with this example: MERRA2 (https://gmao.gsfc.nasa.gov/reanalysis/MERRA-2/). A quick look at the data directory, `./wbm_storage_v1.0.0/data/climate/precip/`, will reveal that there is no example data available with for any of the climate variables. The probable reason for this is due to the required climate data file sizes. For anyone not familiar with working with climate datasets, they tend to be quite large and ... unwieldy to process with traditional tools. Therefore, it is completely reasonable that the WBM developers did not include the climate data as it may have been too large for them to easily host.

The situation is complicated in that MERRA2 is not a climate product which is currently available on ROAR. This means that anyone who is persistent in their desire to recreate the Wyoming example will need to collect the MERRA2 data themselves However, due to account space limitations on ROAR, this is unlikely a practical solution.

This guide will present an alternative solution, which is to change the climate driver used. This will significantly change the results of the WBM but will also serve as a starting point for making important changes for your own models. Instead of MERRA2, instruction will be provided in setting up of the Global Climate Models (GCMs) available from the LOCA dataset as the WBM climate driver. The LOCA dataset was selected as it should be easily accessible for everyone in the PCHES project: `/gpfs/group/kaf26/default/public/LOCA/`. For the purposes of this example, the GCM `CCSM4` will be showcased. There is no reasoning for the selection of `CCSM4` beyond that it is the GCM with the least number of characters in its name, and therefore will save some space in this document. Please feel free to use any other GCM you may be interested in.

From the precipitation `.init` file it can be inferred, based on the `File_Path` and `Bands` variables, that the MERRA2 data are saved as daily files. This introduces another challenge as the PCHES copy of the LOCA data is stored as yearly files (365 days in each year). This can be seen in the LOCA file names after navigating to `./LOCA/raw/CCSM4/16th/historical/r6i1p1/pr`. In order for WBM to correctly interpret the new climate dataset, in the precipitation `.init` file, **set the `Bands` variable to 365, and edit the `Var_Name` variable to "pr"**, the variable name within the NetCDF file **In addition, change the file name within the `File_Path` variable to read "pr_day_CCSM4_historical_r6i1p1__YEAR_0101-_YEAR_1231.LOCA_2016-04-02.16th.nc". Finally, change the `Start_Date` variable year to 2000 and the `End_Date` variable year to 2005**, to match the available glacier data. Once these edits are made, save the file.

One astute observation that may have been caught it that there are no LOCA files with `_YEAR_` in its name, for any GCM.`_YEAR_` is a convenient naming variable provided within the WBM framework. It is a convenient method to be able to cycle through files with names that only differ due to some temporal component. `_YEAR_` is the example being utilized here, but this process will also work for `_MONTH_` and `_DAY_`. The value of `_YEAR_` will change depending upon when the WBM is modeling and the available files. The bounds of `_YEAR_` are set within the variable `.init` file, specifically the `Start_Date` and `End_Date` variables.

While the `Start_Date` and `End_Date` variables define the data that is available for a particular model variable, the time period over which the WBM runs is defined within the model `.init` file. There are two sets of dates which are important to keep in mind. The first are the `Run_Start` and `Run_End` variables which are a part of the `MT_Code_Name` variable set. These two variables define the total run modeling dates, or when the WBM modeling should start and should end. To be consistent with the available data, **change the year of `Run_Start` to 2000 and the year of `Run_End` to 2005.**

The second set of dates are the `Start` and `End` variables which are a part of the `Spinup` variable set. `Spinup` defines how WBM should create its initial water quantity state. Essentially WBM runs through the `Spinup` dates so that the landscape is not "empty" of water when the modeling process begins. **Set the `Start` variable to 2000 and the `End` variable to 2002.**

Returning to the climate data, similar edits as those made to precipitation `.init` file will need to be made to the temperature `.init`. Open that file and **set the `Bands` variable to 365, the `Var_Name` variable to "tasmax", and change the `Start_Date` variable year to 2000 and the `End_Date` variable year to 2005. Finally, change the file name within the `File_Path` variable to read "tasmax_day_CCSM4_historical_r6i1p1__YEAR_0101-_YEAR_1231.LOCA_2016-04-02.16th.nc"**. You may have noticed that these instructions do involve changing the temperature variable from a T-mean to a T-max. This will, or course, affect the results, but will not affect the model setup from a technical perspective.

As the climate drive we are choosing the use is location in a different directory on ROAR, we cannot use the same Singularity set up as in the previous exercises. However, this does not mean we need to change the directory structure within the container. Instead, a couple of new mountings will be performed so that the container will know where to look for the data - and we do not get in trouble with ROAR admin for taking up too much space with copied climate data. Below is an example of the changes that would be needed when preforming the `shell` method of accessing the container. **Use this code to modify your preferred method of accessing the container.**
```shell
##Opening an Interactive Shell with the WBM Container
singularity shell \
  -B ./wbm_storage_v1.0.0/data:/wbm/data \
  -B ./wbm_storage_v1.0.0/data_init:/wbm/data_init \
  -B ./wbm_storage_v1.0.0/spool:/wbm/spool \
  -B ./wbm_storage_v1.0.0/wbm_output:/wbm/wbm_output \
  -B ./wbm_storage_v1.0.0/WBM_run_state:/wbm/WBM_run_state \
  -B ./wbm_storage_v1.0.0/wbm_init:/wbm/wbm_init \
  -B ./wbm_storage_v1.0.0/model:/wbm/model \
  -B ./wbm_storage_v1.0.0/utilities:/wbm/utilities \
  -B ./wbm_storage_v1.0.0/gdal_test_files:/wbm/gdal_test_files \
  -B /gpfs/group/kaf26/default/public/LOCA/raw/CCSM4/16th/historical/r6i1p1/pr:/wbm/data/climate/precip \
  -B /gpfs/group/kaf26/default/public/LOCA/raw/CCSM4/16th/historical/r6i1p1/tasmax:/wbm/data/climate/airTemp \
  ./wbm_opensource_v1.0.0.sif
```

**River Network**  
If you were to attempt running `/wbm/wbm_init/Wyoming_tracking.init` in its current state, it will quickly fail. As previously mentioned, this is due to missing data. One of the missing pieces is arguably the most important data layer for WBM to run, that being the river network layer. The river network defines the directional flow of surface water on the landscape. However, in the WBM modeling framework it is also used to define the geographic projection and extent of the analysis, as well as defines raster cell sizes and other spacial scales.

The river network source that UNH suggests in the WBM user instructions is: http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/, and more specifically the Flow Direction layers available there. The Flow Direction layers require the creation of an account in order to obtain the username and password to download. Once the credentials are obtained, the data may then be downloaded in the same method as the WBM container. Example code would look like **(This does not need to be run at this time.)**:
```shell
##Move to the data directory
cd ./wbm_storage_v1.0.0/data

##Double check to make sure that the network directory exists,
##and then move into it
mkdir ./network
cd ./network

##Download an example river network tile
##I specifically chose n30w120 as it would enclude most, if not all, of WY to minimize the number of changes made to the original example. Repeat the download process for as many tiles as you feel you need.
##Replace XX with the given user name and YY with the given password
wget --user=XX --password=YY http://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/distribute/v1.0/dir_n30w120.tar

##Extract the downloaded data
tar -xvf ./dir_n30w120.tar

##Move back to the data directory
cd ..
```
For this example, the river network has already been created. However, the above data source would be a good source for collecting river network data for other projects. For these other projects, keep in mind that that the data is downloaded in tiles and unless your study area is completely contained on a single tile there will need to be some GIS processing in order to get a useable input.

For this exercise, the correct river network needs to be pointed to. This is done within the model `.init` file. The Wyoming river network is not available from the UNH FTP site. However, UNH has provided a copy of this file which has been made available on ROAR at: `/gpfs/group/kaf26/default/private/mdl5548/WBMOfficialContainer_Data/Wyoming_05min_flwdir.asc`. **Find the `Network` variable in the model `.init` file. Replace the current input with the above file.**

**Dams / Reservoirs**  
To state the obvious, dams have a significant impact on the flow of water on a landscape. Within WBM, dams are used to define the location of reservoirs. Within the model `.init` file, it can be seen that the `Reservoirs` variable is set to accept input from a variable `.init` file. **Open the dams variable `.init` file to take a look at it.**

As described above, the dams `.init` file holds all of the information which is needed for WBM to run with this variable. The one of specific interest now is the `File` variable - which points to a file within `.data/dams`. Should one attempt to open this file, it will be found that it is not possible - **the subdirectory does not exist, along with the data that the dams `.init` file is told to expect.** Left unchanged, WBM will not run.

The data source for dams that UNH suggestions in the user instructions can be found here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5YBWWI. This data requires accepting a license to use, and the download link brings up a dialogue asking for how the user would like to download the file. For some reason I have been unable to download this data with _wget_, although example code has been provided. The data is not very large, and my current suggestion is to **download the `HydConDams_Global_2021_v2.0.csv` file manually. Within the `/data` folder, please make the `data/dams` subdirectory and place the data inside.**
 reservoirs when looking at `/wbm/wbm_init/Wyoming_tracking.init`.
```shell
mkdir ./dams
cd ./dams

##wget calls not currently working
##Please download this data manually
#wget https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/5YBWWI/17ad3cd2895-4ca246ca093d.orig?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27HydConDams_Global_2021_v2.0.csv

##Return to the data directory - if needed
cd ..
```
Once the dams data has been downloaded and stored, **double check to make sure that the model `.init` is pointing to the correct variable `.init`, and that the dams `.init` is pointing to the correct data layer.**

**Soil Data**  
WBM does not only model water that happens to be within river networks. The movement of subsurface water can be of equal importance to environmental health as what is happening on the surface. As soil composition has a significant impact on how groundwater flows, it makes sense for it to be included the WBM modeling framework. The soil input for the Wyoming WBM is specifically the typical available water (AW) for various soil types.

When looking at the model `.init` file, the soils AW can be found assigned to the `soilAWCapacity` variable. However, when stepping through the variable `.init` file to see where the data is located, it can be seen that, again, the data file is not available. With that said, there is available soils data, as well as alternative soil `.init` files. And one of those files does link to the available data. Due to this, a change will be made to the model `.init` file to link to the working variable files. **In the Wyoming `.init` file, edit the `soilAWCapacity` variable to read "/wbm/data_init/soil/harmonized_AWC_5min_mer.init".**

**Glacier Data**  
Glaciers are one of the many potential water sources within WBM. While it is beyond the scope of WBM to model the expanding and/or contraction of glaciers, flow data from these sheets of ice can be used to inform an amount of water entering river systems. From within the model `.init` file it can be seen that `Glaciers` is actually a variable group of three inputs. As the purpose of this document is to cover the technical requirements needed to run a WBM file, we will not be covering the intricacies between these variables and what they mean for the model. It can be seen that each of the glacier variables has its own `.init` file. These files can be found in `./wbm_storage_v1.0.0/data_init/glaciers/`. To begin, **please open `./glacier_RDI40_6min_ERA-Interim_hindcast_Discharge_m.init`**.

For the purposes of this example, there are three lines which are the most important. The first is the `File_Path`, which is the directory in which WBM will look for the data files for this variable. Looking at the glacier data directory, the named file, `glacier_RDI40_6min__YEAR__m.nc`, does not exist.

While looking at the file name of the available glacier data, you may have already noticed the issue - the years of the `Start_Date` and `End_Date` do not match the years that are available. The first edit to this model will correct this: **Change the `Start_Date` year to read '2000' and the `End_Date` year to read '2005'.** The 2000 start year matches the first year of available data. The 2005 end year is determined by the available climate data, as described in that section. Once the above edit has been made, **save the file and open `glacier_RDI40_6min_ERA-Interim_hindcast_Volume_y.init`. Make the same edits to this file and save it.**

The last of the glacier `.init` files is `./global_rgi6_glaciers_area_1km.init`. Upon opening this file, it should be noticed that both `Start_Date` and `End_Date` contain an unusual value: "0000-00-00" (formatted as YYYY-MM-DD). Within WBM, this method is used to specify **every or all**. This means that the data layer for this variable will be used at every timestep, or for all timesteps. The method can also be used in part. For example, the value of "2000-12-00" would transplant as to use the variable data for every day for December 2000. As the data referenced in `./global_rgi6_glaciers_area_1km.init` will be used for every timestep, **there is nothing that needs changed with this file.**

**Analysis Mask**  
Conceptually, masking in GIS is a method used to identify locations which should be included in any analysis. Most geographic based tools work on the Minimum Bounding Rectangle (MBR) - or the smallest "box" of coordinates which can be theoretically "drawn" around any set of geographic data, as the default working extent. However, in the real-world irregular shapes often encountered. These irregular shapes may be a set of political boundaries or a collection of delineated watersheds. In either case, there are likely times when using the MBR is inconvenient. Most often this will often involve preforming the analysis on an area much larger than is needed, therefore taking up more time and computing resources. Less common is the case of the MBR for some input not filling the entire area of analysis, therefore posing and issue if the entire analysis area is not filled. The purpose of a mask is to override the MBR default to allow the user to define the area that best suits their needs.

The Wyoming `.init` file includes has mask arguments set under the variable group `Runoff_mask`. When checking on the `maskFile` and `attFile` variables, these files are also missing. UNH does have the masking files available, but on a different FTP site that the one previously connected to. **Run the following code to create a new masks directory and the download the masking files into it.**
```shell
mkdir ./masks
cd ./masks

##Using wget to connect the the UNH Public FTP server and download the mask data
##First is the mask ASCII file
wget --user=ftp ftp://merrimack.sr.unh.edu/wbm_public/state_6min.ascii

##Second is a supporting state text file
wget --user=ftp ftp://merrimack.sr.unh.edu/wbm_public/state.txt

##Return to the data directory
cd ..
```
Once the files have been downloaded, no other changes should need to be made with thw variables.

**Irrigation / Crop Data**  
Irrigation, particularly in drier environments such as the American West, have a huge image on how water moves across a landscape. It could arguably be the most impactful modifications humans can make to the water cycle. This is further modified by the types of crops being grown, as different crops have differing water needs. It is unsurprising then that both irrigation values and crop water use are important inputs for WBM. To highlight the importance of irrigation, within the Wyoming `.init` file, the `Irrigation` variable set has more input variables than any other set.

In a similar situation as described in the above **Soils** section, data for the `Irrigation` variable set was included with the files downloaded from UNH but adjustments need to be made to make sure the correct variable `.init` files are being pointed to. **For the `IrrEfficiency` variable, change the input to read "/wbm/data_init/irrigation/Merra_irrigationGross_Freq_dc.init". Open the `/wbm/data_init/crops/mirca_cropAreaFraction.init` file. In the variable file, edit the `File_Path` variable to read "/wbm/data/crops/crop_area_fraction.nc;".**

# The End
At this point, all of the modification required for the Wyoming example to run on ROAR have been made. You may go ahead and run the example now if you wish. **Do expect four to six hours for WBM to run this example.**

If you have any questions, please feel free to contact me at: mdl5548@psu.edu.

Happy Modeling!
