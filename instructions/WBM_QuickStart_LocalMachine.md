**[Getting started]{.underline}**

WBM is best suited to large servers with sufficient memory, but a simple
demonstration of a basic rudimentary installation is feasible on a
personal computer. WBM runs in a Linux environment with numerous
dependencies. The Singularity image prepared for WBM has the necessary
software dependencies already included. Singularity also runs in a Linux
environment. There are numerous approaches to setting up a Linux
environment through Windows or Mac OS: Windows Subsystem for Linux,
VirtualBox, dual boot, etc. Alternatively, if you will be accessing WBM
on a Linux server, you need only have an appropriate terminal (e.g.
CygWin for Windows, or XTerm) and appropriate security credentials.

The following is a basic approach for installing and accessing
Singularity on Windows.

We start by following the guide for installing singularity in the
Administrative Guide located at:

<https://docs.sylabs.io/guides/3.7/admin-guide/installation.html#windows>

Install git bash, virtual box, vagrant for windows

(We have tested without downloading the *vagrant manager* \[which is
suggested as necessary by the guide\] since windows gives a warning of a
potential security risk.)

Initialize a vagrant project:

From git bash make a directory to keep virtual machine files localized.
For instance, singularity documentation suggests the folder name \<user
home\>/vm-singularity/. This folder will be mapped as the folder
/vagrant/ from within the vagrant machine.

In the terminal:

vagrant init sylabs/singularity-**3.7**-ubuntu-bionic64

vagrant up

vagrant ssh

NOTE: The directions for version 3.7 at the above link installs
singularity version 3.5. WBM has only been tested with Singularity
v\>3.7.1.

When you want to delete a Vagrant box, and try to (for instance) install
the proper version ... delete the vagrant file, delete the hidden
.vagrant directory, and you also need to open up virtual box, close the
running virtual machine \> shutdown, then you need to remove the virtual
machine.

Using vagrant, the root folder where you stared your Vagrantfile is
located at /vagrant (not /home/vagrant which is your "vagrant" user home
directory.)

Other findings \-\-- The base memory for the Virtual Machine created by
vagrant was too low to compile WBM. A simple fix was to exit the
Singularity and vagrant instances, shutdown the VM in virtual box,
increase the base memory allocation, then restart vagrant with:

vagrant up && vagrant ssh

Alternatively, this appears to be a setting that can be modified in the
Vagrantfile.

Now refer to the instruction manual for setting up the data directories
from the root directory of your virtual machine folder.

For a minimal test case suitable for a personal computer download the
minimal test case from the Water Systems Analysis Group website.

wget https://wbm.unh.edu/resources/ipswich_test.tar.gz

Extract the test directory archive:

tar -xvzf ./ipswich\_test.tar.gz

A listing of the directory should look like:

ls ./ipswich\_test

\\\> build\_spool\_batch.pl data/ data\_init/ gdal\_test\_files/ model/\
spool/ utilities/ wbm\_init/ wbm\_output/ WBM\_run\_state/

Copy the UNH singularity container to the /vagrant root directory.

cd /vagrant

wget https://wbm.unh.edu/v1.0.0/wbm\_opensource\_v1.0.0.sif

Then create a singularity instance as follows:

singularity instance start -B /vagrant/ipswich\_test/:/wbm/
/vagrant/wbm\_opensource\_v1.0.0.sif wbm\_os\_instance1

Now open a singularity shell and change directory to the root directory
of the test case provided:

singularity shell instance://wbm\_os\_instance1

cd /wbm/

We are now going to initialize the test model run by calling the model
with the flag "-noRun". The model will initialize, write an output
directory, and important file called build\_spool\_batch.pl.

model/wbm.pl -v -noRun wbm\_init/ipswich.init

Change directory to the output file. Call the build spool batch file
specifying the number of forks to use, and requesting that NetCDF output
of the spooled data is created in addition to the binary files that wbm
references during run time.

./build\_spool\_batch.pl -f 4 -nc

Note: The number of forks is the number of concurrent processes that the
script will call. 4 is a pretty low number even for a modern laptop, and
if you are deploying the script on a server with multiple cpus, call as
many forks as you have cores available for processing.

build\_spool\_batch will pre-calculate the binary data that wbm needs to
run. This step is not strictly necessary, especially on such a minimal
model run, but it is a good practice. One thing to check to make sure
that things are likely working properly is to see if the data packaged
here spooled properly. (The data in the package was built using the
build\_spool utility at UNH; so if there is minimal difference between
the data and spooled data -- things are looking good.)

gdal\_calc.py -A netcdf:data/metdata\_air.2m\_d/1999.nc:air\_temperature
-B
netcdf:spool/Merit\_30sec\_Ipswich/metdata\_air.2m\_d/1999.nc:air\_temperature
\--outfile=\"test.nc\" \--format=\"netcdf\" \--A\_band=50 \--B\_band=50
\--calc=\"A-B\"

gdalinfo test.nc -stats

The statistics should report that range of differences is approximately
0. Differences on the order of $\pm$1e^-6^ are reasonable.

Now you are ready to run the model:

model/wbm.pl -v wbm\_init/ipswich.init
