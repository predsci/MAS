<img height=125 src="doc/mas_logo.png" alt="MAS"/> <img height=125 src="doc/mas_logo_text.png" alt="MAS"/>
  
# MAS: Magnetohydrodynamic Algorithm outside a Sphere  

[Predictive Science Inc.](https://www.predsci.com)  
  
--------------------------------  
  
## OVERVIEW ##

<img align="right" height=200 src="doc/mas_bast_vis_sml.png" alt="MAS"/>`MAS` is a Fortran code that integrates the time-dependent resistive thermodynamic magnetohydrodynamic (MHD) equations in three-dimentional spherical coordinates and is used extensively in models of coronal structure, coronal dynamics, and coronal mass ejections out to the Earth. It is the primary MHD model in CORHEL (Corona-Heliosphere), a suite of models for describing the solar corona and inner heliosphere and CORHEL-CME, an interface-based system for modeling CME eruptions from the Sun to Earth, both of which are available for public runs-on-demand at NASA's Community Coordinated Modeling Center.  

MAS can be used with MacOS, Linux, and Windows (through WSL) on CPUs and NVIDIA GPUs. 


--------------------------------
  
## HOW TO BUILD MAS ##
  
Create or select a build configuration file in the `conf/` folder. 
Then, run `./build.sh ./conf/<CONF_FILE>.conf` where `<CONF_FILE>.conf` is the configuration file for your system.
  
See the multiple build configuration files in the `conf/` folder for examples.

 
### RUN THE MAS TESTSUITE ###

Coming soon ...

--------------------------------
  
## HOW TO RUN MAS ##
  
### Setting Input Options ###

MAS uses a namelist in an input text file to set all non-default parameters of a run.  

### Launching the Code ###
  
To run `MAS`, set the desired run parameters in a text file, then copy or link the `mas` executable into the same directory as teh input file  
and run the command:  
  `<MPI_LAUNCHER> -np <N> ./mas `  
where `<N>` is the total number of MPI ranks to use (typically equal to the number of CPU cores) and `<MPI_LAUNCHER>` is your MPI run command (e.g. `mpiexec`,`mpirun`, `ibrun`, `srun`, etc).  
For example:  `mpiexec -np 1024 ./mas`
  
--------------------------------


