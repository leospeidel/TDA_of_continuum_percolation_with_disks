# TDA of continuum percolation with disks

This repository contains the scripts used for generating the figures in 

Leo Speidel, Heather A. Harrington, S. Jonathan Chapman, Mason A. Porter.<br>
Topological data analysis of continuum percolation with disks.<br>
[Physical Review E 98, 012318 (2018)](https://journals.aps.org/pre/abstract/10.1103/PhysRevE.98.012318)

Please make sure that Dionysus is installed, and change the path in the scripts to point to your version of Dionysus.<br>
(see [here](https://www.mrzv.org/software/dionysus/) for how to install Dionysus)

# Figs. 3 and 4

You can plot these figures by changing into the analysis/ directory and executing
./plot.sh
from this directory.

## Fig. 3a

Change directory to analysis/birth\_persistence and execute
./generate\_birth\_persistence.sh

Note that there is a precomputed file 2d\_N10000.TDA for N=1e4 disks. <br> 
We use N=1e7 in our paper.<br>
You can change the number of disks in the script.<br>

## Fig. 3b

Change directory to analysis/top\_persisting/ and execute
./run\_TDA.sh

Precomputed values used in for the plot in the paper are under analysis/top\_persisting/TDA_*/ <br>
You can change the number of disks as well as the number of samples in the script.

## Fig. 4

Change directory to analysis/estimate\_constants/ and execute
./run\_TDA.sh

Precomputed values used in for the plot in the paper are under analysis/estimate\_constants/TDA_*/ <br>
You can change the number of disks as well as the number of samples in the script.

