#!/bin/bash

PATH_TO_DIONYSUS="../../../../TDA/Dionysus/"

d=2
N=10000
Rscript ../../Rscripts/generate_configurations.R $d $N 2d_N${N}.txt
${PATH_TO_DIONYSUS}/build/examples/alphashapes/alphashapes2d < 2d_N${N}.txt > 2d_N${N}.TDA  
