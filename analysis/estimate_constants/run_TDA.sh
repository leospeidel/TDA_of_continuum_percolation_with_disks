#!/bin/bash

PATH_TO_DIONYSUS="../../../../TDA/Dionysus/"

maxjobs=10 #number of jobs in parallel

mkdir data

d=2

for N in 10000 100000 1000000 10000000
do

  mkdir -p TDA_$N

  single_run () {
    Rscript ../../Rscripts/generate_configurations.R $d $N data/2d_N${N}_$1.txt 
    ${PATH_TO_DIONYSUS}/build/examples/alphashapes/alphashapes2d < data/2d_N${N}_$1.txt > TDA_$N/2d_N${N}_$1.TDA 
    Rscript GetBettiNumber.R $N TDA_$N/2d_N${N}_$1.TDA 
    rm data/2d_N${N}_$1.txt 
  }


  parallelize () {
    while [ $# -gt 0 ] ; do
      jobcnt=(`jobs -p`)
      if [ ${#jobcnt[@]} -lt $maxjobs ] ; then
        single_run $1 & 
        shift 
      fi
    done
    wait
  }

  LIST=`seq 0 50`
  parallelize $LIST

done

