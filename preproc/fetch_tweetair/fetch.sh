#!/bin/bash

# src global def
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# clean if needed & load 
module purge
module load Lmod/6.5 
source $LMOD_PROFILE 
module load intel/2017a R/3.4.3

# define home
home="/mnt/flock/cr2/vlemaire/proj/collect-tweets/"
dday=`date '+%Y-%m-%d_%H'`

# fetch tweets 
cd $home 

R -quiet --no-save  << EOF > "logs/${dday}.Rlog" 2> "logs/${dday}.Rlog" 
source("fetch.R")
q()
EOF


