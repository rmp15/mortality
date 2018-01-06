#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(15 5 0)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -a models=(10)
declare -i start=1980
declare -i end=2013
declare -i start2=1980
declare -i end2=2013
declare country="USA"
declare dname="t2m"
declare -a metrics1=("meanc3")
declare -a metrics2=("number_of_days_below_nonnormal_90_2") # also DB10 and SD
declare cods=("Other")

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/



for cod in "${cods[@]}"; do
for metric1 in "${metrics1[@]}"; do
for metric2 in "${metrics2[@]}"; do
for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod_2var.R $start2 $end2 $country 10 $dname $metric1 $metric2 $cod &

done; done; done; done;

