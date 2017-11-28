#!/bin/bash

# this script
# runs attributable deaths
# binds the posterior results together
# plots the parameters of the model run

clear

declare -i start=1980
declare -i end=2013
declare country="USA"
declare -a models=(10)
declare dname="t2m"
declare -a metrics1=('meanc3')
declare -a metrics2=("number_of_days_above_nonnormal_90_2")
declare -a metrics3=("number_of_min_3_day_above_nonnormal_90_upwaves_2")
declare -i start2=1979
declare -i end2=2015

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for metric1 in "${metrics1[@]}"; do
for metric2 in "${metrics2[@]}"; do
for metric3 in "${metrics3[@]}"; do

for model in "${models[@]}"; do

echo "starting attribution for 1 var model for $metric";

# runs model
Rscript ~/git/mortality/USA/state/prog/attribution/attribution_3var.R $start $end $country $model $dname $metric1 $metric2 $metric3 $start2 $end2

done; done; done; done;


