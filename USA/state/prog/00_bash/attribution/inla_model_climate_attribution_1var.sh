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
declare -a metrics=('number_of_days_above_nonnormal_90' 'days_changing_by_5' 'meanc3' '10percc3' '90percc3' 'number_of_min_3_day_above_nonnormal_90_upwaves_2' 'number_of_min_3_day_above_+5_jumpupwaves_2' 'number_of_min_3_day_below_nonnormal_90_downwaves_2' 'number_of_min_3_day_below_+5_jumpdownwaves_2')
declare -i start2=1979
declare -i end2=2015

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

echo "starting attribution for 1 var model for $metric";

# runs model
Rscript ~/git/mortality/USA/state/prog/attribution/attribution_1var.R $start $end $country $model $dname $metric $start2 $end2


done; done;


