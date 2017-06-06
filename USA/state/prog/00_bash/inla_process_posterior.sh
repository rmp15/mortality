#!/bin/bash

# this script
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a models=(10)
declare -i start=1980
declare -i end=2013
declare country="USA"
declare dname="t2m"
#declare -a metrics=('number_of_min_3_day_below_nonnormal_90_downwaves' 'number_of_min_3_day_below_+5_jumpdownwaves' 'number_of_min_3_day_above_nonnormal_95_upwaves' 'number_of_min_3_day_above_nonnormal_90_upwaves' 'number_of_min_3_day_above_+5_jumpupwaves' 'number_of_days_above_nonnormal_90' 'meanc3' 'meanc' 'days_changing_by_5' '90percc3' '10percc3' 'number_of_min_5_day_above_+5_jumpupwaves_2')

declare -a metrics=('number_of_min_5_day_below_+5_jumpdownwaves_2')

#################################################
# 1. COMBINE RESULTS
#################################################

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate.R $start $end $country $model $dname $metric

done; done;

#################################################
# 2. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate.R $start $end $country $model $dname $metric

done; done;
