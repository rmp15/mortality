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
declare -a metrics=("number_of_min_3_days_above_nonnormal_90_upwaves" "number_of_days_above_nonnormal_90" "number_of_min_3_days_above_nonnormal_95_upwaves")

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
