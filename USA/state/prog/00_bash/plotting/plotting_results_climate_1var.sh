#!/bin/bash

# this script
# plots results of posterior climate terms of model

clear

declare -i start=1980
declare -i end=2013
declare -a models=(10)
declare country="USA"
declare -a dnames=("t2m")
declare -a metrics=('meanc3')
declare -a metrics=("10percc3" "90percc3" "meanc3" "number_of_min_3_day_above_+5_jumpupwaves_2" "number_of_min_3_day_above_nonnormal_90_upwaves_2" "number_of_min_3_day_below_+5_jumpdownwaves_2" "number_of_min_3_day_below_nonnormal_90_downwaves_2")

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for dname in "${dnames[@]}"; do

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate.R $start $end $country $model $dname $metric

done; done; done;
