#!/bin/bash

# this script
# plots results of posterior climate terms of model

clear

declare -i start=1980
declare -i end=2013
declare -a models=(10)
declare country="USA"
declare -a dnames=("t2m")
declare metric1="meanc3"
declare metric2="number_of_days_above_nonnormal_90_2"
declare metric3="number_of_days_below_nonnormal_90_2"

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for dname in "${dnames[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_3var.R $start $end $country $model $dname $metric1 $metric2 $metric3

done; done;
