#!/bin/bash

# this script
# plots results of posterior climate terms of model

clear

declare -i start=1982
declare -i end=2013
declare -a models=(10 11 12)
declare country="USA"
declare -a dnames=("t2m")
declare -a metrics=("sd")

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for dname in "${dnames[@]}"; do

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate.R 1982 1991 $country $model $dname $metric

done; done; done;
