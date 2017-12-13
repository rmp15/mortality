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
declare cod="External"

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for dname in "${dnames[@]}"; do

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_1var.R $start $end $country $model $dname $metric $cod

done; done; done;
