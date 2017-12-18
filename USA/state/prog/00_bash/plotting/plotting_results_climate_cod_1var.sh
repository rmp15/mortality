#!/bin/bash

# this script
# plots results of posterior climate terms of model

cd ~/git/mortality/USA/state/prog/00_bash/

clear

declare -i start=1980
declare -i end=2013
declare -a models=(10)
declare country="USA"
declare -a dnames=("t2m")
declare -a metrics=('meanc3')
declare -a cods=("AllCause" "External" "Cancer" "Cardiopulmonary" "Other")

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for cod in "${cods[@]}"; do

for dname in "${dnames[@]}"; do

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod.R $start $end $country $model $dname $metric $cod
Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_1var.R $start $end $country $model $dname $metric $cod

done; done; done; done;