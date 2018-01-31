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
declare -a metrics1=('meanc3')
declare -a metrics2=('number_of_days_above_nonnormal_90_2' 'number_of_days_below_nonnormal_90_2')
declare -a cods=("AllCause")
#declare -a metrics2=('number_of_days_above_nonnormal_90_2' 'number_of_days_below_nonnormal_90_2' 'sd')
#declare -a cods=("AllCause" "External" "Cancer" "Cardiopulmonary" "Other")

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

for cod in "${cods[@]}"; do
for dname in "${dnames[@]}"; do
for metric1 in "${metrics1[@]}"; do
for metric2 in "${metrics2[@]}"; do
for model in "${models[@]}"; do

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod_2var.R $start $end $country 10 $dname $metric1 $metric2 $cod
Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_2var.R $start $end $country $model $dname $metric1 $metric2 $cod

done; done; done; done; done;