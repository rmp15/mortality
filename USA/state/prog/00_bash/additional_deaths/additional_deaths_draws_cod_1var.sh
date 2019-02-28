#!/bin/bash

# this script
# takes the INLA age-separated model and makes selected number of draws

clear

declare -a sexstrings=('male' 'female')
declare -a models=(10)
declare -i start=1980
declare -i end=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -a cods=("Unintentional")
declare -i fast=1
declare -i contig=1
declare -a draws=(5000) #5000) also 5000 when running fully

#################################################
# 1. PROCESS DRAWS
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(

for model in "${models[@]}"; do

echo "Preparing $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_process_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws;

done;

) &