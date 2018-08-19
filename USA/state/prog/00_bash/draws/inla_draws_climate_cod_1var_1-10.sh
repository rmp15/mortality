#!/bin/bash

# this script
# takes the INLA age-separated model and makes selected number of draws

clear

declare -a sexstrings=('male' 'female')
declare -a models=(18) #10) not sure if model 18 or model 10 is the authoritative model yet
declare -i start=1980
declare -i end=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -a cods=("Unintentional" )
declare -i fast=1
declare -i contig=1
declare -a draws=(1000) #5000) also 5000 when running fully

#################################################
# 1. MAKE DRAWS
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(

#declare cod="Transport_accidents"

for cod in "${cods[@]}"; do
for model in "${models[@]}"; do
for draw in "${draws[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/draws/draws_inla_climate_cod_1var.R $start $end $country $model $dname $metric $cod $contig $draw;

done; done; done;

) &