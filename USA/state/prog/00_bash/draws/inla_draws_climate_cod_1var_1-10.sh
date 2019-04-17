#!/bin/bash

# this script
# takes the INLA age-separated model and makes selected number of draws

clear

declare -a sexstrings=('male' 'female')
declare -a models=(11)
declare -i start=1980
declare -i end=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
#declare -a cods=("Ischaemic_heart_disease" "Cerebrovascular_disease" "Respiratory_infections" "Chronic_obstructive_pulmonary_disease" "Other_cardiovascular_diseases" "Other_respiratory_diseases") #"External" "Unintentional" "Unintentional_wo_drowning" "Intentional" "Transport_accidents" "Intentional_self-harm" "Accidental_falls" "Accidental_drowning_and_submersion" "Assault" "Other_external_causes_of_injury")
declare -a cods=("Cardiopulmonary")
declare -i fast=1
declare -i contig=1
declare -a draws=(100)

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

Rscript ~/git/mortality/USA/state/prog/draws/draws_inla_climate_cod_1var.R $start $end $country $model $dname $metric $cod $contig $draw;

done; done; done;

) &