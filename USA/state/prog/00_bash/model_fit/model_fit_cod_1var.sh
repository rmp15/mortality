#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(15 5 0)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -a models=(10)
declare -i start=1980
declare -i end=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1
declare -i pw=0
declare -a cods=("AllCause" "Cancer" "Cardiopulmonary" "External" "Other" "Other_cardiovascular_diseases" "Other_respiratory_diseases" "Ischaemic_heart_disease" "Cerebrovascular_disease" "Respiratory_infections" "Chronic_obstructive_pulmonary_disease" "Unintentional" "Unintentional_wo_drowning" "Intentional" "Transport_accidents" "Intentional_self-harm" "Accidental_falls" "Accidental_drowning_and_submersion" "Assault" "Other_external_causes_of_injury")


#################################################
# 1. ERROR STATISTICS
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(

for model in "${models[@]}"; do
for cod in "${cods[@]}"; do
echo "establishing error statistics $model years $start - $end for $cod";

Rscript ~/git/mortality/USA/state/prog/fitted_against_raw/fitted_against_raw_cod_1var.R $start $end $country $model $dname $metric $cod $contig;

done; done;

) &