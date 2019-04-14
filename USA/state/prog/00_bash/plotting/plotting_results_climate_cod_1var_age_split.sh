#!/bin/bash

# this script
# plots results of posterior climate terms of model

cd ~/git/mortality/USA/state/prog/00_bash/

clear

declare -i start=1980
declare -i end=2016
declare -a models=(11 10)
declare country="USA"
declare -a dnames=("t2m")
declare -a metrics=('meanc3')
#declare -a cods=("Other_cardiovascular_diseases" "Other_respiratory_diseases") # "Ischaemic_heart_disease" "Cerebrovascular_disease" "Respiratory_infections" "Chronic_obstructive_pulmonary_disease" "External" "Unintentional" "Unintentional_wo_drowning" "Intentional" "Transport_accidents" "Intentional_self-harm" "Accidental_falls" "Accidental_drowning_and_submersion" "Assault" "Other_external_causes_of_injury")
#declare -a cods=("Cardiopulmonary" "AllCause" "Cancer" "External" "Other" "Unintentional" "Intentional")
declare -a cods=("Ischaemic_heart_disease" "Cerebrovascular_disease" "Chronic_obstructive_pulmonary_disease" "Respiratory_infections")
declare -i contig=1
declare -i pw=0
declare -i break=65

#################################################
# 1. PLOT PARAMETERS OF STATE CLIMATE POSTERIORS
#################################################

for cod in "${cods[@]}"; do

for dname in "${dnames[@]}"; do

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod_1var_age_split.R $start $end $country $model $dname $metric $cod $contig $pw $break
#Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_1var_age_split.R $start $end $country $model $dname $metric $cod $contig $pw $break

#################################################
# 2. PLOT RAW AGAINST FITTED DEATH RATES AND RESIDUALS
#################################################

#Rscript ~/git/mortality/USA/state/prog/fitted_against_raw/fitted_against_raw_cod_1var.R $start $end $country $model $dname $metric $cod $contig
#Rscript ~/git/mortality/USA/state/prog/residuals/residuals_cod_1var.R $start $end $country $model $dname $metric $cod $contig

done; done; done; done;
