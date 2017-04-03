#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(85 75 65 55 45 35 25 15 0)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -a models=(10 11 12 13)
declare -i start=1982
declare -i end=2013
declare country="USA"
declare dname="t2m"
declare -a metrics=("meanc" "sd")
#declare metric="number_of_min_3_day_above_99_upwaves"
#declare -a knotl=(5 10 15)
#declare -a knoth=(20 25 30)

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for metric in "${metrics[@]}"; do

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

for model in "${models[@]}"; do

#echo "starting ${sexstrings[$sex-1]} $age INLA model $model, with climate variable $metric $dname, years $start - $end";
:
# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate.R $age $sex $start $end $model 0 $dname $metric
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R $age $sex $start $end $model 0 $dname $metric

done; done; done; done;


#################################################
# 2. COMBINE RESULTS
#################################################

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate.R $start $end $country $model $dname $metric

done;

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

#echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
