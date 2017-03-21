#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(85 65 55 45 35 25 15 5 0 75)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -a models=(8 9 10 11 12 7)
declare -i start=1982
declare -i end=2013
declare country="USA"
declare dname="t2m"
#declare metric="mean"
declare metric="number_of_min_3_day_above_99_upwaves"
#declare metric="sd"
#declare -a knotl=(5 10 15)
#declare -a knoth=(20 25 30)

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

for model in "${models[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, with climate variable $metric $dname, years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate.R $age $sex $start $end $model 0 $dname $metric

done; done; done;


#################################################
# 2. COMBINE RESULTS
#################################################

#echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior.R $start $end $country 2

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

#echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
