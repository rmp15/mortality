#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(35 75)
declare -a sexes=(1)
declare -a sexstrings=('male' 'female')
declare -i model=2
declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal.R $age $sex $start $end $model 0

done; done;

#################################################
# 2. COMBINE RESULTS
#################################################

echo "combining results into one file from INLA model $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior.R $start $end $country 2

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
