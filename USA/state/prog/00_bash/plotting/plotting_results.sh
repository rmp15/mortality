#!/bin/bash

# this script
# plots results of posterior of model

clear

declare -a ages=(85)
declare -a sexes=('male' 'female')
declare -i start=1982
declare -i end=2013
declare -i model=2
declare country="USA"

# this will disappear once sorted out
#Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior.R $start $end $country $model

#################################################
# 1. PLOT PARAMETERS OF NAT POSTERIORS
#################################################

for age in "${ages[@]}"; do

for sex in "${sexes[@]}"; do

:

Rscript ~/git/mortality/USA/state/prog/parameters_posterior/parameters_posterior_nat.R $age $sex $start $end $model

done; done;

#################################################
# 1. PLOT PARAMETERS OF STATE POSTERIORS
#################################################

for age in "${ages[@]}"; do

for sex in "${sexes[@]}"; do

:

Rscript ~/git/mortality/USA/state/prog/parameters_posterior/parameters_posterior_state.R $age $sex $start $end $model

done; done;

