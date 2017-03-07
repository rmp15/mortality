#!/bin/bash

# this script
# plots results of posterior of model

clear

declare -a ages=(0 5 15 25 35 45 55 65 75 85)
declare -a sexes=('male' 'female')
declare -i start=1982
declare -i end=2013
declare -i model=2
declare country="USA"

# this will disappear once sorted out
#Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior.R $start $end $country $model

#################################################
# 1. PLOT PARAMETERS OF POSTERIOR
#################################################

for age in "${ages[@]}"; do

for sex in "${sexes[@]}"; do

Rscript ~/git/mortality/USA/state/prog/parameters_posterior/parameters_posterior_2.0.R $age $sex $start $end $model

done; done;

#################################################
# 2. PLOT 1
#################################################

# PLOT SOMETHING

#################################################
# 3. PLOT 2
#################################################

# PLOT SOMETHING ELSE

#################################################
# 4. PLOT 3
#################################################

# PLOT SOMETHING ELSE AGAIN
