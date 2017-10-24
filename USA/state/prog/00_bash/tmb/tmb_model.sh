#!/bin/bash

# this script
# runs the TMB age-separated spatial model


clear

declare -a ages=(75)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -i type=2
declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. RUN AGE-SEPARATED TEMPORAL MODEL
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age TMB model $type years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/TMB/01_temporal/01_single_age/03_added_month_slope/temporal_rw1_drift.R ## ARGUEMENTS TO FIX ##

done; done;

#################################################
# 2. RUN AGE-SEPARATED SPATIAL MODEL
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age TMB model $type years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/TMB/02_spatial/01_single_age/bym_1.0.R ## ARGUEMENTS TO FIX ##

done; done;



