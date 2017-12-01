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
declare -i end=2013
declare -i start2=1980
declare -i end2=2013
declare country="USA"
declare dname="t2m"
declare metric1="number_of_days_above_nonnormal_90_2"
declare metric2="number_of_days_below_nonnormal_90_2"

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

(

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 85 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 85 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 75 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 75 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 65 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 65 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 55 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 55 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 45 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 45 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 35 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 35 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 25 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 25 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 15 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R 15 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R  5 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R  5 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R  0 2 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_2var_fast.R  0 1 $start $end 10 0 $dname $metric1 $metric2 $start2 $end2;

) &

for metric in "${metrics[@]}"; do

for model in "${models[@]}"; do

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, with climate variable $metric $dname, years $start2 - $end2";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate.R $age $sex $start $end $model 0 $dname $metric $start2 $end2
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R $age $sex $start $end $model 0 $dname $metric $start2 $end2
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_faster.R $age $sex $start $end $model 0 $dname $metric $start2 $end2

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
