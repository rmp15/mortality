#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(75)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -a models=(11)
declare -i start=1982
declare -i end=1991
declare country="USA"
declare dname="t2m"
declare -a metrics=("number_of_min_3_day_above_99_upwaves")
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
<<<<<<< HEAD
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R $age $sex $start $end $model 0 $dname $metric
=======
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R $age $sex $start $end $model 0 $dname $metric &
>>>>>>> 6c74e4dfa41df8f910b8df1c6e9142043167f43c

done; done; done; done;

(
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 75 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 75 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 65 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 65 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 55 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 55 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 45 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 45 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 35 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 35 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 25 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 25 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 15 1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 15 2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 5  1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 5  2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 0  1 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 0  2 1982 2013 11 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 75 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 75 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 65 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 65 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 55 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 55 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 45 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 45 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 35 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 35 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 25 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 25 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 15 1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 15 2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 5  1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 5  2 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 0  1 1982 2013 10 0 't2m' 'meanc' ;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_fast.R 0  2 1982 2013 10 0 't2m' 'meanc' ;
) &

#################################################
# 2. COMBINE RESULTS
#################################################

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate.R $start $end $country $model $dname $metric

done;

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

#echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
