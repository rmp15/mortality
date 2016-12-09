#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(75)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -i start=1982
declare -i end=2013
declare -a pwls=(1 2)
declare -i type=2
declare -i forecast_length=5
declare -a knots=(1 2 3 4 5 6 7 8 9 10)
declare country="USA"

#################################################
# 0. RUN AGES TOGETHER NATIONAL MODEL
#################################################

# run no pwl

for sex in "${sexes[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_nat_allages.R $sex $start $end 1 $type $forecast_length 0 &

done;

# run pwl

for sex in "${sexes[@]}"; do
for knot in "${knots[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, pwl with knot $knot years before forecast, forecast length $forecast_length, years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_nat_allages.R $sex $start $end 2 $type $forecast_length $knot &

done; done;

#################################################
# 1. RUN AGE-SEPARATED NATIONAL MODEL
#################################################

# run no pwl

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_nat.R $age $sex $start $end 1 $type $forecast_length 0 &

done; done;

# run pwl

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do
for knot in "${knots[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, pwl with knot $knot years before forecast, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_nat.R $age $sex $start $end 2 $type $forecast_length $knot &

done; done; done;

#################################################
# 2. RUN AGE-SEPARATED STATE MODEL
#################################################

# run no pwl

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age INLA model $model, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_state.R $age $sex $start $end 1 $type $forecast_length 0 &

done; done;

# run pwl

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do
for knot in "${knots[@]}"; do


echo "starting ${sexstrings[$sex-1]} $age INLA model $model, pwl with knot $knot years before forecast, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/inla_spatiotemporal_forecast_state.R $age $sex $start $end 2 $type $forecast_length $knot &

done; done; done;

#################################################
# 2. COMBINE RESULTS
#################################################

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior.R $start $end $country 2

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

echo "plotting parameters from INLA model $model years $start - $end";

# PLOT PARAMETERS CODE
