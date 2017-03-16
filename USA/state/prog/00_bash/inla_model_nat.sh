#!/bin/bash

# this script
# runs the INLA age-separated models
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(85 75 65 55 45 35 25 15 5 0)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -i start=1982
declare -i end=2013
declare -i type=2
declare -a month_dists=(1)
declare -a diststrings=('rw1' 'iid')
declare -a month_cyclics=(1)
declare -a cyclicstrings=('ncyclic' 'cyclic')
declare country="USA"

#################################################
# 0. RUN AGES TOGETHER NATIONAL MODEL
#################################################

# run no pwl

#for sex in "${sexes[@]}"; do

#clear
#echo "starting ${sexstrings[$sex-1]} $age INLA model $type, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_forecast_nat_allages.R $sex $start $end 1 $type $forecast_length 0 &

#done;

# run pwl

#for sex in "${sexes[@]}"; do
#for knot in "${knots[@]}"; do

#clear
#echo "starting ${sexstrings[$sex-1]} $age INLA model $type, pwl with knot $knot years before forecast, forecast length $forecast_length, years $start - $end";

# runs model
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_forecast_nat_allages.R $sex $start $end 2 $type $forecast_length $knot &

#done; done;

#################################################
# 1. RUN AGE-SEPARATED NATIONAL MODEL
#################################################

# run no pwl

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do
for month_dist in "${month_dists[@]}"; do
for month_cyclic in "${month_cyclics[@]}"; do

clear
echo "starting ${sexstrings[$sex-1]} $age INLA model $type, month ${diststrings[$month_dist-1]}, ${cyclicstrings[$month_cyclic]}, forecast length $forecast_length, years $start - $end";

# runs model
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_nat.R $age $sex $start $end $type $month_dist $month_cyclic

done; done; done; done;
