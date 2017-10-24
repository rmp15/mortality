#!/bin/bash

# this script
# plots death rates against chosen climate variable
# performs linear regression analysis for chosen climate variable

clear

declare -a ages=(0 5 15 25 35 45 55 65 75 85)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -i start_mort=1982
declare -i end_mort=2013
declare -i start_clim=1982
declare -i end_clim=2015
declare country="USA"
declare dname="t2m"

# NEED TO AUTOMATE AT SOME POINT
declare -a metrics=("mean" "days_below_10" "days_above_30" "sd" "days_changing_by_5" "days_increasing_by_5" "days_decreasing_by_5" "number_of_min_3_day_above_25_upwaves" "number_of_min_3_day_below_5_downwaves")

#################################################
# 1. DEATH RATES AGAINST CLIMATE
#################################################

for metric in "${metrics[@]}"; do

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

echo "starting ${sexstrings[$sex-1]} $age deaths rates with climate variable $metric $dname, years $start_mort - $end_mort";

# runs model
Rscript ~/git/mortality/USA/state/prog/mort_against_climate/mort_against_climate.R $age $sex $start_mort $end_mort $start_clim $end_clim $dname $metric

done; done; done;

#################################################
# 2. DEATH RATES REGRESSION AGAINST CLIMATE
#################################################

declare metric1='mean'
declare metric2='sd'

echo "starting ${sexstrings[$sex-1]} $age deaths rates regression against climate variables $dname, $metric1 and $metric2, years $start_mort - $end_mort";

# runs model
Rscript ~/git/mortality/USA/state/prog/mort_against_climate/line_reg.R $age $sex $start_mort $end_mort $start_clim $end_clim $dname $metric1 $metric2
