#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1982
declare -i end=2013
declare -a states=("01" "02" "04" "05" "06" "08" "09" "10" "11" "12" "13" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "44" "45" "46" "47" "48" "49" "50" "51" "53" "54" "55" "56")


#################################################
# 1. POPULATION OVER TIME BY STATE AND COUNTY
#################################################

echo "plotting population against time by county";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/plot_county_against_time.R

#################################################
# 2. DEATH RATES OVER TIME BY STATE
#################################################

echo "plotting raw death rates by state for years $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore.R $start $end

#################################################
# 3. FIPS CHECK OVER TIME
#################################################

for state in "${states[@]}"; do

echo "checking fips county codes for $state, $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/fips_test.R $start $end $state

done;
