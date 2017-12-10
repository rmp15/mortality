#!/bin/bash

# this script
# formats population files downloaded from https://wonder.cdc.gov/bridged-race-population.html
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

clear

declare -i start=1980
declare -i end=2013

#################################################
# 1. PROCESS DATA
#################################################

echo "preparing monthly death rates for years $start - $end";

Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_cod.R $start $end
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_deaths_adj_cod.R $start $end

