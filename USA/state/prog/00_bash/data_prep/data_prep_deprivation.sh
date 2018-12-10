#!/bin/bash

# this script
# formats population files downloaded from https://wonder.cdc.gov/bridged-race-population.html
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

clear

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

declare -i start=1980
declare -i end=2016

#################################################
# 1. PROCESS POPULATION BY DEPRIVATION
#################################################

echo "preparing population by deprivation for years $start - $end";

# TO FINISH THIS SCRIPT
Rscript ~/git/mortality/USA/state/prog/pop_format/pop_us_infer_deprivation_days.R $start $end

#################################################
# 2. PROCESS DEATH RATES FOR BROAD CAUSES BY DEPRIVATION
#################################################

echo "preparing monthly death rates by deprivation in broad causes of deaths for years $start - $end";

# TO FINISH THIS SCRIPT
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_cod_deprivation.R $start $end