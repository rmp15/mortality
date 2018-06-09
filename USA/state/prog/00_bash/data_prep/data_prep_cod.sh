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
# 1. PROCESS DATA FOR BROAD CAUSES
#################################################

echo "preparing monthly death rates in broad causes of deaths for years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_cod.R $start $end

#################################################
# 2. PROCESS DATA FOR INJURIES CATEGORIES
#################################################

echo "preparing monthly death rates in injury deaths (intentional/unintentional) for years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_injuries_ons.R $start $end

#################################################
# 3. PROCESS DATA FOR INJURIES SUB-CAUSES
#################################################

echo "preparing monthly death rates in injury sub-causes of deaths for years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_subcauses_injuries_ons.R $start $end

#################################################
# 4. PROCESS DATA FOR ELIFE FIRST REVISION
#################################################

echo "preparing monthly death rates in eLife-requested causes of death for years $start - $end";

Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_subcauses_elife.R $start $end