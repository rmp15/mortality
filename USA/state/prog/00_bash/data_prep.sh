#!/bin/bash

# this script
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

#################################################
# 1. RUN POPULATION INFER BY DAY 
#################################################

Rscript ~/git/mortality/USA/state/prog/pop_us_infer/pop_us_infer_state_days.R

#################################################
# 2. PROCESS DATA
#################################################

Rscript ~/git/mortality/USA/state/prog/prep_data/US_state_monthly_prepare_data.R 1982 2011

#################################################
# 3. EXPLORE DATA
#################################################

Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore.R 1982 2011
