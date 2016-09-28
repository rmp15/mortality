#!/bin/bash

# this script
# formats population files downloaded from https://wonder.cdc.gov/bridged-race-population.html
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

#################################################
# 1. FORMAT RAW POPULATION FILES
#################################################

for year in $(seq 2011 2015);

do

echo $year

Rscript ~/git/mortality/USA/state/prog/pop_format/pop_format.R $year

done;

#################################################
# 1. COMPARE OLD AND NEW POP FILES
#################################################

#for year in $(seq 2011 2011);

#do

#echo $year

#Rscript ~/git/mortality/USA/state/prog/pop_format/pop_old_new_compare.R $year

#done;

#################################################
# 2. RUN POPULATION INFER BY DAY
#################################################

#Rscript ~/git/mortality/USA/state/prog/pop_us_infer/pop_us_infer_state_days.R

#################################################
# 3. PROCESS DATA
#################################################

#Rscript ~/git/mortality/USA/state/prog/prep_data/US_state_monthly_prepare_data.R 1982 2011

#################################################
# 4. EXPLORE DATA
#################################################

#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore.R 1982 2011
