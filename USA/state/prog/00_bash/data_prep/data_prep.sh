#!/bin/bash

# this script
# formats population files downloaded from https://wonder.cdc.gov/bridged-race-population.html
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

clear

declare -a years=(2005 $(seq 2011 2015))
declare -i start=2012
declare -i end=2015
declare -i start2=1982
declare -i end2=2013

#################################################
# 1. FORMAT RAW POPULATION FILES
#################################################

for year in "${years[@]}"; do

echo "formatting raw population files for $year";

Rscript ~/git/mortality/USA/state/prog/pop_format/pop_format.R $year

#################################################
# 2. COMPARE OLD AND NEW POP FILES
#################################################

echo "comparing old and new population files for $year";

Rscript ~/git/mortality/USA/state/prog/pop_format/pop_old_new_compare.R $year

done;

#################################################
# 3. ADD NEW YEARS TO POPULATION FILES
#################################################

echo "adding years $start - $end to population files";

Rscript ~/git/mortality/USA/state/prog/pop_format/pop_add_new_years.R $start $end

#################################################
# 4. RUN POPULATION INFER BY DAY
#################################################

echo "inferring monthly populations from yearly figures for years $start2 - $end2";

Rscript ~/git/mortality/USA/state/prog/pop_us_infer/pop_us_infer_state_days.R

#################################################
# 5. PROCESS DATA
#################################################

echo "preparing monthly death rates for years $start2 - $end2";

Rscript ~/git/mortality/USA/state/prog/prep_data/US_state_monthly_prepare_data_deaths_adj.R $start2 $end2

#################################################
# 6. EXPLORE DATA
#################################################

echo "exploring death rates for years $start2 - $end2";

Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore.R $start2 $end2
