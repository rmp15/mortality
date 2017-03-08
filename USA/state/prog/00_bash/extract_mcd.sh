#!/bin/bash

# this script 
# parses the raw tape data
# removes foreign deaths and recodes age
# formats data in same way as old code in Stata for next stage of processing
# check old data against new data, if it exists

clear

# WARNING: only ever tested on 1979-1981. Age recode almost certainly won't
# work for later years
declare -a years=($(seq 1979 1981))

for year in "${years[@]}"; do

#################################################
# 1. REMOVE FOREIGN DEATHS
#################################################

echo "removing foreign deaths for $year";

# filter deaths of people who were not residents of the USA
#Rscript ~/git/mortality/USA/state/prog/format_mort/mcd/rmv_foreign_dths.R $year

#################################################
# 2. RECODE AGE
#################################################

echo "recoding age for $year";

# recode age format
#Rscript ~/git/mortality/USA/state/prog/format_mort/mcd/recode_age.R $year

#################################################
# 3. FIX STATE CODES
#################################################

echo "fixing state code for $year";

# recode age format
#Rscript ~/git/mortality/USA/state/prog/format_mort/mcd/recode_state.R $year

#################################################
# 4. FORMAT DATA FOR NEXT STAGE OF PROCESSING
#################################################

echo "emulating inherited data format for $year";

# emulate Harvard output data form
Rscript ~/git/mortality/USA/state/prog/format_mort/mcd/reformat_data.R $year

done;
