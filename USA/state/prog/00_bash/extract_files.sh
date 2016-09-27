#!/bin/bash

# this script 
# parses the raw tape data (at the moment from 2011 format forwards)
# removes foreign deaths and recodes age
# formats data in same way as old code in Stata for next stage of processing

#################################################
# 1. EXTRACT FROM TAPES
#################################################

for year in $(seq 2011 2013);

do

echo $year

# extracts file for year from tape format into something intelligible
Rscript ~/git/mortality/USA/state/prog/format_mort/format_mort.R $year USPART2
Rscript ~/git/mortality/USA/state/prog/format_mort/format_mort.R $year PSPART2

#################################################
# 2. REMOVE FOREIGN DEATHS
#################################################

# filter deaths of people who were not residents of the USA
Rscript ~/git/mortality/USA/state/prog/format_mort/rmv_foreign_dths.R $year USPART2
Rscript ~/git/mortality/USA/state/prog/format_mort/rmv_foreign_dths.R $year PSPART2

#################################################
# 3. RECODE AGE
#################################################

# recode age format
Rscript ~/git/mortality/USA/state/prog/format_mort/recode_age.R $year USPART2
Rscript ~/git/mortality/USA/state/prog/format_mort/recode_age.R $year PSPART2

#################################################
# 4. FORMAT DATA FOR NEXT STAGE OF PROCESSING
#################################################

# emulate Harvard output data form
Rscript ~/git/mortality/USA/state/prog/format_mort/reformat_data.R $year

done;
