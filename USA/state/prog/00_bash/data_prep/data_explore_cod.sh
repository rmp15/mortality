#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1980
declare -i end=2013

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#################################################
# 1. DEATH RATES BY BROAD COD
#################################################

echo "plotting deaths rates by broad COD $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_cod.R $start $end

#################################################
# 2. DEATH RATES BY INJURY DIVISIONS
#################################################

echo "plotting deaths rates by injury sub-COD $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons.R $start $end

#################################################
# 2. DEATH RATES BY INJURY SUB-DIVISIONS
#################################################

echo "plotting deaths rates by injury subsub-COD $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end