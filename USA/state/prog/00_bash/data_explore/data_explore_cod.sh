#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1980
declare -i end=2016

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#################################################
# 1. DEATH RATES BY BROAD COD
#################################################

echo "plotting deaths rates by broad COD $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_cod.R $start $end

#################################################
# 2. DEATH RATES BY INJURY DIVISIONS (NOT REQUIRED)
#################################################

echo "plotting deaths rates by injury sub-COD $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons.R $start $end
#
#################################################
# 3. DEATH RATES BY INJURY SUB-DIVISIONS
#################################################

echo "plotting deaths rates by injury subsub-COD $start - $end";

## runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end

#################################################
# 4. ZERO PREVALENCE IN INJURIES
#################################################

echo "plotting zeroes in death rates for injuries $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/zeroes_injuries_prevalence_cod.R $start $end

#################################################
# 5. DEATH RATES BY CARDIO SUB-DIVISIONS
#################################################

echo "plotting deaths rates by injury subsub-COD $start - $end";

# runs code
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_cardio_ons_subcod.R $start $end