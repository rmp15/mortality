#!/bin/bash

# this script
# runs a nationalised coherence analysis
# runs a region coherence analysis
# runs a state coherence analysis
# processes results
# plots results on maps etc.

clear

declare -i start=1982
declare -i end=2013
declare -i numsim=10
declare -i sig=5
declare -a ages=(85 75 65 55 45 35 25 15 5 0)
declare country="USA"

#################################################
# 1. NATIONALISED COHERENCE ANALYSIS
#################################################

clear

for age in "${ages[@]}"; do

echo "starting nationalised coherence analysis for age $age, $country, years $start - $end";

# runs coherence analysis
#Rscript ~/git/mortality/USA/state/prog/coherence/coherence_national.R $start $end $numsim $sig $age

done;

#################################################
# 2. REGION COHERENCE ANALYSIS
#################################################

clear

for age in "${ages[@]}"; do

echo "starting regional coherence analysis for age $age, $country, years $start - $end";

# runs coherence analysis
Rscript ~/git/mortality/USA/state/prog/coherence/coherence_region.R $start $end $numsim $sig $age

done;

#################################################
# 3. STATE COHERENCE ANALYSIS
#################################################

clear

echo "starting state coherence analysis for $country, years $start - $end";

# runs coherence analysis
#Rscript ~/git/mortality/USA/state/prog/coherence/coherence_state.R $start $end $numsim

#################################################
# 4. coherence DATA PROCESSING
#################################################

clear

echo "starting coherence analysis data processing for $country, years $start - $end";

# runs coherence analysis
#Rscript ~/git/mortality/USA/state/prog/coherence/coherence_data_process.R $start $end $numsim

#################################################
# 5. coherence DATA PLOTTING
#################################################

clear

echo "starting state coherence analysis plotting for $country, years $start - $end";

# runs coherence analysis
#Rscript ~/git/mortality/USA/state/prog/coherence/coherence_plot.R $start $end $numsim
