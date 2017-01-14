#!/bin/bash

# this script
# runs a nationalised wavelet analysis
# runs a state wavelet analysis
# processes results
# plots results on maps etc.

clear

declare -i start=1981
declare -i end=2009
declare -a numsims=(100 1000 10000)
declare -i sig=5
declare -a noises=(1 2)
declare country="Japan"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

for noise in "${noises[@]}"; do
for numsim in "${numsims[@]}"; do

clear

echo "starting nationalised wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/Japan/pref/prog/wavelet/wavelet_national.R $start $end $numsim $sig $noise &

done; done;

#################################################
# 2. REGION WAVELET ANALYSIS
#################################################

clear

echo "starting region wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_region.R $start $end $numsim

#################################################
# 3. STATE WAVELET ANALYSIS
#################################################

clear

echo "starting state wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_state.R $start $end $numsim

#################################################
# 4. WAVELET DATA PROCESSING
#################################################

clear

echo "starting wavelet analysis data processing for $country, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_data_process.R $start $end $numsim

#################################################
# 5. WAVELET DATA PLOTTING
#################################################

clear

echo "starting state wavelet analysis plotting for $country, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_plot.R $start $end $numsim
