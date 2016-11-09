#!/bin/bash

# this script
# runs a nationalised wavelet analysis
# runs a state wavelet analysis
# processes results
# plots results on maps etc.

clear

declare -i start=1982
declare -i end=2013
declare -i numsim=10
declare country="USA"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

clear

echo "starting nationalised wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_national.R $start $end $numsim

#################################################
# 2. REGION WAVELET ANALYSIS
#################################################

clear

echo "starting region wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_state.R $start $end $numsim

#################################################
# 3. STATE WAVELET ANALYSIS
#################################################

clear

echo "starting state wavelet analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_state.R $start $end $numsim

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
