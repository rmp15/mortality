#!/bin/bash

# this script
# runs a nationalised COM analysis using circular statistics
# runs a state COM analysis using circular statistics
# plots results

clear

declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONALISED COM ANALYSIS
#################################################

echo "starting nationalised COM analysis for $country, years $start - $end";

# runs COM analysis
Rscript ~/git/mortality/USA/state/prog/com/com_analysis_national.R $start $end

# runs anti-COM analysis
Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_national.R $start $end

#################################################
# 2. STATE COM ANALYSIS
#################################################

echo "starting state COM analysis for $country, years $start - $end";

# runs COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_state.R $start $end

# runs anti-COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_state.R $start $end

#################################################
# 3. PLOTTING COM ANALYSIS
#################################################

echo "plotting COM analysis for $country, years $start - $end";

# plots
#Rscript ~/git/mortality/USA/state/prog/com/com_plot.R $start $end

