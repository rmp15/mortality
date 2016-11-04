#!/bin/bash

# this script
# runs a nationalised COM analysis using circular statistics
# runs a state COM analysis using circular statistics
# processes data
# plots results

clear

declare -a ages=(0 5 15 25 35 45 55 65 75 85)
declare -a sexes=(1 2)
declare -a sexstrings=('male' 'female')
declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONALISED COM ANALYSIS
#################################################

for sex in "${sexes[@]}"; do

for age in "${ages[@]}"; do

#echo "starting nationalised COM analysis for $country, years $start - $end";
echo "starting nationalised COM analysis for ${sexstrings[$sex-1]} $age, years $start - $end";

# runs COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_national.R $start $end $age $sex

# runs anti-COM analysis
Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_national.R $start $end $age $sex

done; done;

#################################################
# 2. STATE COM ANALYSIS
#################################################

echo "starting state COM analysis for $country, years $start - $end";

# runs COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_state.R $start $end

# runs anti-COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_state.R $start $end

#################################################
# 3. PROCESSING COM DATA
#################################################

echo "starting state COM analysis for $country, years $start - $end";

# process COM data
Rscript ~/git/mortality/USA/state/prog/com/com_data_process.R $start $end

#################################################
# 3. PLOTTING COM ANALYSIS
#################################################

echo "plotting COM analysis for $country, years $start - $end";

# plots
#Rscript ~/git/mortality/USA/state/prog/com/com_plot.R $start $end

