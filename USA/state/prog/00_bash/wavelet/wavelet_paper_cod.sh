#!/bin/bash

# this script
# runs all the analysis necessary for the USA wavelet paper

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

clear

declare -i start=1980
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONAL WAVELET ANALYSIS
#################################################

declare -a numsims=(10)
declare -i sig=5
declare -a noises=(1 2)
declare -a cods=("AllCause" "Cancer" "Cardiopulmonary" "External" "Other")

for noise in "${noises[@]}"; do
for numsim in "${numsims[@]}"; do
for cod in "${cods[@]}"; do

clear

echo "starting nationalised wavelet analysis for $country, $cod, years $start - $end";

# runs wavelet analysis
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_national_cod.R $start $end $numsim $sig $noise $cod &

done; done; done;

#################################################
# 2. NATIONAL AND REGIONAL COM ANALYSIS
#################################################

declare -a ages=(0 5 15 25 35 45 55 65 75 85)
declare -a sexes=(1 2)

for sex in "${sexes[@]}"; do
for age in "${ages[@]}"; do
for cod in "${cods[@]}"; do

echo "starting nationalised COM analysis for ${sexstrings[$sex-1]} $age, years $start - $end";

# runs COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_national_cod.R $start $end $age $sex $cod &

# runs anti-COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_national_cod.R $start $end $age $sex $cod &

done; done; done;

echo "starting regional COM analysis for years $start - $end";

# runs regional COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_region.R $start $end

# runs regional anti-COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_region.R $start $end

for cod in "${cods[@]}"; do

echo "processing data for $country, years $start - $end";

# process COM data
#Rscript ~/git/mortality/USA/state/prog/com/com_data_process_cod.R $start $end $cod

done;

for cod in "${cods[@]}"; do

echo "plotting COM analysis for $country, years $start - $end";

# plots
Rscript ~/git/mortality/USA/state/prog/com/com_plot_cod.R $start $end $cod

done;

#################################################
# 3. NATIONAL CLIMATE SEASONALITY INDEX
#################################################

declare dname="t2m"
declare metric="mean"

# create seasonality index climate values
#Rscript ~/git/climate/countries/USA/prog/10_seasonality_index/seasonality_index_climate_regions.R $start $end $start $end $dname $metric

#################################################
# 4. NATIONAL SEASONALITY INDEX ANALYSIS
#################################################

for cod in "${cods[@]}"; do

echo "starting seasonality index analysis for $country, years $start - $end";

# runs seasonality index analysis
Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod.R $start $end $start $end $dname $metric $cod

done;