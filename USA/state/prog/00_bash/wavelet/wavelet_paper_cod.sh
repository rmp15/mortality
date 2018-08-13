#!/bin/bash

# this script
# runs all the analysis necessary for the USA wavelet paper for cod as well as all-cause

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

clear

declare -i start=1980
declare -i end=2016
declare country="USA"

#################################################
# 0. NATIONAL DEATH STATISTICS BY CAUSE OF DEATH
#################################################

echo "calculating death statistics for $country by $cod, years $start - $end";

declare -a classes=("broad" "narrow" "injuries" )

for class in "${classes[@]}"; do
:
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_summary_cod.R $start $end $class

done;

#################################################
# 1. NATIONAL WAVELET ANALYSIS
#################################################

declare -a numsims=(100)
declare -i sig=5
declare -a noises=(1)
#declare -a noises=(1 2)
#declare -a cods=("AllCause")
#declare -a cods=("AllCause" "Cancer" "Cardiopulmonary" "External" "Intentional" "Unintentional" "Other" "Cardiovascular" "Chronic_respiratory_diseases" "Respiratory_infections" "Endocrine_disorders" "Genitourinary_diseases" "Maternal_conditions" "Neuropsychiatric_disorders" "Perinatal_conditions" "Substance_use_disorders")
declare -a cods=("Other")
declare -a logs=(0)

for noise in "${noises[@]}"; do
for numsim in "${numsims[@]}"; do
for cod in "${cods[@]}"; do
for log in "${logs[@]}"; do

clear

echo "starting nationalised wavelet analysis for $country, $cod, years $start - $end";
:
# runs wavelet analysis (figure 2, appendix figure 1)
#Rscript ~/git/mortality/USA/state/prog/wavelet/wavelet_national_cod.R $start $end $numsim $sig $noise $cod $log &

done; done; done; done;

#################################################
# 2. NATIONAL COM ANALYSIS
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

#################################################
# 3. REGIONAL COM ANALYSIS
#################################################

for cod in "${cods[@]}"; do

echo "starting regional COM analysis for $cod for years $start - $end";

# runs regional COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/com_analysis_region_cod.R $start $end $cod &

# runs regional anti-COM analysis
#Rscript ~/git/mortality/USA/state/prog/com/anti_com_analysis_region_cod.R $start $end $cod &

#echo "processing data for $country for $cod, years $start - $end";

# process COM data
#Rscript ~/git/mortality/USA/state/prog/com/com_data_process_cod.R $start $end $cod

done;

#################################################
# 4. COM PLOTS
#################################################

for cod in "${cods[@]}"; do

echo "plotting COM analysis for $country for $cod, years $start - $end";

# processes com data
#Rscript ~/git/mortality/USA/state/prog/com/com_data_process_cod.R $start $end $cod

# plots (figure 3, figure 5, appendix figure 2)
#Rscript ~/git/mortality/USA/state/prog/com/com_plot_cod.R $start $end $cod

done;

#################################################
# 5. NATIONAL CLIMATE SEASONALITY INDEX
#################################################

for cod in "${cods[@]}"; do

echo "starting climate seasonality index analysis for $country, years $start - $end";

declare dname="t2m"
declare metric="mean"

# create seasonality index climate values
#Rscript ~/git/climate/countries/USA/prog/10_seasonality_index/seasonality_index_climate_regions_cod.R $start $end $start $end $dname $metric $cod

done;

#################################################
# 6. NATIONAL SEASONALITY INDEX ANALYSIS
#################################################

for cod in "${cods[@]}"; do

echo "starting seasonality index analysis for $country, years $start - $end";
:
# runs seasonality index analysis (figure 5, figure 6, appendix figure 4)
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod.R $start $end $start $end $dname $metric $cod

# runs ols model as per revisions
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_ols.R $start $end $start $end $dname $metric $cod
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_cosinor.R $start $end $start $end $dname $metric $cod

# run alternative figure 6 based on mean annual temperature instead
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_regional_alt.R $start $end $start $end $dname $metric $cod

done;

# (figure 4, appendix figure 3, alternative figure 4, alternative appendix figure 3, figure comparing original and ols, cosinor methods)
Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_plot_all.R $start $end $start $end $dname $metric
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_plot_all_ols.R $start $end $start $end $dname $metric
#Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index_cod_compare_plot.R $start $end $start $end $dname $metric
