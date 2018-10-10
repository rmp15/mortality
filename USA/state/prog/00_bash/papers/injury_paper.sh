#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1980
declare -i end=2016

declare -a sexstrings=('male' 'female')
declare -a model=(10)
declare -i start=1980
declare -i end=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1
declare -a draws=(5000)

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#################################################
# Figures 1 and 2
#################################################

echo "plotting figures 1 and 2 for injury paper $start - $end";

## runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end

#################################################
# Figure 3
#################################################


echo "plotting figure 3 injury paper $start - $end";


#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start $end $dname $metric

#################################################
# Figure 4 and supplementary figure 1
#################################################

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_plot_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws