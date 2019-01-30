#!/bin/bash

# this script
# runs necessary scripts for injury paper

clear

declare -i start=1980
declare -i end=2016

declare -a sexstrings=('male' 'female')
declare -a model=(10)
declare -i start=1980
declare -i end=2016
declare -i start2=1979
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1
declare -a draws=(5000)

#################################################
# Figures 1 and 2
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figures 2 and 3 for injury paper $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons.R $start $end

#################################################
# Figure 3
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting figure 1 injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start2 $end $dname $metric

#################################################
# Figure 4 and Supplementary Figure 1 and 2
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figure 4 and supplementary figure injury paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_plot_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws

#################################################
# Tables XX-XX (not currently in paper)
#################################################

echo "populating table 1 injury paper $start - $end";

# to correct directory
#cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_human_readable_cod_1var.R $start $end $country $model $dname $metric $contig $draws