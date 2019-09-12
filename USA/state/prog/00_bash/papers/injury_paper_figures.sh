#!/bin/bash

# this script
# runs necessary scripts for injury paper

clear

declare -i start=1980
declare -i end=2016

declare -a sexstrings=('male' 'female')
declare -a model=(10)
declare -i start=1980
declare -i end=2017
declare -i start2=1979
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1
declare -a draws=(5000)

#################################################
# Figures 1 and 2 (mortality data summary)
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figures 1 and 2 for injury paper $start - $end";

# runs code
Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons_subcod.R $start $end

# LEGACY (no need to run but left just in case want to summarise by intentional and unintentional)
#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_injuries_ons.R $start $end

#################################################
# Figure 3 (anomaly schematic)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting figure 3 injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start $end $dname $metric

#################################################
# Figure 4 (anomaly summary)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting figure 4 injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/15_anomaly_summaries/anomaly_summaries.R $start2 $end $dname $metric

#################################################
# Figure 5 (additional deaths) and Supplementary Figure 1 and 2
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figure 5 and supplementary figures injury paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_plot_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws

#################################################
# Supplementary table 1
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting supplementary table 1 injury paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/14_comparing_max_min/comparing_max_min.R $start2 $end 'mean' $dname

#################################################
# Tables XX-XX (not currently in paper)
#################################################

echo "populating table XX injury paper $start - $end";

# to correct directory
#cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_injuries_stacked_climate_draws_human_readable_cod_1var.R $start $end $country $model $dname $metric $contig $draws