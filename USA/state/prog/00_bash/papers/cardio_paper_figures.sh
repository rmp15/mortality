#!/bin/bash

# this script
# runs necessary scripts for cardio paper

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
declare cause="Cardiopulmonary"
declare -i draws=5000
declare pw=0

#################################################
# Figures 1 and 2 (mortality data summary)
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figures 1 and 2 for cardio paper $start - $end";

#Rscript ~/git/mortality/USA/state/prog/data_explore/data_explore_cardio_ons_subcod.R $start $end

#################################################
# Figure 3 (same as injury paper)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting figure 3 cardio paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start2 $end $dname $metric

#################################################
# Figure 4 (anomaly summary)
#################################################

# to correct directory
cd ~/git/climate/countries/USA/prog/00_bash/

echo "plotting figure 4 cardio paper $start - $end";

#Rscript ~/git/climate/countries/USA/prog/15_anomaly_summaries/anomaly_summaries.R $start2 $end $dname $metric

#################################################
# Figure 5 (additional deaths) and Supplementary Figure 1 and 2
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figure 5 and supplementary figure cardio paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_cardio_stacked_climate_draws_plot_data_cod_1var.R $start $end $country $model $dname $metric $contig $draws
Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_cardio_stacked_climate_draws_plot_data_cod_1var_2.R $start $end $country $model $dname $metric $contig $draws

#################################################
# Figure 6 (rankings of excess risk)
#################################################

declare -i draws=1000 # TEMPORARY

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

echo "plotting figure 6 for cardio paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_1var.R $start $end $country 11 $dname $metric $cause $contig $pw $draws

#################################################
# Tables XX-XX (not currently in paper)
#################################################

echo "populating table XX cardio paper $start - $end";

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

#Rscript ~/git/mortality/USA/state/prog/additional_deaths/additional_deaths_all_cardio_stacked_climate_draws_human_readable_cod_1var.R $start $end $country $model $dname $metric $contig $draws