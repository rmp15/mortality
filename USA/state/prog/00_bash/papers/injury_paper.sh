#!/bin/bash

# this script
# plots population against time for counties by state
# plots death rates against time by state

clear

declare -i start=1980
declare -i end=2016

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

declare dname='t2m'
declare metric='meanc3'

echo "plotting figure 3 injury paper $start - $end";


Rscript ~/git/climate/countries/USA/prog/06_plots/plots_against_time.R $start $end $dname $metric

#################################################
# Figure 4
#################################################

