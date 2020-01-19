#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a sexstrings=('male' 'female')
declare -i start=1982
declare -i end=2017
declare -i model=27
declare -i cluster=0
declare -i start2=1982
declare -i end2=2017
declare cod="AllCause"
declare -i pw=0
declare county="Maricopa"

#################################################
# 1. RUN AGE-TOGETHER MODEL
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(


Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_single_county.R 1 $start $end $model 0 $start2 $end2 $cod $pw $county;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_single_county.R 2 $start $end $model 0 $start2 $end2 $cod $pw $county;

) &

#################################################
# 2. COMBINE RESULTS
#################################################

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod.R $start2 $end2 $country $model $dname $metric $cod

done;