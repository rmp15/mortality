#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a sexstrings=('male' 'female')
declare -a models=(10)
declare -i start=1980
declare -i end=2016
declare -i start2=1980
declare -i end2=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(

declare cod="Cardiopulmonary"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 85 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 85 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 75 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 75 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 65 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 65 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 55 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 55 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 45 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 45 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 35 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 35 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 25 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 25 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 15 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R 15 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R  5 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R  5 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R  0 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_deprivation.R  0 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

) &

#################################################
# 2. COMBINE RESULTS
#################################################

declare -a cods=('Cardiopulmonary')

for model in "${models[@]}"; do
for cod in "${cods[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";
:
#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod_deprivation.R $start2 $end2 $country $model $dname $metric $cod $contig

done; done;

#################################################
# 3. PLOT RESULTS
#################################################

for model in "${models[@]}"; do
for cod in "${cods[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_deprivation_1var.R $start2 $end2 $country $model $dname $metric $cod $contig

done; done;