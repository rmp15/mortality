#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a sexstrings=('male' 'female')
declare -a models=(10)
declare -i start=1980
declare -i end=2017
declare -i start2=1980
declare -i end2=2017
declare country="USA"
declare dname="t2m"
declare metric="meanc4"
declare -i fast=1
declare -i contig=1
declare -i pw=0

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

# CHANGE MODEL TO 27

(

declare cod="Intentional_self-harm"

#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 85 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 85 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 75 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 75 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 65 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 65 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 55 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
#Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 55 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 45 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 45 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 35 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 35 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 25 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 25 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 15 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R 15 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R  5 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R  5 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R  0 2 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod_era5.R  0 1 $start $end 27 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

) &

#################################################
# 2. COMBINE RESULTS
#################################################

#declare -a cods=('External' 'Intentional' 'Unintentional' 'Unintentional_wo_drowning' 'Accidental_drowning_and_submersion' 'Accidental_falls' 'Transport_accidents' 'Assault' 'Intentional_self-harm')
#
#for model in "${models[@]}"; do
#for cod in "${cods[@]}"; do
#
#echo "combining results into one file from INLA model $model years $start - $end";
#:
##Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod.R $start2 $end2 $country $model $dname $metric $cod $contig
#
#done; done;
#
##################################################
## 3. PLOT RESULTS
##################################################
#
#for model in "${models[@]}"; do
#for cod in "${cods[@]}"; do
#
#echo "combining results into one file from INLA model $model years $start - $end";
#
##Rscript ~/git/mortality/USA/state/prog/mapping_posterior/mapping_posterior_climate_cod_1var.R $start2 $end2 $country $model $dname $metric $cod $contig
#
#done; done;