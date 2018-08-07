#!/bin/bash

# this script
# runs the INLA age-separated model
# binds the posterior results together
# plots the parameters of the model run

clear

declare -a ages=(15 5 0)
declare -a sexes=(2)
declare -a sexstrings=('male' 'female')
declare -a models=(18)
declare -i start=1980
declare -i end=2016
declare -i start2=1980
declare -i end2=2016
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
#declare cod=""
declare -i fast=1
declare -i contig=1

#################################################
# 1. RUN AGE-SEPARATED MODEL
#################################################

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

(

declare cod="Intentional_self-harm"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 1 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  1 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  2 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  1 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  2 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;

declare cod="Transport_accidents"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 2 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  1 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  2 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  1 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  2 $start $end 18 0 $dname $metric $start2 $end2 $cod $fast $contig;

declare cod="Intentional_self-harm"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

declare cod="Transport_accidents"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 5  2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 0  2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig;

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod_1var.R $start2 $end2 $country 10 $dname $metric $cod $contig;

done;

) &

#################################################
# 2. COMBINE RESULTS
#################################################

for model in "${models[@]}"; do

echo "combining results into one file from INLA model $model years $start - $end";

#Rscript ~/git/mortality/USA/state/prog/bind_posterior/bind_posterior_climate_cod.R $start2 $end2 $country $model $dname $metric $cod

done;