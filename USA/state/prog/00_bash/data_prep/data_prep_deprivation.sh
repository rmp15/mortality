#!/bin/bash

# this script
# formats population files downloaded from https://wonder.cdc.gov/bridged-race-population.html
# infers monthly population given year population
# processes monthly death rates by merging death counts with population
# plots data by age gender state combination to check that it looks OK

clear

# to correct directory
cd ~/git/mortality/USA/state/prog/00_bash/

declare -i start=1980
declare -i end=2016

#################################################
# 1. PROCESS POPULATION BY DEPRIVATION
#################################################

echo "preparing population by deprivation for years $start - $end";

# TO FINISH THIS SCRIPT
Rscript ~/git/mortality/USA/state/prog/pop_format/pop_us_infer_deprivation_days.R $start $end

#################################################
# 2. PROCESS DEATH RATES FOR BROAD CAUSES BY DEPRIVATION
#################################################

echo "preparing monthly death rates by deprivation in broad causes of deaths for years $start - $end";

# TO FINISH THIS SCRIPT
Rscript ~/git/mortality/USA/state/prog/prep_data_cod/US_state_monthly_prepare_data_cod_deprivation.R $start $end

#################################################
# 3. RUN AGE-SEPARATED DEPRIVATION MODEL
#################################################

# TO FINISH THIS SCRIPT

(

declare cod="Cardiopulmonary"

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 85 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 85 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 75 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 75 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 65 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 65 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 55 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 55 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 45 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 45 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 35 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 35 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 25 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 25 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R 15 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R  5 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R  5 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R  0 1 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;
Rscript ~/git/mortality/USA/state/prog/models/INLA/03_spatiotemporal/inla_spatiotemporal_climate_cod.R  0 2 $start $end 10 0 $dname $metric $start2 $end2 $cod $fast $contig $pw;

) &