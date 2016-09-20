#!/bin/bash

#################################################
# 1. RUN AGE-SEPARATED 1a MODEL
#################################################

# MALE

for age in $(0);

do

echo $age;

Rscript ~/git/countries/USA/state/prog/models/INLA/spatiotemporal/spatio_temporal_types_function.R $age 1 1982 1983 2 0

done;

# FEMALE

for age in $(0);

do

echo $age;

Rscript ~/git/countries/USA/state/prog/models/INLA/spatiotemporal/spatio_temporal_types_function.R $age 2 1982 1983 2 0

done;

#################################################
# 2. COMBINE RESULTS
#################################################

# RBIND FILES CODE

#################################################
# 3. PLOTTING PARAMETERS FROM MODEL
#################################################

# PLOT PARAMETERS CODE

#################################################
# 4. PLOTTING RESULTS FROM MODEL
#################################################

# PLOT RESULTS CODE

