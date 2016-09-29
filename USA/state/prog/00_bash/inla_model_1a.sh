#!/bin/bash

#################################################
# 2. RUN AGE-SEPARATED 1a MODEL
#################################################

# MALE

for age in 0 5 15 25 35 45 55 65 75 85;

do

echo $age;

# runs model for males
Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/spatio_temporal_types_function.R $age 1 1982 2013 2 0

done;

# FEMALE

for age in 0 5 15 25 35 45 55 65 75 85;

do

echo $age;

# runs model for females
Rscript ~/git/mortality/USA/state/prog/models/INLA/spatiotemporal/spatio_temporal_types_function.R $age 2 1982 2013 2 0

done;

#################################################
# 3. COMBINE RESULTS
#################################################

# RBIND FILES CODE

#################################################
# 4. PLOTTING PARAMETERS FROM MODEL
#################################################

# PLOT PARAMETERS CODE

#################################################
# 5. PLOTTING RESULTS FROM MODEL
#################################################

# PLOT RESULTS CODE

