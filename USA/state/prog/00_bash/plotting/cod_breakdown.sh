#!/bin/bash

# this script
# plots breakdowns of cods for different years nationally

clear
declare -a years=(1980 2013)

for year in "${years[@]}"; do
:
Rscript ~/git/mortality/USA/state/prog/format_mort_cod/breakdown_cod.R $year

done;


