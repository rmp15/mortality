#!/bin/bash

# this script
# plots breakdowns of cods for different years nationally

cd ~/git/mortality/USA/state/prog/00_bash/

clear
declare -a years=(1999 2013)

for year in "${years[@]}"; do
:
Rscript ~/git/mortality/USA/state/prog/format_mort_cod/breakdown_cod.R $year

done;


