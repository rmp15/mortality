#!/bin/bash

# this script
# runs and plots seasonality changes over time

clear

declare -i start=1982
declare -i end=2013
declare country="USA"

#################################################
# 1. NATIONALISED WAVELET ANALYSIS
#################################################

clear

echo "starting nationalised seasonality index analysis for $country, years $start - $end";

# runs wavelet analysis
Rscript ~/git/mortality/USA/state/prog/seasonality_index/seasonality_index.R $start $end
