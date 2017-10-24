#!/bin/bash

# this script
# creates an adjacency matrix from a shapefile
# connects Hawaii to California and Alaska to Washington

clear

#################################################
# 1. CREATE ADJACENCY MATRIX AND ADJUST
#################################################

echo "creating adjacency matrix for USA from shapefile";

Rscript ~/git/mortality/USA/state/prog/adj_matrix_create/adj_matrix_create.R
#Rscript ~/git/mortality/USA/state/prog/adj_matrix_create/adj_matrix_fix.R

