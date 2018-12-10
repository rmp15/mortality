#!/bin/bash

# this script
# runs necessary scripts for injury paper

clear

declare -i start=1980
declare -i end=2016

declare -a sexstrings=('male' 'female')
declare -a model=(10)
declare -i start=1980
declare -i end=2016
declare -i start2=1979
declare country="USA"
declare dname="t2m"
declare metric="meanc3"
declare -i fast=1
declare -i contig=1
declare -a draws=(5000)

######### FIGURES #########

#################################################
# Figure 1
#################################################