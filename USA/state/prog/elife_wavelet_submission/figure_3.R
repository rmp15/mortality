rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start.arg = as.numeric(args[1])   ; year.end.arg = as.numeric(args[2])
age.arg = as.numeric(args[3])          ; sex.arg = as.numeric(args[4])

# load required packages
packages = c('plyr', 'CircStats')
lapply(packages, require, character.only=TRUE)

# create output directories
output.loc = paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/entire_period/")
ifelse(!dir.exists(output.loc), dir.create(output.loc,recursive=TRUE), FALSE)

# source relevant objects
source('../../data/objects/objects.R')

# load data
input.loc = 'file_here'
dat = readRDS(input.loc)

# source com functions
source('../01_functions/com_functions.R')

# perform function for each age, gender combination
mapply(circular.age.mean.rate.2, age.selected=age.arg,sex.selected=sex.arg)

