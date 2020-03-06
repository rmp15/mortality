icd_coding_charlie.Rrm(list=ls())

library(dplyr)
library(plyr)
library(foreign)

# Sections of code to
# add extra natural or other grouping for CODs based on relevant ICD year
# code will not run on its own; needs to be slotted in to other code selectively
# It should all be good but there will be inevitable teething issues as I haven't run the code for your specific dataset
# All my love
# Robbie xxxx

# 'dat' is your dataframe to load
# 'cause_icd10' is the column name for death coding

# 1. ICD 10 coding for cvd (for inclusion of icd-10 cvd deaths only)

# to make all codings four characters long (one letter then three numbers)
dat$cause_icd10[nchar(dat$cause_icd10)==3] <- paste0(dat$cause_icd10[nchar(dat$cause_icd10)==3],'0')
dat$letter = substr(dat$cause_icd10,1,1)

# to code and only keep non-injury
dat$cause.group = ifelse(!(dat$letter%in%c('V','W','X','Y','Z')),'Natural', 'Injury')
dat$cause.group = as.character(dat$cause.group)
dat = subset(dat,cause.group=='Natural')

# numerical cause for subsets of ICD-10 CVD codings
dat$cause.numeric = as.numeric(as.character(substr(dat$cause_icd10,2,4)))