rm(list=ls())

library(dplyr)
library(plyr)
library(foreign)

# Sections of code to
# add extra cardiovascular grouping for CODs based on relevant ICD year
# code will not run on its own; needs to be slotted in to other code selectively
# I have included ICD-10 and ICD-9 codings take what you need
# It should all be good but there will be inevitable teething issues as I haven't run the code for your specific dataset

# 'dat' is your dataframe to load
# 'cause_icd10' is the column name for death coding
# 'cause_icd9' is the column name for prior condition at baseline coding

# 1. ICD 10 coding for cvd (for inclusion of icd-10 cvd deaths only)

# to make all codings four characters long (one letter then three numbers)
dat$cause_icd10[nchar(dat$cause_icd10)==3] <- paste0(dat$cause_icd10[nchar(dat$cause_icd10)==3],'0')
dat$letter = substr(dat$cause_icd10,1,1)

# to code and only keep cvd
dat$cause.group = ifelse(dat$letter=='I','CVD', 'Other')
dat$cause.group = as.character(dat$cause.group)
dat = subset(dat,cause.group=='CVD')

# numerical cause for subsets of ICD-10 CVD codings
dat$cause.numeric = as.numeric(as.character(substr(dat$cause_icd10,2,4)))

# CVD subgroups
dat$cause.sub =
                    # Cardiovascular diseases (I00-I99)
                    ifelse(dat$letter=='I'&dat$cause.numeric>=0&dat$cause.numeric<=9,		'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=10&dat$cause.numeric<=99,	    'Rheumatic heart disease',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=100&dat$cause.numeric<=139,	'Hypertensive heart disease',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=140&dat$cause.numeric<=199,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=200&dat$cause.numeric<=259,	'Ischaemic heart disease',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=260&dat$cause.numeric<=299,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=300&dat$cause.numeric<=339,	'Inflammatory heart diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=340&dat$cause.numeric<=379,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=380&dat$cause.numeric<=389,	'Inflammatory heart diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=390&dat$cause.numeric<=399,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=400&dat$cause.numeric<=409,	'Inflammatory heart diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=410&dat$cause.numeric<=419,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=420&dat$cause.numeric<=429,	'Inflammatory heart diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=430&dat$cause.numeric<=599,	'Other cardiovascular diseases',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=600&dat$cause.numeric<=699,	'Cerebrovascular disease',
                    ifelse(dat$letter=='I'&dat$cause.numeric>=700&dat$cause.numeric<=999,	'Other cardiovascular diseases',
                    'NA'))))))))))))))))

# 2. ICD 9 coding for cvd (for exclusion of incidence before baseline)

# to make all codings four numbers long
dat$cause_icd9[nchar(dat$cause_icd9)==3] <- paste0(dat$cause_icd9[nchar(dat$cause_icd9)==3],'0')
dat$cause.numeric = as.numeric(dat$cause_icd9)

# to code and only keep cvd
dat$cause.group = ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=4599,'CVD', 'Other')
dat$cause.group = as.character(dat$cause.group)
dat = subset(dat,cause.group=='CVD')

# cause subgroups (edit as you want)
dat$cause.sub =
                        ifelse(dat$cause.numeric>=3900&dat$cause.numeric<=3989, 'Rheumatic heart disease',
                        ifelse(dat$cause.numeric>=3990&dat$cause.numeric<=4009, 'Other cardiovascular diseases',
                        ifelse(dat$cause.numeric>=4010&dat$cause.numeric<=4059, 'Hypertensive heart disease',
                        ifelse(dat$cause.numeric>=4060&dat$cause.numeric<=4099, 'Other cardiovascular diseases',
                        ifelse(dat$cause.numeric>=4100&dat$cause.numeric<=4149, 'Ischaemic heart disease',
                        ifelse(dat$cause.numeric>=4150&dat$cause.numeric<=4199, 'Other cardiovascular diseases',
                        ifelse(dat$cause.numeric>=4200&dat$cause.numeric<=4229, 'Inflammatory heart diseases',
                        ifelse(dat$cause.numeric>=4230&dat$cause.numeric<=4249, 'Other cardiovascular diseases',
                        ifelse(dat$cause.numeric>=4250&dat$cause.numeric<=4259, 'Inflammatory heart diseases',
                        ifelse(dat$cause.numeric>=4260&dat$cause.numeric<=4299, 'Other cardiovascular diseases',
                        ifelse(dat$cause.numeric>=4300&dat$cause.numeric<=4389, 'Cerebrovascular disease',
                        ifelse(dat$cause.numeric>=4390&dat$cause.numeric<=4599, 'Other cardiovascular diseases',
                        'NA'))))))))))))