rm(list=ls())

library(dplyr)
library(plyr)
library(foreign)

# create output directory
ifelse(!dir.exists("../../output/prep_data_cod/cods/"), dir.create("../../output/prep_data_cod/cods/",recursive=TRUE), FALSE)

# load file
file.loc = 'LOCATION OF FILE'
dat = read.csv(file.loc)

# get rid of weird 'NA' rows (maybe you want to check this?)
dat = na.omit(dat)

# process cause of death into letter and maximum 3-digit number (e.g. 00.0 = 0, 10.0 = 100, 98.1 = 981 etc.)
dat$causa_basica = as.character(dat$causa_basica)
dat$causa_basica = ifelse(substr(dat$causa_basica,4,4)=='X',substr(dat$causa_basica,1,3),dat$causa_basica)
dat$causa_basica[nchar(dat$causa_basica)==3] <- paste0(dat$causa_basica[nchar(dat$causa_basica)==3],'0')
dat$letter = substr(dat$causa_basica,1,1)

# numerical cause
dat$cause.numeric = as.numeric(as.character(substr(dat$causa_basica,2,4)))

# cause groups (edit as you want)
dat$cause.sub =

                            # cardiorespiratory subcauses
                            ifelse(dat$letter=='I'&dat$cause.numeric>=0&dat$cause.numeric<=999,'Cardiovascular',
                            ifelse(dat$letter=='J'&dat$cause.numeric>=0&dat$cause.numeric<=69,'Respiratory infections',
                            ifelse(dat$letter=='J'&dat$cause.numeric>=90&dat$cause.numeric<=189,'Respiratory infections',
                            ifelse(dat$letter=='J'&dat$cause.numeric>=200&dat$cause.numeric<=229,'Respiratory infections',
                            ifelse(dat$letter=='H'&dat$cause.numeric>=650&dat$cause.numeric<=669,'Respiratory infections',
                            ifelse(dat$letter=='J'&dat$cause.numeric>=300&dat$cause.numeric<=989,'Chronic respiratory diseases',
			                # maternal and perinatal subcauses
                            ifelse(dat$letter=='O'&dat$cause.numeric>=0&dat$cause.numeric<=999,'Maternal conditions',
                            ifelse(dat$letter=='P'&dat$cause.numeric>=0&dat$cause.numeric<=969,'Perinatal conditions',
			                # endocrine disorders
                            ifelse(dat$letter=='D'&dat$cause.numeric>=550&dat$cause.numeric<=648,'Endocrine disorders',
                            ifelse(dat$letter=='D'&dat$cause.numeric>=650&dat$cause.numeric<=899,'Endocrine disorders',
                            ifelse(dat$letter=='E'&dat$cause.numeric>=30&dat$cause.numeric<=79,'Endocrine disorders',
                            ifelse(dat$letter=='E'&dat$cause.numeric>=150&dat$cause.numeric<=169,'Endocrine disorders',
                            ifelse(dat$letter=='E'&dat$cause.numeric>=200&dat$cause.numeric<=349,'Endocrine disorders',
                            ifelse(dat$letter=='E'&dat$cause.numeric>=650&dat$cause.numeric<=889,'Endocrine disorders',
                            # genitourinary diseases
                            ifelse(dat$letter=='N'&dat$cause.numeric>=0&dat$cause.numeric<=649,'Genitourinary diseases',
                            ifelse(dat$letter=='N'&dat$cause.numeric>=750&dat$cause.numeric<=989,'Genitourinary diseases',
                            # neuropsychiatric disorders
                            ifelse(dat$letter=='F'&dat$cause.numeric>=10&dat$cause.numeric<=99,'Neuropsychiatric disorders',
                            ifelse(dat$letter=='F'&dat$cause.numeric>=170&dat$cause.numeric<=179,'Neuropsychiatric disorders',
                            ifelse(dat$letter=='F'&dat$cause.numeric>=200&dat$cause.numeric<=999,'Neuropsychiatric disorders',
                            ifelse(dat$letter=='G'&dat$cause.numeric>=60&dat$cause.numeric<=139,'Neuropsychiatric disorders',
                            ifelse(dat$letter=='G'&dat$cause.numeric>=150&dat$cause.numeric<=989,'Neuropsychiatric disorders',
                            # substance use disorders
                            ifelse(dat$letter=='F'&dat$cause.numeric>=100&dat$cause.numeric<=169,'Substance use disorders',
                            ifelse(dat$letter=='F'&dat$cause.numeric>=180&dat$cause.numeric<=199,'Substance use disorders',
                            ifelse(dat$letter=='X'&dat$cause.numeric>=410&dat$cause.numeric<=429,'Substance use disorders',
                            ifelse(dat$letter=='X'&dat$cause.numeric>=450&dat$cause.numeric<=459,'Substance use disorders',
                            ifelse(dat$letter=='X'&dat$cause.numeric>=490&dat$cause.numeric<=499,'Substance use disorders',

                            # to close the brackets above
                            'No match'))))))))))))))))))))))))))

# output  file as RDS and csv
saveRDS(dat,'OUTPUT FILE LOCATION') ; write.csv(dat,'OUTPUT FILE LOCATION')