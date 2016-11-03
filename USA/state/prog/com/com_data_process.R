rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(plyr)
require(CircStats)

# create output directories
file.loc <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)
file.loc.entire <- paste0(file.loc,'/values/entire_period/')
ifelse(!dir.exists(file.loc.entire), dir.create(file.loc.entire,recursive=TRUE), FALSE)
file.loc.split <- paste0(file.loc,'/values/split_period/')
ifelse(!dir.exists(file.loc.split), dir.create(file.loc.split,recursive=TRUE), FALSE)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# load data and filter results
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg))

# rename levels in table
dat.COM$sex <- as.factor(dat.COM$sex)
levels(dat.COM$sex) <- sex.lookup

# create split dataset for national analysis
dat.split <- data.frame()
for(i in c(0,5,15,25,35,45,55,65,75,85)){
        for(k in c(1,2)){
            dat.split <- rbind(dat.split,circular.split(i,k))
        }}

write.csv(dat.COM,paste0(file.loc,'USA_COM_',year.start.arg,'_',year.end.arg,'.csv'))
saveRDS(dat.split,paste0(file.loc,'com_national_split_values_',year.start.arg,'-',year.end.arg))
