rm(list=ls())

library(plyr)
library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# gender state and age lookup
gender.lookup <- c('male','female')
age.lookup <- unique(dat$age)

############################
# organise popualaton data
############################


# load population data
filename <- paste0('../../data/population/original/datpopjapan20160307')
dat.pop <- readRDS(filename)

# only keep national data
dat.pop.nat <- subset(dat.pop,pref=='0')

# create new column which will group all of the 80+ ages to eventually subtract from 75+ age group
dat.pop.nat$age.group <- ifelse(dat.pop.nat$age<=75,dat.pop.nat$age,80)

# summarise by new age groupings to be able to compare 80+ with 75+
dat.pop.nat.2 <- ddply(dat.pop.nat,.(pref,age.group,sex,deathyear),summarize,population=sum(population))

############################
# organise mortality data
############################

# load mortality data
filename <- paste0('../../data/mortality/original/datmortjapan20160307')
dat.mort <- readRDS(filename)

# summarise mortality data


# NATIONAL

# OUTPUT PLOTS

# create directory for output
file.loc <- paste0('../../output/data_explore/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# plot national population by age group over time
pdf(paste0(file.loc,'japan_nat_pop_1975_2013','.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat.pop.nat,aes(x=deathyear,y=population)) +
geom_line(aes(color=as.factor(age))) +
facet_wrap(~sex)
dev.off()

pdf(paste0(file.loc,'japan_nat_pop_adj_1975_2013','.pdf'),height=0,width=0,paper='a4r')
ggplot(data=dat.pop.nat.2,aes(x=deathyear,y=population)) +
geom_line(aes(color=as.factor(age.group))) +
facet_wrap(~sex)
dev.off()

