rm(list=ls())

library(foreign)
library(readr)
library(dplyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year.arg <- as.numeric(args[1])

# create output directory
ifelse(!dir.exists("../../output/pop_old_new_compare"), dir.create("../../output/pop_old_new_compare"), FALSE)
ifelse(!dir.exists(paste0("../../output/pop_old_new_compare/",year.arg)), dir.create(paste0("../../output/pop_old_new_compare/",year.arg)), FALSE)

# load 2 files to compare
dat.old <- read.dta(paste0("../../data/population/original/countyPopulations.dta"))
dat.new <- read.dta(paste0("../../output/pop_format/popcounty",year.arg,'.dta'))

# filter for year of interest and get rid of 99 age group
dat.old <- subset(dat.old,year==year.arg & age!=99)

##### 1. compare county by county population

# merge files
merged <- merge(dat.old,dat.new,by=c('fips','sex','age','year','stateFips','countyFips'))

# rows which do not match death counts in merged file
merged$diff <- abs(merged$pop.x - merged$pop.y)
dat.diff <-  merged[merged$diff!=0,]
dat.diff$percent.change <- 100*dat.diff$diff/dat.diff$pop.x

# check unmatched rows from new and old files
unmatched.old <- anti_join(dat.old,dat.new,by=c('fips','sex','age','year','stateFips','countyFips'))
unmatched.new <- anti_join(dat.new,dat.old,by=c('fips','sex','age','year','stateFips','countyFips'))

# plot population against each other to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/old_against_new_population_county_',year.arg,'.png'))
print(
ggplot(merged,aes(x=pop.x,y=pop.y)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new county population ',year.arg)) +
xlab('original population') +
ylab('new population') +
theme_bw()
)
dev.off()

# output files of mismatched data
write.csv(unmatched.old,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_old_county_',year.arg,'.csv'),row.names=FALSE)
write.csv(unmatched.new,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_new_county_',year.arg,'.csv'),row.names=FALSE)
write.csv(dat.diff,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_population_county_',year.arg,'.csv'),row.names=FALSE)

##### 2. compare state by state population

# summarise by state for old and new data
library(plyr)
dat.state.old <- plyr::ddply(dat.old,.(stateFips,sex,age,year),summarise,pop.old=sum(pop))
dat.state.new <- plyr::ddply(dat.new,.(stateFips,sex,age,year),summarise,pop.new=sum(pop))

# merge files
merged.state <- merge(dat.state.old,dat.state.new,by=c('sex','age','year','stateFips'))

# rows which do not match death counts in merged file
merged.state$diff <- abs(merged.state$pop.old - merged.state$pop.new)
merged.state$percent.change <- 100*merged.state$diff/merged.state$pop.old
dat.diff.state <-  merged.state[merged.state$diff!=0,]
dat.diff.state$percent.change <- 100*dat.diff.state$diff/dat.diff.state$pop.old

# check unmatched rows from new and old files
unmatched.state.old <- anti_join(dat.state.old,dat.state.new,by=c('sex','age','year','stateFips'))
unmatched.state.new <- anti_join(dat.state.new,dat.state.old,by=c('sex','age','year','stateFips'))

# output files of mismatched data
write.csv(unmatched.state.old,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_old_state_',year.arg,'.csv'),row.names=FALSE)
write.csv(unmatched.state.new,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_new_state_',year.arg,'.csv'),row.names=FALSE)
write.csv(dat.diff.state,paste0('../../output/pop_old_new_compare/',year.arg,'/unmatched_population_state_',year.arg,'.csv'),row.names=FALSE)

# plot population against each other to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/old_against_new_population_state_',year.arg,'.png'))
print(
ggplot(merged.state,aes(x=pop.old,y=pop.new)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new state population ',year.arg)) +
xlab('original population') +
ylab('new population') +
theme_bw()
)
dev.off()

# plot population against each other by state to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/old_against_new_population_state_by_state_',year.arg,'.png'))
print(
ggplot(merged.state,aes(x=pop.old,y=pop.new)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new state population ',year.arg)) +
xlab('original population') +
ylab('new population') +
facet_wrap(~stateFips) + 
theme_bw()
)
dev.off()

# plot population against each other by age group to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/old_against_new_population_state_by_age_',year.arg,'.png'))
print(
ggplot(merged.state,aes(x=pop.old,y=pop.new)) +
geom_point() +
geom_abline(slope=1) +
ggtitle(paste0('old against new state population ',year.arg)) +
xlab('original population') +
ylab('new population') +
facet_wrap(~age) + 
theme_bw()
)
dev.off()

# plot percentage change by state to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/percentage_change_population_by_state_',year.arg,'.png'))
print(
ggplot(merged.state,aes(x=as.factor(age),y=percent.change)) +
geom_point() +
geom_abline(slope=0) +
ggtitle(paste0('percentage population difference by state ',year.arg)) +
xlab('state') +
ylab('percentage change') +
facet_wrap(~stateFips) + 
theme_bw()
)
dev.off()

# plot percentage change by age to check
png(paste0('../../output/pop_old_new_compare/',year.arg,'/percentage_change_population_by_age_',year.arg,'.png'))
print(
ggplot(merged.state,aes(x=as.factor(stateFips),y=percent.change)) +
geom_point() +
geom_abline(slope=0) +
ggtitle(paste0('percentage population difference by age ',year.arg)) +
xlab('state') +
ylab('percentage change') +
facet_wrap(~age) + 
theme_bw()
)
dev.off()




