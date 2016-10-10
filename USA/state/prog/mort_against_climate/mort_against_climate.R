rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.mort.arg <- as.numeric(args[3])
year.end.mort.arg <- as.numeric(args[4])
year.start.clim.arg <- as.numeric(args[5])
year.end.clim.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# lookups
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

# create files for output
file.loc <- '../../output/mort_against_climate/'
file.loc <- paste0(file.loc,dname.arg)
file.loc <- paste0(file.loc,'/',metric.arg)
file.loc <- paste0(file.loc,'/',year.start.arg,'_',year.end.arg)
file.loc <- paste0(file.loc,'/',sex.lookup[sex.arg])
file.loc <- paste0(file.loc,'/',age.arg)
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA mortality data and filter for relevant years
dat <- readRDS(paste0('../../output/prep_data/datus_state_rates_',year.start.mort.arg,'_',year.end.mort.arg))
dat <- subset(dat, year %in% years & sex == sex.arg & age== age.arg)

# load climate data and filter for relevant years
dat.climate <- readRDS(paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/state_weighted_summary_',metric.arg,'_',dname.arg,'_',year.start.clim.arg,'_',year.end.clim.arg,'.rds'))
dat.climate <- subset(dat.climate, year %in% years)
dat.climate$state.fips <- as.numeric(dat.climate$state.fips)

# merge mortality and climate data and reorder
dat.merged <- merge(dat,dat.climate,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# exclude Hawaii and Alaska
state.lookup <- subset(state.lookup, fips !=2) 
state.lookup <- subset(state.lookup, fips !=15)
state.names <- as.character(state.lookup$full_name)
dat.merged <- subset(dat.merged, fips !=2) 
dat.merged <- subset(dat.merged, fips !=15) 

# generalise variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# rename rows and remove unnecessary columns
dat.merged$id <- NULL
rownames(dat.merged) <- 1:nrow(dat.merged)

library(ggplot2)
library(plyr)

# variables for pretty and easy to read plots
dat.merged$month.short <- mapvalues(dat.merged$month,from=unique(dat.merged$month),to=month.short)
dat.merged$month.short <- reorder(dat.merged$month.short,dat.merged$month)
# variables for pretty and easy to read plots
dat.merged$state.name <- mapvalues(dat.merged$fips,from=unique(dat.merged$fips),to=state.names)
dat.merged$state.name <- reorder(dat.merged$state.name,dat.merged$fips)

#############################################
# 1. PLOT ALL TOGETHER
#############################################

pdf(paste0(file.loc,'/','deaths_rates_together_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000)) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
theme_bw()
dev.off()

#############################################
# 2. FACET BY MONTH
#############################################

pdf(paste0(file.loc,'/','deaths_rates_month_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000)) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
facet_wrap(~month.short) +
theme_bw()
dev.off()

#############################################
# 3. FACET BY STATE
#############################################

pdf(paste0(file.loc,'/','deaths_rates_state_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000)) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates by state ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
facet_wrap(~state.name) +
theme_bw()
dev.off()
