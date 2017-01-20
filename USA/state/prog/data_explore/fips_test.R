rm(list=ls())

library(foreign)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
state.arg <- as.character(args[3])

# create output directory
file.loc <-paste0("../../output/fips_test/",state.arg,"/")
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=1), FALSE)

# DEATHS

# load data
filename <- paste0('~/data/mortality/US/state/processed/county/deaths',year.start.arg,'.dta')
dat <- read.dta(filename)

# create state and county fips
dat$state.fips <- substr(dat$fips,1,2)
dat$county.fips <- substr(dat$fips,3,5)

# filter by chosen state and create unique county fips list
county.fips <- subset(dat,state.fips==state.arg)
county.fips <- data.frame(new=sort(unique(county.fips$county.fips)))

county.fips$dummy <- county.fips[,ncol(county.fips)]

# rename column to include year
colnames(county.fips)[ncol(county.fips)-1] <- paste0('county.fips.',year.start.arg)

# prepare for looping
test <- county.fips

# define years to loop over
loop.years <- c((year.start.arg+1):year.end.arg)

for(year.current in loop.years){

    # load data
    filename <- paste0('~/data/mortality/US/state/processed/county/deaths',year.current,'.dta')
    dat <- read.dta(filename)

    # create state and county fips
    dat$state.fips <- substr(dat$fips,1,2)
    dat$county.fips <- substr(dat$fips,3,5)

    # filter by chosen state and create unique county fips list
    temp <- subset(dat,state.fips==state.arg)
    temp <- data.frame(new=sort(unique(temp$county.fips)))

    colnames(temp)[ncol(temp)] <- paste0('county.fips.',year.current)
    temp$dummy <- temp[,ncol(temp)]
    
    test <- merge(test,temp,by.x=('dummy'),by.y=(paste0('dummy')),all.x=TRUE,all.y=TRUE)
    test$dummy <- test[,ncol(test)]
    
}

test$dummy <- NULL

# sort results
#test <- test[order(test$county.fips.allyears),]

# only keep incompete records
output <- test[rowSums(is.na(test)) > 0,]

# output results
write.csv(output,paste0(file.loc,'fips_test_deaths_',state.arg,'.csv'),row.names=FALSE)

# POPULATION

# load data and filter for first year
dat.pop <- read.dta("~/data/mortality/US/state/processed/county/countyPopulationsnewyears.dta")

dat <- dat.pop

# create state and county fips
dat$state.fips <- dat$stateFips
dat$county.fips <- dat$countyFips

# filter by chosen state and create unique county fips list
county.fips <- subset(dat,state.fips==state.arg)
county.fips <- data.frame(new=sort(unique(county.fips$county.fips)))

county.fips$dummy <- county.fips[,ncol(county.fips)]

# rename column to include year
#colnames(county.fips)[ncol(county.fips)-1] <- paste0('county.fips.',year.start.arg)
colnames(county.fips)[ncol(county.fips)-1] <- paste0('county.fips.allyears')

# prepare for looping
test <- county.fips

# define years to loop over
loop.years <- c((year.start.arg):year.end.arg)

for(year.current in loop.years){
    
    # load data
    dat <- subset(dat.pop,year==year.current)
    
    # create state and county fips
    dat$state.fips <- dat$stateFips
    dat$county.fips <- dat$countyFips
    
    # filter by chosen state and create unique county fips list
    temp <- subset(dat,state.fips==state.arg)
    temp <- data.frame(new=sort(unique(temp$county.fips)))
    
    colnames(temp)[ncol(temp)] <- paste0('county.fips.',year.current)
    temp$dummy <- temp[,ncol(temp)]
    
    test <- merge(test,temp,by.x=('dummy'),by.y=(paste0('dummy')),all.x=TRUE,all.y=TRUE)
    test$dummy <- test[,ncol(test)]
    
}

test$dummy <- NULL

# sort results
test <- test[order(test$county.fips.allyears),]

# only keep incompete records
output <- test[rowSums(is.na(test)) > 0,]

# output results
write.csv(output,paste0(file.loc,'fips_test_pop_',state.arg,'.csv'),row.names=FALSE)





