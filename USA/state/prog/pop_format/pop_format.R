rm(list=ls())

library(foreign)
library(plyr)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year <- as.numeric(args[1])

# load population files, split into three from downloading restrictions
filename.1 <- paste0('../../data/population/cleaned/',year,'_0_29_county_pop.txt')
dat.1 <- read.table(filename.1,sep="\t", header=TRUE)
filename.2 <- paste0('../../data/population/cleaned/',year,'_30_64_county_pop.txt')
dat.2 <- read.table(filename.2,sep="\t", header=TRUE)
filename.3 <- paste0('../../data/population/cleaned/',year,'_65_85_county_pop.txt')
dat.3 <- read.table(filename.3,sep="\t", header=TRUE)

# append files
dat <- rbind(dat.1,dat.2,dat.3)

# get rid of unnecessary columns
dat$Notes <- NULL
dat$Age.Group <- NULL
dat$State <- NULL
dat$County <- NULL
dat$Gender <- NULL

# fix age
new.age <- c('0','0','10','15','20','25','5','30','35','40','45','50','55','60','65','70','75','80','85')
#new.age <- c('0','0','5','15','15','25','5','25','35','35','45','45','55','55','65','65','75','75','85')
dat$age <- mapvalues(dat$Age.Group.Code, from = as.vector(levels(dat$Age.Group.Code)), to = new.age)

# fix gender
new.gender <- c('2','1')
dat$sex <- mapvalues(dat$Gender.Code, from= as.vector(levels(dat$Gender.Code)), to = new.gender)

# fix fips
# add '0' to fips codes
dat$stateFips<- paste0('0',as.character(dat$State.Code))

# function to strip last two values of string
substrRight <- function(x,n){
    substr(x, nchar(x)-n+1, nchar(x))
}

# take last two values of string to create two-digit state fips codes
dat$stateFips<- substrRight(dat$stateFips,2)

# fix county code
dat$fips <- paste0('0',as.character(dat$County.Code))

# take last two values of string to create two-digit state fips codes
dat$fips <- substrRight(dat$fips,5)

# create county code
dat$countyFips <- substrRight(dat$fips,3)

# copy population column to fix name
dat$pop <- dat$Population

# add year code
dat$year <- year

# get rid of old columns
dat <- dat[,c('fips','sex','age','pop','year','stateFips','countyFips')]

# convert columns to correct class
dat$sex <- as.numeric(as.character(dat$sex))
dat$age <- as.numeric(as.character(dat$age))
dat$pop <- as.numeric(as.character(dat$pop))

# remove NAs
dat <- na.omit(dat)

# create output directory
ifelse(!dir.exists("../../output/pop_format"), dir.create("../../output/pop_format"), FALSE)

# output file
write.dta(dat,paste0("../../output/pop_format/popcounty",year,".dta"))
