rm(list=ls())

library(plyr)
library(ggplot2)
library(zoo)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# gender state and age lookup
gender.lookup <- c('Men','Women')

# load population data
filename <- paste0('../../data/population/original/datpopjapan20160307')
dat.pop <- readRDS(filename)

# load mortality data
filename <- paste0('../../data/mortality/original/datmortjapan20160307')
dat.mort <- readRDS(filename)

# NATIONAL MONTHLY

############################
# organise population data
############################

# only keep national data
dat.pop.nat <- subset(dat.pop,pref=='0')

# rename columns
dat.pop.nat <- rename(dat.pop.nat,c('deathyear'='year'))

# rename sexes
dat.pop.nat$sex <- as.numeric(as.character(revalue(dat.pop.nat$sex, c("Male"="1", "Female"="2"))))

# centre yearly populations on June
dat.pop.nat$month <- 6

# create grid to attach population values to and interpolate missing values
years  <-    c(min(min(na.omit(dat.mort$deathyear)),min(dat.pop.nat$year)):max(max(na.omit(dat.mort$deathyear)),max(dat.pop.nat$year)))
months <-    c(1:12)
sexes  <-    c(1:2)
ages   <-    c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)

complete.grid <- expand.grid(year=years, month=months,sex=sexes,age=ages)

# merge population with complete grid to highlight missing years
dat.pop.nat.complete <- merge(complete.grid,dat.pop.nat,by=c('year','month','sex','age'),all.x='TRUE')

# reorder complete file
dat.pop.nat.complete <- dat.pop.nat.complete[order(dat.pop.nat.complete$age,dat.pop.nat.complete$sex,dat.pop.nat.complete$year,dat.pop.nat.complete$month),]
rownames(dat.pop.nat.complete) <- 1:nrow(dat.pop.nat.complete)

# remove IHME and pref column
dat.pop.nat.complete$pref_IHME <- dat.pop.nat.complete$pref <- NULL

# interpolate missing populations using zoo and ddply package
dat.pop.nat.complete <- ddply(dat.pop.nat.complete,.(sex,age),function(z) (na.approx(zoo(z))))

############################
# organise mortality data
############################

# rename columns
dat.mort <- rename(dat.mort,c('deathyear'='year','deathmonth'='month','age5'='age'))

# rename sexes
dat.mort$sex <- as.numeric(as.character(revalue(dat.mort$sex, c("Male"="1", "Female"="2"))))

# summarise mortality data nationally
dat.mort.nat <- ddply(dat.mort,.(age,sex,year,month),summarize,deaths=sum(deaths))

# merge mortality with complete grid to highlight missing years
dat.mort.nat.complete <- merge(complete.grid,dat.mort.nat,by=c('year','month','sex','age'),all.x='TRUE')

# remove unknown records
dat.mort.nat <- na.omit(dat.mort.nat)

# leap year test
is.leapyear=function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

dat.mort.nat$leap <- as.integer(is.leapyear(dat.mort.nat$year))

# adjust deaths to a 31-day month
# 30-day months = April, June, September, November (4,6,9,11)
# 31-day months = January, March, May, July, August, October, December (1,3,5,7,8,10,12)
# 28/29-day months = Februray (2)
dat.mort.nat$deaths.adj <- ifelse(dat.mort.nat$month %in% c(1,3,5,7,8,10,12), dat.mort.nat$deaths,
ifelse(dat.mort.nat$month %in% c(4,6,9,11), dat.mort.nat$deaths*(31/30),
ifelse((dat.mort.nat$month==2 & dat.mort.nat$leap==0), dat.mort.nat$deaths*(31/28),
ifelse((dat.mort.nat$month==2 & dat.mort.nat$leap==1), dat.mort.nat$deaths*(31/29),
'ERROR'
))))
dat.mort.nat$deaths.adj <- as.numeric(dat.mort.nat$deaths.adj)

############################
# merge population and mortality data
############################

dat.merged <- merge(dat.mort.nat,dat.pop.nat.complete,by=c('year','month','age','sex'),all.x=1)

# add agegroup groupings
dat.age <- data.frame(  age=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
                        age.new=c(0,5,5,15,15,25,25,35,35,45,45,55,55,65,65,75,75,85,85,85,85))
dat.merged <- merge(dat.merged,dat.age,by='age',all.x=1)

# summarise by age group
dat.merged <- ddply(dat.merged,.(age.new,sex,year,month),summarize,deaths.adj=sum(deaths.adj),population=sum(population))

# rename columns
dat.merged <- rename(dat.merged,c('age.new'='age'))

# reorder complete file
dat.merged <- dat.merged[order(dat.merged$age,dat.merged$sex,dat.merged$year,dat.merged$month),]
rownames(dat.merged) <- 1:nrow(dat.merged)

# only keep 1981-2009
dat.merged <- subset(dat.merged,year %in% c(1981:2009))

# calculate death rates
dat.merged$rate.adj <- with(dat.merged,deaths.adj/population)

# OUTPUT

# create directory for output
file.loc <- paste0('../../output/prep_data/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

saveRDS(dat.merged,paste0(file.loc,'datjp_nat_rate_',year.start,'_',year.end))

# OUTPUT PLOTS

# MONTHLY

# plot national deaths by age group over time
#pdf(paste0(file.loc,'japan_nat_deaths_1969_2011','.pdf'),height=0,width=0,paper='a4r')
#ggplot(data=dat.mort.nat,aes(x=year,y=deaths)) +
#geom_line(aes(color=as.factor(age))) +
#facet_wrap(~sex)
#dev.off()

# plot national population by age group over time
#pdf(paste0(file.loc,'japan_nat_pop_1975_2013','.pdf'),height=0,width=0,paper='a4r')
#ggplot(data=dat.pop.nat,aes(x=year,y=population)) +
#geom_line(aes(color=as.factor(age))) +
#facet_wrap(~sex)
#dev.off()

#pdf(paste0(file.loc,'japan_nat_pop_1975_2013_interpolated','.pdf'),height=0,width=0,paper='a4r')
#ggplot() +
#geom_line(dat=na.omit(dat.pop.nat.complete),aes(x=year,y=population,color=as.factor(age))) +
#facet_wrap(~sex)
#dev.off()

# plot national death rates by age group over time
#pdf(paste0(file.loc,'japan_nat_log_deathrates_1980_2010','.pdf'),height=0,width=0,paper='a4r')
#ggplot(data=dat.merged,aes(x=year,y=log(rate))) +
#geom_line(aes(color=as.factor(age))) +
#facet_wrap(~sex)
#dev.off()

# SUBNATIONAL MONTHLY

############################
# organise population data
############################

# delete national summary data
dat.pop <- subset(dat.pop,pref!='0')

# rename columns
dat.pop <- rename(dat.pop,c('deathyear'='year'))

# rename sexes
dat.pop$sex <- as.numeric(as.character(revalue(dat.pop$sex, c("Male"="1", "Female"="2"))))

# centre yearly populations on June
dat.pop$month <- 6

# create grid to attach population values to and interpolate missing values
years  <-    c(1980:max(max(na.omit(dat.mort$deathyear)),max(dat.pop$year)))
months <-    c(1:12)
sexes  <-    c(1:2)
prefs  <-   c(sort(unique(as.character(dat.pop$pref))))
ages   <-    c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)

complete.grid <- expand.grid(year=years, month=months,sex=sexes,age=ages, pref=prefs)

# merge population with complete grid to highlight missing years
dat.pop.complete <- merge(complete.grid,dat.pop,by=c('year','month','sex','age','pref'),all.x='TRUE')

# convert pref from factor to character
dat.pop.complete$pref <- as.character(dat.pop.complete$pref)

# reorder complete file
dat.pop.complete <- dat.pop.complete[order(dat.pop.complete$pref,dat.pop.complete$age,dat.pop.complete$sex,dat.pop.complete$year,dat.pop.complete$month),]
rownames(dat.pop.complete) <- 1:nrow(dat.pop.complete)

# remove IHME and pref column
dat.pop.complete$pref_IHME <- NULL

# create a lookup table of prefectures and create unique numerical lookup
dat.pref <- data.frame(pref=unique(dat.pop.complete$pref),pref_id=1:nrow(dat.pref))

# merge with population complete table
dat.pop.complete <- merge(dat.pop.complete,dat.pref,by='pref')

# interpolate missing populations using zoo and ddply package
dat.pop.complete2 <- ddply(dat.pop.complete$population,.(sex,age,pref_id),function(z) (na.approx((z))))
test <- ddply(dat.pop.complete,.(sex,age,pref),function(z) (sum(is.na(z))))
test2 <- ddply(dat.pop.complete,.(sex,age,pref),function(z) (nrow(z)))
test3 <- merge(


# plot by age by prefecture
ggplot(data=subset(dat.pop.complete, sex == 2 & age==60)) +
geom_point(aes(x=year,y=population,color=as.factor(pref))) +
theme_bw()