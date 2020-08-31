rm(list=ls())

library(spdep)
library(rgdal)
library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)

setwd("~/git/mortality/USA/state/data/covariates/")

################ DEFINE ######################

polnam <- 'pm25' # select pollution type
id_sex <- 2 # select sex type (1=male, 2=female)
startyear <- 1999 # extracting data starting in this year
obs.Years <- 18 # ... for n years

refyr <- 2000 # reference year to which LC mort rate and income should be standardised
id_race <- 2 #1=white, 2=black, 3=native, 4=asian

##################### INCLUDE POPULATION DATA ####################

load('nchs_raw_annotated_withag_1990_to_2016') # CURRENTLY MISSING
pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#
################## INCLUDE INCOME DATA ####################
load('income_with_sc')
income_us_sc <- subset(income_us_sc, year >= startyear & year < (startyear+obs.Years))

# remove all US and all county values as well as overseas, AK, HI
income_us_sc$STATEFP <- substr(income_us_sc$fips,1,2)
income_us_sc$COUNTYFP <- substr(income_us_sc$fips,3,5)
income_us_sc <- subset(income_us_sc,STATEFP != '00')
income_us_sc <- subset(income_us_sc,COUNTYFP != '000')
income_us_sc <- subset(income_us_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))

# adjust for inflation 
CPIdf <- read.csv2('CPI.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
CPI_base <- subset(CPIdf,year==refyr)$CPI
CPIdf$ratio <- as.numeric(unlist(CPI_base))/as.numeric(unlist(CPIdf$CPI))
income_us_sc <- left_join(income_us_sc,CPIdf,by='year')
income_us_sc$pc_const <- round(income_us_sc$inc_pc_sc*income_us_sc$ratio)

################## INCLUDE POVERTY DATA ##################
load('povperc_with_sc')
poppov_us_sc <- subset(poppov_us_sc, year >= startyear & year < (startyear+obs.Years))

# remove all US and all county figures as well as overseas, AK, HI
poppov_us_sc$STATEFP <- substr(poppov_us_sc$fips,1,2)
poppov_us_sc$COUNTYFP <- substr(poppov_us_sc$fips,3,5)
poppov_us_sc <- subset(poppov_us_sc,STATEFP != '00')
poppov_us_sc <- subset(poppov_us_sc,COUNTYFP != '000')
poppov_us_sc <- subset(poppov_us_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))

################## INCLUDE RACE DATA ##################
# trim race data 
pop_race <- subset(dat_nchs,race==id_race & sex==id_sex & year >= startyear & year < (startyear+obs.Years))

# sum for this race for each fips-year
pop_race_allage <- data.frame(summarise(group_by(pop_race,year,fips),popsum=sum(popsum)))

# get race perc 
poprace_us_merged <- left_join(pop_race_allage,pop_nchs_allage,by=c('year','fips'))
poprace_us_merged$popraceprop <- poprace_us_merged$popsum.x/poprace_us_merged$popsum.y

# remove AK + HI + overseas 
poprace_us_merged$STATEFP <- substr(poprace_us_merged$fips,1,2)
poprace_us_merged <- subset(poprace_us_merged,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))
poprace_us_merged <- poprace_us_merged[,c(1,2,6)]

################## INCLUDE EDU DATA ##################

load('edu_with_sc')
# cut dataset to particular years 
edu_sc <- subset(edu_sc, year >= startyear & year < (startyear+obs.Years))
# remove all US and all county figures as well as overseas, AK, HI
edu_sc$STATEFP <- substr(edu_sc$fips,1,2)
edu_sc$COUNTYFP <- substr(edu_sc$fips,3,5)
edu_sc <- subset(edu_sc,STATEFP != '00')
edu_sc <- subset(edu_sc,COUNTYFP != '000')
edu_sc <- subset(edu_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79','72'))

################# INCLUDE UR DATA ######################
load('urbanpop_with_sc')

# merge by mc
daturban_mctag <- daturban
daturban_mc <- data.frame(summarise(group_by(daturban_mctag,fips,year),totpop=sum(totpop),toturban=sum(toturban)))
daturban_mc$urban <- with(daturban_mc,toturban/totpop)

# interpolate between years 
daturbaninterp <- data.frame()
for (cty in unique(daturban_mc$fips)) {
  
  x <- c(2000,2010)
  y <- subset(daturban_mc,fips==cty)$urban
  xout <- c(2001:2009)
  if (sum(is.na(y))==0 ) {
    yinterp <- approx(x,y,xout)$y
  }
  if (sum(is.na(y))>0) {
    y[is.na(y)] <- y[is.finite(y)]
    yinterp <- approx(x,y,xout)$y
  }
  
  dattemp <- data.frame(fips=rep(cty,obs.Years),year=c(startyear:(startyear+obs.Years-1)),urban=numeric(length=obs.Years))
  dattemp[dattemp$year==1999,'urban'] <- y[1]
  dattemp[dattemp$year==2000,'urban'] <- y[1]
  dattemp[dattemp$year==2010,'urban'] <- y[2]
  dattemp[dattemp$year==2011,'urban'] <- y[2]
  dattemp[dattemp$year==2012,'urban'] <- y[2]
  dattemp[dattemp$year==2013,'urban'] <- y[2]
  dattemp[dattemp$year==2014,'urban'] <- y[2]
  dattemp[dattemp$year==2015,'urban'] <- y[2]
  dattemp[dattemp$year==2016,'urban'] <- y[2]
  dattemp[dattemp$year %in% xout,'urban'] <- yinterp
  daturbaninterp <- rbind(daturbaninterp,dattemp)
}
  
################# INCLUDE UNEMPLOYMENT DATA ######################
load('unemployment_with_sc')
dat_unemployment_sc <- subset(dat_unemployment_sc,year >= startyear & year < (startyear + obs.Years))

dat_unemployment_sc$unemp_rate <- dat_unemployment_sc$unemployed_tot/dat_unemployment_sc$labour_force_tot

################## COLLECT ALL PROCESSED COVARIATES ##################

income_us_sc = income_us_sc[,c(1,2,8)]
poppov_us_sc = poppov_us_sc[,c(1,2,3)]
poprace_us_merged = poprace_us_merged
edu_sc = edu_sc[,c(1,2,3)]
daturbaninterp = daturbaninterp
dat_unemployment_sc = dat_unemployment_sc[,c(1,2,5)]

# make expanded grid of years, fips to merge all completely to
fips_codes = sort(unique(popsum_nchs$fips)) # SHOULD THIS BE ALL THE FIPS CODES FROM THE TROPICAL STORM FILE?
years = c(startyear:(startyear+obs.Years-1))
data_complete = expand.grid(year=years,fips=fips_codes)

# merge all above files together
data_complete = merge(data_complete, income_us_sc,by=c('year','fips'),all.X=TRUE)
data_complete = merge(data_complete, poppov_us_sc,by=c('year','fips'),all.X=TRUE)
data_complete = merge(data_complete, poprace_us_merged,by=c('year','fips'),all.X=TRUE)
data_complete = merge(data_complete, daturbaninterp,by=c('year','fips'),all.X=TRUE)
data_complete = merge(data_complete, dat_unemployment_sc,by=c('year','fips'),all.X=TRUE)
data_complete = merge(data_complete, edu_sc,by=c('year','fips'),all.X=TRUE)

# observe NA values
data_complete_na = data_complete[rowSums(is.na(data_complete))>0,]

# save









