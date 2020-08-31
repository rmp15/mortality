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
fips_codes = sort(unique(popsum_nchs$fips))
years = c(startyear:(startyear+obs.Years-1))
data_complete = expand.grid(year=years,fips=fips_codes)

# merge all above files together
data_complete = merge(data_complete, income_us_sc,by=c('year','fips'),all.X=TRUE)

# TO FINISH FROM HERE!

# BELOW NOT USED MAYBE TIDY UP LATER

################## GETTING AGE RELATED TERMS ##################
#
#adj.age <- vector('numeric',length = Ages*2-2)
#adj.age[seq(1,length(adj.age),2)] <- c(2:Ages)
#adj.age[seq(2,length(adj.age),2)] <- c(1:(Ages-1))
#weights.age <- rep(1,length(adj.age))
#num.age <- rep(2,Ages)
#num.age[c(1,Ages)] <- 1
#
#
################### GETTING MORTALITY AND POPULATION DATA ##################
#
## mortality and population data
#pop_ag <- data.frame(summarise(group_by(dat_nchs,year,fips,sex,ag),pop=sum(popsum)))
#pop_ag_mc <- combine_mc(combine_sc(pop_ag,scloc.df.sc),scloc.df)
#pop <- data.frame(summarise(group_by(pop_ag_mc,year,fips,sex,ag),pop=sum(pop)))
#pop <- subset(pop,sex==id_sex)
#
#dat <- data.frame()
#for (yrget in startyear:(startyear+obs.Years-1)) {
#  mort <- read.dta(paste0('archive/code/make_data/outputs/causespec_deaths',yrget,'_with_sc.dta'))
#  if (yrget >= 1999) {
#    mort_LC <- get_mort_causespec(mort,causefindLC_icd10)
#    mort <- get_mort_causespec(mort,causefind_icd10)
#  }
#  if(yrget < 1999){
#    ####################### adjust for icd9 ########################
#    mort$cause[nchar(mort$cause)==3] <- paste0(mort$cause[nchar(mort$cause)==3],'0')
#    ################################################################
#    mort_LC <- get_mort_causespec(mort,causefindLC_icd9)
#    mort <- get_mort_causespec(mort,causefind_icd9)
#  }
#  mort <- subset(mort,sex==id_sex)
#  mort_mc <- combine_mc(mort,scloc.df)
#  mortyr <- as.data.frame(summarise(group_by(mort_mc,fips,sex,age,year),deathsum=sum(deaths)))
#  popyr <- subset(pop,year==yrget)
#
#  #exclude alaska and hawaii from mortality and population data
#  mortyr <- mortyr[-c(grep('02...',mortyr$fips),grep('15...',mortyr$fips)),]
#  popyr <- popyr[-c(grep('02...',popyr$fips),grep('15...',popyr$fips)),]
#
#  # check which counties not zero pop counties
#  popnonzero <- popyr[popyr$pop > 0,]
#
#  # join mortality and population data
#  # but first add ag to mort
#  dat_ag <- data.frame(age=sort(unique(mortyr$age)),ag=c(1:length(unique(mortyr$age))))
#  mortyr <- left_join(mortyr,dat_ag)
#  datadd <- left_join(y = mortyr,x=popnonzero,by=c('fips','ag','sex','year'))
#
#  # merge LC death rates
#  mort_LC <- subset(mort_LC,sex==id_sex)
#  #sum(mort_LC$deaths)
#  mort_mc_LC <- combine_mc(mort_LC,scloc.df)
#  #sum(mort_mc_LC$deaths)
#  mortyr_LC <- as.data.frame(summarise(group_by(mort_mc_LC,fips,sex,age,year),deathsum=sum(deaths)))
#  #sum(mortyr_LC$deathsum)
#  #exclude alaska and hawaii from mortality and population data
#  mortyr_LC <- mortyr_LC[-c(grep('02...',mortyr_LC$fips),grep('15...',mortyr_LC$fips)),]
#  #sum(mortyr_LC$deathsum)
#  colnames(mortyr_LC)[5] <- 'deathsum_LC'
#  #sum(mortyr_LC$deathsum)
#  mortyr_LC <- left_join(mortyr_LC,dat_ag)
#  datadd_LC <- left_join(datadd,mortyr_LC,by=c('fips','ag','sex','year'))
#  #sum(datadd_LC$deathsum_LC,na.rm=TRUE)
#
#
#  # append deaths to previous years
#  dat <- rbind(dat,as.data.frame(datadd_LC))
#
#  morttrim <- subset(mort,sex==id_sex)
#  morttrim <- morttrim[-c(grep('02...',morttrim$fips),grep('15...',morttrim$fips)),]
#  morttrimLC <- subset(mort_LC,sex==id_sex)
#  morttrimLC <- morttrimLC[-c(grep('02...',morttrimLC$fips),grep('15...',morttrimLC$fips)),]
#  print(paste0('year=',yrget,' deathbugs=',sum(datadd$deathsum,na.rm=TRUE),' deathuscodfile=',sum(morttrim$deaths,na.rm=TRUE),' deathbugsLC=',sum(datadd_LC$deathsum_LC,na.rm=TRUE),' deathuscodfileLC=',sum(morttrimLC$deaths,na.rm=TRUE)))
#
#}
#
#
#  ################## CLEANING AND COLLECTING VALUES INTO DATAFRAME ##################
#  totdeaths <- data.frame(summarise(group_by(dat,year,fips),totdeath=sum(deathsum,na.rm = TRUE),totLCdeath=sum(deathsum_LC,na.rm=TRUE)))
#  subset(totdeaths,totdeath==0 | totLCdeath==0)
#
#  # change NA deaths to 0
#  dat$deathsum[is.na(dat$deathsum)] <- 0
#  dat$deathsum_LC[is.na(dat$deathsum_LC)] <- 0
#  # assign county id
#  dat <- left_join(dat,ctyid,by='fips')
#
#  #fix strange mortality rates for males and females
#  dat$mortrate <- dat$deathsum/dat$pop
#  dat[dat$mortrate > 1,]$deathsum <- dat[dat$mortrate > 1,]$pop
#
#  # convert obs year to year number
#  dat$year <- dat$year-startyear+1
#
#  # include race to data
#  poprace_us_merged$year <- poprace_us_merged$year - startyear + 1
#  dat <- left_join(dat,poprace_us_merged,by=c('fips','year'))
#
#  # include hisp to data
#  #pophisp_us_merged$year <- pophisp_us_merged$year - startyear + 1
#  #dat <- left_join(dat,pophisp_us_merged,by=c('fips','year'))
#
#  # include edu to data
#  edu_us_merged$year <- edu_us_merged$year - startyear + 1
#  dat <- left_join(dat,edu_us_merged,by=c('fips','year'))
#
#  # include PM to data
#  dat <- left_join(dat,apmc_bugs,by=c('fips','year'))
## include O3 to data
#  dat <- left_join(dat,o3mc_bugs,by=c('fips','year'))
## include NO2 to data
#  dat <- left_join(dat,no2mc_bugs,by=c('fips','year'))
## include SO2 to data
#  dat <- left_join(dat,so2mc_bugs,by=c('fips','year'))
## include PM10 to data
#  dat <- left_join(dat,pm10mc_bugs,by=c('fips','year'))
## include CO to data
#  dat <- left_join(dat,comc_bugs,by=c('fips','year'))
#
#
#
#
#  # include temperature
#  dat <- left_join(dat,temp,by=c('fips','year'))
#
#  # include realtive humidity
#  dat <- left_join(dat,relh,by=c('fips','year'))
#
#
#
#  ## calculate age-standardised LC rates
#  pop_mf_mc <- data.frame(summarise(group_by(pop_ag_mc,year,fips,sex,ag),pop=sum(pop)))
#  dat$mxLC <- dat$deathsum_LC/dat$pop
#  poprefyr <- data.frame(summarise(group_by(subset(pop_mf_mc,year==refyr),fips,ag),popmf=sum(pop)))
#  dat_popref <- left_join(dat,poprefyr,by=c('fips','ag'))
#  dat_popref$mxpop <- dat_popref$mxLC*dat_popref$popmf
#  dat_popref_agestand <- data.frame(summarise(group_by(dat_popref,year,fips),mx_agestand=sum(mxpop,na.rm=TRUE)/sum(popmf,na.rm=TRUE)))
#  # fill 0 LC deaths with mean of 3 yr prior and after...
#  LCzero <- data.frame(year=subset(dat_popref_agestand,mx_agestand==0)$year,fips=subset(dat_popref_agestand,mx_agestand==0)$fips)
#  if (nrow(LCzero)>0) {
#    for (n in 1:nrow(LCzero)) {
#      mxmean <- mean(subset(dat_popref_agestand,fips==LCzero[n,'fips'] & year %in% c((LCzero[n,'year']-3):(LCzero[n,'year']+3)))$mx_agestand)
#      dat_popref_agestand[dat_popref_agestand$fips==LCzero[n,'fips'] & dat_popref_agestand$year==LCzero[n,'year'],]$mx_agestand <- mxmean
#    }
#  }
#  minLC <- min(dat_popref_agestand$mx_agestand[dat_popref_agestand$mx_agestand >0])
#  dat_popref_agestand$mx_agestand[dat_popref_agestand$mx_agestand==0] <- minLC
#  dat_popref_agestand$mx_agestand <- log(dat_popref_agestand$mx_agestand)
#  mx_agestand_mean <- mean(dat_popref_agestand$mx_agestand)
#  dat_popref_agestand$mx_agestand <- dat_popref_agestand$mx_agestand - mx_agestand_mean
#  dat_agestand <- left_join(dat,dat_popref_agestand)
#
#  # include poverty data to dataframe
#  poppov_us_merged$year <- poppov_us_merged$year - startyear + 1
#  dat_agestand_pov <- left_join(dat_agestand,poppov_us_merged,by=c('fips','year'))
#
#  # include income data to dataframe
#  income_us_merged$year <- income_us_merged$year - startyear + 1
#  dat_agestand_pov_inc <- left_join(dat_agestand_pov,income_us_merged,by=c('fips','year'))
#
#  # include urban-rural data to dataframe
#  dat_ur <- subset(daturbaninterp,year >= startyear & year <= startyear+obs.Years-1)
#  dat_ur$year <- dat_ur$year-startyear+1
#  #dat_ur_mean <- mean(dat_ur$urban)
#  #dat_ur$urban <- dat_ur$urban - dat_ur_mean
#  dat_agestand_pov_inc_ur <- left_join(dat_agestand_pov_inc,dat_ur,by=c('fips','year'))
#
#  # include unemployment to data frame
#  dat_unemployment_mc$year <- dat_unemployment_mc$year - startyear + 1
#  dat_agestand_pov_inc_ur_unemp <- left_join(dat_agestand_pov_inc_ur,dat_unemployment_mc[,c('fips','year','unemp_rate')],by=c('fips','year'))
#
#  dat <- dat_agestand_pov_inc_ur_unemp
#
#  # include metro to data frame
#  #dat_agestand_pov_inc_ur_unemp_metro <- left_join(dat_agestand_pov_inc_ur_unemp,datmetro_mc[,c('fips','metro','metro_forceurban','metro_forcerural')])
#  #dat <- dat_agestand_pov_inc_ur_unemp_metro
#
#
#  # include smk to data - not sure if need this at the end but you definitely need to create the SMK file after all the other ones. Otherwise something odd means you have to do the approx linear interpolation by sex??? see JB smoking (no vcersion number)
#  # also included via this is log smoking, both centred
#  smk_us_merged$year <- smk_us_merged$year - startyear + 1
#  dat <- left_join(dat,smk_us_merged,by=c('fips',"sex",'year'))
#
#  #create deciles and then the midpoints of each decile for later plotting - should be done here in order to know that order is preserved
#  dat  <- dat %>% mutate(PMdec = ntile(apmerge+apmean, 10)); table(dat$PMdec)
#  #PM0dec <- bin$quantile;
#  PM0dec_mid <- dat %>% group_by(PMdec) %>% summarise(mid=median(apmerge+apmean))
#  PM0dec_mid <- PM0dec_mid$mid; PM0dec_mid
##M 5.879881  7.309273  8.134246  8.807810  9.481540 10.144370 10.949785 11.901220 12.987820 14.589200
##F 5.880708  7.310580  8.136660  8.810442  9.483443 10.148300 10.952380 11.904435 12.988490 14.591460
#
#
#  #create bins of PM which are evenly apaced
#  dat$PM2s <- cut(dat$apmerge+apmean, breaks=c(0,5,7,9,11,13,15,17,19,100))
#   # (0,5]    (5,7]    (7,9]   (9,11]  (11,13]  (13,15]  (15,17]  (17,19] (19,100]
#    # 7542    41634   105930   113256    80424    46116    12438     1764      630
#dat$PM2s <- factor(dat$PM2s, levels=c("(0,5]", "(5,7]", "(7,9]", "(9,11]", "(11,13]", "(13,15]", "(15,17]", "(17,19]", "(19,100]"), labels=c(1:9) )
#table(round(dat$apmerge+apmean,0),dat$PM2s)
#  #binT <-  subset(dat, PM2s=="(19,100]")
#  #min(binT$apmerge)+apmean;  max(binT$apmerge)+apmean
#  #top one 19.03247 - 22.12176
# # bottom one 2.83018  - 4.991188
#
#  ################## GETTING pm2s RELATED TERMS ##################
#Npm2s <- 9
#adj.pm2s <- vector('numeric',length = Npm2s*2-2)
#adj.pm2s[seq(1,length(adj.pm2s),2)] <- c(2:Npm2s)
#adj.pm2s[seq(2,length(adj.pm2s),2)] <- c(1:(Npm2s-1))
#weights.pm2s <- rep(1,length(adj.pm2s))
#num.pm2s <- rep(2,Npm2s)
#num.pm2s[c(1,Npm2s)] <- 1
#
#
#  ################## WRITING OUT DATA TO BUGS ##################
#
#  n <- dat$pop
#  y <- dat$deathsum
#  yr <- dat$year
#  age <- dat$ag
#  LAD <- dat$id
#  N <- nrow(dat)
#  RACE <- dat$popraceprop
#  pm0 <- dat$apmerge
#  LC <- dat$mx_agestand
#  income_bugs <- dat$log_pc_const
#  poppov_bugs <- dat$poppov_merged
#  #HISP <- dat$pophispprop
#  EDU <- dat$hsgrad
#  UR <- dat$urban
#  UNEMP <- dat$unemp_rate
#  #METRO <- dat$metro
#  #METRO_FORCEURBAN <- dat$metro_forceurban
#  #METRO_FORCERURAL <- dat$metro_forcerural
#  SMK <- dat$smk
#  LSMK <- dat$Lsmk
#  TMP <- dat$temp_ymean
#  RELH <- dat$relh_ymean
#  O3 <- dat$o3merge
#  NO2 <- dat$no2merge
#  SO2 <- dat$so2merge
#  PM10 <- dat$pm10merge
#  PM1025 <- dat$pm10merge +pm10mean - dat$apmerge - apmean # this not centred accidentally but leave for now.
#  CO <- dat$comerge
#
#  pm0dec <- dat$PMdec
#  pm2s <- as.numeric(dat$PM2s)
#
#  dataout = list(n=n,y=y,N=N,yr=yr,age=age,Ages=Ages,adj.age=adj.age,weights.age=weights.age,num.age=num.age,obs.Years=obs.Years,LAD=LAD,LADs=LADs,adj.LAD=adj.LAD,weights.LAD=weights.LAD,num.LAD=num.LAD,PM0=pm0,INC=income_bugs,PERCPOV=poppov_bugs,RACE=RACE,LC=LC,EDU=EDU,UR=UR,UNEMP=UNEMP, SMK=SMK, LSMK=LSMK, TMP=TMP, RELH=RELH, O3=O3, NO2=NO2, PM1025=PM1025, SO2=SO2, CO=CO, pm0dec=pm0dec, pm2s=pm2s)
#
#  ## write out data to file
#  source('archive/code/make_data/writeDatafileRmod.txt')
#  #writeDatafileR(dataout,fileout) #takes long time
#  writeDatafileR(dataout,fileoutT)
#
#  #create winbugs version which has no unnecessary columns and also has binned PM
#    dataoutWB = list(n=n,y=y,N=N,yr=yr,age=age,Ages=Ages,adj.age=adj.age,weights.age=weights.age,num.age=num.age,obs.Years=obs.Years,LAD=LAD,LADs=LADs,adj.LAD=adj.LAD,weights.LAD=weights.LAD,num.LAD=num.LAD,PM0=pm0,INC=income_bugs,PERCPOV=poppov_bugs,RACE=RACE,LC=LC,EDU=EDU,UR=UR,UNEMP=UNEMP, TMP=TMP, RELH=RELH,  pm0dec=pm0dec ,
#    pm2s=pm2s, Npm2s=Npm2s,adj.pm2s=adj.pm2s,weights.pm2s=weights.pm2s,num.pm2s=num.pm2s)
#
#  writeDatafileR(dataoutWB,fileoutTWB)
#
#
#if(FALSE){  #finding quantiles of county temp and relh as doing from the usual route is complicated by the datafiles not coming from the right place.
##what really need to do is make the all cause version of data creation the same as this one and then will have new correct versions and temp and relh on the E where Helen reads them
#
#dat_uniq <- subset(dat,sex==2 & year %in% c(1,17)) %>% group_by(id,year) %>%
#		summarise(temp1 = temp_ymean[1]+temp_ymean_mean, relh1 = relh_ymean[1]+relh_ymean_mean, pov1=poppov_merged[1]+povmean, apmerge1 = apmerge[1]+apmean)
#
#out1_temp_relh <- 	dat_uniq %>% group_by(year) %>%
#		summarise(
#			temp1000=quantile(temp1,0.01),temp1025=quantile(temp1,0.25),temp1050=quantile(temp1,0.50), temp1075=quantile(temp1,0.75), temp1100=quantile(temp1,0.99),
#		relh1000=quantile(relh1,0.01),relh1025=quantile(relh1,0.25),relh1050=quantile(relh1,0.50), relh1075=quantile(relh1,0.75), relh1100=quantile(relh1,0.99) )
#		#pov1000=quantile(pov1,0.01),pov1025=quantile(pov1,0.25),pov1050=quantile(pov1,0.50), pov1075=quantile(pov1,0.75), pov1100=quantile(pov1,0.99),
#		#apmerge1000=quantile(apmerge1,0.01),apmerge1025=quantile(apmerge1,0.25),apmerge1050=quantile(apmerge1,0.50), apmerge1075=quantile(apmerge1,0.75), apmerge1100=quantile(apmerge1,0.99) )
#
#write.csv(out1_temp_relh,   "S:\\Projects\\Disease_trends\\US_county\\US national PM and mortality\\From My Book Duo - Helen\\paper\\nejm\\figures/Table 1 v001 099 v2 ONLY temp-relh_F.csv")
#
##then read it in where we create the table for the paper, rbind and carry on
##NB F only
#
#}
#
#
#
#
#
#
#
#if(FALSE){ #plots, no need to do every time
##compare LC asdr vs IHME asr for each sex. - limit to 1 age group as just repeated
#bin <- dat[dat$ag %in% c(1),] %>% group_by(year,sex) %>% summarise(cor=cor(mx_agestand, smk))
#pdf(paste0('archive/code/make_data/data/smoking/LC-smoking','_',sexnam,'_',startyear,'_',obs.Years,'yrs.pdf'), height=8,width=15)
#	ggplot(dat, aes(x=mx_agestand, y=smk)) +  geom_point(size=0.2) + facet_wrap(~(year+startyear-1)) + geom_text(data=bin,aes(-3,10,label=round(cor,2)),colour="blue") + ylab("Smoking prevalence") + xlab("log ASDR LC")
#	ggplot(bin, aes(x=year+startyear-1, y=cor)) + geom_point() + ylab("Correlation between log ASDR LC and Smoking prevalence") + xlab("Year")
#dev.off()
#
##compare LC asdr vs IHME asr for each sex. - limit to 1 age group as just repeated
#bin <- dat[dat$ag %in% c(1),] %>% group_by(year,sex) %>% summarise(cor=cor(mx_agestand, Lsmk))
#pdf(paste0('archive/code/make_data/data/smoking/LC-Lsmoking','_',sexnam,'_',startyear,'_',obs.Years,'yrs.pdf'), height=8,width=15)
#	ggplot(dat, aes(x=mx_agestand, y=Lsmk)) +  geom_point(size=0.2) + facet_wrap(~(year+startyear-1)) + geom_text(data=bin,aes(-3,0,label=round(cor,2)),colour="blue") + ylab("Log Smoking prevalence") + xlab("log ASDR LC")
#	ggplot(bin, aes(x=year+startyear-1, y=cor)) + geom_point() + ylab("Correlation between log ASDR LC and Log Smoking prevalence") + xlab("Year")
#dev.off()
#
##to check unempl rate in 20-24 age group - seems to be fine, must just be some unidentifiability of the coefficients?
##ggplot(dat, aes(x=unemp_rate,colour=sex)) + geom_histogram(fill="white", alpha=0.5, position="identity") + facet_wrap(~ag)
#
#  #correlation pairs for pollution
#
#  library("GGally") # think not used but leave in
#
#
#pdf(paste0('archive/code/make_data/data/smoking/CorrelationsScatters','_',sexnam,'_',startyear,'_',obs.Years,'yrs.pdf'), height=10,width=10)
#  bin <- data.frame(year=yr,PM25=pm0,O3=O3, NO2=NO2, PM_26_10=PM1025)
#  bin <- bin[age==1,] # because repeated for age groups
#  bin$PM25 <- bin$PM25 + apmean
#  bin$O3 <- bin$O3 + o3mean
#  bin$NO2 <- bin$NO2 + no2mean
#
#upper.panel<-function(x, y){
#  points(x,y, pch=".")		#, col=c("red", "green3", "blue")[iris$Species])
#  r <- round(cor(x, y), digits=2)
#  txt <- paste0("R = ", r)
#  usr <- par("usr"); on.exit(par(usr))
#  par(usr = c(0, 1, 0, 1))
#  text(0.85, 0.1, txt,col=4,cex=1.1, font=2) # txt oroginally 0.85
#}
#pairs(bin[                  ,2:5], lower.panel = NULL, upper.panel = upper.panel,main="Merged counties, 1999-2015")
#pairs(bin[bin$year==1,2:5], lower.panel = NULL, upper.panel = upper.panel,main="Merged counties, 1999")
#pairs(bin[bin$year==17,2:5], lower.panel = NULL, upper.panel = upper.panel,main="Merged counties, 2015")
#
#	#alternative but couldn't make points smaller
#	#ggpairs(bin[                  ,2:5])+ggtitle("Merged counties, 1999-2015")
#	#ggpairs(bin[bin$year==1,2:5])+ggtitle("Merged counties, 1999")
#	#ggpairs(bin[bin$year==17,2:5]) +ggtitle("Merged counties, 2015")
#	#good for just correlations
#	#ggcorr(bin[,2:5]  , low = "steelblue", mid = "white", high = "darkred", label = TRUE, label_size = 3, label_color = "black")	+ ggtitle("")	#, nbreaks = 6, palette = "RdGy"
#
#dev.off()
#
#
#pdf(paste0('archive/code/make_data/data/smoking/LC-smoking','_',sexnam,'_',startyear,'_',obs.Years,'yrs.pdf'), height=8,width=15)
#bin <- dat[dat$ag %in% c(1),] %>% group_by(year,sex) %>% summarise(cor=cor(mx_agestand, smk))
#	ggplot(dat, aes(x=mx_agestand, y=smk)) +  geom_point(size=0.2) + facet_wrap(~(year+startyear-1)) + geom_text(data=bin,aes(-3,10,label=round(cor,2)),colour="blue") + ylab("Smoking prevalence") + xlab("log ASDR LC")
#	ggplot(bin, aes(x=year+startyear-1, y=cor)) + geom_point() + ylab("Correlation between log ASDR LC and Smoking prevalence") + xlab("Year")
#dev.off()
#
#
#
#
#
#
#} # plots FALSE
#
#
#
#
#
#
#
#
#
#
#
#
#
####################################################
####################################################
########### NATIONAL VERSION OF DATA - WITH INTEGER BINS OF PM ###
#
#if(FALSE){ #national
#
#datN <- dat %>% group_by(sex,year,ag) %>%
#	summarise(popA=sum(pop), deathsumA = sum(deathsum),
#						popraceprop = sum(pop*popraceprop)/popA,
#						apmerge = sum(pop*apmerge)/popA,
#						mx_agestand = sum(pop*mx_agestand)/popA,
#						log_pc_const = sum(pop*log_pc_const)/popA,
#						poppov_merged = sum(pop*poppov_merged)/popA,
#						hsgrad = sum(pop*hsgrad)/popA,
#						urban = sum(pop*urban)/popA,
#						unemp_rate = sum(pop*unemp_rate)/popA,
#						smk = sum(pop*smk)/popA,
#						Lsmk = sum(pop*Lsmk)/popA,
#						temp_ymean = sum(pop*temp_ymean)/popA,
#						relh_ymean = sum(pop*relh_ymean)/popA)
#
#
#  ################## WRITING OUT DATA TO BUGS ##################
#  n <- datN$popA
#  y <- datN$deathsumA
#  yr <- datN$year
#  age <- datN$ag
#  N <- nrow(datN)
#  RACE <- datN$popraceprop
#  pm0 <- datN$apmerge
#  LC <- datN$mx_agestand
#  income_bugs <- datN$log_pc_const
#  poppov_bugs <- datN$poppov_merged
#  EDU <- datN$hsgrad
#  UR <- datN$urban
#  UNEMP <- datN$unemp_rate
#  SMK <- datN$smk
#  LSMK <- datN$Lsmk
#  TMP <- datN$temp_ymean
#  RELH <- datN$relh_ymean
#
#  pm0_bin <- apmean + datN$apmerge
#
#  dataoutN = list(n=n,y=y,N=N,yr=yr,age=age,Ages=Ages,adj.age=adj.age,weights.age=weights.age,num.age=num.age,obs.Years=obs.Years,PM0=pm0,INC=income_bugs,PERCPOV=poppov_bugs,RACE=RACE,LC=LC,EDU=EDU,UR=UR,UNEMP=UNEMP, SMK=SMK, LSMK=LSMK, TMP=TMP, RELH=RELH)
#
#  writeDatafileR(dataoutN,fileoutN)
#
#
##data_cardiorespiratory_pm25_female_1999_17yrs_N
#
#} #FALSE national
#











