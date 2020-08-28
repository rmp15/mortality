rm(list=ls())

library(spdep)
library(rgdal)
library(foreign)
library(dplyr)
library(reshape2)
library(ggplot2)

#setwd("~/git/mortality/USA/state/data/covariates/")
#
#source('archive/code/make_data/get_mort_causespec.R')
#source('archive/code/make_data/combine_sc.R')
#source('archive/code/make_data/combine_mc.R')
#source('archive/code/make_data/shp2gg.R')
# source('P:/code/functions/ggsave_a4.R')


################ DEFINE ######################

polnam <- 'pm25' # select pollution type
id_sex <- 2 # select sex type (1=male, 2=female)
startyear <- 1999 # extracting data starting in this year
obs.Years <- 18 # ... for n years

#file_sclist_orig <- 'archive/code/make_data/outputs/scfips.dta' # file summarising admin boundary changes
#file_sclist <- 'archive/code/make_data/outputs/mfips_25000' # file summarising merging up to population threshold
#
#
## !!!! also make sure that pop and mort data going in at L70+ are consistent with above supercounty merging
#Ages <- 18 # total number of age-groups
#if (id_sex==1) {
#  sexnam <- 'male'
#  fileout <- paste0('archive/code/make_data/outputs/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs.txt')
#  fileoutT <- paste0('C:\\Users\\umahx99/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs.txt')
#  fileoutTWB <- paste0('C:\\Users\\umahx99/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrsWB.txt')
#  #fileoutN <- paste0('archive/code/make_data/outputs/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs_N.txt')
#}
#if (id_sex==2) {
#  sexnam <- 'female'
#  fileout <- paste0('archive/code/make_data/outputs/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs.txt')
#  fileoutT <- paste0('C:\\Users\\umahx99/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs.txt')
#  fileoutTWB <- paste0('C:\\Users\\umahx99/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrsWB.txt')
#  #fileoutN <- paste0('archive/code/make_data/outputs/data_cardiorespiratory_',polnam,'_',sexnam,'_',startyear,'_',obs.Years,'yrs_N.txt')
#}
#
#causes_icd10 <- c('CVD','respiratory')
#causeregex_icd10 <- c('I','J')
#causefind_icd10 <- data.frame(cause=causes_icd10,causeregex=causeregex_icd10)
#causes_icd9 <- c('respiratory','respiratory','CVD','CVD')
#causeregex_icd9 <- c('4[6789][0123456789][0123456789]','5[01][0123456789][0123456789]','39[0123456789][0123456789]','4[012345][0123456789][0123456789]')
#causefind_icd9 <- data.frame(cause=causes_icd9,causeregex=causeregex_icd9)
#
#causesLC_icd10 <- c('lung cancer')
#causeregexLC_icd10 <- c('C3[34]')
#causefindLC_icd10 <- data.frame(cause=causesLC_icd10,causeregex=causeregexLC_icd10)
#
#causesLC_icd9 <- c('lung cancer')#,'respiratory','respiratory','CVD','CVD','external causes')
#causeregexLC_icd9 <- c('162[0123456789]')#,'4[6789][0123456789][0123456789]','5[01][0123456789][0123456789]','39[0123456789][0123456789]','4[012345][0123456789][0123456789]','[89][0123456789][0123456789][0123456789]')
#causefindLC_icd9 <- data.frame(cause=causesLC_icd9,causeregex=causeregexLC_icd9)
#
refyr <- 2000 # reference year to which LC mort rate and income should be standardised
#
id_race <- 2 #1=white, 2=black, 3=native, 4=asian
##id_hisp <- 2 #1=non-hisp, 2=hisp
#
#file_inla_adj <- 'archive/code/make_data/outputs/us.graph'
#################### making adjacency matrix ####################
## subset map to continental US only
#shp <- readOGR(dsn="archive/code/make_data/data/us_county_data/shapefile",layer="county_shp_2015_proj")
#fipslist <- read.csv('archive/code/make_data/data/us_county_data/county_climreg_statefips.csv',header = TRUE,stringsAsFactors = FALSE)
#fipslist$statefips <- sprintf('%02d',fipslist$statefips)
#fipsmain <- fipslist$statefips
#fipsmain <- fipsmain[!fipsmain %in% c('02','15')]
#shpmain <- subset(shp,STATEFP %in% fipsmain)
#
## merge counties in spatial polygons data format (accounting for admin boundary changes)
#scloc.sc <- read.dta(file_sclist_orig)
#scloc.df.sc <- data.frame(lapply(scloc.sc, as.character), stringsAsFactors=FALSE)
#source("archive/code/make_data/shpmerge.R")
#shpmainmerged <- shpmerge(shpmain,'GEOID',scloc.df.sc)
#
## merge counties in spatial polygons data format (accounting for merging smaller population counties)
#scloc <- readRDS(file_sclist)
#scloc.df <- data.frame(lapply(scloc, as.character), stringsAsFactors=FALSE)
#shpmainmerged2 <- shpmerge(shpmainmerged,'GEOID',scloc.df)
#names(shpmainmerged2) <- 'id'
#map <- shp2gg(shpmainmerged2)
#names(shpmainmerged2) <- 'GEOID'
#
## convert shapefile to inla adjacency matrix
#shpnb <- poly2nb(shpmainmerged2)
#
## data frame summarising county number/FIPS conversion
#ctyid <- data.frame(fips = shpmainmerged2@data$GEOID)
#ctyid$id <- c(1:nrow(ctyid))
#
## accounting for various island counties in adjacency matrix
#num.LAD <- c()
#adj.LAD <- c()
#for (n in 1:length(shpnb)) {
#  num.LAD.temp <- length(shpnb[[n]])
#  adj.LAD.temp <- shpnb[[n]]
#  if (ctyid$fips[n] == '25M01') {
#    num.LAD.temp <- 1
#    adj.LAD.temp <- ctyid$id[ctyid$fips %in% c('25001')] # this line may need modifying depending on the merging threshold
#    shpnb[[n]] <- adj.LAD.temp
#  }
#  if (ctyid$fips[n] == '25001') {
#    num.LAD.temp <- 2
#    adj.LAD.temp <- ctyid$id[ctyid$fips %in% c('25M01','25023')] # this line may need modifying depending on the merging threshold
#    shpnb[[n]] <- adj.LAD.temp
#  }
#  if (ctyid$fips[n] == '53055') { # san juan - add skagit
#    num.LAD.temp <- 1
#    adj.LAD.temp <- ctyid$id[ctyid$fips %in% '53057']
#    shpnb[[n]] <- adj.LAD.temp
#  }
#  if (ctyid$fips[n] == '53057') { # skagit - add san juan
#    num.LAD.temp <- num.LAD.temp + 1
#    adj.LAD.temp <- c(adj.LAD.temp,ctyid$id[ctyid$fips %in% '53055'])
#    shpnb[[n]] <- adj.LAD.temp
#  }
#  num.LAD <- c(num.LAD,num.LAD.temp)
#  adj.LAD <- c(adj.LAD,adj.LAD.temp)
#}
#LADs <- length(num.LAD)
#weights.LAD <- rep(1,length(adj.LAD))
#
## make INLA adjacency matrix
#nb2INLA(file_inla_adj,nb = shpnb)
#
#
#
##################### INCLUDE POLLUTION DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_',polnam,'.RData'))
#apannual <- P3v1.1clu_county
#apannual$year <- as.numeric(apannual$year)
#colnames(apannual)[1] <- 'fips'
#apannual <- subset(apannual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
load('nchs_raw_annotated_withag_1990_to_2016') # CURRENTLY MISSING
#load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#ap_pop_nchs <- left_join(apannual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##ap_pop_nchs[is.na(ap_pop_nchs$popsum)==1,'popsum'] <- 0
#
#ap_pop_nchs[ap_pop_nchs$fips == '08014' & ap_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#ap_pop_nchs[ap_pop_nchs$fips == '46113' & ap_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#ap_pop_nchs[ap_pop_nchs$fips == '51515' & ap_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#ap_pop_nchs_sctag <- combine_sc(ap_pop_nchs,scloc.df.sc)
#ap_pop_nchs_sctag$appop <- ap_pop_nchs_sctag$pred.wght*ap_pop_nchs_sctag$popsum
#ap_pop_nchs_sc <- data.frame(summarise(group_by(ap_pop_nchs_sctag,fips,year),apsc.wght=sum(appop)/sum(popsum),popsum=sum(popsum)))
#ap_pop_nchs_mctag <- combine_mc(ap_pop_nchs_sc,scloc.df)
#ap_pop_nchs_mctag$appop <- ap_pop_nchs_mctag$apsc.wght*ap_pop_nchs_mctag$popsum
#ap_pop_nchs_mc <- data.frame(summarise(group_by(ap_pop_nchs_mctag,fips,year),apmc.wght=sum(appop)/sum(popsum)))
#
## centering values
#apmc_bugs <- ap_pop_nchs_mc
#colnames(apmc_bugs)[3] <- 'apmerge'
#apmean <- mean(apmc_bugs$apmerge)
#apmc_bugs$apmerge <- apmc_bugs$apmerge - apmean
#apmc_bugs$year <- apmc_bugs$year - startyear + 1
#
##################### INCLUDE OZONE DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_o3.RData'))
#o3annual <- P3v1.1clu_county
#o3annual$year <- as.numeric(o3annual$year)
#colnames(o3annual)[1] <- 'fips'
#o3annual <- subset(o3annual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
#load('archive/code/make_data/data/nchs_raw_annotated_withag_1990_to_2016')
##load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#o3_pop_nchs <- left_join(o3annual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##o3_pop_nchs[is.na(o3_pop_nchs$popsum)==1,'popsum'] <- 0
#
#o3_pop_nchs[o3_pop_nchs$fips == '08014' & o3_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#o3_pop_nchs[o3_pop_nchs$fips == '46113' & o3_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#o3_pop_nchs[o3_pop_nchs$fips == '51515' & o3_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#o3_pop_nchs_sctag <- combine_sc(o3_pop_nchs,scloc.df.sc)
#o3_pop_nchs_sctag$o3pop <- o3_pop_nchs_sctag$pred.wght*o3_pop_nchs_sctag$popsum
#o3_pop_nchs_sc <- data.frame(summarise(group_by(o3_pop_nchs_sctag,fips,year),o3sc.wght=sum(o3pop)/sum(popsum),popsum=sum(popsum)))
#o3_pop_nchs_mctag <- combine_mc(o3_pop_nchs_sc,scloc.df)
#o3_pop_nchs_mctag$o3pop <- o3_pop_nchs_mctag$o3sc.wght*o3_pop_nchs_mctag$popsum
#o3_pop_nchs_mc <- data.frame(summarise(group_by(o3_pop_nchs_mctag,fips,year),o3mc.wght=sum(o3pop)/sum(popsum)))
#
## centering values
#o3mc_bugs <- o3_pop_nchs_mc
#colnames(o3mc_bugs)[3] <- 'o3merge'
#o3mean <- mean(o3mc_bugs$o3merge)
#o3mc_bugs$o3merge <- o3mc_bugs$o3merge - o3mean
#o3mc_bugs$year <- o3mc_bugs$year - startyear + 1
#
##################### INCLUDE NO2 DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_no2.RData'))
#no2annual <- P3v1.1clu_county
#no2annual$year <- as.numeric(no2annual$year)
#colnames(no2annual)[1] <- 'fips'
#no2annual <- subset(no2annual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
#load('archive/code/make_data/data/nchs_raw_annotated_withag_1990_to_2016')
##load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#no2_pop_nchs <- left_join(no2annual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##no2_pop_nchs[is.na(no2_pop_nchs$popsum)==1,'popsum'] <- 0
#
#no2_pop_nchs[no2_pop_nchs$fips == '08014' & no2_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#no2_pop_nchs[no2_pop_nchs$fips == '46113' & no2_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#no2_pop_nchs[no2_pop_nchs$fips == '51515' & no2_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#no2_pop_nchs_sctag <- combine_sc(no2_pop_nchs,scloc.df.sc)
#no2_pop_nchs_sctag$no2pop <- no2_pop_nchs_sctag$pred.wght*no2_pop_nchs_sctag$popsum
#no2_pop_nchs_sc <- data.frame(summarise(group_by(no2_pop_nchs_sctag,fips,year),no2sc.wght=sum(no2pop)/sum(popsum),popsum=sum(popsum)))
#no2_pop_nchs_mctag <- combine_mc(no2_pop_nchs_sc,scloc.df)
#no2_pop_nchs_mctag$no2pop <- no2_pop_nchs_mctag$no2sc.wght*no2_pop_nchs_mctag$popsum
#no2_pop_nchs_mc <- data.frame(summarise(group_by(no2_pop_nchs_mctag,fips,year),no2mc.wght=sum(no2pop)/sum(popsum)))
#
## centering values
#no2mc_bugs <- no2_pop_nchs_mc
#colnames(no2mc_bugs)[3] <- 'no2merge'
#no2mean <- mean(no2mc_bugs$no2merge)
#no2mc_bugs$no2merge <- no2mc_bugs$no2merge - no2mean
#no2mc_bugs$year <- no2mc_bugs$year - startyear + 1
#
##################### INCLUDE so2 DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_so2.RData'))
#so2annual <- P3v1.1clu_county
#so2annual$year <- as.numeric(so2annual$year)
#colnames(so2annual)[1] <- 'fips'
#so2annual <- subset(so2annual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
#load('archive/code/make_data/data/nchs_raw_annotated_withag_1990_to_2016')
##load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#so2_pop_nchs <- left_join(so2annual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##so2_pop_nchs[is.na(so2_pop_nchs$popsum)==1,'popsum'] <- 0
#
#so2_pop_nchs[so2_pop_nchs$fips == '08014' & so2_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#so2_pop_nchs[so2_pop_nchs$fips == '46113' & so2_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#so2_pop_nchs[so2_pop_nchs$fips == '51515' & so2_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#so2_pop_nchs_sctag <- combine_sc(so2_pop_nchs,scloc.df.sc)
#so2_pop_nchs_sctag$so2pop <- so2_pop_nchs_sctag$pred.wght*so2_pop_nchs_sctag$popsum
#so2_pop_nchs_sc <- data.frame(summarise(group_by(so2_pop_nchs_sctag,fips,year),so2sc.wght=sum(so2pop)/sum(popsum),popsum=sum(popsum)))
#so2_pop_nchs_mctag <- combine_mc(so2_pop_nchs_sc,scloc.df)
#so2_pop_nchs_mctag$so2pop <- so2_pop_nchs_mctag$so2sc.wght*so2_pop_nchs_mctag$popsum
#so2_pop_nchs_mc <- data.frame(summarise(group_by(so2_pop_nchs_mctag,fips,year),so2mc.wght=sum(so2pop)/sum(popsum)))
#
## centering values
#so2mc_bugs <- so2_pop_nchs_mc
#colnames(so2mc_bugs)[3] <- 'so2merge'
#so2mean <- mean(so2mc_bugs$so2merge)
#so2mc_bugs$so2merge <- so2mc_bugs$so2merge - so2mean
#so2mc_bugs$year <- so2mc_bugs$year - startyear + 1
#
##################### INCLUDE pm10 DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_pm10.RData'))
#pm10annual <- P3v1.1clu_county
#pm10annual$year <- as.numeric(pm10annual$year)
#colnames(pm10annual)[1] <- 'fips'
#pm10annual <- subset(pm10annual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
#load('archive/code/make_data/data/nchs_raw_annotated_withag_1990_to_2016')
##load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#pm10_pop_nchs <- left_join(pm10annual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##pm10_pop_nchs[is.na(pm10_pop_nchs$popsum)==1,'popsum'] <- 0
#
#pm10_pop_nchs[pm10_pop_nchs$fips == '08014' & pm10_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#pm10_pop_nchs[pm10_pop_nchs$fips == '46113' & pm10_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#pm10_pop_nchs[pm10_pop_nchs$fips == '51515' & pm10_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#pm10_pop_nchs_sctag <- combine_sc(pm10_pop_nchs,scloc.df.sc)
#pm10_pop_nchs_sctag$pm10pop <- pm10_pop_nchs_sctag$pred.wght*pm10_pop_nchs_sctag$popsum
#pm10_pop_nchs_sc <- data.frame(summarise(group_by(pm10_pop_nchs_sctag,fips,year),pm10sc.wght=sum(pm10pop)/sum(popsum),popsum=sum(popsum)))
#pm10_pop_nchs_mctag <- combine_mc(pm10_pop_nchs_sc,scloc.df)
#pm10_pop_nchs_mctag$pm10pop <- pm10_pop_nchs_mctag$pm10sc.wght*pm10_pop_nchs_mctag$popsum
#pm10_pop_nchs_mc <- data.frame(summarise(group_by(pm10_pop_nchs_mctag,fips,year),pm10mc.wght=sum(pm10pop)/sum(popsum)))
#
## centering values
#pm10mc_bugs <- pm10_pop_nchs_mc
#colnames(pm10mc_bugs)[3] <- 'pm10merge'
#pm10mean <- mean(pm10mc_bugs$pm10merge)
#pm10mc_bugs$pm10merge <- pm10mc_bugs$pm10merge - pm10mean
#pm10mc_bugs$year <- pm10mc_bugs$year - startyear + 1
#
##################### INCLUDE co DATA ####################
#load(paste0('archive/code/make_data/data/pollution/CACES_P3v1.1clu_county_co.RData'))
#coannual <- P3v1.1clu_county
#coannual$year <- as.numeric(coannual$year)
#colnames(coannual)[1] <- 'fips'
#coannual <- subset(coannual,year >= startyear & year <= (startyear+obs.Years-1))
#
## attach pop data
#load('archive/code/make_data/data/nchs_raw_annotated_withag_1990_to_2016')
##load('archive/code/make_data/outputs/nchs_pop_with_sc_withag_1990_to_2016')
#pop_nchs_allage <- as.data.frame(summarise(group_by(subset(dat_nchs,sex==id_sex),year,fips,sex),popsum=sum(popsum)))
#popsum_nchs <- subset(pop_nchs_allage,sex==id_sex)
#co_pop_nchs <- left_join(coannual,popsum_nchs)
#
## accounting for some missing data due to merging/splitting counties
##co_pop_nchs[is.na(co_pop_nchs$popsum)==1,'popsum'] <- 0
#
#co_pop_nchs[co_pop_nchs$fips == '08014' & co_pop_nchs$year <= 1999,'popsum'] <- popsum_nchs[popsum_nchs$fips == '08014' & popsum_nchs$year == 2000,'popsum']
#co_pop_nchs[co_pop_nchs$fips == '46113' & co_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '46102' & popsum_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum']
#co_pop_nchs[co_pop_nchs$fips == '51515' & co_pop_nchs$year %in% c(2010,2011,2012,2013,2014,2015),'popsum'] <- popsum_nchs[popsum_nchs$fips == '51515' & popsum_nchs$year %in% c(2009),'popsum']
#
## merging counties with population-weighted pollution
#co_pop_nchs_sctag <- combine_sc(co_pop_nchs,scloc.df.sc)
#co_pop_nchs_sctag$copop <- co_pop_nchs_sctag$pred.wght*co_pop_nchs_sctag$popsum
#co_pop_nchs_sc <- data.frame(summarise(group_by(co_pop_nchs_sctag,fips,year),cosc.wght=sum(copop)/sum(popsum),popsum=sum(popsum)))
#co_pop_nchs_mctag <- combine_mc(co_pop_nchs_sc,scloc.df)
#co_pop_nchs_mctag$copop <- co_pop_nchs_mctag$cosc.wght*co_pop_nchs_mctag$popsum
#co_pop_nchs_mc <- data.frame(summarise(group_by(co_pop_nchs_mctag,fips,year),comc.wght=sum(copop)/sum(popsum)))
#
## centering values
#comc_bugs <- co_pop_nchs_mc
#colnames(comc_bugs)[3] <- 'comerge'
#comean <- mean(comc_bugs$comerge)
#comc_bugs$comerge <- comc_bugs$comerge - comean
#comc_bugs$year <- comc_bugs$year - startyear + 1
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
##################### INCLUDE YEARLY MEAN DAILY TEMPERATURE DATA ####################
##load temperature data - made up by Robbie from mean of 365 24hr mean temperature - population weighting 1st for sc and then again for mc
#  temp <- readRDS('archive\\code\\make_data\\data\\meterology_sc\\meterology_sc\\t2m\\ymean_t2m/supercounty_summary_ymean_t2m_1999_2015.rds')
#  temp <- subset(temp, sex==id_sex)
#   colnames(temp)[which(colnames(temp)=="apsc.wght")] <- 'temp_ymean'
#   temp <- temp[,which(names(temp) %in% c("fips","year","temp_ymean"))]
##centre
#  temp_ymean_mean <- mean(temp$temp_ymean)
#  temp$temp_ymean <- temp$temp_ymean - temp_ymean_mean
#  temp$year <- temp$year - startyear + 1
#
#
##################### INCLUDE YEARLY MEAN DAILY TEMPERATURE DATA ####################
##load temperature data - made up by Robbie from mean of 365 24hr mean temperature - population weighting 1st for sc and then again for mc
#  relh <- readRDS('archive\\code\\make_data\\data\\meterology_sc\\meterology_sc\\rh\\ymean_rh/supercounty_summary_ymean_rh_1999_2015.rds')
#  relh <- subset(relh, sex==id_sex)
#   colnames(relh)[which(colnames(relh)=="apsc.wght")] <- 'relh_ymean'
#   relh <- relh[,which(names(relh) %in% c("fips","year","relh_ymean"))]
#
##centre
#  relh_ymean_mean <- mean(relh$relh_ymean)
#  relh$relh_ymean <- relh$relh_ymean - relh_ymean_mean
#  relh$year <- relh$year - startyear + 1
#
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

# merge counties, work out total income
#popsum_income <- data.frame(summarise(group_by(pop_nchs_allage,fips,year),poptot=sum(popsum)))
#popsum_income_sc <- combine_sc(popsum_income,scloc.df.sc)
#income_us_sc <- left_join(income_us_sc,popsum_income_sc,by=c('fips','year'))
#income_us_sc$total <- income_us_sc$inc_pc_sc*income_us_sc$poptot
#income_us_mctag <- combine_mc(income_us_sc,scloc.df)
#income_us_merged <- data.frame(summarise(group_by(income_us_mctag,fips,year),pc_merged=sum(total,na.rm=TRUE)/sum(poptot,na.rm=TRUE)))

# adjust for inflation 
CPIdf <- read.csv2('CPI.csv',header = TRUE,sep=',',stringsAsFactors = FALSE)
CPI_base <- subset(CPIdf,year==refyr)$CPI
CPIdf$ratio <- as.numeric(unlist(CPI_base))/as.numeric(unlist(CPIdf$CPI))
income_us_sc <- left_join(income_us_sc,CPIdf,by='year')
income_us_sc$pc_const <- income_us_sc$inc_pc_sc*income_us_sc$ratio

## convert to personal income per some unit to help with convergence
#income_us_merged$log_pc_const <- log(income_us_merged$pc_const)
#income_mean <- mean(income_us_merged$log_pc_const)
#income_us_merged$log_pc_const <- income_us_merged$log_pc_const - income_mean

################## INCLUDE POVERTY DATA ##################
load('povperc_with_sc')
poppov_us_sc <- subset(poppov_us_sc, year >= startyear & year < (startyear+obs.Years))

# remove all US and all county figures as well as overseas, AK, HI
poppov_us_sc$STATEFP <- substr(poppov_us_sc$fips,1,2)
poppov_us_sc$COUNTYFP <- substr(poppov_us_sc$fips,3,5)
poppov_us_sc <- subset(poppov_us_sc,STATEFP != '00')
poppov_us_sc <- subset(poppov_us_sc,COUNTYFP != '000')
poppov_us_sc <- subset(poppov_us_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))

# merge counties 
#poppov_us_sc <- left_join(poppov_us_sc,popsum_income_sc,by=c('fips','year'))
#poppov_us_sc$total <- poppov_us_sc$poppov_sc*poppov_us_sc$poptot
#poppov_us_mctag <- combine_mc(poppov_us_sc,scloc.df)
#poppov_us_merged <- data.frame(summarise(group_by(poppov_us_mctag,fips,year),poppov_merged=sum(total,na.rm=TRUE)/sum(poptot,na.rm=TRUE)))
#povmean <- mean(poppov_us_merged$poppov_merged)
#poppov_us_merged$poppov_merged <- poppov_us_merged$poppov_merged - povmean

################## INCLUDE RACE DATA ##################
# trim race data 
pop_race <- subset(dat_nchs,race==id_race & sex==id_sex & year >= startyear & year < (startyear+obs.Years))
# sum for this race for each fips-year
pop_race_allage <- data.frame(summarise(group_by(pop_race,year,fips),popsum=sum(popsum)))
# merge for ctys
#pop_race_allage_mc <- combine_mc(combine_sc(pop_race_allage,scloc.df.sc),scloc.df)
# sum of race for each cty 
#pop_race_allage_mc_sum <- data.frame(summarise(group_by(pop_race_allage_mc,year,fips),poprace=sum(popsum)))

# get total population at mc level 
#pop_nchs_allage_mc <- combine_mc(combine_sc(pop_nchs_allage,scloc.df.sc),scloc.df)
#pop_nchs_allage_mc_sum <- data.frame(summarise(group_by(pop_nchs_allage_mc,year,fips),poptot=sum(popsum)))

# get race perc 
poprace_us_merged <- left_join(pop_race_allage,pop_nchs_allage,by=c('year','fips'))
poprace_us_merged$popraceprop <- poprace_us_merged$popsum.x/poprace_us_merged$popsum.y

# remove AK + HI + overseas 
poprace_us_merged$STATEFP <- substr(poprace_us_merged$fips,1,2)
poprace_us_merged <- subset(poprace_us_merged,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))
poprace_us_merged <- poprace_us_merged[,c(1,2,6)]

#racemean <- mean(poprace_us_merged$popraceprop)
#poprace_us_merged$popraceprop <- poprace_us_merged$popraceprop - racemean

# 
#   
#   ################## INCLUDE HISP DATA ##################
#   # trim race data 
#   pop_hisp <- subset(dat_nchs,hisp==id_hisp & sex==id_sex & year >= startyear & year < (startyear+obs.Years))
#   # sum for this hisp for each fips-year
#   pop_hisp_allage <- data.frame(summarise(group_by(pop_hisp,year,fips),popsum=sum(popsum)))
#   # merge for ctys
#   pop_hisp_allage_mc <- combine_mc(combine_sc(pop_hisp_allage,scloc.df.sc),scloc.df)
#   # sum of hisp for each cty 
#   pop_hisp_allage_mc_sum <- data.frame(summarise(group_by(pop_hisp_allage_mc,year,fips),pophisp=sum(popsum)))
#   
#   # get total population at mc level 
#   pop_nchs_allage_mc <- combine_mc(combine_sc(pop_nchs_allage,scloc.df.sc),scloc.df)
#   pop_nchs_allage_mc_sum <- data.frame(summarise(group_by(pop_nchs_allage_mc,year,fips),poptot=sum(popsum)))
#   
#   # get hisp perc 
#   pophisp_us_merged <- left_join(pop_hisp_allage_mc_sum,pop_nchs_allage_mc_sum,by=c('year','fips'))
#   pophisp_us_merged$pophispprop <- pophisp_us_merged$pophisp/pophisp_us_merged$poptot
#   
#   # remove AK + HI + overseas 
#   pophisp_us_merged$STATEFP <- substr(pophisp_us_merged$fips,1,2)
#   pophisp_us_merged <- subset(pophisp_us_merged,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79'))
#   pophisp_us_merged <- pophisp_us_merged[,c(1,2,5)]
#   
#   hispmean <- mean(pophisp_us_merged$pophispprop)
#   pophisp_us_merged$pophispprop <- pophisp_us_merged$pophispprop - hispmean
#   
#   length(unique(pophisp_us_merged$fips))
  
  
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

#edu_us_mctag <- combine_mc(edu_sc,scloc.df)
#pop_us_allage <- data.frame(summarise(group_by(dat_nchs,year,fips),popsum=sum(popsum,na.rm = TRUE)))
#pop_us_mctag <- combine_mc(combine_sc(pop_us_allage,scloc.df.sc),scloc.df)
#pop_us_mc <- data.frame(summarise(group_by(pop_us_mctag,year,fips),popsum=sum(popsum,na.rm = TRUE)))
#edu_us_mctag <- left_join(edu_us_mctag,pop_us_mc,by=c('year','fips'))
#edu_us_mctag$pophsgrad <- edu_us_mctag$popsum*edu_us_mctag$hsgrad
#edu_us_merged <- as.data.frame(summarise(group_by(edu_us_mctag,fips,year),hsgrad=sum(pophsgrad,na.rm = TRUE)/sum(popsum,na.rm = TRUE)))
#
#edu_sc$type <- 'before merging'
#edu_us_merged_plot <- edu_us_merged
#edu_us_merged_plot$type <- 'after merging'
#edu_plot <- rbind(edu_sc[,c('hsgrad','type')],edu_us_merged_plot[,c('hsgrad','type')])
#p <- ggplot(edu_plot,aes(hsgrad))+geom_histogram()
#p + facet_wrap(~type)
#
#edumean <- mean(edu_us_merged$hsgrad)
#edu_us_merged$hsgrad <- edu_us_merged$hsgrad - edumean
  

################# INCLUDE UR DATA ######################
load('urbanpop_with_sc')

# merge by mc
daturban_mctag <- daturban
daturban_mc <- data.frame(summarise(group_by(daturban_mctag,fips,year),totpop=sum(totpop),toturban=sum(toturban)))
daturban_mc$urban <- with(daturban_mc,toturban/totpop)

# replace population by NCHS pop - use pop_nchs_mctag
#daturban_mc_nchs <- left_join(daturban_mc,pop_us_mc) # NEED TO FIX
#daturban_mc_nchs$urban <- daturban_mc_nchs$toturban/daturban_mc_nchs$popsum

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

#urbanmean <- mean(daturbaninterp$urban)
#daturbaninterp$urban <- daturbaninterp$urban - urbanmean
  
################# INCLUDE UNEMPLOYMENT DATA ######################
load('unemployment_with_sc')
dat_unemployment_sc <- subset(dat_unemployment_sc,year >= startyear & year < (startyear + obs.Years))

dat_unemployment_sc$unemp_rate <- dat_unemployment_sc$unemployed_tot/dat_unemployment_sc$labour_force_tot

# account for mc 
#dat_unemployment_mctag <- combine_mc(dat_unemployment_sc,scloc.df)
#dat_unemployment_mc <- data.frame(summarise(group_by(dat_unemployment_mctag,fips,year),labour_force_tot=sum(labour_force_tot),unemployed_tot=sum(unemployed_tot)))
#dat_unemployment_mc$unemp_rate <- dat_unemployment_mc$unemployed_tot/dat_unemployment_mc$labour_force_tot
#
#unempmean <- mean(dat_unemployment_mc$unemp_rate)
#dat_unemployment_mc$unemp_rate <- dat_unemployment_mc$unemp_rate - unempmean

# TEMPORARY SUMMARY


# ################# INCLUDE METRO DATA ######################
# 
# file_metro <- 'archive/code/make_data/data/urban/urban_classification.csv'
# datmetro <- read.csv(file_metro,header=FALSE)
# datmetro <- datmetro[,c('V1','V7','V8','V9')]
# names(datmetro) <- c('fips','Y1990','Y2006','Y2013')
# datmetro$fips[nchar(datmetro$fips)==4] <- paste0('0',datmetro$fips[nchar(datmetro$fips)==4])
# datmetro <- melt(datmetro,id.vars='fips')
# datmetro$metro <- '0'
# datmetro$metro[datmetro$value >= 5] <- '1'
# 
# datmetro$metro[datmetro$fips %in% c('04012','04027','35006','35061')] <- 0
# 
# #subset(datmetro,fips %in% )
# 
# 
# ## merging for sc ##
# #   datmetro_sctag <- combine_sc(datmetro,scloc.df.sc)
# #   datmetro_sc <- data.frame(summarise(group_by(datmetro_sctag,fips),metro2006=floor(mean(as.numeric(metro)[variable=='Y2006'])),metro2013=floor(mean(as.numeric(metro)[variable=='Y2013']))))
# #   
# #   # merging for mc
# #   datmetro_mctag <- combine_mc(datmetro_sc,scloc.df)
# #   datmetro_mc <- data.frame(summarise(group_by(datmetro_mctag,fips),metro=floor(mean(as.numeric(metro2006))),metrodiff=sum(diff(metro2006))))
# #   
# 
# ## merging for sc ##
# datmetro_sctag <- combine_sc(datmetro,scloc.df.sc)
# datmetro_sc <- data.frame(summarise(group_by(datmetro_sctag,fips),metro2006=mean(as.numeric(metro)[variable=='Y2006'])))
# 
# # merging for mc
# datmetro_mctag <- combine_mc(datmetro_sc,scloc.df)
# datmetro_mc <- data.frame(summarise(group_by(datmetro_mctag,fips),metro=mean(as.numeric(metro2006)),metro_forceurban=floor(mean(as.numeric(metro2006))),metro_forcerural=ceiling(mean(as.numeric(metro2006))),metrodiff=sum(diff(metro2006))))
# datmetro_mc$metro[!datmetro_mc$metro %in% c(1,0)] <- 2
  

################## INCLUDE SMK DATA ##################

#load('archive/code/make_data/outputs/smk_with_sc')
## cut dataset to particular years
#smk_sc <- subset(smk_sc, year >= startyear & year < (startyear+obs.Years))
## remove all US and all county figures as well as overseas, AK, HI
#smk_sc$STATEFP <- substr(smk_sc$fips,1,2)
#smk_sc$COUNTYFP <- substr(smk_sc$fips,3,5)
#smk_sc <- subset(smk_sc,STATEFP != '00')
#smk_sc <- subset(smk_sc,COUNTYFP != '000')
#smk_sc <- subset(smk_sc,!STATEFP %in% c('02','15','03','07','14','43','52','64','68','70','74','81','84','86','67','89','71','76','95','79','72'))
#
#smk_us_mctag <- combine_mc(smk_sc,scloc.df)
#pop_us_allage <- data.frame(summarise(group_by(dat_nchs,year,sex,fips),popsum=sum(popsum,na.rm = TRUE)))
#pop_us_mctag <- combine_mc(combine_sc(pop_us_allage,scloc.df.sc),scloc.df)
#pop_us_mc <- data.frame(summarise(group_by(pop_us_mctag,year,sex,fips),popsum=sum(popsum,na.rm = TRUE)))
#smk_us_mctag <- left_join(smk_us_mctag,pop_us_mc,by=c('year',"sex",'fips'))
#smk_us_mctag$popsmk <- smk_us_mctag$popsum*smk_us_mctag$smk
#smk_us_merged <- as.data.frame(summarise(group_by(smk_us_mctag,fips,sex,year),smk=sum(popsmk,na.rm = TRUE)/sum(popsum,na.rm = TRUE)))
#
#smk_sc$type <- 'before merging'
#smk_us_merged_plot <- smk_us_merged
#smk_us_merged_plot$type <- 'after merging'
#smk_plot <- rbind(smk_sc[,c('smk','type')],smk_us_merged_plot[,c('smk','type')])
#p <- ggplot(smk_plot,aes(smk))+geom_histogram()
#p + facet_wrap(~type)
#
##log transformed smoking prev
#Lsmkmean <- mean(log(smk_us_merged$smk))
#smk_us_merged$Lsmk <- log(smk_us_merged$smk) - Lsmkmean
##smk prev
#smkmean <- mean(smk_us_merged$smk)
#smk_us_merged$smk <- smk_us_merged$smk - smkmean
#


################## COLLECT ALL PROCESSED COVARIATES ##################

income_us_sc = income_us_sc[,c(1,2,8)]
poppov_us_sc = poppov_us_sc[,c(1,2,3)]
poprace_us_merged = poprace_us_merged
edu_sc = edu_sc[,c(1,2,3)]
daturbaninterp = daturbaninterp
dat_unemployment_sc = dat_unemployment_sc[,c(1,2,5)]

# make expanded grid of years, fips to merge completely to

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











