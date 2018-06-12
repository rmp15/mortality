rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
cod.arg <- as.character(args[3]);cod.arg <- gsub('_',' ',cod.arg)

library(plyr)

# source relevant objects
source('../../data/objects/objects.R')

# create output directories
file.loc.nat <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,'/national/values/')
file.loc.nat.entire <- paste0(file.loc.nat,"entire_period/")
file.loc.nat.split <- paste0(file.loc.nat,"split_period/")
file.loc.nat.output <- paste0(file.loc.nat,"combined_results/")
ifelse(!dir.exists(file.loc.nat.output), dir.create(file.loc.nat.output,recursive=TRUE), FALSE)

file.loc.reg <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,'/region/values/')
file.loc.reg.entire <- paste0(file.loc.reg,"entire_period/")
file.loc.reg.split <- paste0(file.loc.reg,"split_period/")
file.loc.reg.output <- paste0(file.loc.reg,"combined_results/")
ifelse(!dir.exists(file.loc.reg.output), dir.create(file.loc.reg.output,recursive=TRUE), FALSE)

# NATIONAL DEATH RATES
# COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/com_rate_cod_',sex.lookup[k],'_',i,'_',cod.arg))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

# NATIONAL DEATH RATES
# INV COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/anti_com_rate_cod_',sex.lookup[k],'_',i,'_',cod.arg))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

# produce dataset for national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))
dat.COM$sex <- as.factor(as.character(dat.COM$sex))
levels(dat.COM$sex) <- c('Men','Women')
dat.COM$type <- 'max'
dat.COM$size <- with(dat.COM,1/(COM.95-COM.5))
dat.COM$size <- 3*(dat.COM$size/max(dat.COM$size))

dat.inv.COM <- readRDS(paste0(file.loc.nat.input,'inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))
dat.inv.COM$sex <- as.factor(as.character(dat.inv.COM$sex))
levels(dat.inv.COM$sex) <- c('Men','Women')
dat.inv.COM$type <- 'min'
dat.inv.COM$size <- with(dat.inv.COM,1/(COM.95-COM.5))
dat.inv.COM$size <- 3*(dat.inv.COM$size/max(dat.inv.COM$size))

dat.nat <- rbind(dat.COM,dat.inv.COM)

# output
saveRDS(dat.nat,paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

#REGIONAL DEATH RATES
#COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in c(0,5,15,25,35,45,55,65,75,85)){
           dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/com_rate_',sex.lookup[k],'_',i,'_',j,'_',cod.arg))
           dat.entire <- rbind(dat.entire,dat.temp)
           print(dat.entire)
       }}}
saveRDS(dat.entire,paste0(file.loc.reg.output,'com_rates_regional_values_method_2_entire_',cod.arg,'_',
        year.start.arg,'_',year.end.arg))

# #INV COM
#
# #construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
   for(k in c(1,2)){
       for(i in c(0,5,15,25,35,45,55,65,75,85)){
           dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/anti_com_rate_',sex.lookup[k],'_',i,'_',j,'_',cod.arg))
           dat.temp <- cbind(dat.temp,j)
           dat.entire <- rbind(dat.entire,dat.temp)
           print(dat.entire)
       }}}
names(dat.entire)[6] <- 'region'
dat.entire <- dat.entire[,c('age','sex','region','COM.mean','COM.5','COM.95')]
saveRDS(dat.entire,paste0(file.loc.reg.output,'anti_com_rates_regional_values_method_2_entire_',cod.arg,'_',year.start.arg,'_',year.end.arg))

#dat.entire <- rbind()