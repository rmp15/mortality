rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
cod.arg <- as.character(args[3])


library(plyr)

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85), age.print=age.print)
sex.lookup <- c('male','female')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

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

# NATIONAL DEATH COUNTS
# COM

# construct dataset for entire period national analysis method 1
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_1/com_',sex.lookup[k],'_',i))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_1_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/com_',tolower(sex.lookup[k]),'_',i))
            dat.entire <- rbind(dat.entire,dat.temp)
            print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part1'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_1_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part2'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_2_',year.start.arg,'_',year.end.arg))

# NATIONAL DEATH COUNTS
# INV COM

# construct dataset for entire period national analysis method 1
#dat.entire <- data.frame()
#for(k in c(1,2)){
#	for(i in c(0,5,15,25,35,45,55,65,75,85)){
#	    dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_1/inv_com_',sex.lookup[k],'_',i))
#            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
#}}
#saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_national_values_method_1_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/anti_com_',sex.lookup[k],'_',i))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/anti_com_',sex.lookup[k],'_',i,'_part1'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_national_values_method_2_split_1_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35,45,55,65,75,85)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/anti_com_',sex.lookup[k],'_',i,'_part2'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_national_values_method_2_split_2_',year.start.arg,'_',year.end.arg))

# produce dataset for national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))
dat.COM$sex <- as.factor(as.character(dat.COM$sex))
levels(dat.COM$sex) <- c('Men','Women')
dat.COM$type <- 'max'
dat.COM$size <- with(dat.COM,1/(COM.95-COM.5))
dat.COM$size <- 3*(dat.COM$size/max(dat.COM$size))

dat.inv.COM <- readRDS(paste0(file.loc.nat.input,'inv_com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))
dat.inv.COM$sex <- as.factor(as.character(dat.inv.COM$sex))
levels(dat.inv.COM$sex) <- c('Men','Women')
dat.inv.COM$type <- 'min'
dat.inv.COM$size <- with(dat.inv.COM,1/(COM.95-COM.5))
dat.inv.COM$size <- 3*(dat.inv.COM$size/max(dat.inv.COM$size))

dat.nat <- rbind(dat.COM,dat.inv.COM)

# produce dataset for split national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
dat.COM.1 <- readRDS(paste0(file.loc.nat.input,'com_national_values_method_2_split_1_',year.start.arg,'_',year.end.arg))
dat.COM.1$period <- 1
dat.COM.1$sex <- as.factor(dat.COM.1$sex)
levels(dat.COM.1$sex) <- c('Men','Women')
dat.COM.1$type <- 'max'
dat.COM.1$size <- with(dat.COM.1,1/(COM.95-COM.5))
dat.COM.1$size <- 3*(dat.COM.1$size/max(dat.COM.1$size))
dat.COM.2 <- readRDS(paste0(file.loc.nat.input,'com_national_values_method_2_split_2_',year.start.arg,'_',year.end.arg))
dat.COM.2$period <- 2
dat.COM.2$sex <- as.factor(dat.COM.2$sex)
levels(dat.COM.2$sex) <- c('Men','Women')
dat.COM.2$type <- 'max'
dat.COM.2$size <- with(dat.COM.2,1/(COM.95-COM.5))
dat.COM.2$size <- 3*(dat.COM.2$size/max(dat.COM.2$size))

dat.inv.COM.1 <- readRDS(paste0(file.loc.nat.input,'inv_com_national_values_method_2_split_1_',year.start.arg,'_',year.end.arg))
dat.inv.COM.1$period <- 1
dat.inv.COM.1$sex <- as.factor(dat.inv.COM.1$sex)
levels(dat.inv.COM.1$sex) <- c('Men','Women')
dat.inv.COM.1$type <- 'min'
dat.inv.COM.1$size <- with(dat.inv.COM.1,1/(COM.95-COM.5))
dat.inv.COM.1$size <- 3*(dat.inv.COM.1$size/max(dat.inv.COM.1$size))
dat.inv.COM.2 <- readRDS(paste0(file.loc.nat.input,'inv_com_national_values_method_2_split_2_',year.start.arg,'_',year.end.arg))
dat.inv.COM.2$period <- 2
dat.inv.COM.2$sex <- as.factor(dat.COM.2$sex)
levels(dat.inv.COM.2$sex) <- c('Men','Women')
dat.inv.COM.2$type <- 'min'
dat.inv.COM.2$size <- with(dat.COM.2,1/(COM.95-COM.5))
dat.inv.COM.2$size <- 3*(dat.COM.2$size/max(dat.COM.2$size))

dat.nat.split <- rbind(dat.COM.1,dat.inv.COM.1,dat.COM.2,dat.inv.COM.2)

saveRDS(dat.nat,paste0(file.loc.nat.input,'com_inv_com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

saveRDS(dat.nat.split,paste0(file.loc.nat.input,'com_inv_com_national_values_method_2_split_',year.start.arg,'_',year.end.arg))

# REGIONAL DEATH COUNTS
# COM

# load region data
dat.region <- readRDS(paste0('../../output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
dat.region$climate_region <- gsub(' ','_',dat.region$climate_region)
region.lookup <- unique(dat.region$climate_region)

# construct dataset for entire period national analysis method 1
#dat.entire <- data.frame()
#for(k in c(1,2)){
#    for(i in c(0,5,15,25,35,45,55,65,75,85)){
#        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_1/com_',sex.lookup[k],'_',i))
#        dat.entire <- rbind(dat.entire,dat.temp)
#        #print(dat.entire)
#    }}
#saveRDS(dat.entire,paste0(file.loc.nat.output,'com_regional_values_method_1_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
    for(k in c(1,2)){
        for(i in c(0,5,15,25,35,45,55,65,75,85)){
            dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/com_',tolower(sex.lookup[k]),'_',i,'_',j))
            dat.entire <- rbind(dat.entire,dat.temp)
            print(dat.entire)
}}}
saveRDS(dat.entire,paste0(file.loc.reg.output,'com_regional_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
#dat.entire <- data.frame()
#for(k in c(1,2)){
#    for(i in c(0,5,15,25,35,45,55,65,75,85)){
#        dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part1'))
#        dat.entire <- rbind(dat.entire,dat.temp)
        #print(dat.entire)
#    }}
#saveRDS(dat.entire,paste0(file.loc.nat.output,'com_regional_values_method_2_split_1_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
#dat.entire <- data.frame()
#for(k in c(1,2)){
#    for(i in c(0,5,15,25,35,45,55,65,75,85)){
#        dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part2'))
#        dat.entire <- rbind(dat.entire,dat.temp)
        #print(dat.entire)
#    }}
#saveRDS(dat.entire,paste0(file.loc.nat.output,'com_regional_values_method_2_split_2_',year.start.arg,'_',year.end.arg))

# REGIONAL DEATH COUNTS
# INV COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
    for(k in c(1,2)){
        for(i in c(0,5,15,25,35,45,55,65,75,85)){
            dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/anti_com_',tolower(sex.lookup[k]),'_',i,'_',j))
            dat.temp <- cbind(dat.temp,j)
            dat.entire <- rbind(dat.entire,dat.temp)
            print(dat.entire)
        }}}
names(dat.entire)[6] <- 'region'
dat.entire <- dat.entire[,c('age','sex','region','COM.mean','COM.5','COM.95')]
saveRDS(dat.entire,paste0(file.loc.reg.output,'anti_com_regional_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# NATIONAL DEATH RATES
# COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/com_rate_',tolower(sex.lookup[k]),'_',i))
        dat.entire <- rbind(dat.entire,dat.temp)
        print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_rates_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# NATIONAL DEATH RATES
# INV COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
    for(i in c(0,5,15,25,35,45,55,65,75,85)){
        dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/anti_com_rate_',sex.lookup[k],'_',i))
        dat.entire <- rbind(dat.entire,dat.temp)
        #print(dat.entire)
    }}
saveRDS(dat.entire,paste0(file.loc.nat.output,'inv_com_rates_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# produce dataset for national data
file.loc.nat.input <- paste0("../../output/com/",year.start.arg,'_',year.end.arg,"/national/values/combined_results/")
dat.COM <- readRDS(paste0(file.loc.nat.input,'com_rates_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))
dat.COM$sex <- as.factor(as.character(dat.COM$sex))
levels(dat.COM$sex) <- c('Men','Women')
dat.COM$type <- 'max'
dat.COM$size <- with(dat.COM,1/(COM.95-COM.5))
dat.COM$size <- 3*(dat.COM$size/max(dat.COM$size))

dat.inv.COM <- readRDS(paste0(file.loc.nat.input,'inv_com_rates_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))
dat.inv.COM$sex <- as.factor(as.character(dat.inv.COM$sex))
levels(dat.inv.COM$sex) <- c('Men','Women')
dat.inv.COM$type <- 'min'
dat.inv.COM$size <- with(dat.inv.COM,1/(COM.95-COM.5))
dat.inv.COM$size <- 3*(dat.inv.COM$size/max(dat.inv.COM$size))

dat.nat <- rbind(dat.COM,dat.inv.COM)

# output
saveRDS(dat.nat,paste0(file.loc.nat.input,'com_inv_com_rates_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# REGIONAL DEATH RATES
# COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
    for(k in c(1,2)){
        for(i in c(0,5,15,25,35,45,55,65,75,85)){
            dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/com_rate_',tolower(sex.lookup[k]),'_',i,'_',j))
            dat.entire <- rbind(dat.entire,dat.temp)
            print(dat.entire)
        }}}
saveRDS(dat.entire,paste0(file.loc.reg.output,'com_rates_regional_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# REGIONAL DEATH RATES
# INV COM

# construct dataset for entire period national analysis method 2
dat.entire <- data.frame()
for(j in region.lookup) {
    for(k in c(1,2)){
        for(i in c(0,5,15,25,35,45,55,65,75,85)){
            dat.temp <- readRDS(paste0(file.loc.reg.entire,'method_2/anti_com_rate_',tolower(sex.lookup[k]),'_',i,'_',j))
            dat.temp <- cbind(dat.temp,j)
            dat.entire <- rbind(dat.entire,dat.temp)
            print(dat.entire)
        }}}
names(dat.entire)[6] <- 'region'
dat.entire <- dat.entire[,c('age','sex','region','COM.mean','COM.5','COM.95')]
saveRDS(dat.entire,paste0(file.loc.reg.output,'anti_com_rates_regional_values_method_2_entire_',year.start.arg,'_',year.end.arg))
