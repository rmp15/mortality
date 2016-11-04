rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

library(plyr)
require(CircStats)

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

# NATIONAL
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
	for(i in c(0,5,15,25,35)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part1'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_1',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part2'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_2',year.start.arg,'_',year.end.arg))

# NATIONAL
# INV COM

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
	    dat.temp <- readRDS(paste0(file.loc.nat.entire,'method_2/com_',sex.lookup[k],'_',i))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_entire_',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part1'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_1',year.start.arg,'_',year.end.arg))

# construct dataset for first period national analysis method 2
dat.entire <- data.frame()
for(k in c(1,2)){
	for(i in c(0,5,15,25,35)){
	    dat.temp <- readRDS(paste0(file.loc.nat.split,'method_2/com_',sex.lookup[k],'_',i,'_part2'))
            dat.entire <- rbind(dat.entire,dat.temp)
	    #print(dat.entire)
}}
saveRDS(dat.entire,paste0(file.loc.nat.output,'com_national_values_method_2_split_2',year.start.arg,'_',year.end.arg))
