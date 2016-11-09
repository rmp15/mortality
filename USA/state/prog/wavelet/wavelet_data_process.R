rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])
num.sim <- as.numeric(args[3])

# 1. NATIONAL

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/national/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# combine all separate max power files into one single file (both for entire and split period)
dat.entire <- data.frame()
dat.split <- data.frame()
file.loc.entire <- paste0(file.loc,'12_month_values/entire_period/')
file.loc.split <- paste0(file.loc,'12_month_values/split_period/')
for(i in c(0,5,15,25,35,45,55,65,75,85)){
    
    dat.temp.entire.m <- readRDS(paste0(file.loc.entire,i,'_Men'))
    dat.temp.entire.f <- readRDS(paste0(file.loc.entire,i,'_Women'))
    dat.entire <- rbind(dat.entire,dat.temp.entire.m,dat.temp.entire.f)
    
    dat.temp.split.m.1 <- readRDS(paste0(file.loc.split,i,'_Men_part1'))
    dat.temp.split.m.2 <- readRDS(paste0(file.loc.split,i,'_Men_part2'))
    dat.temp.split.m <- merge(dat.temp.split.m.1,dat.temp.split.m.2)
    dat.temp.split.f.1 <- readRDS(paste0(file.loc.split,i,'_Women_part1'))
    dat.temp.split.f.2 <- readRDS(paste0(file.loc.split,i,'_Women_part2'))
    dat.temp.split.f <- merge(dat.temp.split.f.1,dat.temp.split.f.2)
    dat.split <- rbind(dat.split,dat.temp.split.m,dat.temp.split.f)
}

# combine entire period with split period results
dat.complete <- merge(dat.entire,dat.split)

# export results
ifelse(!dir.exists(paste0(file.loc,'12_month_values/combined_results/')), dir.create(paste0(file.loc,'12_month_values/combined_results'),recursive=TRUE), FALSE)
saveRDS(dat.complete,paste0(file.loc,'12_month_values/combined_results/power_12_months_national_',year.start.arg,'_',year.end.arg))

# 2. REGION

# load USA map look-up for regions
dat.region <- readRDS(paste0('../../output/mapping_posterior/INLA/type1a/1982_2013/maps/USA_state_data'))
dat.region$climate_region <- gsub(' ','_',dat.region$climate_region)
region.lookup <- unique(dat.region$climate_region)

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/region/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# combine all separate max power files into one single file (both for entire and split period)
dat.entire <- data.frame()
file.loc.entire <- paste0(file.loc,'12_month_values/entire_period/')
file.loc.split <- paste0(file.loc,'12_month_values/split_period/')
for(i in c(0,5,15,25,35,45,55,65,75,85)){
    for(j in region.lookup){
        
        dat.temp.entire.m <- readRDS(paste0(file.loc.entire,i,'/',i,'_Men_',j))
        dat.temp.entire.f <- readRDS(paste0(file.loc.entire,i,'/',i,'_Women_',j))
        dat.entire <- rbind(dat.entire,dat.temp.entire.m,dat.temp.entire.f)
        
        #dat.temp.split.m.1 <- readRDS(paste0(file.loc.split,i,'/',i,'_Men_',j,'_part1'))
        #dat.temp.split.m.2 <- readRDS(paste0(file.loc.split,i,'/',i,'_Men_',j,'_part2'))
        #dat.temp.split.m <- merge(dat.temp.split.m.1,dat.temp.split.m.2)
        #dat.temp.split.f.1 <- readRDS(paste0(file.loc.split,i,'/',i,'_Women_',j,'_part1'))
        #dat.temp.split.f.2 <- readRDS(paste0(file.loc.split,i,'/',i,'_Women_',j,'_part2'))
        #dat.temp.split.f <- merge(dat.temp.split.f.1,dat.temp.split.f.2)
        #dat.split <- rbind(dat.split,dat.temp.split.m,dat.temp.split.f)
    }}

# combine entire period with split period results
dat.complete <- dat.entire
#dat.complete <- merge(dat.entire,dat.split)

# export results
ifelse(!dir.exists(paste0(file.loc,'12_month_values/combined_results/')), dir.create(paste0(file.loc,'12_month_values/combined_results'),recursive=TRUE), FALSE)
saveRDS(dat.complete,paste0(file.loc,'12_month_values/combined_results/power_12_months_region_',year.start.arg,'_',year.end.arg))

# 3. STATE

# create output directories
file.loc <- paste0("../../output/wavelet/",year.start.arg,'_',year.end.arg,"/state/")
file.loc <- paste0(file.loc,num.sim,'_sim/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# combine all separate max power files into one single file (both for entire and split period)
dat.entire <- data.frame()
dat.split <- data.frame()
file.loc.entire <- paste0(file.loc,'12_month_values/entire_period/')
file.loc.split <- paste0(file.loc,'12_month_values/split_period/')
for(i in c(0,5,15,25,35,45,55,65,75,85)){
    for(j in unique(dat$fips)){
    
    dat.temp.entire.m <- readRDS(paste0(file.loc.entire,i,'/',i,'_Men_',j))
    dat.temp.entire.f <- readRDS(paste0(file.loc.entire,i,'/',i,'_Women_',j))
    dat.entire <- rbind(dat.entire,dat.temp.entire.m,dat.temp.entire.f)
    
    dat.temp.split.m.1 <- readRDS(paste0(file.loc.split,i,'/',i,'_Men_',j,'_part1'))
    dat.temp.split.m.2 <- readRDS(paste0(file.loc.split,i,'/',i,'_Men_',j,'_part2'))
    dat.temp.split.m <- merge(dat.temp.split.m.1,dat.temp.split.m.2)
    dat.temp.split.f.1 <- readRDS(paste0(file.loc.split,i,'/',i,'_Women_',j,'_part1'))
    dat.temp.split.f.2 <- readRDS(paste0(file.loc.split,i,'/',i,'_Women_',j,'_part2'))
    dat.temp.split.f <- merge(dat.temp.split.f.1,dat.temp.split.f.2)
    dat.split <- rbind(dat.split,dat.temp.split.m,dat.temp.split.f)
    }}

# combine entire period with split period results
dat.complete <- merge(dat.entire,dat.split)

# export results
ifelse(!dir.exists(paste0(file.loc,'12_month_values/combined_results/')), dir.create(paste0(file.loc,'12_month_values/combined_results'),recursive=TRUE), FALSE)
saveRDS(dat.complete,paste0(file.loc,'12_month_values/combined_results/power_12_months_state_',year.start.arg,'_',year.end.arg))
