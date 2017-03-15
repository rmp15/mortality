rm(list=ls())

library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
year.start.mort.arg <- as.numeric(args[1])
year.end.mort.arg <- as.numeric(args[2])
year.start.clim.arg <- as.numeric(args[3])
year.end.clim.arg <- as.numeric(args[4])
dname.arg <- as.character(args[5])
metric.arg <- as.character(args[6])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# add sourced data
source('../../data/objects/objects.R')

# create dummy dataframe
dat <- data.frame()

# loop through all dataframes compiling them
for(i in c(1,2)){
    for(j in age.code[,1]){

    # create files for output
    file.loc <- '../../output/mort_against_climate/'
    file.loc <- paste0(file.loc,dname.arg)
    file.loc <- paste0(file.loc,'/',metric.arg)
    file.loc <- paste0(file.loc,'/',year.start.arg,'_',year.end.arg)
    file.loc <- paste0(file.loc,'/',sex.lookup[i])
    file.loc <- paste0(file.loc,'/',j)

    # read file
    dat.temp <- readRDS(paste0(file.loc,'/mort_against_climate_',j,'_',i,'_',year.start.mort.arg,'_',year.end.mort.arg,'_',dname.arg,'_',metric.arg))

    # add to existing dataframe
    dat <- rbind(dat, dat.temp)

}
}

# file location for output
file.loc <- '../../output/mort_against_climate/'
file.loc <- paste0(file.loc,dname.arg)
file.loc <- paste0(file.loc,'/',metric.arg)
file.loc <- paste0(file.loc,'/',year.start.arg,'_',year.end.arg,'/')

# output combined file
saveRDS(dat,paste0(file.loc,'mort_against_climate_complete'))
