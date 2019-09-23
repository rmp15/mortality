rm(list=ls())

# set up argument (don't get why but it should be like this apparently)
seedVal <-as.numeric(commandArgs()[4])

# create complete grid of age, sex, and cause of death values
sexes = c(1,2)
ages = c(0,5,15,25,35,45,55,65,75,85)
causes = c('Transport accidents','Accidental falls','Accidental drowning and submersion','Intentional self-harm','Assault')

seed.grid = expand.grid(sex=sexes,age=ages,cause=causes)

chosen.row =seed.grid[seedVal,]

# break down the arguments from Rscript
sex.arg <- as.numeric(chosen.row[1,1])
age.arg <- as.numeric(chosen.row[1,2])
year.start.arg <- 1980
year.end.arg <- 2017
type.arg <- 27
cluster.arg <- 0
dname.arg <- 't2m'
metric.arg <- 'meanc4'
year.start.analysis.arg <- 1980
year.end.analysis.arg <- 2017
cod.arg <- as.character(chosen.row[1,3])
fast.arg <- 1
contig.arg <- 1
pw.arg <- 0

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9')
type.selected <- types[type.arg]

print(paste(year.start.analysis.arg,year.end.analysis.arg,age.arg,sex.arg,type.selected,cod.arg))

# create directory for output
if(pw.arg==0){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_era5_toy/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.arg)
}
if(pw.arg==1){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_era5_toy/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups/',age.arg)
}
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# prep data for output
mod.test = data.frame

# lookups
source('../../data/objects/objects.R')

# output string for filenames
output.string = paste0('USA_rate_pred_type',type.selected,'_',age.arg,'_',sex.lookup[sex.arg],'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)

# save all parameters of INLA model
parameters.name <- paste0(output.string)
saveRDS(mod.test,paste0(file.loc,'/',parameters.name))
