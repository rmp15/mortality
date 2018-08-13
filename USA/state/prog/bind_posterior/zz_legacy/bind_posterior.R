rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])

# select models

# establish parameters for file name
age.filter <- c(0,5,15,25,35,45,55,65,75,85)
sex.filter <- c('male','female')

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','4a')
model <- models[model]

# append the data

dat <- data.frame()

for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {
        file.name <- paste0('~/data/mortality/US/state/predicted/type_',model,'/age_groups/',age.filter[j],'/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end)
        print(file.name)
        current.file <- readRDS(file.name)
        dat <- rbind(dat,current.file)
    }
}

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/predicted/type_',model,'/combined/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local), FALSE)
file.loc.git <- paste0('../../data/predicted/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git), FALSE)

# save bound posterior
save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end)
saveRDS(dat,paste0(file.loc.local,save.name))
saveRDS(dat,paste0(file.loc.git,save.name))
