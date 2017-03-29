rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])

# establish parameters for file name
age.filter <- c(0,5,15,25,35,45,55,65,75,85)
sex.filter <- c('male','female')

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f')
model <- models[model]

# append the data

if(model=='1d'){

    # create dataframe with each of the national terms for entire group of age and sexes
    dat <- data.frame()

#   for (i in seq(length(sex.filter))) {
    for (i in c(2)) {
        for (j in seq(length(age.filter))) {
            file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],'/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters')
            print(file.name)
            model.current <- readRDS(file.name)
            current.file <- model.current$summary.random$month5
            current.file$age <- age.filter[j] ; current.file$sex <- i
            dat <- rbind(dat,current.file)
        }
    }

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local, recursive=TRUE), FALSE)
file.loc.git <- paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior
save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric)
saveRDS(dat,paste0(file.loc.local,save.name))
saveRDS(dat,paste0(file.loc.git,save.name))

}

# lookup for state codes from INLA
drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

if(model=='1e'){
    
    # create dataframe with each of the national terms for entire group of age and sexes
    dat <- data.frame()
    
    #   for (i in seq(length(sex.filter))) {
    for (i in c(2)) {
    for (j in seq(length(age.filter))) {
        file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],'/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters')
        print(file.name)
        model.current <- readRDS(file.name)
        current.file <- model.current$summary.random$month5
        current.file$age <- age.filter[j] ; current.file$sex <- i
        current.file$state <- rep(1:51,each=12)
        
        # attach fips and then state name information
        current.file <- merge(current.file,drawseq.lookup,by.x='state',by.y='DRAWSEQ')
        current.file <- merge(current.file, state.lookup[,c('full_name','fips')],by='fips')
        
        dat <- rbind(dat,current.file)

    }
}

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local, recursive=TRUE), FALSE)
file.loc.git <- paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior
save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric)
saveRDS(dat,paste0(file.loc.local,save.name))
saveRDS(dat,paste0(file.loc.git,save.name))

}
