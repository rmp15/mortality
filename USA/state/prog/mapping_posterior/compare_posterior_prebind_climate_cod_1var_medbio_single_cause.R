rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4]) ; model.2 = as.numeric(args[4])
dname <- as.character(args[6])
metric <- as.character(args[7])
cause <- as.character(args[8]) ; cause <- gsub('_',' ',cause)
cause.2 <- as.character(args[9]) ; cause.2 <- gsub('_',' ',cause.2)
contig.arg <- as.numeric(args[10])
pw.arg <- as.numeric(args[11])

# for model testing
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 29 ; dname='t2m' ; metric='meanc4'
# cause.2 = 'Accidental drowning and submersion' ;  contig.arg = 1 ; pw.arg = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]

library(INLA)

# create dataframe with each of the national terms for entire group of age and sexes
dat <- data.frame()
# create dataframe of all the actual parameter terms
dat.parameters <- data.frame()
dat.intercepts <- data.frame()
dat.abs.temps  <- data.frame()

# find the posterior exponential mean
for (i in c(1,2)) {
    for (j in c(1,2,3,4,5,6,7,8,9,10)) {
    # for (j in c(1,2,7)) {

        # load data
        print(paste0(cause.2,' ',i,' ',j))

        # load file name
        file.name.2 <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',
        dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
        '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
        year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_parameters_fast_contig')

        print(file.exists(file.name.2))

        try(model.current.2 <- readRDS(file.name.2))

        # add temperature anomaly coefficients
        try(dat.parameters.2 <- data.frame(model.2.mean=model.current.2$summary.random$month5$mean,model.2.ll=model.current.2$summary.random$month5$`0.025quant`,
                                        model.2.ul=model.current.2$summary.random$month5$`0.975quant`))
        try(dat.param.current <- cbind(dat.parameters.2))
        try(dat.param.current$age <- age.filter[j]) ; try(dat.param.current$sex <-i) ; try(dat.param.current$cause <-cause.2)
        try(dat.parameters <- rbind(dat.parameters,dat.param.current))

        # add intercept term
        try(dat.intercept.2 <- data.frame(model.2.mean=model.current.2$summary.fixed$mean[1]))
        try(dat.intercept.current <- cbind(dat.intercept.2))
        try(dat.intercept.current$age <- age.filter[j]) ; try(dat.intercept.current$sex <-i) ; try(dat.intercept.current$cause <-cause.2)
        try(dat.intercepts <- rbind(dat.intercepts,dat.intercept.current))

        if(model%in%c('1d11')){
            try(dat.abs.temp.2 <- data.frame(model.2.mean=model.current.2$summary.random$month8$mean))
            try(dat.abs.temp.current <- cbind(dat.abs.temp.2))
            try(dat.abs.temp.current$age <- age.filter[j]) ; try(dat.abs.temp.current$sex <-i) ; try(dat.abs.temp.current$cause <-cause.2)
            try(dat.abs.temps <- rbind(dat.abs.temps,dat.abs.temp.current))
        }

        # }

    }
}

# create directories for output
file.loc.git <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior and summaries
# save.name <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_fast_contig.csv')
save.name.param <- paste0(country,'_parameters_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')
save.name.intercept <- paste0(country,'_intercepts_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')
save.name.abs.temp <- paste0(country,'_abs_temp_values_',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause.2,'_',i,'_fast_contig.csv')

write.csv(dat.parameters,paste0(file.loc.git,save.name.param))
write.csv(dat.intercepts,paste0(file.loc.git,save.name.intercept))
write.csv(dat.abs.temps,paste0(file.loc.git,save.name.abs.temp))