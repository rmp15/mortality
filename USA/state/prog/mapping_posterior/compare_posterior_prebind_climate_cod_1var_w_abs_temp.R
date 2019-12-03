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
# year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model.1 = 27 ; model.2 = 29 ; dname='t2m' ; metric='meanc4'
# contig.arg = 1 ; pw.arg = 0

# source variables
source('../../data/objects/objects.R')
model.1 <- models[model.1]
model.2 <- models[model.2]

# directories for output
file.loc.git.1 <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model.1,'/parameters/')
file.loc.git.2 <- paste0('../../output/compare_posterior_climate_era5/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model.2,'/parameters/')

causes = c('Accidental drowning and submersion','Assault','Transport accidents','Accidental falls','Intentional self-harm')

# load first model for comparisons's data
dat.param.1 = data.frame()
dat.intercept.1 = data.frame()

for(cause in causes){

    save.name.param <- paste0(country,'_parameters_',model.1,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
    save.name.intercept <- paste0(country,'_intercepts_',model.1,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')

    dat.param.1.temp = read.csv(paste0(file.loc.git.1,save.name.param))
    dat.intercept.1.temp = read.csv(paste0(file.loc.git.1,save.name.intercept))

    dat.param.1 = rbind(dat.param.1,dat.param.1.temp)
    dat.intercept.1 = rbind(dat.intercept.1,dat.intercept.1.temp)

}

# load second model for comparisons's data
dat.param.2 = data.frame()
dat.intercept.2 = data.frame()
dat.abs.temp.2 = data.frame()

for(cause in causes){

    save.name.param <- paste0(country,'_parameters_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
    save.name.intercept <- paste0(country,'_intercepts_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')
    save.name.abs.temp <- paste0(country,'_abs_temp_values_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_2_fast_contig.csv')

    dat.param.2.temp = read.csv(paste0(file.loc.git.2,save.name.param))
    dat.intercept.2.temp = read.csv(paste0(file.loc.git.2,save.name.intercept))
    dat.name.abs.temp.2.temp = read.csv(paste0(file.loc.git.2,save.name.abs.temp))

    dat.param.2 = rbind(dat.param.2,dat.param.2.temp)
    dat.intercept.2 = rbind(dat.intercept.2,dat.intercept.2.temp)
    dat.abs.temp.2 = rbind(dat.abs.temp.2,dat.name.abs.temp.2.temp)

}

# 1. merge temperature terms against each other and plot
# TEMPORARILY CUT OUT THE 65 FEMALES AS IT's WRONG
dat.param.1$X = NULL ; dat.param.2$X = NULL
dat.param.1$month=c(1:12) ; dat.param.2$month=c(1:12)
names(dat.param.1) = c('model.1.mean',"model.1.ll", "model.1.ul", "age", "sex",'cause')
dat.param.merged = merge(dat.param.1,dat.param.2,by.x=c('age','sex','cause','month'))
dat.param.merged

# 2. merge intercept terms against each other and plot
dat.intercept.1$X = NULL ; dat.intercept.2$X = NULL
names(dat.intercept.1) = c('model.1.mean', "age", "sex",'cause')
dat.intercept.merged = merge(dat.intercept.1,dat.intercept.2,by.x=c('age','sex','cause'))

# 3. plot absolute temperature terms
dat.abs.temp.2$month=c(1:12)