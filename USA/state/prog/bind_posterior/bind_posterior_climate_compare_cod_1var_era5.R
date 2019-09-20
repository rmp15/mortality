rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig.arg <- as.numeric(args[8])
pw.arg <- as.numeric(args[9])

# for testing
# year.start = 1980 ; year.end = 2016 ; country = 'USA' ; dname = 't2m' ; metric = 'meanc3' ; cause = 'Transport accidents'
# contig.arg = 1 ; pw.arg = 0

library(ggplot2)

# lookups
source('../../data/objects/objects.R')
model <- models[model]

# create directories for output
file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/')
if(contig==1){
    file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/parameters/contig/',cause,'/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)


# MODEL 1D
file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_1d/age_groups/85/',country,'_rate_pred_type1d_85_Men_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
model.current <- readRDS(file.name)
model.1d <- model.current$summary.random$month5
model.1d$model = 'Base'
model.1d = as.data.frame(model.1d[,c('mean')])
names(model.1d) = c('mean.base')

# MODEL 1D9
file.name <- paste0('~/data/mortality/US/state/climate_effects_era5/',dname,'/',metric,'/non_pw/type_1d9/age_groups/85/',country,'_rate_pred_type1d7_85_Men_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
model.current <- readRDS(file.name)
model.1d9 <- model.current$summary.random$month5
model.1d7$model = 'Additional abs climate value'
model.1d7 = as.data.frame(model.1d7[,c('mean')])
names(model.1d7) = c('mean.hyper.edit.3')

models = cbind(model.1d,model.1d5,model.1d6,model.1d7)

# plots against each other
plot(models)