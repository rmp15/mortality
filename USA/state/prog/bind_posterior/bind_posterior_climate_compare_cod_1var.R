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

# MODEL 1D5
file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_1d5/age_groups/85/',country,'_rate_pred_type1d5_85_Men_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
model.current <- readRDS(file.name)
model.1d5 <- model.current$summary.random$month5
model.1d5$model = 'Hyper edit 2'
model.1d5 = as.data.frame(model.1d5[,c('mean')])
names(model.1d5) = c('mean.hyper.edit.2')

# MODEL 1D6
file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_1d6/age_groups/85/',country,'_rate_pred_type1d6_85_Men_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
model.current <- readRDS(file.name)
model.1d6 <- model.current$summary.random$month5
model.1d6$model = 'Hyper edit 1'
model.1d6 = as.data.frame(model.1d6[,c('mean')])
names(model.1d6) = c('mean.hyper.edit.1')


# MODEL 1D7
file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_1d7/age_groups/85/',country,'_rate_pred_type1d7_85_Men_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
model.current <- readRDS(file.name)
model.1d7 <- model.current$summary.random$month5
model.1d7$model = 'Hyper edit 3'
model.1d7 = as.data.frame(model.1d7[,c('mean')])
names(model.1d7) = c('mean.hyper.edit.3')

models = cbind(model.1d,model.1d5,model.1d6,model.1d7)

# plots against each other
plot(models)