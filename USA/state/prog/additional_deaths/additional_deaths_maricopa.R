rm(list=ls())

# set up argument (don't get why but it should be like this apparently)
seedVal <-as.numeric(commandArgs()[4])

# break down the arguments from Rscript
sex <- 1
cause <- 'AllCause'
year.start <- 1982
year.end <- 2017
country <- 'USA'
model <- 27
pw <- 1
dname <- 't2m'
metric <- 'mean'

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
INLA:::inla.dynload.workaround()

# load inla
library(INLA)

# create directories for output
file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/age_groups/',age,'/')
if(contig==1){
    file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
    '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/age_groups/',age,'/')
    }
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load the full model for a particular age and sex
if(cause!='AllCause'){
    file.name <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',
    dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age,
    '/',country,'_rate_pred_type',model,'_',age,'_',sex.lookup[sex],'_',
    year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
}
if(cause=='AllCause'){
    file.name <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',
    dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age,
    '/',country,'_rate_pred_type',model,'_',age,'_',sex.lookup[sex],
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
}

print(paste0('Reading ',file.name))
model.current <- readRDS(file.name)

# make draws from the model for the parameters
print(paste0('Making ',num.draws, ' draws...'))
# draws.current = try(inla.posterior.sample(num.draws,model.current))
draws.current = try(inla.posterior.sample(num.draws,model.current,selection=list('month5'=1:12)))

# save draws as an rds file
print('Saving file...')
save.name = paste0(country,'_rate_pred_type',model,'_',age,'_',sex.lookup[sex],
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
try(saveRDS(draws.current,paste0(file.loc,save.name)))
