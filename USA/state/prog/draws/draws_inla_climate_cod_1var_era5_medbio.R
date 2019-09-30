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
sex <- as.numeric(chosen.row[1,1])
age <- as.numeric(chosen.row[1,2])
cause <- as.character(chosen.row[1,3])
year.start <- 1980
year.end <- 2017
country <- 'USA'
model <- 27
dname <- 't2m'
metric <- 'meanc4'
contig <- 1
num.draws <- 1

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
INLA:::inla.dynload.workaround()

# load inla paradiso (what on earth is this?)
library(INLA)
inla.pardiso()
inla.setOption(pardiso.license="~/git/mortality/USA/state/prog/00_bash/pardiso.lic")
inla.pardiso.check()

# create directories for output
file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
'/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[age],'/')
if(contig==1){
    file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
    '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[age],'/')
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

# OTHER ATTEMPTS
# draws.current = try(inla.posterior.sample(num.draws,model.current,selection=list('Predictor'=1:10)))
# try(do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i]), draws.current)))

# save draws as an rds file
print('Saving file...')
save.name = paste0(country,'_rate_pred_type',model,'_',age.filter[age],'_',sex.lookup[sex],
    '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
try(saveRDS(draws.current,paste0(file.loc,save.name)))