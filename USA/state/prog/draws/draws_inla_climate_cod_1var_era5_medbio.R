rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cause <- as.character(args[7]) ; cause <- gsub('_',' ',cause)
contig <- as.numeric(args[8])
num.draws <- as.numeric(args[9])

# NEED TO MAKE CONTIG OPTION ACTUALLY DO SOMETHING

#year.start = 1980 ; year.end = 2017 ; country = 'USA' ; model = 27 ; dname = 't2m' ; metric = 'meanc4' ; cause = 'Transport accidents'; contig=1 ; num.draws = 1000

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
INLA:::inla.dynload.workaround()

# load inla paradiso (what on earth is this?)
library(INLA)
inla.paradiso()
inla.setOption(pardiso.license="~/git/mortality/USA/state/prog/00_bash/paradiso.lic")
inla.paradiso.check()

# temporary list of injury causes
causes = c("Transport accidents", "Intentional_self-harm", "Accidental falls", "Accidental drowning and submersion", "Assault")

# load the data for each age and sex to make draws
# for(cause in causes){
for (i in seq(length(sex.filter))) {
    for (j in seq(length(age.filter))) {

        # create directories for output
        file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
        '/',dname,'/',metric,'/non_pw/type_',model,'/non_contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
        if(contig==1){
            file.loc <- paste0('~/data/mortality/US/state/draws_era5/',year.start,'_',year.end,
            '/',dname,'/',metric,'/non_pw/type_',model,'/contig/',cause,'/',num.draws,'_draws/age_groups/',age.filter[j],'/')
            }
        ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

        # load the full model for a particular age and sex
        if(cause!='AllCause'){
            file.name <- paste0('~/data/mortality/US/state/climate_effects_era5/',
            dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
            '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
            year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
        }
        if(cause=='AllCause'){
            file.name <- paste0('~/data/mortality/US/state/climate_effects_era5/',
            dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
            '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
        }

        print(paste0('Reading ',file.name))
        model.current <- readRDS(file.name)

        # make draws from the model for the parameters
        print(paste0('Making ',num.draws, ' draws...'))
        # draws.current = try(inla.posterior.sample(num.draws,model.current))
        draws.current = try(inla.posterior.sample(num.draws,model.current,selection=list('Predictor'=1:10)))
        # try(do.call("<-", list(paste0('draws.',age.filter[j],'.',sex.lookup[i]), draws.current)))

        # save draws as an rds file
        print('Saving file...')
        save.name = paste0(country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
            '_',year.start,'_',year.end,'_',dname,'_',metric,'_',num.draws,'_draws_fast_contig')
        try(saveRDS(draws.current,paste0(file.loc,save.name)))
}}
# }
### LEGACY ONLY FOR COMPARISON OF CENTRAL ESTIMATES

# # load the data for quoting parameters
# if(contig==1){
#     if(cause!='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
#         country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig'))
#     }
#     if(cause=='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
#         ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig'))
#     }
# }
# if(contig==0){
#     if(cause!='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/',
#         country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast'))
#     }
#     if(cause=='AllCause'){
#         dat <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters/'
#         ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast'))
#     }
# }

# to check distribution of parameters if required
# ggplot() + geom_point(data=subset(dat,age==25&sex==1),aes(x=as.factor(ID),y=odds.mean,color='red',size=5)) +
#     geom_boxplot(data=complete.table,aes(x=as.factor(ID),y=climate.values)) +
#     geom_point(data=subset(dat,age==25&sex==1),aes(x=as.factor(ID),y=odds.mean,color='red',size=5))
