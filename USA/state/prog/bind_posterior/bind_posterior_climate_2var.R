# NEED AN OPTION FOR FAST AND FASTER

rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric1 <- as.character(args[6])
metric2 <- as.character(args[7])

library(INLA)

# establish parameters for file name
age.filter <- c(0,5,15,25,35,45,55,65,75,85)
sex.filter <- c('male','female')

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0')
model <- models[model]

metric = paste(sort(c(metric1,metric2)),collapse='_')

# MODEL 1D
if(model=='1d'){
    
    # create dataframes with each of the national terms for entire group of age and sexes
    dat1 <- data.frame()
    dat2 <- data.frame()
    
    # find the posterior exponential mean
    for (i in seq(length(sex.filter))) {
        for (j in seq(length(age.filter))) {
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/2var/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],'/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast')
                print(file.name)
                model.current <- readRDS(file.name)
                
                # FIRST CLIMATE VARIABLE
                current.file <- model.current$summary.random$month5
                current.file$age <- age.filter[j] ; current.file$sex <- i
                
                # find mean and CIs of transformed distributions and probability of increased odds from posterior marginal
                dat.mean.exp <- data.frame(ID=numeric(0),odds.mean=numeric(0),odds.ll=numeric(0),odds.ul=numeric(0),odds.prob=numeric(0))
                for(k in c(1:12)) {
                    # find the exponentiated means and CIs
                    marginal.exp <- inla.tmarginal(function(x) exp(x), model.current$marginals.random$month5[[k]])
                    odds.mean <- inla.emarginal(function(x) x,marginal.exp) - 1
                    odds.ll <- inla.qmarginal(0.025,marginal.exp) - 1
                    odds.ul <- inla.qmarginal(0.975,marginal.exp) - 1
                    
                    # find the probability of increased odds from posterior marginal
                    odds.prob <- 1 - inla.pmarginal(1,marginal.exp)
                    dat.temp <- data.frame(ID=k,odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
                    dat.mean.exp <- rbind(dat.mean.exp,dat.temp)
                }
                # merge exponentiated means
                current.file <- merge(current.file,dat.mean.exp,by=('ID'))
                
                # attached new age sex profile to master file
                dat1 <- rbind(dat1,current.file)
                
                # SECOND CLIMATE VARIABLE
                current.file <- model.current$summary.random$month6
                current.file$age <- age.filter[j] ; current.file$sex <- i
                
                # find mean and CIs of transformed distributions and probability of increased odds from posterior marginal
                dat.mean.exp <- data.frame(ID=numeric(0),odds.mean=numeric(0),odds.ll=numeric(0),odds.ul=numeric(0),odds.prob=numeric(0))
                for(k in c(1:12)) {
                    # find the exponentiated means and CIs
                    marginal.exp <- inla.tmarginal(function(x) exp(x), model.current$marginals.random$month6[[k]])
                    odds.mean <- inla.emarginal(function(x) x,marginal.exp) - 1
                    odds.ll <- inla.qmarginal(0.025,marginal.exp) - 1
                    odds.ul <- inla.qmarginal(0.975,marginal.exp) - 1
                    
                    # find the probability of increased odds from posterior marginal
                    odds.prob <- 1 - inla.pmarginal(1,marginal.exp)
                    dat.temp <- data.frame(ID=k,odds.mean=odds.mean,odds.ll=odds.ll,odds.ul=odds.ul,odds.prob=odds.prob)
                    dat.mean.exp <- rbind(dat.mean.exp,dat.temp)
                }
                # merge exponentiated means
                current.file <- merge(current.file,dat.mean.exp,by=('ID'))
                
                # attached new age sex profile to master file
                dat2 <- rbind(dat2,current.file)
        }
    }
    
# attach names for each of the datasets
dat1$var = 1
dat2$var = 2

# bind the files
dat = rbind(dat1,dat2)

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/2var/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local, recursive=TRUE), FALSE)
file.loc.git <- paste0('../../data/climate_effects/',dname,'/2var/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior
save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast')
saveRDS(dat,paste0(file.loc.local,save.name))
saveRDS(dat,paste0(file.loc.git,save.name))

}

# NOT VALID FROM HERE....
