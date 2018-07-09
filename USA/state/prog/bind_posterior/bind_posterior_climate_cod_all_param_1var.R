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

# year.start=1980;year.end=2016;country='USA';model=10;dname='t2m';metric='meanc3';cause='Unintentional';contig.arg=1


library(INLA)

# lookups
source('../../data/objects/objects.R')
model <- models[model]

# MODEL 1D
if(model=='1d'){
    
    # create dataframe with each of the national terms for entire group of age and sexes
    dat.weather <- data.frame()
    dat.intercept <- data.frame()
    dat.slope <- data.frame()

    
    # find the posterior exponential mean
    for (i in seq(length(sex.filter))) {
        for (j in seq(length(age.filter))) {
            # load data
            if(cause!='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
            }
            if(cause=='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
            }
            model.current <- try(readRDS(file.name))
            if(inherits(model.current,"try-error")){
                if(cause!='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
            }
                if(cause=='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
            }
            }
            #file.name <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],'/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.filter[i],'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast')
            print(file.name)
            model.current <- readRDS(file.name)

            ######################################
            # global intercept (1)
            ######################################

            ######################################
            # global slope (year.month)
            ######################################

            ######################################
            # month-specific intercept (month)
            ######################################
            current.file <- model.current$summary.random$month
            current.file$age <- age.filter[j] ; current.file$sex <- i

            # find mean and CIs of transformed distributions and probability of increased odds from posterior marginal
            dat.mean.exp <- data.frame(ID=numeric(0),odds.mean=numeric(0),odds.ll=numeric(0),odds.ul=numeric(0),odds.prob=numeric(0))
            for(k in c(1:12)) {
                # find the exponentiated means and CIs
                marginal.exp <- inla.tmarginal(function(x) exp(x), model.current$marginals.random$month[[k]])
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
            dat.intercept <- rbind(dat.intercept,current.file)

            ######################################
            # month-specific slope (month2)
            ######################################
            current.file <- model.current$summary.random$month2
            current.file$age <- age.filter[j] ; current.file$sex <- i

            # find mean and CIs of transformed distributions and probability of increased odds from posterior marginal
            dat.mean.exp <- data.frame(ID=numeric(0),odds.mean=numeric(0),odds.ll=numeric(0),odds.ul=numeric(0),odds.prob=numeric(0))
            for(k in c(1:12)) {
                # find the exponentiated means and CIs
                marginal.exp <- inla.tmarginal(function(x) exp(x), model.current$marginals.random$month2[[k]])
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
            dat.slope <- rbind(dat.slope,current.file)

            ######################################
            # state-month specific intercept (month3)
            ######################################

            ######################################
            # state-month specific slope (month4)
            ######################################

            ######################################
            # state specific intercept (ID)
            ######################################

            ######################################
            # state specific slope (ID2)
            ######################################

            ######################################
            # state-month specific slope (month4)
            ######################################

            ######################################
            # weather parameter (month5)
            ######################################
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
            dat.weather <- rbind(dat.weather,current.file)

            ######################################
            # random walk across time (year.month3)
            ######################################

            ######################################
            # overdispersion term (e)
            ######################################

        }
    }

# create directories for output
file.loc.local <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters_all/')
ifelse(!dir.exists(file.loc.local), dir.create(file.loc.local, recursive=TRUE), FALSE)
file.loc.git <- paste0('../../data/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/parameters_all/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)


######################################
# month-specific intercept (month)
######################################
# save bound posterior
if(cause!='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_month_fast_contig')
}
if(cause=='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_month_fast_contig')
}
saveRDS(dat.intercept,paste0(file.loc.local,save.name))
saveRDS(dat.intercept,paste0(file.loc.git,save.name))

######################################
# month-specific slope (month2)
######################################
# save bound posterior
if(cause!='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_month2_fast_contig')
}
if(cause=='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_month2_fast_contig')
}
saveRDS(dat.slope,paste0(file.loc.local,save.name))
saveRDS(dat.slope,paste0(file.loc.git,save.name))

######################################
# weather parameter (month5)
######################################
# save bound posterior
if(cause!='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_month5_fast_contig')
}
if(cause=='AllCause'){
    save.name <- paste0(country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric,'_month5_fast_contig')
}
saveRDS(dat.weather,paste0(file.loc.local,save.name))
saveRDS(dat.weather,paste0(file.loc.git,save.name))

}