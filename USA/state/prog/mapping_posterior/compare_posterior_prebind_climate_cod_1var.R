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
contig.arg <- as.numeric(args[9])
pw.arg <- as.numeric(args[10])

# for model testing
# year.start = 1980 ; year.end = 2016 ; country = 'USA' ; model = 10 ; model.2 = 26 ; dname='t2m' ; metric='meanc3'
# cause = 'Transport accidents' ;  contig.arg = 1 ; pw.arg = 0

# source variables
source('../../data/objects/objects.R')
model <- models[model]
model.2 <- models[model.2]

if(pw.arg==0){
    # create dataframe with each of the national terms for entire group of age and sexes
    dat <- data.frame()
    
    # find the posterior exponential mean
    for (i in seq(length(sex.filter))) {
        for (j in seq(length(age.filter))) {
            # load data
            if(cause!='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
                file.name.2 <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model.2,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model.2,'_',age.filter[j],'_',sex.lookup[i],'_',
                year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_parameters_fast_contig')
            }
            if(cause=='AllCause'){
                file.name <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
                file.name.2 <- paste0('~/data/mortality/US/state/climate_effects/',
                dname,'/',metric,'/non_pw/type_',model.2,'/age_groups/',age.filter[j],
                '/',country,'_rate_pred_type',model.2,'_',age.filter[j],'_',sex.lookup[i],
                '_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters_fast_contig')
            }

            # load parameters
            model.current <- readRDS(file.name)
            model.current.2 <- readRDS(file.name.2)

            corr.temp.terms=cor(model.current$summary.random$month5$mean,model.current.2$summary.random$month5$mean)

            corr.state.intercept.terms=cor(model.current$summary.random$ID$mean,model.current.2$summary.random$ID$mean)
            corr.month.intercept.terms=cor(model.current$summary.random$month$mean,model.current.2$summary.random$month$mean)
            corr.state.month.intercept.terms=cor(model.current$summary.random$month3$mean,model.current.2$summary.random$month3$mean)

            corr.state.slope.terms=cor(model.current$summary.random$ID2$mean,model.current.2$summary.random$ID2$mean)
            corr.month.slope.terms=cor(model.current$summary.random$month2$mean,model.current.2$summary.random$month2$mean)
            corr.state.month.slope.terms=cor(model.current$summary.random$month4$mean,model.current.2$summary.random$month4$mean)

            scale.month7 = mean(100*model.current.2$summary.random$month7$mean / mean(model.current$summary.random$month3$mean))

            # obtain correlation values
            corr.current = data.frame(age=age.filter[j],sex=i,corr.temp.terms=corr.temp.terms, corr.state.intercept.terms= corr.state.intercept.terms,
            corr.month.intercept.terms=corr.month.intercept.terms,corr.state.month.intercept.terms=corr.state.month.intercept.terms,
            corr.state.slope.terms=corr.state.slope.terms,corr.month.slope.terms=corr.month.slope.terms,corr.state.month.slope.terms=corr.state.month.slope.terms,
            scale.new.temp.slope=scale.month7)

            dat = rbind(dat,corr.current)

        }
    }

# create directories for output
file.loc.git <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,'/',dname,'/',metric,'/non_pw/type_',model,'_and '_,model.2,'/parameters/')
ifelse(!dir.exists(file.loc.git), dir.create(file.loc.git, recursive=TRUE), FALSE)

# save bound posterior
if(cause!='AllCause'){
    save.name <- paste0(country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_',cause,'_fast_contig.csv')
}
if(cause=='AllCause'){
    save.name <- paste0(country,'_correlations_',model,'_',model.2,'_',year.start,'_',year.end,'_',dname,'_',metric,'_fast_contig.csv')
}

write.csv(dat,paste0(file.loc.git,save.name))

}