rm(list=ls())

# FOR ALL AGES AND ALL CAUSES TOGETHER IN ONE STATE

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
type.arg <- as.numeric(args[5])
cluster.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])
year.start.analysis.arg <- as.numeric(args[9])
year.end.analysis.arg <- as.numeric(args[10])
cod.arg <- as.character(args[11]) ; cod.arg <- gsub('_',' ',cod.arg)
fast.arg <- as.numeric(args[12])
contig.arg <- as.numeric(args[13])
pw.arg <- as.numeric(args[14])

# for test runs
# year.start.arg = 1980 ; year.end.arg = 2017 ; type.arg = 27 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc4' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 2017 ; cod.arg = 'AllCause'; fast.arg = 1 ; contig.arg = 1
# pw.arg=0 ; state.arg = 4

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9','1d10')
type.selected <- types[type.arg]

print(paste(year.start.analysis.arg,year.end.analysis.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create file location for output
if(pw.arg==0){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages')), dir.create(paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages'),recursive=TRUE), FALSE)
}
if(pw.arg==1){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages')), dir.create(paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages'),recursive=TRUE), FALSE)
}
# load data and summarise for all cause
library(plyr)
dat.inla.load <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
dat.inla.load <- subset(dat.inla.load,fips==state.arg)
dat.inla.load <- ddply(dat.inla.load,.(year,month),summarise,deaths.adj=sum(deaths.adj),pop.adj=sum(pop.adj)/4) # /4 because four broad causes and need to adjust for that
dat.inla.load$rate.adj = with(dat.inla.load,deaths.adj/pop.adj)

# load climate data for 1980-2017
file.loc <- paste0('~/git/climate/countries/USA/output/metrics_development_era5/',dname.arg,'/',metric.arg,'_',dname.arg,'/')
dat.climate <- readRDS(paste0(file.loc,'state_weighted_summary_',metric.arg,'_',dname.arg,'_1980_2017.rds'))
dat.climate$state.fips <- as.numeric(as.character(dat.climate$state.fips))

# take one age-sex combination for single state (very similar values anyway)
dat.climate = subset(dat.climate,state.fips==state.arg&sex==1&age==55)
dat.climate = with(dat.climate,dat.climate[,c('year','month','t2m.meanc4')])

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by=c('year','month'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$year,dat.merged$month),]

# optional addition of long-term normals into model (currently testing may be removed)
if(type.arg %in% c(26)){

    # load long-term average (for sensitivity of model to inclusion). This may be a temporary inclusion depending on the outcome
    file.loc.abs <- paste0('~/git/climate/countries/USA/output/multiyear_normals/',dname.arg,'/mean/')
    dat.climate.abs = readRDS(paste0(file.loc.abs,'state_longterm_95_nonnormals_mean_',dname.arg,'_1980_2009.rds'))
    dat.climate.abs$state.fips = as.numeric(dat.climate.abs$state.fips)
    names(dat.climate.abs)[grep('mean',names(dat.climate.abs))] <- 'variable.abs'
    dat.climate.abs = subset(dat.climate.abs,select=c(month,state.fips,sex,age,variable.abs))

    # merge existing mortality and climate data with long-run absolute values
    dat.merged <- merge(dat.merged,dat.climate.abs,by.x=c('sex','age','month','fips'),by.y=c('sex','age','month','state.fips'),all.x=TRUE)

    # reorder one more time as had to add another column
    dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

}

# generalise climate variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# create lookup table for climate regions
# regions.lookup <- data.frame(climate_region=sort(unique(dat.merged$climate_region)))
# regions.lookup$ID.clim <- seq(nrow(regions.lookup))

# dat.merged <- merge(dat.merged,regions.lookup,by='climate_region')

library(dplyr)

# lookups
source('../../data/objects/objects.R')

##############

# filter all data by sex age and month
fit.years <- year.start.analysis.arg:year.end.analysis.arg
dat.inla <- dat.merged[dat.merged$year %in% fit.years,]

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# make sure that the order of the main data file matches that of the shapefile otherwise the model will not be valid
dat.inla <- dat.inla[order(dat.inla$year.month),]

# fix rownames
rownames(dat.inla) <- 1:nrow(dat.inla)

# variables for INLA model
dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
dat.inla$e <- 1:nrow(dat.inla)

# create piecewise climate variable if required
if(pw.arg==1){
    dat.inla$variable2 = ifelse(dat.inla$variable<0,0,dat.inla$variable)
    dat.inla$variable3 = ifelse(dat.inla$variable>0,0,dat.inla$variable)
}

# create directory for output
if(pw.arg==0){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages/')
}
if(pw.arg==1){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

library(INLA)

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
INLA:::inla.dynload.workaround()

# load inla pardiso (what on earth is this?)
inla.setOption(pardiso.license="~/git/mortality/USA/state/prog/00_bash/pardiso.lic")
inla.pardiso.check()

# define model
# 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
if(type.arg==27){
    if(pw.arg==0){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # climate specific terms
        f(month5, variable, model="rw1", cyclic=TRUE) +                                 # month specific climate slope
        # random walk across time
        f(year.month3, model="rw1") +                                           		# rw1 over time
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }

    # if piece-wise then this model
    if(pw.arg==1){
        fml  <- deaths.adj ~
        # global terms
        1 +                                                                     		# global intercept
        year.month +                                                           			# global slope
        # month specific terms
        f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
        f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
        # climate specific terms
        f(month5, variable_pos, model="rw1", cyclic=TRUE) +                   # month specific climate slope, by age
        f(month6, variable_neg, model="rw1", cyclic=TRUE) +                   # month specific climate slope, by age
        # random walk across time (could make by age group if converges OK
        f(year.month3, model="rw1") +                                           		# rw1 over time
        # overdispersion term
        f(e, model = "iid")                                                    		 	# overdispersion term
    }
}

# functions to enable sex to be selected with faster AR1 structure in addition to rough run
inla.function.climate.faster <- function() {

    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    verbose=TRUE
    ))

    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop.adj,
    control.compute = list(config=TRUE, dic=TRUE),
    control.predictor = list(link = 1),
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    verbose=TRUE
    ))

    return(mod)
}

mod = inla.function.climate.faster()

# prep data for output

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]

state.name = as.character(subset(fips.lookup,fips==state.arg)[1,1])

if(pw.arg==0){
    # output string for filenames
    output.string = paste0(state.name,'_rate_pred_type',type.selected,'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)
}
if(pw.arg==1){
    # output string for filenames
    output.string = paste0(output.string,'_pw')
}

# save all parameters of INLA model
parameters.name <- paste0(output.string)
if(cod.arg!='AllCause'){parameters.name = paste0(parameters.name,'_',cod.arg,'_parameters')}
if(cod.arg=='AllCause'){parameters.name = paste0(parameters.name,'_parameters')}
saveRDS(mod,paste0(file.loc,'/',parameters.name))

# save summary of INLA model
summary.name <- paste0(output.string)
if(cod.arg!='AllCause'){summary.name = paste0(summary.name,'_',cod.arg,'_summary')}
if(cod.arg=='AllCause'){summary.name = paste0(summary.name,'_summary')}
inla.summary.mod <- summary(mod)
capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))

# save RDS of INLA results
RDS.name <- paste0(output.string)
if(cod.arg!='AllCause'){RDS.name = paste0(RDS.name,'_',cod.arg)}
if(cod.arg=='AllCause'){RDS.name = paste0(RDS.name)}
plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))

# send email notification

# subject for email
subject.arg = paste0(state.name,' single state all age and sex model ',type.selected,' ',dname.arg,' ',metric.arg,' ',cod.arg,' ',year.start.analysis.arg,'-',year.end.analysis.arg)
if(contig.arg == 1){subject.arg = paste0(subject.arg,' contig')}
if(fast.arg==0){subject.arg = paste0(subject.arg,' ')}
if(fast.arg==1){subject.arg = paste0(subject.arg,' fast')}
if(fast.arg==2){subject.arg = paste0(subject.arg,' faster')}
if(pw.arg==0){subject.arg = paste0(subject.arg,' non-pw done')}
if(pw.arg==1){subject.arg = paste0(subject.arg,' pw done')}

print(subject.arg)

# # sending email
# sender = "emailr349@gmail.com"
# recipients = c("r.parks15@imperial.ac.uk")
# send.mail(from = sender,
# to = recipients,
# subject = subject.arg,
# body = "Well done",
# smtp = list(host.name = "smtp.gmail.com", port = 465,
# user.name = "emailr349@gmail.com",
# passwd = "inlaisthebest", ssl = TRUE),
# authenticate = TRUE,
# send = TRUE)