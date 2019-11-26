rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
sex.arg <- as.numeric(args[1])
year.start.arg <- as.numeric(args[2])
year.end.arg <- as.numeric(args[3])
type.arg <- as.numeric(args[4])
cluster.arg <- as.numeric(args[5])
year.start.analysis.arg <- as.numeric(args[6])
year.end.analysis.arg <- as.numeric(args[7])
cod.arg <- as.character(args[8]) ; cod.arg <- gsub('_',' ',cod.arg)
pw.arg <- as.numeric(args[9])

# for test runs
# sex.arg = 1 ; year.start.arg = 1982 ; year.end.arg = 2017 ; type.arg=27
# cluster.arg = 0 ; year.start.analysis.arg = 1982 ; year.end.analysis.arg = 2017 ;
# cod.arg = 'Cardiopulmonary'; pw.arg=0

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8')
# type.selected <- types[type.arg]

# print(paste(year.start.analysis.arg,year.end.analysis.arg,age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create file location for output
if(pw.arg==0){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}
if(pw.arg==1){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}

# load data and filter results
if(cod.arg%in%c('Cancer','Cardiopulmonary','External','Other')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_maricopa_rates_cod_1982_2017'))
	dat.inla.load <- subset(dat.inla.load,cause==cod.arg & sex==sex.arg)
}

# load climate data for 1980-2018
file.loc <- paste0('~/git/climate/countries/USA/data/temp_maricopa/PHX_temp_monthly_stats_C.csv')
dat.climate <- read.csv(file.loc)
dat.climate$X = NULL

library(dplyr)

# centre each month by its long-term average
dat.climate = ddply(dat.climate, c("month"), transform, temp_mean_detrend = pracma::detrend(temp_mean), temp_mean_scale = scale(temp_mean))

# plot to check if desired
ggplot(data=dat.test,aes(x=year)) +
geom_line(aes(y=temp_mean_detrend,color=as.factor(month)),linetype=1) +
geom_line(aes(y=temp_mean_scale,color=as.factor(month)),linetype=2) +
facet_wrap(~month)+geom_hline(yintercept=0)

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('year','month'),by.y=c('year','month'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

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

# lookups
source('../../data/objects/objects.R')

# rename main data file
dat.inla <- dat.merged

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# make sure that the order of the main data file matches that of the shapefile otherwise the model will not be valid
dat.inla <- dat.inla[order(dat.inla$sex,dat.inla$age,dat.inla$year.month),]

# fix rownames
rownames(dat.inla) <- 1:nrow(dat.inla)

# create a factor variable for age (1 to 10)
dat.inla$age.rank = with(dat.inla, match(age, unique(age)))

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
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.arg)
}
if(pw.arg==1){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups/',age.arg)
}
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

library(INLA)

# load inla function

# 1. Type Id space-time interaction with besag state interaction terms and state-month specific variable slope (rw1)
fml  <- deaths.adj ~
# global terms
1 +                                                                     		# global intercept
year.month +                                                           			# global slope
# month specific terms
f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
# age-month specific terms
f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                                  # age-month specific intercept
f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                     # age-month specific slope
# age specific terms
f(ID, model="rw1") +                                      		                # age specific intercept (RW1)
f(ID2, year.month2, model="rw1") +                        		                # age specific slope (RW1)
# climate specific terms
f(month5, temp_mean, model="rw1", cyclic=TRUE,group=ID) +                       # month specific climate slope
# random walk across time (could make by age group if converges OK
f(year.month3, model="rw1") +                                           		# rw1
# overdispersion term
f(e, model = "iid")                                                    		 	# overdispersion term

# if piece-wise (to finish)
if(pw.arg==1){
fml  <- deaths.adj ~
# global terms
1 +                                                                     		# global intercept
year.month +                                                           			# global slope
# month specific terms
f(month, model='rw1',cyclic = TRUE) +                                           # month specific intercept
f(month2, year.month2, model='rw1', cyclic= TRUE) +                             # month specific slope
# state-month specific terms
f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='bym',graph=USA.adj))+                  # state-month specific intercept (spatially-correlated)
f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID, control.group=list(model='bym',graph=USA.adj))+    # state-month specific slope (spatially-correlated)
# state specific terms
f(ID, model="bym",graph=USA.adj) +                                      		# state specific intercept (BYM)
f(ID2, year.month2, model="bym",graph=USA.adj) +                        		# state specific slope (BYM)
# climate specific terms
f(month5, variable2, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (above 0)
f(month6, variable3, model="rw1", cyclic=TRUE) +                                 # month specific climate slope (below 0)
# random walk across time
f(year.month3, model="rw1") +                                           		# rw1
# overdispersion term
f(e, model = "iid")
}

# functions to enable age group and sex to be selected with faster AR1 structure in addition to rough run
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
    #verbose=TRUE
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
    #verbose=TRUE
    ))

    return(mod)
}

mod = inla.function.climate.faster()

# prep data for output

# output string for filenames
output.string = paste0('USA_rate_pred_type',type.selected,'_',age.arg,'_',sex.lookup[sex.arg],'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)

# save all parameters of INLA model
parameters.name <- paste0(output.string)
if(cod.arg!='AllCause'){parameters.name = paste0(parameters.name,'_',cod.arg,'_parameters')}
if(cod.arg=='AllCause'){parameters.name = paste0(parameters.name,'_parameters')}
if(fast.arg==1){parameters.name = paste0(parameters.name,'_fast')}
if(fast.arg==2){parameters.name = paste0(parameters.name,'_faster')}
if(contig.arg == 1){parameters.name = paste0(parameters.name,'_contig')}
#mod$misc <- NULL ; mod$.args$.parent.frame <- NULL
saveRDS(mod,paste0(file.loc,'/',parameters.name))

# save summary of INLA model
summary.name <- paste0(output.string)
if(cod.arg!='AllCause'){summary.name = paste0(summary.name,'_',cod.arg,'_summary')}
if(cod.arg=='AllCause'){summary.name = paste0(summary.name,'_summary')}
if(fast.arg==1){summary.name = paste0(summary.name,'_fast')}
if(fast.arg==2){summary.name = paste0(summary.name,'_faster')}
if(contig.arg == 0){summary.name = paste0(summary.name,'.txt')}
if(contig.arg == 1){summary.name = paste0(summary.name,'_contig.txt')}
inla.summary.mod <- summary(mod)
capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))

# save RDS of INLA results
RDS.name <- paste0(output.string)
if(cod.arg!='AllCause'){RDS.name = paste0(RDS.name,'_',cod.arg)}
if(cod.arg=='AllCause'){RDS.name = paste0(RDS.name)}
if(fast.arg==1){RDS.name = paste0(RDS.name,'_fast')}
if(fast.arg==2){RDS.name = paste0(RDS.name,'_faster')}
if(contig.arg == 1){RDS.name = paste0(RDS.name,'_contig')}
plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))

# send email notification

# subject for email
subject.arg = paste0(sex.lookup[sex.arg],' ',age.arg,' model ',type.selected,' ',dname.arg,' ',metric.arg,' ',cod.arg,' ',year.start.analysis.arg,'-',year.end.analysis.arg)
if(contig.arg == 1){subject.arg = paste0(subject.arg,' contig')}
if(fast.arg==0){subject.arg = paste0(subject.arg,' ')}
if(fast.arg==1){subject.arg = paste0(subject.arg,' fast')}
if(fast.arg==2){subject.arg = paste0(subject.arg,' faster')}
if(pw.arg==0){subject.arg = paste0(subject.arg,' non-pw done')}
if(pw.arg==1){subject.arg = paste0(subject.arg,' pw done')}


print(subject.arg)

# sending email
sender = "emailr349@gmail.com"
recipients = c("r.parks15@imperial.ac.uk")
send.mail(from = sender,
to = recipients,
subject = subject.arg,
body = "Well done",
smtp = list(host.name = "smtp.gmail.com", port = 465,
user.name = "emailr349@gmail.com",
passwd = "inlaisthebest", ssl = TRUE),
authenticate = TRUE,
send = TRUE)