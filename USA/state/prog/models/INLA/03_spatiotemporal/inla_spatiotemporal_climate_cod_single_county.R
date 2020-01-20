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
# sex.arg = 2 ; year.start.arg = 1982 ; year.end.arg = 2017 ; type.arg=27
# cluster.arg = 0 ; year.start.analysis.arg = 1982 ; year.end.analysis.arg = 2017 ;
# cod.arg = 'AllCause'; pw.arg=0
# fips.arg = '04013' ; fips.arg = as.numeric(fips.arg) # currently doesn't do anything until code is generalised to county

# meteoroligical variables (currently hard-coded)
dname.arg='t2m'; metric.arg='mean'

# types character for file strings (internal model references so not so important for anyone else apart from code developer)
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9')
type.selected <- types[type.arg]

# range of years
years <- year.start.arg:year.end.arg

require(mailR)
library(plyr)

# create file location for output
if(pw.arg==0){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_single_county/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages')), dir.create(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages'),recursive=TRUE), FALSE)
}
if(pw.arg==1){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects_single_county/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages')), dir.create(paste0('~/data/mortality/US/state/climate_effects_maricopa/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages'),recursive=TRUE), FALSE)
}

# load data and filter results
if(cod.arg%in%c('Cancer','Cardiopulmonary','External','Other')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_maricopa_rates_cod_1982_2017'))
	dat.inla.load <- subset(dat.inla.load,cause==cod.arg & sex==sex.arg)
}
if(cod.arg%in%c('AllCause')){
	dat.inla.load <- readRDS(paste0('../../output/prep_data/datus_maricopa_rates_cod_1982_2017'))
    dat.inla.load <- plyr::ddply(dat.inla.load,.(sex,age,year,month,fips),summarize,deaths=sum(deaths),deaths.adj=sum(deaths.adj),pop=mean(pop),pop.adj=mean(pop.adj))
	dat.inla.load$rate.adj <- with(dat.inla.load,deaths.adj/pop.adj)
    dat.inla.load <- subset(dat.inla.load,sex==sex.arg)
}

# load climate data for 1980-2018 (currently hard coded for Maricopa but will need to generalised
file.loc <- paste0('~/git/climate/countries/USA/data/temp_maricopa/PHX_temp_monthly_stats_C.csv')
dat.climate <- read.csv(file.loc)
dat.climate$X = NULL

# centre each month by its long-term average (either detrend or scale)
dat.climate = plyr::ddply(dat.climate, c("month"), transform, temp_mean_detrend = pracma::detrend(temp_mean), temp_mean_scale = scale(temp_mean))

# plot to check if desired
# ggplot(data=dat.climate,aes(x=year)) +
# geom_line(aes(y=temp_mean_detrend,color=as.factor(month)),linetype=1) +
# geom_line(aes(y=temp_mean_scale,color=as.factor(month)),linetype=2) +
# facet_wrap(~month)+geom_hline(yintercept=0)

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('year','month'),by.y=c('year','month'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

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

# reorder file for neatness
dat.inla <- dat.inla[order(dat.inla$sex,dat.inla$age,dat.inla$year.month),]

# fix rownames
rownames(dat.inla) <- 1:nrow(dat.inla)

# create a factor variable for age (1 to 10 for the 10 age groups)
dat.inla$ID = with(dat.inla, match(age, unique(age)))

# variables for INLA model (quirks of model mean that to re-use a column for another element of model, you have to clone it)
dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
dat.inla$e <- 1:nrow(dat.inla)
dat.inla$ID2 = dat.inla$ID

# create piecewise climate variable if required
if(pw.arg==1){
    dat.inla$temp_mean_detrend_pos = ifelse(dat.inla$temp_mean_detrend<0,0,dat.inla$temp_mean_detrend)
    dat.inla$temp_mean_detrend_neg = ifelse(dat.inla$temp_mean_detrend>0,0,dat.inla$temp_mean_detrend)
}

# plot to check if desired
# ggplot(data=dat.inla,aes(x=year)) +
# geom_line(aes(y=temp_mean_detrend_pos,color=as.factor(month)),linetype=1) +
# facet_wrap(~month)+geom_hline(yintercept=0)
#
# ggplot(data=dat.inla,aes(x=year)) +
# geom_line(aes(y=temp_mean_detrend_neg,color=as.factor(month)),linetype=1) +
# facet_wrap(~month)+geom_hline(yintercept=0)

# create directory for output
if(pw.arg==0){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_county/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages/')
}
if(pw.arg==1){
    file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_county/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/all_ages/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

library(INLA)

# load inla function

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
        # age-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                                  # age-month specific intercept
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                     # age-month specific slope
        # age specific terms
        f(ID, model="rw1") +                                      		                # age specific intercept (RW1)
        f(ID2, year.month2, model="rw1") +                        		                # age specific slope (RW1)
        # climate specific terms
        f(month5, temp_mean_detrend, model="rw1", cyclic=TRUE,group=ID) +               # month specific climate slope, by age
        # random walk across time (could make by age group if converges OK
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
        # age-month specific terms
        f(month3, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                                  # age-month specific intercept
        f(month4, year.month2, model="rw1",cyclic = TRUE,group=ID,control.group=list(model='rw1'))+                     # age-month specific slope
        # age specific terms
        f(ID, model="rw1") +                                      		                # age specific intercept (RW1)
        f(ID2, year.month2, model="rw1") +                        		                # age specific slope (RW1)
        # climate specific terms
        f(month5, temp_mean_detrend_pos, model="rw1", cyclic=TRUE,group=ID) +           # month specific climate slope, by age
        f(month6, temp_mean_detrend_neg, model="rw1", cyclic=TRUE,group=ID) +           # month specific climate slope, by age
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

if(pw.arg==0){
    # output string for filenames
    output.string = paste0('single_county_rate_pred_type',type.selected,'_',sex.lookup[sex.arg],'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)
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
subject.arg = paste0(sex.lookup[sex.arg],' single county all ages model ',type.selected,' ',dname.arg,' ',metric.arg,' ',cod.arg,' ',year.start.analysis.arg,'-',year.end.analysis.arg)
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