rm(list=ls())

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
contiguous.arg <- as.numeric(args[13])

# age.arg = 65 ; sex.arg = 1 ; year.start.arg = 1980 ; year.end.arg = 2013 ; type.arg = 10 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc3' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 1989 ; cod.arg = 'Cardiopulmonary'; fast.arg = 1 ; contiguous.arg = 0

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1')
type.selected <- types[type.arg]

print(paste(year.start.arg,year.end.arg,age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create files for output
ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)

# load data and filter results
source('../models/INLA/03_spatiotemporal/inla_load_data_cod.R')

# load climate region data and fix names
source('../models/INLA/03_spatiotemporal/inla_climate_regions.R')

# merge mortality data with climate region data and get new deaths rates
dat.inla.load <- merge(dat.inla.load,dat.region,by.x=('fips'),by.y=('STATE_FIPS'),all.x=TRUE)

# load climate data for 1979-2015
file.loc <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/')
dat.climate <- readRDS(paste0(file.loc,'state_weighted_summary_',metric.arg,'_',dname.arg,'_1979_2015.rds'))
dat.climate$state.fips <- as.numeric(as.character(dat.climate$state.fips))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$climate_region,dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# rename rows and remove unnecessary columns
rownames(dat.merged) <- 1:nrow(dat.merged)

# generalise climate variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# create lookup table for climate regions
regions.lookup <- data.frame(climate_region=sort(unique(dat.merged$climate_region)))
regions.lookup$ID.clim <- seq(nrow(regions.lookup))

dat.merged <- merge(dat.merged,regions.lookup,by='climate_region')

library(dplyr)

# lookups
source('../../data/objects/objects.R')

# adjacency matrix with connections
if(contiguous.arg == 0){
    # Hawaii -> California, Alaska -> Washington
    USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"
}
if(contiguous.arg == 1){
    # only contiguous USA
    USA.adj <- "../../output/adj_matrix_create/USA.graph.contig"
}

##############

library(INLA)

# filter all data by sex age and month
sex <- sex.arg
age <- age.arg
fit.years <- year.start.arg:year.end.arg
dat.inla <- dat.merged[dat.merged$sex==sex & dat.merged$age==age & dat.merged$year %in% fit.years,]

# filter Hawaii and Alaska if required and load correct drawseq lookup
if(contig == 0){
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
}
if(contig == 1){
    drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.contig.rds')
    dat.inla = subset(dat.inla,!(DRAWSEQ %in% c('1','51')))
}

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# make sure that the order of the main data file matches that of the shapefile,
# otherwise the model will not be valid
dat.inla <- dat.inla[order(dat.inla$DRAWSEQ,dat.inla$sex,dat.inla$age,dat.inla$year.month),]

# add ID column for INLA
dat.inla$ID <- dat.inla$DRAWSEQ

# fix rownames
rownames(dat.inla) <- 1:nrow(dat.inla)

# variables for INLA model
dat.inla$year.month4 <- dat.inla$year.month3 <- dat.inla$year.month2 <- dat.inla$year.month
dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
dat.inla$e <- 1:nrow(dat.inla)

# create directory for output
file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age)
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions_cod.R')

# input arguments into function to perform inference
if(fast.arg==0){
    inla.function.climate()
}
if(fast.arg==1){
    inla.function.climate.fast()
}
if(fast.arg==2){
    inla.function.climate.faster()
}

# prep data for output
if(cod.arg!='AllCause'){
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_parameters')
    #mod$misc <- NULL ; mod$.args$.parent.frame <- NULL
    if(fast.arg==1){parameters.name = paste0(parameters.name,'_fast')}
    if(fast.arg==2){parameters.name = paste0(parameters.name,'_faster')}
    if(contig == 1){parameters.name = paste0(parameters.name,'_contig')}
    saveRDS(mod,paste0(file.loc,'/',parameters.name))

    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause,'_summary')
    inla.summary.mod <- summary(mod)
    if(fast.arg==1){summary.name = paste0(summary.name,'_fast')}
    if(fast.arg==2){summary.name = paste0(summary.name,'_faster')}
    if(contig == 0){summary.name = paste0(summary.name,'.txt')}
    if(contig == 1){summary.name = paste0(summary.name,'_contig.txt')}
    capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))

    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_',cause)
    if(fast.arg==1){RDS.name = paste0(RDS.name,'_fast')}
    if(fast.arg==2){RDS.name = paste0(RDS.name,'_faster')}
    if(contig == 1){RDS.name = paste0(RDS.name,'_contig')}
    saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))
}

if(cod.arg =='AllCause'){
    # save all parameters of INLA model
    parameters.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_parameters')
    #mod$misc <- NULL ; mod$.args$.parent.frame <- NULL
    if(contig == 1){parameters.name = paste0(parameters.name,'_contig')}
    saveRDS(mod,paste0(file.loc,'/',parameters.name))

    # save summary of INLA model
    summary.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg,'_summary')
    inla.summary.mod <- summary(mod)
    if(contig == 0){summary.name = paste0(summary.name,'.txt')}
    if(contig == 1){summary.name = paste0(summary.name,'_contig.txt')}
    capture.output(inla.summary.mod,file=paste0(file.loc,'/',summary.name))

    # save RDS of INLA results
    plot.dat <- as.data.frame(cbind(dat.inla,rate.pred=mod$summary.fitted.values$mean,sd=mod$summary.fitted.values$sd))
    RDS.name <- paste0('USA_rate_pred_type',type.selected,'_',age,'_',sex.lookup[sex],'_',year.start,'_',year.end,'_',dname.arg,'_',metric.arg)
    if(contig == 1){RDS.name = paste0(RDS.name,'_contig')}
    saveRDS(plot.dat,paste0(file.loc,'/',RDS.name))
}

# subject for email
subject = paste0(sex.lookup[sex.sel],' ',age.sel,' model ',type.selected,' ',dname.arg,' ',metric.arg,' ',cause,' ',year.start.arg,'-',year.end.arg)
if(fast.arg==0){subject = paste0(subject,' non-pw done')}
if(fast.arg==1){subject = paste0(subject,' fast non-pw done')}
if(fast.arg==2){subject = paste0(subject,' faster non-pw done')}

# email notification
sender = "emailr349@gmail.com"
recipients = c("r.parks15@imperial.ac.uk")
send.mail(from = sender,
to = recipients,
subject = subject,
body = "Well done",
smtp = list(host.name = "smtp.gmail.com", port = 465,
user.name = "emailr349@gmail.com",
passwd = "inlaisthebest", ssl = TRUE),
authenticate = TRUE,
send = TRUE)