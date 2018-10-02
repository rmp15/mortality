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
contig.arg <- as.numeric(args[13])
pw.arg <- as.numeric(args[14])

# for test runs
# age.arg = 65 ; sex.arg = 1 ; year.start.arg = 1980 ; year.end.arg = 2016 ; type.arg = 10 ;
# cluster.arg = 0 ; dname.arg = 't2m' ; metric.arg = 'meanc3' ; year.start.analysis.arg = 1980 ;
# year.end.analysis.arg = 2016 ; cod.arg = 'External'; fast.arg = 1 ; contig.arg = 1
# pw.arg=1

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5')
type.selected <- types[type.arg]

print(paste(year.start.analysis.arg,year.end.analysis.arg,age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

require(mailR)

# create file location for output
if(pw.arg==0){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}
if(pw.arg==1){
    ifelse(!dir.exists(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/US/state/climate_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}
# load data and filter results
source('../models/INLA/03_spatiotemporal/inla_load_data_cod.R')

# load climate region data and fix names
# source('../models/INLA/03_spatiotemporal/inla_climate_regions.R')

# merge mortality data with climate region data
# dat.inla.load <- merge(dat.inla.load,dat.region,by.x=('fips'),by.y=('STATE_FIPS'),all.x=TRUE)

# load climate data for 1979-2016
file.loc <- paste0('~/git/climate/countries/USA/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/')
dat.climate <- readRDS(paste0(file.loc,'state_weighted_summary_',metric.arg,'_',dname.arg,'_1979_2016.rds'))
dat.climate$state.fips <- as.numeric(as.character(dat.climate$state.fips))

# merge mortality and climate data and reorder
dat.merged <- merge(dat.inla.load,dat.climate,by.x=c('sex','age','year','month','fips'),by.y=c('sex','age','year','month','state.fips'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$fips,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# generalise climate variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# create lookup table for climate regions
# regions.lookup <- data.frame(climate_region=sort(unique(dat.merged$climate_region)))
# regions.lookup$ID.clim <- seq(nrow(regions.lookup))

# dat.merged <- merge(dat.merged,regions.lookup,by='climate_region')

library(dplyr)

# lookups
source('../../data/objects/objects.R')

# adjacency matrix with connections
# Hawaii -> California, Alaska -> Washington
if(contig.arg == 0){USA.adj <- "../../output/adj_matrix_create/USA.graph.edit"}
# only contiguous USA
if(contig.arg == 1){USA.adj <- "../../output/adj_matrix_create/USA.graph.contig"}

##############

# filter all data by sex age and month
fit.years <- year.start.analysis.arg:year.end.analysis.arg
dat.inla <- dat.merged[dat.merged$sex==sex.arg & dat.merged$age==age.arg & dat.merged$year %in% fit.years,]

# filter Hawaii and Alaska if required and load correct drawseq lookup
if(contig.arg == 0){drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')}
if(contig.arg == 1){drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.contig.rds')}
dat.inla = merge(dat.inla,drawseq.lookup,by='fips')

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat.inla[,c('year', 'month')])
dat.year.month <- dat.year.month[order(dat.year.month$year,dat.year.month$month),]
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat.inla <- merge(dat.inla,dat.year.month, by=c('year','month'))

# make sure that the order of the main data file matches that of the shapefile otherwise the model will not be valid
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

# create piecewise climate variable if required
if(pw.arg==1){
    dat.inla$variable2 = ifelse(dat.inla$variable<0,0,dat.inla$variable)
    dat.inla$variable3 = ifelse(dat.inla$variable>0,0,dat.inla$variable)
    dat.inla$month6 <- dat.inla$month5
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
source('../models/INLA/03_spatiotemporal/inla_functions_cod.R')

# input arguments into function to perform inference
if(fast.arg==0){
    mod = inla.function.climate()
}
if(fast.arg==1){
    mod = inla.function.climate.fast()
}
if(fast.arg==2){
    mod = inla.function.climate.faster()
}

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
if(fast.arg==0){subject.arg = paste0(subject.arg,' non-pw done')}
if(fast.arg==1){subject.arg = paste0(subject.arg,' fast non-pw done')}
if(fast.arg==2){subject.arg = paste0(subject.arg,' faster non-pw done')}

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