rm(list=ls())

# set up argument (don't get why but it should be like this apparently)
seedVal <-as.numeric(commandArgs()[4])

seedVal = 1

# create complete grid of age, sex, and cause of death values
sexes = c(0, 1, 2)
ages = c(25, 40, 50, 65)
causes = c('Suicide')

seed.grid = expand.grid(sex=sexes,age=ages,cause=causes)

chosen.row =seed.grid[seedVal,]

# break down the arguments from Rscript
sex.arg <- as.numeric(chosen.row[1,1])
age.arg <- as.numeric(chosen.row[1,2])
year.start.arg <- 1999
year.end.arg <- 2018
type.arg <- 29 #CURRENTLY THE MODEL WITH TEMPERATURE (LONG-TERM) INCLUDED
cluster.arg <- 0
dname.arg <- 't2m'
metric.arg <- 'meanc4'
year.start.analysis.arg <- 1999
year.end.analysis.arg <- 2018
cod.arg <- as.character(chosen.row[1,3])
fast.arg <- 1
contig.arg <- 1
pw.arg <- 0

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9','1d10','1d11')
type.selected <- types[type.arg]

print(paste(year.start.analysis.arg,year.end.analysis.arg,age.arg,sex.arg,type.selected,cod.arg))

# range of years
years <- year.start.arg:year.end.arg

# require(mailR)

# create file location for output
if(pw.arg==0){
    ifelse(!dir.exists(paste0('~/data/mortality/Spain/province/unemployment_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/Spain/province/unemployment_effects/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}
if(pw.arg==1){
    ifelse(!dir.exists(paste0('~/data/mortality/Spain/province/unemployment_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups')), dir.create(paste0('~/data/mortality/Spain/province/unemployment_effects/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups'),recursive=TRUE), FALSE)
}
# load data and filter results
source('../models/INLA/inla_load_data_cod.R')

library(dplyr)

# lookups
#source('../../data/objects/objects.R')

# adjacency matrix with connections
# only contiguous Spain FINISHING FROM adj_matrix.....R script first
if(contig.arg == 1){Spain.adj <- "../../output/adj_matrix_create/spain.graph.contig"} # FIX FOR REST OF CODE

# UP TO HERE SO FAR!

##############

# filter all data by sex age and month
fit.years <- year.start.analysis.arg:year.end.analysis.arg
dat.inla <- dat.merged[dat.merged$sex==sex.arg & dat.merged$age==age.arg & dat.merged$year %in% fit.years,]

# filter Hawaii and Alaska if required and load correct drawseq lookup NEED TO FIX HERE AND WITH AGE SPLIT!!
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
dat.inla$month8 <- dat.inla$month7 <- dat.inla$month6 <- dat.inla$month5 <- dat.inla$month4 <- dat.inla$month3 <- dat.inla$month2 <- dat.inla$month
dat.inla$ID3 <- dat.inla$ID2 <- dat.inla$ID
dat.inla$e <- 1:nrow(dat.inla)

# create piecewise climate variable if required
if(pw.arg==1){
    dat.inla$variable2 = ifelse(dat.inla$variable<0,0,dat.inla$variable)
    dat.inla$variable3 = ifelse(dat.inla$variable>0,0,dat.inla$variable)
}

# create directory for output
if(pw.arg==0){
    file.loc <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/age_groups/',age.arg)
}
if(pw.arg==1){
    file.loc <- paste0('/rds/general/user/rmp15/ephemeral/data/mortality/US/state/climate_effects_era5/',dname.arg,'/',metric.arg,'/pw/type_',type.selected,'/age_groups/',age.arg)
}
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

library(INLA)

# load inla function
source('../models/INLA/03_spatiotemporal/inla_functions_cod.R')

# temporary workaround to avoid GLIBC error (???) from:
# https://www.mn.uio.no/math/english/services/it/help/status/2018-07-26-inla-r.html
# INLA:::inla.dynload.workaround()

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
if(fast.arg==0){subject.arg = paste0(subject.arg,' ')}
if(fast.arg==1){subject.arg = paste0(subject.arg,' fast')}
if(fast.arg==2){subject.arg = paste0(subject.arg,' faster')}
if(pw.arg==0){subject.arg = paste0(subject.arg,' non-pw done')}
if(pw.arg==1){subject.arg = paste0(subject.arg,' pw done')}

print(subject.arg)

#
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