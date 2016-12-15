rm(list=ls())

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.arg <- as.numeric(args[3])
year.end.arg <- as.numeric(args[4])
pwl.arg <- as.numeric(args[5])
type.arg <- as.numeric(args[6])
forecast.length.arg <- as.numeric(args[7])
knot.year.arg <- as.numeric(args[8])
month.dist.arg <- as.numeric(args[9])
month.cyclic.arg <- as.numeric(args[10])

# temp
age=age.arg ; sex = sex.arg ; year.start = year.start.arg ; year.end = year.end.arg ; pwl = pwl.arg ; type = type.arg
forecast.length = forecast.length.arg ; knot.year = knot.year.arg; age.sel <- age.arg ; month.dist = month.dist.arg
month.cyclic = month.cyclic.arg

# types character for file strings
types <- c('1','1a','2','2a','3','3a','4','4a')
type.selected <- types[type.arg]
pwl.lookup <- c('nopwl','pwl')
dist.lookup <- c('rw1','iid')
cyclic.lookup <- c('cyclic','ncyclic')

# lookups
age.filter <- unique(dat.inla.load$age)
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
sex.lookup <- c('male','female')
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')

# choose test forecast years
years.fit <- year.start:(year.end-forecast.length)
years.forecast <- (year.end-forecast.length+1):(year.end)
years.total <- year.start:year.end

# create file location
file.loc <- paste0('data/mortality/US/national/forecast/type_',type.selected,'/age_groups/')
file.loc <- paste0('~/',file.loc)
file.loc <- paste0(file.loc,age.sel,'/')

# load file to score
RDS.name <- paste0('USAnat_',age,sex.lookup[sex],'_pred_type',type.selected,'_',pwl.lookup[pwl],'knot',knot.year,
    '_forecast',forecast.length,'_monthterms',dist.lookup[month.dist],cyclic.lookup[month.cyclic+1],'_',year.start,'_',year.end)

# load file
dat <- readRDS(paste0(file.loc,RDS.name))

# filter for forecast years
dat <- subset(dat, year %in% years.forecast)

# create summary information about model and model performance
dat.model.info <- data.frame(type=type.selected,pwl=pwl.lookup[pwl.arg],month.dist=dist.lookup[month.dist.arg],
				month.cyclic=cyclic.lookup[month.cyclic.arg],age=age.arg,sex=sex.arg,year.start.fit=year.start.arg,year.end.fit=max(years.fit),
				knot.year=max(years.fit)-knot.year.arg,forecast.length=forecast.length.arg)#,
dat.perf <- data.frame(bias.abs=mean(dat$bias.abs),deviation.abs=mean(dat$deviation.abs),bias.rel=mean(dat$bias.rel),deviation.rel=mean(dat$deviation.rel))

# output file location
file.loc.output <- '../../output/forecast_perf/'
file.loc.output <- paste0(file.loc.output,'national/forecast/type_',type.selected,'/age_groups/')
file.loc.output <- paste0(file.loc.output,age.arg,'/')
ifelse(!dir.exists(file.loc.output), dir.create(file.loc.output,recursive=TRUE), FALSE)

# output file
saveRDS(dat.model.info,paste0(file.loc.output,RDS.name,'_performance'))





