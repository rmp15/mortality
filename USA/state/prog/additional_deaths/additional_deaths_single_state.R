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

# load state fips lookup code
fips.lookup <- read.csv('~/git/mortality/USA/state/data/fips_lookup/name_fips_lookup.csv')
fips.lookup = fips.lookup[!(fips.lookup$fips%in%c(2,15)),]
require(mailR)
state.name = as.character(subset(fips.lookup,fips==state.arg)[1,1])

# load parameters file
output.string = paste0(state.name,'_rate_pred_type',type.selected,'_',year.start.analysis.arg,'_',year.end.analysis.arg,'_',dname.arg,'_',metric.arg)
parameters.name <- paste0(output.string)
parameters.name = paste0(parameters.name,'_parameters')
file.loc <- paste0('~/data/mortality/US/state/climate_effects_single_state/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/all_ages/')

model.current = readRDS(paste0(file.loc,parameters.name))

# MAKE 1000 DRAWS
num.draws <- 1000

# load inla paradiso (what on earth is this?)
library(INLA)

# make draws from the model for the parameters
print(paste0('Making ',num.draws, ' draws...'))
draws.current = try(inla.posterior.sample(num.draws,model.current,selection=list('month5'=1:12)))

# create directories for output
file.loc <- paste0('../../output/additional_deaths_climate_single_state/',year.start.arg,'_',year.end.arg,
'/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/non_contig/all_cause/',num.draws,'_draws/')
if(contig==1){
    file.loc <- paste0('../../output/additional_deaths_climate_single_state/',year.start.arg,'_',year.end.arg,
'/',dname.arg,'/',metric.arg,'/non_pw/type_',type.selected,'/non_contig/all_cause/',num.draws,'_draws/')
}
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# CALCULATE ADDITIONAL DEATHS BASED ON POSITIVE ANOMALY
if(model%in%c('1d','1d2','1d9','1d10')){

    # load mortality file
    library(plyr)
    dat.national <- readRDS(paste0('../../output/prep_data_cod/datus_state_rates_cod_',year.start.arg,'_',year.end.arg))
    dat.national <- subset(dat.national,fips==state.arg)
    dat.national <- ddply(dat.national,.(year,month),summarise,deaths.adj=sum(deaths.adj),pop.adj=sum(pop.adj)/4) # /4 because four broad causes and need to adjust for that
    dat.national$rate.adj = with(dat.national,deaths.adj/pop.adj)

    # load climate data for 1980-2017
    file.loc <- paste0('~/git/climate/countries/USA/output/metrics_development_era5/',dname.arg,'/',metric.arg,'_',dname.arg,'/')
    dat.climate <- readRDS(paste0(file.loc,'state_weighted_summary_',metric.arg,'_',dname.arg,'_1980_2017.rds'))
    dat.climate$state.fips <- as.numeric(as.character(dat.climate$state.fips))
    dat.climate <- subset(dat.climate,state.fips==state.arg&sex==1&age==65)
    dat.climate$age = NULL ; dat.climate$sex = NULL ; dat.climate$state.fips = NULL

    # only look at positive anomalies
    dat.climate$t2m.meanc4 = ifelse(dat.climate$t2m.meanc4>0,dat.climate$t2m.meanc4,0)

    # with all the draws made for each age and sex, will now make an estimate for additional deaths
    additional.deaths = data.frame()
    additional.deaths.monthly = data.frame()
    for(k in seq(num.draws)){

            print(paste0('draw ',k))
            dat.merged.sub.all = data.frame()

            # empty data frame for parameters
            parameter.table = data.frame()

            # for each draw make a parameter summary to then calculate additional deaths
            climate.values = draws.current[[k]]$latent[grep('month5',rownames(draws.current[[k]]$latent))]
            climate.values = exp(climate.values)
            table = data.frame(ID=c(1:12),odds.mean=climate.values)
            parameter.table = rbind(parameter.table,table)

            # 1. ADDITIONAL DEATHS FROM ACTUAL HISTORICAL ANOMALY RECORD

            # merge odds and deaths files and reorder
            dat.merged <- merge(dat.national,parameter.table,by.x=c('month'),by.y=c('ID'),all.x=TRUE)
            dat.merged <- dat.merged[order(dat.merged$year,dat.merged$month),]
            dat.merged <- na.omit(dat.merged)

            # merge temperature records
            dat.merged <- merge(dat.merged,dat.climate,by=c('year','month'),all.x=TRUE)

            # calculate additional deaths for 2 unit change in climate parameter
            dat.merged$deaths.attributable <- with(dat.merged,t2m.meanc4*(odds.mean-1)*deaths.adj)

            dat.merged.sub.all=dat.merged

            # integrate across year by month for entire population
            dat.merged.sub.year.monthly = ddply(dat.merged.sub.all,.(month),summarise,deaths.attributable=sum(deaths.attributable))
            # dat.total.sex.monthly = ddply(dat.merged.sub.year.monthly,.(cause,sex),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.sex.monthly$month = 99
            # dat.total.monthly = ddply(dat.merged.sub.year.monthly,.(cause),summarise,deaths.added=sum(deaths.added),deaths.added.two.deg=sum(deaths.added.two.deg)) ; dat.total.monthly$month = 99 ; dat.total$sex = 0

            # dat.merged.sub.year.monthly = rbind(dat.merged.sub.year.monthly,dat.total.monthly)
            dat.merged.sub.year.monthly$draw = k

            additional.deaths.monthly = rbind(additional.deaths.monthly,dat.merged.sub.year.monthly)

    }

    # summarise by month
    additional.deaths.monthly.summary = ddply(additional.deaths.monthly,.(month),summarise,
    deaths.attributable.median=median(deaths.attributable),deaths.attributable.mean=mean(deaths.attributable),deaths.attributable.ll=quantile(deaths.attributable,0.025),deaths.attributable.ul=quantile(deaths.attributable,0.975))

    # summarise by summer period
    additional.deaths.monthly$summer = ifelse(additional.deaths.monthly$month%in%c(5:9),1,0)
    additional.deaths.monthly.summer = subset(additional.deaths.monthly,summer==1)
    additional.deaths.monthly.summer.summary = ddply(additional.deaths.monthly.summer,.(draw),summarise,deaths.attributable=sum(deaths.attributable))

    additional.deaths.summer.summary = ddply(additional.deaths.monthly.summer.summary,.(),summarise,
    deaths.attributable.median=median(deaths.attributable),deaths.attributable.mean=mean(deaths.attributable),deaths.attributable.ll=quantile(deaths.attributable,0.025),deaths.attributable.ul=quantile(deaths.attributable,0.975))

    saveRDS(additional.deaths.monthly.summary,paste0(file.loc,'additional_deaths_summary_monthly_draws.rds'))
    saveRDS(additional.deaths.summer.summary,paste0(file.loc,'additional_deaths_summary_summer_draws.rds'))

    write.csv(additional.deaths.monthly.summary,paste0(file.loc,'additional_deaths_summary_monthly_draws.csv'))
    write.csv(additional.deaths.summer.summary,paste0(file.loc,'additional_deaths_summary_summer_draws.csv'))
