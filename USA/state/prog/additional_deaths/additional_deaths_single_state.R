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

    # CURRENTLY HERE!

    # summarise by
    additional.deaths.intent.monthly.summary = ddply(additional.deaths.intent.monthly,.(sex,month,intent),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    # saveRDS(additional.deaths.intent.monthly.summary,paste0(file.loc,'additional_deaths_intent_summary_monthly_draws.rds'))

    # processing for plotting (meant to match the original method of bind_posterior...)
    additional.deaths.summary = ddply(additional.deaths,.(sex,age,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary.monthly = ddply(additional.deaths.monthly,.(sex,month,cause),summarise,
        deaths.added.median=median(deaths.added),deaths.added.mean=mean(deaths.added),deaths.added.ll=quantile(deaths.added,0.025),deaths.added.ul=quantile(deaths.added,0.975),
        deaths.added.two.deg.median=median(deaths.added.two.deg),deaths.added.two.deg.mean=mean(deaths.added.two.deg),deaths.added.two.deg.ll=quantile(deaths.added.two.deg,0.025),deaths.added.two.deg.ul=quantile(deaths.added.two.deg,0.975)
    )

    additional.deaths.summary$age.long <- mapvalues(additional.deaths.summary$age,from=sort(unique(additional.deaths.summary$age)),to=c(as.character(age.code[,2]),'All ages'))
    additional.deaths.summary$age.long <- reorder(additional.deaths.summary$age.long,additional.deaths.summary$age)

    additional.deaths.summary$sex.long <- mapvalues(additional.deaths.summary$sex,from=sort(unique(additional.deaths.summary$sex)),to=c('Both','Male','Female'))
    additional.deaths.summary$sex.long <- reorder(additional.deaths.summary$sex.long,additional.deaths.summary$sex)

    additional.deaths.intent.summary$age.long <- mapvalues(additional.deaths.intent.summary$age,from=sort(unique(additional.deaths.intent.summary$age)),to=c(as.character(age.code[,2])))
    additional.deaths.intent.summary$age.long <- reorder(additional.deaths.intent.summary$age.long,additional.deaths.intent.summary$age)

    additional.deaths.intent.summary$sex.long <- mapvalues(additional.deaths.intent.summary$sex,from=sort(unique(additional.deaths.intent.summary$sex)),to=c('Male','Female'))
    additional.deaths.intent.summary$sex.long <- reorder(additional.deaths.intent.summary$sex.long,additional.deaths.intent.summary$sex)

    # FOR PLOT BY AGE AND SEX

    # FIX NAMES OF CAUSES

    fix_cause_names = function(dat){
    dat$cause <- gsub('Transport accidents', '1. Transport', dat$cause)
    dat$cause <- gsub('Accidental falls', '2. Falls', dat$cause)
    dat$cause <- gsub('Other external causes of injury', '4. Other injuries', dat$cause)
    dat$cause <- gsub('Accidental drowning and submersion', '3. Drownings', dat$cause)
    dat$cause <- gsub('Intentional self-harm', '6. Intentional self-harm', dat$cause)
    dat$cause <- gsub('6. Intentional self-harm', '6. Intentional\nself-harm', dat$cause)
    dat$cause <- gsub('Assault', '5. Assault', dat$cause)

    return(dat)
    }

    fix_intent_names = function(dat){
    dat$intent <- gsub('Intentional', '2. Intentional', dat$intent)
    dat$intent <- gsub('Unintentional', '1. Unintentional', dat$intent)

    return(dat)
    }

    additional.deaths.summary = fix_cause_names(additional.deaths.summary)
    #1
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_unintentional_to_transport_falls_drownings_other_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&!(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary,intent=='Unintentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='Unintentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of unintentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()
    # #2
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_to_assault_intentional_self-harm_fast_contig.pdf'),paper='a4r',height=0,width=0)
    # ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99&(cause%in%c('5. Assault','6. Intentional\nself-harm'))), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary,intent=='Intentional'),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary,intent=='Intentional'),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_wrap(~sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of intentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    # dev.off()

    additional.deaths.summary$intent = ifelse(additional.deaths.summary$cause%in%c('5. Assault','6. Intentional\nself-harm'),'2. Intentional','1. Unintentional')
    additional.deaths.intent.summary = fix_intent_names(additional.deaths.intent.summary)
    #3
    # pdf(paste0(file.loc,country,'_rate_pred_type',model,
    #     '_',year.start,'_',year.end,'_',dname,'_',metric,'_intentional_unintentional_contig.pdf'),paper='a4r',height=0,width=0)
    # p1 = ggplot() +
    #     geom_bar(data=subset(additional.deaths.summary,sex>0&age<99), aes(x=as.factor(age.long),y=deaths.added.mean,fill=cause), stat='identity') +
    #     geom_point(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),y=deaths.added.mean),shape=16) +
    #     geom_errorbar(data=subset(additional.deaths.intent.summary),aes(x=as.factor(age.long),ymax=deaths.added.ul,ymin=deaths.added.ll),width=.3,size=0.5) +
    #     geom_hline(yintercept=0,linetype='dotted') +
    #     xlab('Age group (years)') + ylab('Additional deaths associated with 1 degree \n additional warming (based on 2016 population)') +
    #     # ylim(c(min.plot,max.plot)) +
    #     facet_grid(. ~intent + sex.long) +
    #     scale_fill_manual(values=colors.subinjuries[c(1,2,3,4,5,6)]) +
    #     # scale_y_continuous(breaks = seq(min.plot, max.plot, by = 50),limits=c(min.plot,max.plot)) +
    #     guides(fill=guide_legend(title="Subcategory of intentional injury",nrow=1)) +
    #     # ggtitle('Additional deaths by types of intentional injuries') +
    #     theme_bw() + theme(text = element_text(size = 15),
    #     panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    #     plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    #     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #     panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    #     legend.position = 'bottom',legend.justification='center',
    #     legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #
    # print(p1)
    #
    # dev.off()

    additional.deaths.summary.monthly$month.short <- mapvalues(additional.deaths.summary.monthly$month,from=sort(unique(additional.deaths.summary.monthly$month)),to=c(as.character(month.short)))
    additional.deaths.summary.monthly$month.short <- reorder(additional.deaths.summary.monthly$month.short,additional.deaths.summary.monthly$month)

    additional.deaths.intent.monthly.summary$month.short <- mapvalues(additional.deaths.intent.monthly.summary$month,from=sort(unique(additional.deaths.intent.monthly.summary$month)),to=c(as.character(month.short)))
    additional.deaths.intent.monthly.summary$month.short <- reorder(additional.deaths.intent.monthly.summary$month.short,additional.deaths.intent.monthly.summary$month)

    additional.deaths.summary.monthly$sex.long <- mapvalues(additional.deaths.summary.monthly$sex,from=sort(unique(additional.deaths.summary.monthly$sex)),to=c('Male','Female'))
    additional.deaths.summary.monthly$sex.long <- reorder(additional.deaths.summary.monthly$sex.long,additional.deaths.summary.monthly$sex)

    additional.deaths.intent.monthly.summary$sex.long <- mapvalues(additional.deaths.intent.monthly.summary$sex,from=sort(unique(additional.deaths.intent.monthly.summary$sex)),to=c('Male','Female'))
    additional.deaths.intent.monthly.summary$sex.long <- reorder(additional.deaths.intent.monthly.summary$sex.long,additional.deaths.intent.monthly.summary$sex)