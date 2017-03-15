rm(list=ls())

library(RColorBrewer)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age.arg <- as.numeric(args[1])
sex.arg <- as.numeric(args[2])
year.start.mort.arg <- as.numeric(args[3])
year.end.mort.arg <- as.numeric(args[4])
year.start.clim.arg <- as.numeric(args[5])
year.end.clim.arg <- as.numeric(args[6])
dname.arg <- as.character(args[7])
metric.arg <- as.character(args[8])

# range of years
years <- (max(year.start.mort.arg,year.start.clim.arg):min(year.end.mort.arg,year.end.clim.arg))
year.start.arg <- min(years)
year.end.arg <- max(years)

# add sourced data
source('../../data/objects/objects.R')

# create files for output
file.loc <- '../../output/mort_against_climate/'
file.loc <- paste0(file.loc,dname.arg)
file.loc <- paste0(file.loc,'/',metric.arg)
file.loc <- paste0(file.loc,'/',year.start.arg,'_',year.end.arg)
file.loc <- paste0(file.loc,'/',sex.lookup[sex.arg])
file.loc <- paste0(file.loc,'/',age.arg)
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# load USA mortality data and filter for relevant years
dat <- readRDS(paste0('../../output/prep_data/datjp_pref_rate_',year.start.mort.arg,'_',year.end.mort.arg))
dat <- subset(dat, year %in% years & sex == sex.arg & age== age.arg)

# load climate data and filter for relevant years
dat.climate <- readRDS(paste0('~/git/climate/countries/Japan/output/metrics_development/',dname.arg,'/',metric.arg,'_',dname.arg,'/state_weighted_summary_',metric.arg,'_',dname.arg,'_',year.start.clim.arg,'_',year.end.clim.arg,'.rds'))
dat.climate <- subset(dat.climate, year %in% years)
dat.climate$state_id <- as.numeric(dat.climate$state_id)

# load prefecture lookup and merge with climate data
dat.pref <- read.csv('../../data/pref/pref_lookup.csv')
dat.climate <- merge(dat.climate,dat.pref,by='state_id')

# merge mortality and climate data and reorder
dat.merged <- merge(dat,dat.climate,by.x=c('year','month','pref_id'),by.y=c('year','month','pref_id'),all.x=TRUE)
dat.merged <- dat.merged[order(dat.merged$pref_id,dat.merged$sex,dat.merged$age,dat.merged$year,dat.merged$month),]

# generalise variable name
names(dat.merged)[grep(dname.arg,names(dat.merged))] <- 'variable'

# rename rows and remove unnecessary columns
dat.merged$X <- NULL
rownames(dat.merged) <- 1:nrow(dat.merged)

library(ggplot2)
library(plyr)

# variables for pretty and easy to read plots
dat.merged$month.short <- mapvalues(dat.merged$month,from=unique(dat.merged$month),to=month.short)
dat.merged$month.short <- reorder(dat.merged$month.short,dat.merged$month)

# output file
saveRDS(dat.merged,paste0(file.loc,'/mort_against_climate_',age.arg,'_',sex.arg,'_',year.start.mort.arg,'_',year.end.mort.arg,'_',dname.arg,'_',metric.arg))
write.csv(dat.merged,paste0(file.loc,'/mort_against_climate_',age.arg,'_',sex.arg,'_',year.start.mort.arg,'_',year.end.mort.arg,'_',dname.arg,'_',metric.arg,'.csv'))

#############################################
# 1. PLOT ALL TOGETHER
#############################################

pdf(paste0(file.loc,'/','deaths_rates_together_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000,color=factor(month))) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
ggtitle(paste0('Death rates by state fitted by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw()
dev.off()

#############################################
# 2. FACET BY MONTH
#############################################

pdf(paste0(file.loc,'/','deaths_rates_month_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000,color=factor(month))) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
facet_wrap(~month.short) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw()
dev.off()

#############################################
# 3. FACET BY STATE
#############################################

pdf(paste0(file.loc,'/','deaths_rates_state_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
ggplot(data=dat.merged) + 
geom_point(aes(x=variable,y=rate.adj*100000,color=factor(month))) +
stat_smooth(aes(x=variable,y=rate.adj*100000)) +
ggtitle(paste0('Death rates by state ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
facet_wrap(~pref) +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
theme_bw()
dev.off()

#############################################
# 4. PLOT ALL TOGETHER WITH MONTHS FIT
#############################################

#pdf(paste0(file.loc,'/','deaths_rates_together_month_fit_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
#ggplot(data=dat.merged) + 
#geom_point(aes(x=variable,y=rate.adj*100000,alpha=0.2)) +
#stat_smooth(span=0.8, aes(x=variable,y=rate.adj*100000,color=factor(month.short))) +
#ggtitle(paste0('Death rates by state fitted by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
#xlab(paste0(dname.arg,' ',metric.arg)) +
#ylab('death rate (per 100,000)') +
#theme_bw()
#dev.off()

#############################################
# 5a. PLOT BY STATE WITH MONTHS FIT (LM)
#############################################

pdf(paste0(file.loc,'/','deaths_rates_states_month_fit_lm_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.merged$state_id)) {
print(
ggplot(data=subset(dat.merged,state_id==i)) +
geom_point(aes(x=variable,y=rate.adj*100000,alpha=0.2,color=factor(month))) +
stat_smooth(se=FALSE,method='lm',span=0.8, aes(x=variable,y=rate.adj*100000,color=factor(month))) +
ggtitle(paste0('Death rates by state fitted by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
xlab(paste0(dname.arg,' ',metric.arg)) +
ylab('death rate (per 100,000)') +
scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
facet_wrap(~pref) +
theme_bw()
)
}
dev.off()

#############################################
# 5b. PLOT BY STATE WITH MONTHS FIT (LOESS)
#############################################

#pdf(paste0(file.loc,'/','deaths_rates_states_month_fit_loess_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
#for(i in unique(dat.merged$state_id)) {
#print(
#ggplot(data=subset(dat.merged,state_id==i)) +
#geom_point(aes(x=variable,y=rate.adj*100000,alpha=0.2,color=factor(month))) +
#stat_smooth(se=FALSE,span=0.8, aes(x=variable,y=rate.adj*100000,color=factor(month))) +
#ggtitle(paste0('Death rates by state fitted by month ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
#xlab(paste0(dname.arg,' ',metric.arg)) +
#ylab('death rate (per 100,000)') +
#scale_colour_manual(values=colorRampPalette(rev(brewer.pal(12,"RdYlBu")[c(9:10,2:1,1:2,10:9)]))(12),guide = guide_legend(title = 'month'),labels=month.short) +
#facet_wrap(~pref) +
#theme_bw()
#)
#}
#dev.off()

#############################################
# 5. PLOT BY MONTH WITH STATES SEPARATE FIT
#############################################

#pdf(paste0(file.loc,'/','deaths_rates_month_states_fit_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
#ggplot(data=dat.merged) + 
#geom_point(aes(x=variable,y=rate.adj*100000,alpha=0.2)) +
#stat_smooth(se=FALSE,span=0.8,method='lm', aes(x=variable,y=rate.adj*100000,color=factor#(state.name))) +
#ggtitle(paste0('Death rates by month fitted by state ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
#xlab(paste0(dname.arg,' ',metric.arg)) +
#ylab('death rate (per 100,000)') +
#facet_wrap(~month.short) +
#theme_bw()
#dev.off()

#############################################
# 5. PLOT ALL TOGETHER BY MONTH 
#############################################

#pdf(paste0(file.loc,'/','deaths_rates_month_fit_separate_',year.start.arg,'_',year.end.arg,'_',dname.arg,'_',metric.arg,'_',sex.lookup[sex.arg],'_',age.arg,'.pdf'),paper='a4r',height=0,width=0)
#for(i in c(1:12)) {
#print(ggplot(data=subset(dat.merged,month==i)) + 
#geom_point(aes(x=variable,y=rate.adj*100000,alpha=0.2)) +
#stat_smooth(span=0.9, aes(x=variable,y=rate.adj*100000,color=factor(month))) +
#ggtitle(paste0('Death rates nationally ',month.short[i],' ',year.start.arg,'-',year.end.arg,' against ',dname.arg,' ',metric.arg,' : ',sex.lookup[sex.arg],' ',age.arg)) +
#xlab(paste0(dname.arg,' ',metric.arg)) +
#ylab('death rate (per 100,000)') +
#theme_bw())
#}
#dev.off()


