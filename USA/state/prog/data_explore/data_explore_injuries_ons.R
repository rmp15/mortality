rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# load data
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
source('../../data/objects/objects.R')

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat <- merge(dat,dat.year.month, by=c('year','month'))
####

library(plyr)

# create nationalised data
dat.national <- ddply(dat,.(cause,year,month,sex,age),summarize,deaths=sum(deaths),pop.adj=sum(pop.adj))
dat.national$rate.adj <- with(dat.national,deaths/pop.adj)
dat.national <- dat.national[order(dat.national$cause,dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]

library(ggplot2)

# for nationalised data

# 1.       x axis age-group, y-axis injury death rate for 2013

# subset of last year's data
dat.last.year = subset(dat.national,year==year.end.arg)

dat.last.year$sex.long <- mapvalues(dat.last.year$sex,from=sort(unique(dat.last.year$sex)),to=c('Male','Female'))
dat.last.year$sex.long <- with(dat.last.year,reorder(dat.last.year$sex.long,sex))

ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(age),y=1000000*rate.adj,color=as.factor(month))) +
    xlab('Age group') +
    ylab('Mortality rate per million') +
    scale_x_discrete(breaks=age.filter,labels=age.print) +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

# 2.       x axis month, y-axis injury death rate for 2013 perhaps average last 5 years
ggplot(data=dat.last.year) +
    geom_point(aes(x=as.factor(month),y=1000000*rate.adj,color=age)) +
    xlab('Age group') +
    ylab('Mortality rate per million') +
    scale_x_discrete(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    ggtitle(year.end.arg) +
    facet_grid(sex.long~cause) +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90),
plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
strip.background = element_blank(), axis.line = element_line(colour = "black"),
legend.position = 'bottom',legend.justification='center',
legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))


# 3.       time trends for injuries across time, age-standarised (annual time) stacked plot

# graph data by age for a particular state cod and sex for rate
plot.state <- function(state=1,sex=1) {
    ggplot(dat=subset(dat,fips==state&sex==sex))+
    	geom_line(aes(x=year.month,y=1000000*rate.adj,color=cause)) +
    	xlab(label='Time') +
    	ylab(label='Mortality rate per million') +
    	ggtitle(paste0(state.lookup[state.lookup$fips==state,][[1]],', ',sex.filter2[sex],' : mortality rates by age group for injuries')) +
		facet_wrap(~age,scale='free') +
    	scale_colour_brewer(palette = "Set1") +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
}

# create output directory
ifelse(!dir.exists("../../output/data_explore_cod"), dir.create("../../output/data_explore_cod"), FALSE)

# plot all states for males
pdf(paste0('../../output/data_explore_cod/states_by_age_male_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state(i,1))
}
dev.off()

# plot all states for females
pdf(paste0('../../output/data_explore_cod/states_by_age_female_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state(i,2))
}
dev.off()

# graph data by age for a particular state cod and sex for absolute numbers
plot.state.abs <- function(state=1,sex=1) {
    ggplot(dat=subset(dat,fips==state&sex==sex))+
    	geom_line(aes(x=year.month,y=deaths,color=cause)) +
    	xlab(label='Time') +
    	ylab(label='Deaths') +
    	ggtitle(paste0(state.lookup[state.lookup$fips==state,][[1]],', ',sex.filter2[sex],' : deaths by age group for injuries')) +
		facet_wrap(~age,scale='free') +
    	scale_colour_brewer(palette = "Set1") +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
}

# create output directory
ifelse(!dir.exists("../../output/data_explore_cod"), dir.create("../../output/data_explore_cod"), FALSE)

# plot all states for males
pdf(paste0('../../output/data_explore_cod/states_by_age_male_abs_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state.abs(i,1))
}
dev.off()

# plot all states for females
pdf(paste0('../../output/data_explore_cod/states_by_age_female_abs_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state.abs(i,2))
}
dev.off()

# graph data by state for a particular age and sex
plot.age <- function(age=1,sex=1) {
	dat$fips <- as.factor(dat$fips)
	levels(dat$fips) <- state.lookup$full_name
   	ggplot(dat[dat$age==age.filter[age] & dat$sex==sex,],aes(x=year.month)) +
    	geom_line(aes(y=rate.adj*1000000,color=cause),) +
    	xlab(label='Time') +
    	ylab(label='Mortality rate per million') +
    	ggtitle(paste0(age.filter[age],', ',sex.filter2[sex],': mortality rates by agegroup')) +
    	facet_wrap(~fips) +
    	scale_colour_brewer(palette = "Set1") +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
}

# plot all ages for males
pdf(paste0('../../output/data_explore_cod/age_by_state_male_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in seq(length(age.filter))) {
	print(plot.age(i,1))
}
dev.off()

# plot all ages for females
pdf(paste0('../../output/data_explore_cod/age_by_state_female_injuries_ons_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in seq(length(age.filter))) {
	print(plot.age(i,2))
}
dev.off()



