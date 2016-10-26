rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# load data
#filename <- paste0('../../output/prep_data/datus_state_rates_',year.start.arg,'_',year.end.arg)
filename <- paste0('../../output/prep_data/datus_state_rates_deaths_adj_',year.start.arg,'_',year.end.arg)
dat <- readRDS(filename)

# gender state and age lookup
gender.lookup <- c('male','female')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
age.lookup <- unique(dat$age)

# extract unique table of year and months to generate year.month
dat.year.month <- unique(dat[,c('year', 'month')])
dat.year.month$month <- as.integer(dat.year.month$month)
dat.year.month$year.month <- seq(nrow(dat.year.month))

# merge year.month table with population table to create year.month id
dat <- merge(dat,dat.year.month, by=c('year','month'))
####

library(ggplot2)

# graph data by age for a particular state and sex
plot.state <- function(state=1,sex=1) {
   	ggplot(dat[dat$fips==state & dat$sex==sex,],aes(x=year.month)) +
    	geom_line(aes(y=rate.adj),color='forestgreen') +
    	xlab(label='time') +
    	ylab(label='mortality rate') +
    	ggtitle(paste0(state.lookup[state.lookup$fips==state,][[1]],', ',gender.lookup[sex],': mortality rates by agegroup')) +
    	facet_wrap(~age, scale='free') +
    	scale_colour_brewer(palette = "Set3") +
    	theme_bw()
}

# create output directory
ifelse(!dir.exists("../../output/data_explore"), dir.create("../../output/data_explore"), FALSE)

# plot all states for males
pdf(paste0('../../output/data_explore/states_by_age_male_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state(i,1))
}
dev.off()

# plot all states for females
pdf(paste0('../../output/data_explore/states_by_age_female_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state(i,2))
}
dev.off()

# graph data by state for a particular age and sex
plot.age <- function(age=1,sex=1) {
	dat$fips <- as.factor(dat$fips)
	levels(dat$fips) <- state.lookup$full_name
   	ggplot(dat[dat$age==age.lookup[age] & dat$sex==sex,],aes(x=year.month)) +
    	geom_line(aes(y=rate.adj),color='forestgreen') +
    	xlab(label='time') +
    	ylab(label='mortality rate') +
    	ggtitle(paste0(age.lookup[age],', ',gender.lookup[sex],': mortality rates by agegroup')) +
    	facet_wrap(~fips) +
    	scale_colour_brewer(palette = "Set3") +
    	theme_bw()
}

# plot all ages for males
pdf(paste0('../../output/data_explore/age_by_state_male_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in seq(length(age.lookup))) {
	print(plot.age(i,1))
}
dev.off()

# plot all ages for females
pdf(paste0('../../output/data_explore/age_by_state_female_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',width=0,height=0)
for (i in seq(length(age.lookup))) {
	print(plot.age(i,2))
}
dev.off()



