rm(list=ls())

# load data
filename <- 'datus_state_rates_1982_2010'
dat <- readRDS(filename)

# gender lookup
gender.lookup <- c('male','female')

# state lookup
state.lookup <- read.csv('name_fips_lookup.csv')

# age lookup
age.lookup <- unique(dat$age)

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

# plot all states for males
pdf('states_by_age_male.pdf',paper='a4r',width=0,height=0)
for (i in state.lookup$fips) {
	print(plot.state(i,1))
}
dev.off()

# plot all states for females
pdf('states_by_age_female.pdf',paper='a4r',width=0,height=0)
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
pdf('age_by_state_male.pdf',paper='a4r',width=0,height=0)
for (i in seq(length(age.lookup))) {
	print(plot.age(i,1))
}
dev.off()

# plot all ages for females
pdf('age_by_state_female.pdf',paper='a4r',width=0,height=0)
for (i in seq(length(age.lookup))) {
	print(plot.age(i,2))
}
dev.off()



