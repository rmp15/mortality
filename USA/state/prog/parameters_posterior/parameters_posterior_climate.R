rm(list=ls())

library(INLA)
library(ggplot2)
library(plyr)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down the arguments from Rscript
age <- as.numeric(args[1])
sex <- as.character(args[2])
year.start <- as.numeric(args[3])
year.end <- as.numeric(args[4])
model <- as.numeric(args[5])
dname <- as.character(args[6])
metric <- as.character(args[7])

# selected data attributes
#age <- 85
#sex <- 'male'
#year.start <- 1982
#year.end <- 2013
#model <- '1a'

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f')
model <- models[model]

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
age.print=age.print)
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
sex.lookup <- c('Men','Women')
sex.lookup2 <- c('male','female')

# load INLA parameters file
file.loc <- paste0('~/data/mortality/US/state/climate_effects/',dname,'/',metric,'/non_pw/type_',model,'/age_groups/',age,'/USA_rate_pred_type',model,'_',age,'_male_',year.start,'_',year.end,'_',dname,'_',metric,'_parameters')
dat <- readRDS(file.loc)

# lists of marginals
marginals.fixed 	<- 	names(dat$marginals.fixed)
marginals.random 	<- 	names(dat$marginals.random)

# create directories for output
file.loc <- paste0('../../output/parameters_posterior/',year.start,'_',year.end,'/',dname,'/',metric,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
ifelse(!dir.exists(paste0(file.loc,'statemonth_intercepts')), dir.create(paste0(file.loc,'statemonth_intercepts'), recursive=TRUE), FALSE)
ifelse(!dir.exists(paste0(file.loc,'month_intercepts')), dir.create(paste0(file.loc,'month_intercepts'), recursive=TRUE), FALSE)
ifelse(!dir.exists(paste0(file.loc,'statemonth_heatmaps')), dir.create(paste0(file.loc,'statemonth_heatmaps'), recursive=TRUE), FALSE)

# lookup for state codes from INLA
drawseq.lookup <-readRDS('~/git/mortality/USA/state/output/adj_matrix_create/drawseq.lookup.rds')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

#######################################
# 1. Plotting intercepts
#######################################

# plot month intercept
#plot(inla.tmarginal(function(x) 1/x, dat$marginals.random$month$index.1))

#######################################
# 2. Plotting slopes
#######################################

# plot month slope
#plot(inla.tmarginal(function(x) 1/x, dat$marginals.random$month2$index.1))

#######################################
# 3. Plotting random terms
#######################################

if(model=='1d'){

# state-month intercept
month5 <- dat$summary.random$month5

pdf(paste0(file.loc,'month_intercepts/climate_statemonth_params_',age,'_',sex,'_',model,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)

print(ggplot(data=month5) +
geom_line(aes(x=ID,y=mean)) +
geom_ribbon(aes(x=ID,ymax=`0.975quant`,ymin=`0.025quant`),alpha=0.1,fill='red') + 
geom_hline(yintercept=0,alpha=0.5,linetype=2) +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
xlab('month') +
ylab('month intercept') +
ggtitle(paste0(age,' ',sex,'month intercept')) +
scale_colour_discrete(name="State") +
guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
theme(legend.position="bottom"))

dev.off()

}

if(model %in% c('1e','1f')){

# state-month intercept
month5 <- dat$summary.random$month5
month5$state <- rep(1:51,each=12)

# attach fips and then state name information
month5 <- merge(month5,drawseq.lookup,by.x='state',by.y='DRAWSEQ')
month5 <- merge(month5, state.lookup[,c('full_name','fips')],by='fips')

pdf(paste0(file.loc,'statemonth_intercepts/climate_statemonth_params_',age,'_',sex,'_',model,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)

print(ggplot(data=month5) +
geom_line(aes(x=ID,y=mean,color=as.factor(full_name))) +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
xlab('month') +
ylab('state month intercept') +
ggtitle(paste0(age,' ',sex,' state-month intercept')) +
scale_colour_discrete(name="State") +
guides(col = guide_legend(ncol = 10, byrow=TRUE)) +
theme(legend.position="bottom"))

dev.off()

}

#######################################
# 4. Plotting random walks
#######################################

#######################################
# 5. Plotting overdispersion terms
#######################################
