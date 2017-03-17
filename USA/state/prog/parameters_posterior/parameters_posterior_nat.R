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

# selected data attributes
#age <- 85
#sex <- 'male'
#year.start <- 1982
#year.end <- 2013
#model <- 1

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','4a')
model <- models[model]

# coding for graph-friendly information
source('../../data/objects/objects.R')

# load INLA parameters file
file.loc <- paste0('~/data/mortality/US/national/predicted/type_',model,'/age_groups/',age,'/USAnat_',age,sex,'_pred_type',model,'_monthtermsrw1cyclic_',year.start,'_',year.end,'_parameters')
dat <- readRDS(file.loc)

# lists of marginals
marginals.fixed 	<- 	names(dat$marginals.fixed)
marginals.random 	<- 	names(dat$marginals.random)

# create directories for output
file.loc <- paste0('../../output/parameters_posterior/',year.start,'_',year.end,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
ifelse(!dir.exists(paste0(file.loc,'natmonth_intercepts')), dir.create(paste0(file.loc,'natmonth_intercepts'), recursive=TRUE), FALSE)
ifelse(!dir.exists(paste0(file.loc,'natmonth_heatmaps')), dir.create(paste0(file.loc,'natmonth_heatmaps'), recursive=TRUE), FALSE)

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

# state-month intercept
month <- dat$summary.random$month

pdf(paste0(file.loc,'natmonth_intercepts/natmonth_params_',age,'_',sex,'_',model,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)

ggplot(data=month) +
geom_line(aes(x=ID,y=mean)) +
#geom_ribbon(aes(x=ID,ymin=mean-1.96*sd,ymax=mean+1.96*sd),alpha=0.2) +
scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
xlab('month') +
ylab('month intercept') +
ggtitle(paste0(age,' ',sex,' month intercept'))

dev.off()

pdf(paste0(file.loc,'natmonth_heatmaps/natmonth_heatmap_',age,'_',sex,'_',model,'_',year.start,'_',year.end,'.pdf'),paper='a4r',height=0,width=0)

dat.heatmap <- ddply(month,.(),summarize,month=ID,rank=rank(mean))
dat.heatmap$y <- 1

ggplot(data=dat.heatmap, aes(x=as.factor(month),y=y)) +
geom_tile(aes(fill=rank)) +
scale_fill_gradient(low='green',high='red',guide = guide_legend(title = 'Rank')) +
xlab('Month') +
ylab('State') +
scale_x_discrete(labels=month.short) +
ggtitle(paste0(age,' ',sex,' state-month intercept ranking'))

dev.off()

#######################################
# 4. Plotting random walks
#######################################

#######################################
# 5. Plotting overdispersion terms
#######################################
