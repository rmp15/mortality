rm(list=ls())

library(INLA)
library(ggplot2)

# selected data attributes
age.selected <- 85
sex.selected <- 'male'

# load INLA parameters file for model 1a
dat.1a <- readRDS(paste0('USA_rate_pred_type1a_',age.selected,'_',sex.selected,'_1982_1991_parameters')) 

# load INLA parameters file for model 3a
dat.3a <- readRDS(paste0('USA_rate_pred_type3a_',age.selected,'_male_1982_1991_parameters'))


# write to pdf
#pdf.name <- 'model_1a_3a_parameter_comparison.pdf'
#pdf(pdf.name,paper='a4r',height=0,width=0)


# plot global intercept
pdf(paste0(age.selected,'_',sex.selected,'_global_intercept_comparison.pdf'),paper='a4r',height=0,width=0)
global.int.1a <- data.frame(x='NRW',intercept=dat.1a$summary.fixed$mean[1],sd=dat.1a$summary.fixed$sd[1])
global.int.3a <- data.frame(x='SRW',intercept=dat.3a$summary.fixed$mean[1],sd=dat.3a$summary.fixed$sd[1])
global.int <- rbind(global.int.1a,global.int.3a)
ggplot() +
geom_point(data=global.int,aes(x=x,y=intercept)) +
geom_errorbar(data=global.int,aes(x=x,ymin=intercept-1.6449*sd,ymax=intercept+1.6449*sd)) +
ggtitle('Global intercept values with 90% CI') +
ylab('global intercept') +
xlab('model') + 
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot month intercept
month.int.1a <- dat.1a$summary.random$month
month.int.3a <- dat.3a$summary.random$month
month.int.1a$month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
month.int.3a$month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
pdf(paste0(age.selected,'_',sex.selected,'_month_intercept_comparison.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_line(data=month.int.1a,aes(x=ID,y=mean,color='red')) +
geom_ribbon(data=month.int.1a,alpha=0.2,fill='red',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
geom_line(data=month.int.3a,aes(x=ID,y=mean,color='green')) +
geom_ribbon(data=month.int.3a,alpha=0.2,fill='green',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
xlab('month') +
scale_x_continuous(breaks=c(1:12)) +
ylab('month intercept') + 
ggtitle('month intercept comparison (red=NRW, green=SRW)') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot state intercept
pdf(paste0(age.selected,'_',sex.selected,'_state_intercept_comparison.pdf'),paper='a4r',height=0,width=0)
state.int.1a <- dat.1a$summary.random$ID
state.int.3a <- dat.3a$summary.random$ID
ggplot() +
geom_line(data=state.int.1a,aes(x=ID,y=mean,color='red')) +
geom_line(data=state.int.3a,aes(x=ID,y=mean,color='green')) +
geom_ribbon(data=state.int.1a,alpha=0.2,fill='red',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
geom_ribbon(data=state.int.3a,alpha=0.2,fill='green',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
xlab('state') +
ggtitle('state intercept comparison (red=NRW, green=SRW)') +
ylab('state intercept') + 
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot state-month intecept
pdf(paste0(age.selected,'_',sex.selected,'_statemonth_intercept_comparison.pdf'),paper='a4r',height=0,width=0)
statemonth.int.1a <- dat.1a$summary.random$month3
statemonth.int.1a$row.id <- seq(1:nrow(statemonth.int.1a))
statemonth.int.3a <- dat.3a$summary.random$month3
statemonth.int.3a$row.id <- seq(1:nrow(statemonth.int.3a))
ggplot() +
geom_line(data=statemonth.int.1a,aes(x=row.id,y=mean,color='red')) +
geom_line(data=statemonth.int.3a,aes(x=row.id,y=mean,color='green')) +
geom_ribbon(data=statemonth.int.1a,alpha=0.2,fill='red',aes(x=row.id,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd))+
geom_ribbon(data=statemonth.int.3a,alpha=0.2,fill='green',aes(x=row.id,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd))+
xlab('state-month') +
ylab('state-month intercept') + 
ggtitle('state-month intercept comparison (red=NRW, green=SRW)') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot global slope
pdf(paste0(age.selected,'_',sex.selected,'_global_slope_comparison.pdf'),paper='a4r',height=0,width=0)
global.slp.1a <- data.frame(x='1a',slope=dat.1a$summary.fixed$mean[2],sd=dat.1a$summary.fixed$sd[2])
global.slp.3a <- data.frame(x='3a',slope=dat.3a$summary.fixed$mean[2],sd=dat.3a$summary.fixed$sd[2])
global.slp <- rbind(global.slp.1a,global.slp.3a)
ggplot() +
geom_point(data=global.slp,aes(x=x,y=slope)) +
geom_errorbar(data=global.slp,aes(x=x,ymin=slope-1.6449*sd,ymax=slope+1.6449*sd)) +
ggtitle('Global slope values with 90% CI') +
ylab('global slope') +
xlab('model') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot month slope
month.slp.1a <- dat.1a$summary.random$month2
month.slp.3a <- dat.3a$summary.random$month2
pdf(paste0(age.selected,'_',sex.selected,'_month_slope_comparison.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_line(data=month.slp.1a,aes(x=ID,y=mean,color='red')) +
geom_ribbon(data=month.slp.1a,alpha=0.2,fill='red',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
geom_line(data=month.slp.3a,aes(x=ID,y=mean,color='green')) +
geom_ribbon(data=month.slp.3a,alpha=0.2,fill='green',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
xlab('month') +
scale_x_continuous(breaks=c(1:12)) +
ylab('month slope') +
ggtitle('month slope comparison (red=NRW, green=SRW)') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot state slope
state.slp.1a <- dat.1a$summary.random$ID2
state.slp.3a <- dat.3a$summary.random$ID2
pdf(paste0(age.selected,'_',sex.selected,'_state_slope_comparison.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_line(data=state.slp.1a,aes(x=ID,y=mean,color='red')) +
geom_line(data=state.slp.3a,aes(x=ID,y=mean,color='green')) +
geom_ribbon(data=state.slp.1a,alpha=0.2,fill='red',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
geom_ribbon(data=state.slp.3a,alpha=0.2,fill='green',aes(x=ID,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd)) +
xlab('state') +
ggtitle('state slope comparison (red=NRW, green=SRW)') +
ylab('state slope') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()



# plot state-month slope
pdf(paste0(age.selected,'_',sex.selected,'_statemonth_slope_comparison.pdf'),paper='a4r',height=0,width=0)
statemonth.slp.1a <- dat.1a$summary.random$month4
statemonth.slp.1a$row.id <- seq(1:nrow(statemonth.slp.1a))
statemonth.slp.3a <- dat.3a$summary.random$month4
statemonth.slp.3a$row.id <- seq(1:nrow(statemonth.slp.3a))
ggplot() +
geom_line(data=statemonth.slp.1a,aes(x=row.id,y=mean,color='red')) +
geom_line(data=statemonth.slp.3a,aes(x=row.id,y=mean,color='green')) +
geom_ribbon(data=statemonth.slp.1a,alpha=0.2,fill='red',aes(x=row.id,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd))+
geom_ribbon(data=statemonth.slp.3a,alpha=0.2,fill='green',aes(x=row.id,ymin=mean-1.6449*sd,ymax=mean+1.6449*sd))+
xlab('state-month') +
ylab('state-month slope') +
ggtitle('state-month slope comparison (red=NRW, green=SRW)') +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot fitted rates on x-y scatter
fitted.1a <- dat.1a$summary.fitted.values$mean
fitted.3a <- dat.3a$summary.fitted.values$mean
compare.xy <- as.data.frame(cbind(fitted.1a,fitted.3a))
pdf(paste0(age.selected,'_',sex.selected,'_model_scatterplot.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_point(data=compare.xy,aes(x=100000*fitted.1a,y=100000*fitted.3a)) +
ggtitle('NRW against SRW x-y scatterplot') +
xlab('NRW fitted death rates (per 100,000)') +
ylab('SRW fitted deaths rates (per 100,000)') + 
geom_abline(slope=1,linetype=2) +
guides(color=FALSE) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

# plot random walks
num.year <- 10
random.walk.1a <- dat.1a$summary.random$year.month3
random.walk.3a <- data.frame(x=rep(1:(num.year*12), 51),rw1=dat.3a$summary.random$year.month4$mean,sd=dat.3a$summary.random$year.month4$sd, ID=rep(1:51,each=num.year*12))

# plot each state's random walk on top of each other
pdf(paste0(age.selected,'_',sex.selected,'_SRW_random_walks.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_line(data=random.walk.3a,aes(x=x,y=rw1,color='red')) +
geom_ribbon(data=random.walk.3a,alpha=0.2,fill='red',aes(x=x,ymax=rw1-1.6649*sd,ymin=rw1+1.6649*sd)) + 
facet_wrap(~ID) +
guides(color=FALSE) +
ylab('random walk value') +
xlab('time') + 
ggtitle('SRW: random walks per state over time') + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()

pdf(paste0(age.selected,'_',sex.selected,'_NRW_random_walk.pdf'),paper='a4r',height=0,width=0)
ggplot() +
geom_line(data=random.walk.1a,aes(x=ID,y=mean,color='green')) + 
geom_ribbon(data=random.walk.1a,alpha=0.2,fill='red',aes(x=ID,ymax=mean-1.6649*sd,ymin=mean+1.6649*sd)) + 
guides(color=FALSE) +
ylab('random walk value') +
xlab('time') + 
ggtitle('NRW: random walk over time') + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
rect = element_blank())
dev.off()


# turn off pdf write
dev.off()


