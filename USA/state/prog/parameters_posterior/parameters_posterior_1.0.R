rm(list=ls())

library(INLA)
library(ggplot2)

# load INLA parameters file for model 1a
dat.1a <- readRDS('USA_rate_pred_type1a_85_male_1982_1991_parameters') 

# load INLA parameters file for model 3a
dat.3a <- readRDS('USA_rate_pred_type3a_85_male_1982_1991_parameters')

# write to pdf
pdf.name <- 'model_1a_3a_parameter_comparison.pdf'
pdf(pdf.name,paper='a4r',height=0,width=0)

# plot global intercept
global.int.1a <- data.frame(x='1a',intercept=dat.1a$summary.fixed$mean[1])
global.int.3a <- data.frame(x='3a',intercept=dat.3a$summary.fixed$mean[1])
global.int <- rbind(global.int.1a,global.int.3a)
ggplot() +
geom_point(data=global.int,aes(x=x,y=intercept,color='black')) +
ggtitle('Global intercept values') +
ylab('global intercept') + 
guides(color=FALSE) +
theme_bw()

# plot month intercept
month.int.1a <- dat.1a$summary.random$month
month.int.3a <- dat.3a$summary.random$month
ggplot() +
geom_line(data=month.int.1a,aes(x=ID,y=mean,color='blue')) +
geom_line(data=month.int.3a,aes(x=ID,y=mean,color='red')) +
xlab('month') +
ylab('month intercept') + 
ggtitle('month intercept comparison (blue=national rw1, red=state rw1)') +
guides(color=FALSE) +
theme_bw()

# plot month and global intercept added together
global.month.int.1a <- month.int.1a
global.month.int.1a$mean2 <- global.month.int.1a$mean + global.int.1a[1,2]
global.month.int.3a <- month.int.3a
global.month.int.3a$mean2 <- global.month.int.3a$mean + global.int.3a[1,2]
ggplot() +
geom_line(data=global.month.int.1a,aes(x=ID,y=mean2,color='blue')) +
geom_line(data=global.month.int.3a,aes(x=ID,y=mean2,color='red')) +
xlab('month') +
ylab('global + month intercept') + 
ggtitle('global + month intercept comparison (blue=national rw1, red=state rw1)') +
guides(color=FALSE) +
theme_bw()

# plot state intercept
state.int.1a <- dat.1a$summary.random$ID
state.int.3a <- dat.3a$summary.random$ID
ggplot() +
geom_line(data=state.int.1a,aes(x=ID,y=mean,color='blue')) +
geom_line(data=state.int.3a,aes(x=ID,y=mean,color='red')) +
xlab('state') +
ggtitle('state intercept comparison') +
ylab('state intercept') + 
guides(color=FALSE) +
theme_bw()

# plot state-month intecept
statemonth.int.1a <- dat.1a$summary.random$month3
statemonth.int.1a$row.id <- seq(1:nrow(statemonth.int.1a))
statemonth.int.3a <- dat.3a$summary.random$month3
statemonth.int.3a$row.id <- seq(1:nrow(statemonth.int.3a))
ggplot() +
geom_line(data=statemonth.int.1a,aes(x=row.id,y=mean,color='blue')) +
geom_line(data=statemonth.int.3a,aes(x=row.id,y=mean,color='red')) +
xlab('state-month') +
ylab('state-month intercept') + 
ggtitle('state-month intercept comparison (blue=national rw1, red=state rw1)') +
guides(color=FALSE) +
theme_bw()

# plot global slope
global.slp.1a <- data.frame(x='1a',intercept=dat.1a$summary.fixed$mean[2])
global.slp.3a <- data.frame(x='3a',intercept=dat.3a$summary.fixed$mean[2])
global.int <- rbind(global.slp.1a,global.slp.3a)
ggplot() +
geom_point(data=global.int,aes(x=x,y=intercept,color='black')) +
ggtitle('Global slope values') +
ylab('global slope') + 
guides(color=FALSE) +
theme_bw()

# plot month slope
month.slp.1a <- dat.1a$summary.random$month2
month.slp.3a <- dat.3a$summary.random$month2
ggplot() +
geom_line(data=month.slp.1a,aes(x=ID,y=mean,color='blue')) +
geom_line(data=month.slp.3a,aes(x=ID,y=mean,color='red')) +
xlab('month') +
ylab('month slope') + 
ggtitle('month slope comparison (blue=national rw1, red=state rw1)') +
guides(color=FALSE) +
theme_bw()

# plot state slope
state.slp.1a <- dat.1a$summary.random$ID2
state.slp.3a <- dat.3a$summary.random$ID2
ggplot() +
geom_line(data=state.slp.1a,aes(x=ID,y=mean,color='blue')) +
geom_line(data=state.slp.3a,aes(x=ID,y=mean,color='red')) +
xlab('state') +
ggtitle('state slope comparison (blue=national rw1, red=state rw1)') +
ylab('state slope') + 
guides(color=FALSE) +
theme_bw()

# plot state-month slope
statemonth.slp.1a <- dat.1a$summary.random$month4
statemonth.slp.1a$row.id <- seq(1:nrow(statemonth.slp.1a))
statemonth.slp.3a <- dat.3a$summary.random$month4
statemonth.slp.3a$row.id <- seq(1:nrow(statemonth.slp.3a))
ggplot() +
geom_line(data=statemonth.int.1a,aes(x=row.id,y=mean,color='blue')) +
geom_line(data=statemonth.slp.3a,aes(x=row.id,y=mean,color='red')) +
xlab('state-month') +
ylab('state-month slope') + 
ggtitle('state-month slope comparison (blue=national rw1, red=state rw1)') +
guides(color=FALSE) +
theme_bw()

# plot fitted rates on x-y scatter
fitted.1a <- dat.1a$summary.fitted.values$mean
fitted.3a <- dat.3a$summary.fitted.values$mean
compare.xy <- as.data.frame(cbind(fitted.1a,fitted.3a))
ggplot() +
geom_point(data=compare.xy,aes(x=fitted.1a,y=fitted.3a)) +
ggtitle('Model 1a against 3a x-y scatter') +
xlab('1a fitted rates') +
ylab('3a fitted rates') + 
geom_abline(slope=1) +
guides(color=FALSE) +
theme_bw()

# plot random walks
num.year <- 10
random.walk.1a <- dat.1a$summary.random$year.month3
random.walk.3a <- data.frame(x=rep(1:(num.year*12), 51),rw1=dat.3a$summary.random$year.month4$mean, ID= rep(1:51,each=num.year*12))

# plot each state's random walk on top of each other
ggplot() +
geom_line(data=random.walk.3a,aes(x=x,y=rw1,color='red')) +
facet_wrap(~ID) +
guides(color=FALSE) +]
ylab('random walk value') +
xlab('time') + 
ggtitle('Random Walks per state over time') + 
theme_bw()

ggplot() +
geom_line(data=random.walk.1a,aes(x=ID,y=mean,color='blue')) + 
guides(color=FALSE) +
ylab('random walk value') +
xlab('time') + 
ggtitle('National Random Walk over time') + 
theme_bw()

# turn off pdf write
dev.off()


