rm(list=ls())

# load data with random walk terms
dat <- readRDS('USA_rate_pred_type3_55_male_1982_2010_parameters')
num.year <- 29

random.walk <- data.frame(x=rep(1:(num.year*12), 51),rw1=dat$summary.random$year.month4$mean, ID= rep(1:51,each=num.year*12))

# plot entire random walk in one line
plot(dat$summary.random$year.month4$mean)

# plot each state's random walk on top of each other
ggplot(random.walk,aes(x=x,y=rw1,color=as.factor(ID))) +
geom_line()
