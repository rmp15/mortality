rm(list=ls())

# coding for graph-friendly information
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),
age.print=age.print)
sex.lookup <- c('male','female')

# load data and filter results
dat <- readRDS('USA_rate_pred_type1a_1982_2010')

year.start <- min(dat$year)
year.end <- max(dat$year)

year.list <- c()
# create vector which contains year values for plotting only every 12 months
for(i in year.start:year.end) {
    year.list <- c(year.list,i,rep(' ',11))
}

# nationalised data
dat$deaths.pred <- with(dat,pop.adj*rate.pred)
library(plyr)
dat.national <- ddply(dat,.(year,month,sex,age),summarize,deaths=sum(deaths),deaths.pred=sum(deaths.pred),pop.adj=sum(pop.adj))
dat.national$rate <- with(dat.national,deaths/pop.adj)
dat.national$rate.pred <- with(dat.national,deaths.pred/pop.adj)
dat.national <- dat.national[order(dat.national$sex,dat.national$age,dat.national$year,dat.national$month),]
dat.national$ID <- seq(1:nrow(dat.national))

library(ggplot2)


# function to plot a particular age group over time
plot.age <- function(age.selected=0,sex.selected=1) {
    
dat.subset <- subset(dat.national,age==age.selected & sex==sex.selected)
dat.subset$ID <- seq(1:nrow(dat.subset))

age.single <- as.matrix(age.code[age.code==age.selected,])[2]
pdf.name <- paste0(sex.lookup[sex.selected],'_',age.selected,'_national_death_rates.pdf')
pdf(pdf.name,height=0,width=0,paper='a4r')
print(ggplot(data=dat.subset) +
geom_line(color='forest green',aes(x=ID,y=100000*rate)) +
#ggtitle(paste0(age.single,' ',sex.lookup[sex.selected],': USA national deaths rates 1982 - 2010')) +
xlab('year') +
ylab('death rate (per 100,000)') +
scale_x_discrete(labels=year.list) +
theme(text = element_text(size = 20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),axis.text.x = element_text(angle=90)))
dev.off()
}

dir.create('national_death_rates')
setwd('national_death_rates')

dir.create('male')
setwd('male')
mapply(plot.age,unique(dat$age),1)
setwd('..')

dir.create('female')
setwd('female')
mapply(plot.age,unique(dat$age),2)
setwd('..')
setwd('..')


