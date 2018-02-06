rm(list=ls())

# break down the arguments from Rscript
args = commandArgs(trailingOnly=TRUE)
year.start = as.numeric(args[1])   ;   year.end = as.numeric(args[2])
year.start.2 = as.numeric(args[3]) ;   year.end.2 = as.numeric(args[4])
dname = as.character(args[5])      ;   metric = as.character(args[6])
cod = as.character(args[7])

# load required packages
packages = c('maptools', 'mapproj','rgeos','rgdal','RColorBrewer','ggplot2','plyr','scales')
lapply(packages, require, character.only=TRUE)

# length of analysis period
num.years = year.end - year.start + 1

# source relevant objects
sex.lookup = c('Men','Women')

###############################################################
# DATA PROCESSING
###############################################################

# load data
input.loc = 'file_here'
dat = readRDS(input.loc)

# load com data to establish max min locations
dat.COM = 'file_here'

# round to get month required for merging
dat.COM$COM.mean = round(dat.COM$COM.mean)
dat.COM$COM.mean = ifelse(dat.COM$COM.mean==0,12,dat.COM$COM.mean)
dat.COM$month = dat.COM$COM.mean
levels(dat.COM$sex) = c(1,2)

# METHOD TAKING ACCCOUNT OF POPULATION

dat.pois = merge(dat,dat.COM,by=c('age','sex','month'))
dat.pois = dat.pois[,c('age','sex','year','deaths.pred','pop.adj','type')]
dat.pois$maxmonth = ifelse(dat.pois$type=='max',1,0)
dat.pois = with(dat.pois,dat.pois[order(age,sex,year,maxmonth),])

# apply Poisson glm with population offsetting
dat.pois.summary = ddply(dat.pois,.(sex,age,year),
    function(z)coef(summary(glm(deaths.pred ~ maxmonth + offset(log(pop.adj)),family=poisson,data=z))))

# generate exponential versions to get back into correct world
#dat.pois.coef$ratio = exp(dat.pois.coef$maxmonth)
dat.pois.summary = dat.pois.summary[!c(TRUE,FALSE),]
dat.pois.summary$se = dat.pois.summary$`Std. Error`
dat.pois.summary$ratio = exp(dat.pois.summary$Estimate)

# add time value that starts at 0
dat.pois.summary$year.centre = with(dat.pois.summary,year-year.start)

# apply linear regression to each group by sex, age, month to find gradient
lin.reg.grad.weight  = ddply(dat.pois.summary, .(sex,age),
    function(z)coef(lm(ratio ~ year.centre, data=z, weights=1/(se^2))))
lin.reg.grad.weight$start.value = lin.reg.grad.weight$`(Intercept)`
lin.reg.grad.weight$end.value = with(lin.reg.grad.weight,`(Intercept)`+year.centre*(num.years-1))
lin.reg.grad.weight$sex.long = with(lin.reg.grad.weight,as.factor(as.character(sex)))
levels(lin.reg.grad.weight$sex.long) = sex.lookup

# obtain significance of slopes
lin.reg.sig.weight = ddply(dat.pois.summary, .(sex,age),
    function(z)coef(summary(lm(ratio ~ year.centre, data=z,weights=1/(se^2)))))
lin.reg.sig.weight = lin.reg.sig.weight[!c(TRUE,FALSE),]
lin.reg.sig.weight$sig.test.10 = ifelse(lin.reg.sig.weight[,6]<0.10,1,0)
lin.reg.sig.weight$sig.test.5 = ifelse(lin.reg.sig.weight[,6]<0.05,1,0)

# merge with data about gradients
lin.reg.grad.weight = merge(lin.reg.grad.weight,lin.reg.sig.weight,by=c('sex','age'))

# add ci info about differences between start and end year
lin.reg.grad.weight$grad.uci = with(lin.reg.grad.weight,year.centre+1.96*`Std. Error`)
lin.reg.grad.weight$grad.lci = with(lin.reg.grad.weight,year.centre-1.96*`Std. Error`)
lin.reg.grad.weight$diff = with(lin.reg.grad.weight,100*year.centre*(num.years-1))
lin.reg.grad.weight$diff.uci = with(lin.reg.grad.weight,100*grad.uci*(num.years-1))
lin.reg.grad.weight$diff.lci = with(lin.reg.grad.weight,100*grad.lci*(num.years-1))

# sort out the ordering by age
lin.reg.grad.weight = with(lin.reg.grad.weight,lin.reg.grad.weight[order(sex,age),])

# fix start and end values
lin.reg.grad.weight$start.value.2 = with(lin.reg.grad.weight,round(100*(start.value),1)-100)
lin.reg.grad.weight$end.value.2 = with(lin.reg.grad.weight,round(100*(end.value),1)-100)

# establish confidence intervals for linear regression start and end values
dat.ci = data.frame()
for (j in c(1:2)) {
    for (i in unique(dat.pois$age)){
        lm = lm(ratio ~ year.centre, data=subset(dat.pois.summary,age==i & sex==j), weights=1/(se^2))
        temp.start = predict(lm, data.frame(year.centre=min(dat.pois.summary$year.centre)),interval='confidence')
        temp.end = predict(lm, data.frame(year.centre=max(dat.pois.summary$year.centre)),interval='confidence')
        dat.ci = rbind(dat.ci,cbind(i,j,temp.start,temp.end))
    }}

# create directories for output
file.loc = paste0('/output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

## GRAPHS ##

# plotting colours
age.colours = c('#FF1493','#B8860B','#808080','#00BFFF','#00CED1')
age.colours = c(age.colours,'#66CDAA','#9ACD32','#ADFF2F','#9932CC','#FF8C00')
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function <- function(shape.selected) {

    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='hot pink',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=5) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=3) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(0,(100/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(0,(100/100))) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    facet_wrap(~cause) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(.85, .2),text = element_text(size = 10),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
    axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
    rect = element_blank(),legend.background = element_rect(fill = "grey95"))

    )
}

pdf(paste0(file.loc,'output.pdf'),height=0,width=0,paper='a4r')
plot.function(17)
dev.off()