rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start.arg <- as.numeric(args[1])
year.end.arg <- as.numeric(args[2])

# create directories for output
file.loc <- paste0('../../output/zeroes_prevalence/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

# load data for broad causes of injuries
filename <- paste0('../../output/prep_data_cod/datus_state_rates_cod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat.broad <- readRDS(filename)

# load data for sub causes of injuries
filename <- paste0('../../output/prep_data_cod/datus_nat_deaths_subcod_injuries_ons_',year.start.arg,'_',year.end.arg)
dat.sub <- readRDS(filename)

# remove alaska and hawaii
dat.broad = subset(dat.broad,!(fips%in%c(2,15)))
dat.sub = subset(dat.sub,!(fips%in%c(2,15)))

# lookups
source('../../data/objects/objects.R')

library(plyr)
library(ggplot2)

# calculate percentage of zeroes
dat.broad.summary = ddply(dat.broad,.(cause,fips,age,sex,month),summarise,zeroes=sum(deaths %in% 0 ),count=length(deaths),percentage.zeroes=round(100*sum(deaths %in% 0)/length(deaths),1))
dat.sub.summary = ddply(dat.sub,.(cause.sub,fips,age,sex,month),summarise,zeroes=sum(deaths %in% 0 ),count=length(deaths),percentage.zeroes=round(100*sum(deaths %in% 0)/length(deaths),1))
names(dat.sub.summary)[1] = 'cause'

# colorway for plotting
colorway = c("navy","deepskyblue2","deepskyblue3","lightgreen","white","gold","orange","red","darkred")

# heat map
heatmap.zeroes <- function(dat,cause.selected) {

    dat = subset(dat,cause==cause.selected)

    dat$sex.long <- mapvalues(dat$sex,from=sort(unique(dat$sex)),to=c('Male','Female'))
    dat$sex.long <- with(dat,reorder(dat$sex.long,sex))

    print(ggplot(data=subset(dat)) +
    geom_tile(aes(x=month,y=as.factor(age),fill=percentage.zeroes)) +
    scale_fill_gradientn(colours=colorway,
    breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    na.value = "grey98",limits = c(0,100),
    guide = guide_legend(nrow = 1,title = paste0("Percentage of zeroes"))) +
    guides(fill = guide_colorbar(barwidth = 30, barheight = 1,title = paste0("Percentage of zeroes"))) +
    scale_x_continuous(breaks=c(seq(1,12,by=1)),labels=month.short)   +
    scale_y_discrete(labels=age.print) +
    ggtitle(cause.selected) +
    scale_size(guide = 'none') +
    facet_wrap(~sex.long) +
    xlab("Month") + ylab('Age') +
    theme_bw() + theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    )
}

# save to pdf
pdf(paste0(file.loc,'percentage_zeroes_injury_subcauses_',year.start.arg,'_',year.end.arg,'.pdf'),paper='a4r',height=0,width=0)
for(i in unique(dat.broad.summary$cause)){
    heatmap.zeroes(dat.broad.summary,i)
}
for(j in unique(dat.sub.summary$cause)){
    heatmap.zeroes(dat.sub.summary,j)
}
dev.off()
