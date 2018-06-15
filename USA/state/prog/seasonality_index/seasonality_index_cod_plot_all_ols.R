rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
cod <- as.character(args[7])

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'

# length of analysis period
num.years <- year.end - year.start + 1

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# DIRECTORY CREATION AND DATA
###############################################################

# output directory
file.loc <- paste0('../../output/seasonality_index_ols/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)

## DATA ##

# FIX FILE.LOC

# load files for all cause and main sub-causes
lin.reg.grad.weight = data.frame()
for(i in cod.broad){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight = rbind(lin.reg.grad.weight,dat.temp)
}
lin.reg.grad.weight$cause <- gsub('AllCause', 'All cause', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('External', 'Injuries', lin.reg.grad.weight$cause)
lin.reg.grad.weight$age = as.numeric(lin.reg.grad.weight$age)
lin.reg.grad.weight$pvalue = lin.reg.grad.weight$`p-value`


# load files for cardio sub-causes
lin.reg.grad.weight.cardio = data.frame()
for(i in cod.cardio){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.cardio = rbind(lin.reg.grad.weight.cardio,dat.temp)
}
lin.reg.grad.weight.cardio$age = as.numeric(lin.reg.grad.weight.cardio$age)
lin.reg.grad.weight.cardio$pvalue = lin.reg.grad.weight.cardio$`p-value`


# load files for injury sub-causes
lin.reg.grad.weight.injury = data.frame()
for(i in cod.injuries){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.injury = rbind(lin.reg.grad.weight.injury,dat.temp)
}
lin.reg.grad.weight.injury$age = as.numeric(lin.reg.grad.weight.injury$age)
lin.reg.grad.weight.injury$pvalue = lin.reg.grad.weight.injury$`p-value`

# load files for other sub-causes
lin.reg.grad.weight.other = data.frame()
for(i in cod.other){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_',i,'_',year.start,'_',year.end))
    lin.reg.grad.weight.other = rbind(lin.reg.grad.weight.other,dat.temp)
}
lin.reg.grad.weight.other$age = as.numeric(lin.reg.grad.weight.other$age)
lin.reg.grad.weight.other$pvalue = lin.reg.grad.weight.other$`p-value`

###############################################################
# PLOTTING MATERIAL
###############################################################
age.colours <- c('#FF1493','#B8860B','#808080','#00BFFF','#00CED1')
age.colours <- c(age.colours,'#66CDAA','#9ACD32','#ADFF2F','#9932CC','#FF8C00')
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

###############################################################
# PLOTTING
###############################################################

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(data,shape.selected,x,y,n,xmin,xmax,ymin,ymax,legend){

    p = ggplot() +
    geom_point(data=subset(data,pvalue<=0.05),colour='black',aes(shape=as.factor(sex),x=(start.seas.index-1),y=(end.seas.index-1)),size=5) +
    geom_point(data=subset(data,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.seas.index-1),y=(end.seas.index-1)),size=3) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(xmin/100,(xmax/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(ymin/100,(ymax/100))) +
    #geom_hline(linetype=2, yintercept = seq(0,1,0.1), alpha=0.2) +
    #geom_vline(linetype=2, xintercept = seq(0,1,0.1), alpha=0.2) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    facet_wrap(~cause,ncol=n)

    if(legend==0){
        p = p +
        theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(x, y),text = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        rect = element_blank(),legend.background = element_rect(fill = "grey95"))
    }
    if(legend==1){
        p = p +
        theme(legend.position = 'bottom', legend.box.just = "centre",legend.box = "horizontal",text = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        rect = element_blank(),legend.background = element_rect(fill = "grey95"))
    }

    print(p)

}

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_with_allcause_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight,17,.85,.2,3,1,101,1,101,0)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_with_cardio_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.cardio,17,0.85,.2,2,1,500,1,500,0)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_with_injuries_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.injury,17,0.85,.2,2,1,150,1,150,1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_with_other_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.other,17,.77,.2,3,2,300,2,300,1)
dev.off()



# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5 <- function(data,shape.selected,x,y,n,xmin,xmax,ymin,ymax,legend){

    p = ggplot() +
    geom_point(data=subset(data,pvalue<=0.05),colour='hot pink',aes(shape=as.factor(sex),x=age,y=(per.decade.perc)),size=5) +
    geom_point(data=subset(data,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=age,y=(per.decade.perc)),size=3) +
    geom_abline(slope=0,intercept=0, linetype=2,alpha=0.5) +
    # scale_x_continuous(name=paste0('Age'),labels=age.print) +
    scale_y_continuous(name=paste0('Percentage point change per decade'),limits=c(-10,(10))) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    facet_wrap(~cause,ncol=n)

    if(legend==0){
        p = p +
        theme(legend.box.just = "centre",legend.box = "horizontal",legend.position=c(x, y),text = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        rect = element_blank(),legend.background = element_rect(fill = "grey95"))
    }
    if(legend==1){
        p = p +
        theme(legend.position = 'bottom', legend.box.just = "centre",legend.box = "horizontal",text = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        rect = element_blank(),legend.background = element_rect(fill = "grey95"))
    }

    print(p)

}

pdf(paste0(file.loc,'seasonality_index_diff_sig5_weighted_allcauses_with_allcause_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight,17,.85,.2,3,-5,120,-5,120,0)
dev.off()

pdf(paste0(file.loc,'seasonality_index_diff_sig5_weighted_allcauses_with_cardio_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.cardio,17,0.85,.2,2,-10,300,-10,300,0)
dev.off()

pdf(paste0(file.loc,'seasonality_index_diff_sig5_weighted_allcauses_with_injuries_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.injury,17,0.85,.2,2,-5,150,-5,150,1)
dev.off()

pdf(paste0(file.loc,'seasonality_index_diff_sig5_weighted_allcauses_with_other_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
plot.function.diff.seas.sig.5(lin.reg.grad.weight.other,17,.77,.2,3,-5,150,-5,150,1)
dev.off()

# plot coefficient of seasonality for each age nationally at start and end of period with significance
plot.function.diff.seas.sig.5.wo.allcause <- function(shape.selected) {

    lin.reg.grad.weight = subset(lin.reg.grad.weight,cause!='Allcause')

    print(ggplot() +
    geom_point(data=subset(lin.reg.grad.weight,sig.test.5==1),colour='black',aes(shape=as.factor(sex),x=(start.value.2/100),y=(end.value.2/100)),size=8) +
    geom_point(data=subset(lin.reg.grad.weight,sex==1|2),aes(shape=as.factor(sex), color=as.factor(age),x=(start.value.2/100),y=(end.value.2/100)),size=6) +
    geom_abline(slope=1,intercept=0, linetype=2,alpha=0.5) +
    scale_x_continuous(name=paste0('Percent difference in death rates in ',year.start),labels=percent,limits=c(0,(100/100))) +
    scale_y_continuous(name=paste0('Percent difference in death rates in ',year.end),labels=percent,limits=c(0,(100/100))) +
    geom_hline(linetype=2, yintercept = seq(0,1,0.1), alpha=0.2) +
    geom_vline(linetype=2, xintercept = seq(0,1,0.1), alpha=0.2) +
    scale_shape_manual(values=c(16,shape.selected),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),values=age.colours,guide = guide_legend(title = 'Age group:')) +
    facet_wrap(~cause) +
    theme(legend.box.just = "centre",legend.box = "horizontal",legend.position='bottom',text = element_text(size = 15),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
    axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),rect = element_blank())
    #,legend.background = element_rect(fill = "grey95"))
    )
}

# pdf(paste0(file.loc,'seasonality_index_change_sig5_weighted_allcauses_wo_allcause_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
# plot.function.diff.seas.sig.5.wo.allcause(17)
# dev.off()
