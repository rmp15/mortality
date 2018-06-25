rm(list=ls())

library(ggplot2)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
year.start.2 <- as.numeric(args[3])
year.end.2 <- as.numeric(args[4])
dname <- as.character(args[5])
metric <- as.character(args[6])
# cod <- as.character(args[7])

#year.start = 1980 ; year.end = 2016 ; year.start.2 = 1980 ; year.end.2 = 2016 ; dname = 't2m' ; metric = 'mean'

# length of analysis period
num.years <- year.end - year.start + 1

# source relevant objects
source('../../data/objects/objects.R')

###############################################################
# DIRECTORY CREATION AND DATA
###############################################################

# create directories for output
file.loc <- paste0('../../output/seasonality_index/national/')
ifelse(!dir.exists(file.loc), dir.create(file.loc, recursive=TRUE), FALSE)
file.loc.regional <- paste0('../../output/seasonality_index/regional/')
ifelse(!dir.exists(file.loc.regional), dir.create(file.loc.regional, recursive=TRUE), FALSE)

# 1. ORIGINAL METHOD

# input directory
file.loc <- paste0('../../output/seasonality_index/national/')

# load files for all cause and main sub-causes
lin.reg.grad.weight = data.frame()
for(i in cod.broad){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight = rbind(lin.reg.grad.weight,dat.temp)
}
lin.reg.grad.weight$cause <- gsub('AllCause', 'All cause', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('Cardiopulmonary', 'Cardiorespiratory', lin.reg.grad.weight$cause)
lin.reg.grad.weight$cause <- gsub('External', 'Injuries', lin.reg.grad.weight$cause)
lin.reg.grad.weight$age = as.numeric(lin.reg.grad.weight$age)

# load files for cardio sub-causes
lin.reg.grad.weight.cardio = data.frame()
for(i in cod.cardio){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.cardio = rbind(lin.reg.grad.weight.cardio,dat.temp)
}
lin.reg.grad.weight.cardio$age = as.numeric(lin.reg.grad.weight.cardio$age)

# load files for injury sub-causes
lin.reg.grad.weight.injury = data.frame()
for(i in cod.injuries){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.injury = rbind(lin.reg.grad.weight.injury,dat.temp)
}
lin.reg.grad.weight.injury$age = as.numeric(lin.reg.grad.weight.injury$age)

# load files for other sub-causes
lin.reg.grad.weight.other = data.frame()
for(i in cod.other){
    dat.temp = readRDS(paste0(file.loc,'seasonality_index_nat_changes_', i,'_',year.start,'_',year.end))
    dat.temp$cause = i
    lin.reg.grad.weight.other = rbind(lin.reg.grad.weight.other,dat.temp)
}
lin.reg.grad.weight.other$age = as.numeric(lin.reg.grad.weight.other$age)

dat.old = rbind(lin.reg.grad.weight, lin.reg.grad.weight.cardio, lin.reg.grad.weight.injury, lin.reg.grad.weight.other)

# 2. OLS METHOD (RECOMMENDED BY ELIFE REVISIONS)

# input directory
file.loc <- paste0('../../output/seasonality_index_ols/')

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

dat.new = rbind(lin.reg.grad.weight, lin.reg.grad.weight.cardio, lin.reg.grad.weight.injury, lin.reg.grad.weight.other)

# 3. COSINOR METHOD (RECOMMENDED BY ELIFE REVISIONS)

# input directory
file.loc <- paste0('../../output/seasonality_index_cosinor/')

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

dat.new.cosinor = rbind(lin.reg.grad.weight, lin.reg.grad.weight.cardio, lin.reg.grad.weight.injury, lin.reg.grad.weight.other)

###############################################################
# MERGING AND PLOTTING PARAMETERS
###############################################################

dat.merged = merge(dat.old[,c(1,2,8,11,21)],dat.new[,c(24,26,27,28,29)],by=c('sex','age','cause'))
dat.merged$per.year.perc.ols = dat.merged$per.year.perc ; dat.merged$per.year.perc = NULL
dat.merged = merge(dat.merged,dat.new.cosinor[,c(12,14:16)],by=c('sex','age','cause'))

# remove wacky results that are meaningless by definition
dat.merged = subset(dat.merged,!(age%in%c(0,5,55,65,75,85)&cause=='Maternal conditions'))
dat.merged = subset(dat.merged,!(sex==1&cause=='Maternal conditions'))
dat.merged = subset(dat.merged,!(age%in%c(5,15,25,35,45,55,65,75,85)&cause=='Perinatal conditions'))
names(dat.merged) = c('sex','age','cause','grad.old','p.old','p.new','grad.ols','grad.cosinor')

# plot old against ols
pdf(paste0(file.loc,'seasonality_original_method_against_elife_revisions_proposal_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
    ggplot() +
    geom_point(data=subset(dat.merged,p.old<=0.05),colour='hot pink',aes(shape=as.factor(sex),x=(grad.old*100),y=grad.ols),size=3) +
    geom_point(data=dat.merged,aes(shape=as.factor(sex),x=(grad.old*100),y=grad.ols)) +
    geom_abline(slope=1,intercept=0, linetype=1,alpha=0.5) +
    geom_abline(slope=0,intercept=0, linetype=2,alpha=0.5) +
    geom_vline(xintercept=0,linetype=2,alpha=0.5) +
    xlab(c('Original method')) + ylab(c('eLife review method')) +
    scale_shape_manual(values=c(16,17),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    facet_wrap(~cause,scale='free')
dev.off()

# plot old against cosinor
pdf(paste0(file.loc,'seasonality_original_method_against_elife_revisions_proposal_cosinor_',year.start,'_',year.end,'.pdf'),height=0,width=0,paper='a4r')
    ggplot() +
    geom_point(data=subset(dat.merged,p.old<=0.05),colour='hot pink',aes(shape=as.factor(sex),x=(grad.old*100),y=grad.cosinor),size=3) +
    geom_point(data=dat.merged,aes(shape=as.factor(sex),x=(grad.old*100),y=grad.cosinor)) +
    geom_abline(slope=1,intercept=0, linetype=1,alpha=0.5) +
    geom_abline(slope=0,intercept=0, linetype=2,alpha=0.5) +
    geom_vline(xintercept=0,linetype=2,alpha=0.5) +
    xlab(c('Original method')) + ylab(c('eLife review method')) +
    scale_shape_manual(values=c(16,17),labels=c('Male','Female'),guide = guide_legend(title = '')) +
    scale_colour_manual(labels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'),
    values=age.colours,guide = guide_legend(title = 'Age group (years)')) +
    facet_wrap(~cause,scale='free')
dev.off()