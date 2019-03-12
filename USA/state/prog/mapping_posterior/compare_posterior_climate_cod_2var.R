# FINISH ADDING ADDITIONAL DEATHS

rm(list=ls())

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)
year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])
country <- as.character(args[3])
model <- as.numeric(args[4])
dname <- as.character(args[5])
metric1 <- as.character(args[6])
metric2 <- as.character(args[7])
cause <- as.character(args[8])

print(args)

# year.start = 1980 ; year.end = 2013 ; country = 'USA' ; model = 10 ; dname = 't2m' ;
# metric1 = 'meanc3' ; metric2 = 'sd'; cause = 'External'

# other metrics to use are
# metric2 = 'number_of_days_above_nonnormal_90_2',
# metric2 = 'number_of_min_3_day_above_nonnormal_90_upwaves_2'

# need to use meanc3 with sd/number of days above limit/number of days above limit

# source variables
source('../../data/objects/objects.R')
model <- models[model]

# combine three metrics in alphabetical order in a single string
metric.combined = paste(sort(c(metric1,metric2)),collapse='_')

# until things are completely run we need to use different year range (but shouldn't change too much)
year.end.2 = 2016

# bespoke colourway
colorway = c("navy","deepskyblue2","deepskyblue3","darkgreen","yellow3","gold","orange","red","darkred")

# identify which variables by short name
var1.short = as.character(dat.dict[which(dat.dict$metric==metric1),][,2])
var2.short = as.character(dat.dict[which(dat.dict$metric==metric2),][,2])

# load the data for 1 variables
if(cause!='AllCause'){
    dat.1var <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric1,'/non_pw/type_',model,'/parameters/',
    country,'_rate_pred_type',model,'_',year.start,'_',year.end.2,'_',dname,'_',metric1,'_',cause,'_fast_contig'))
}
if(cause=='AllCause'){
    dat.1var <- readRDS(paste0('../../data/climate_effects/',dname,'/',metric1,'/non_pw/type_',model,'/parameters/'
    ,country,'_rate_pred_type',model,'_',year.start,'_',year.end.2,'_',dname,'_',metric1,'_fast_contig'))
}

# load the data for 2 variables
if(cause!='AllCause'){
    dat.2var <- readRDS(paste0('../../data/climate_effects/',dname,'/2var/',metric.combined,'/non_pw/type_',model,'/parameters/',
    country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.combined,'_',cause,'_fast'))
}
if(cause=='AllCause'){
    dat.2var <- readRDS(paste0('../../data/climate_effects/',dname,'/2var/',metric.combined,'/non_pw/type_',model,'/parameters/'
    ,country,'_rate_pred_type',model,'_',year.start,'_',year.end,'_',dname,'_',metric.combined,'_fast'))
}

# get printable name
cod.print = ifelse(cause=='AllCause', 'All cause',
        ifelse(cause=='Cancer', 'Cancer',
        ifelse(cause=='Cardiopulmonary', 'Cardiorespiratory',
        ifelse(cause=='External', 'Injuries',
        ifelse(cause=='Other', 'Other'
        )))))

# create directories for output
file.loc <- paste0('../../output/compare_posterior_climate/',year.start,'_',year.end,
'/',dname,'/2var/',metric,'/non_pw/type_',model,'/parameters/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

# add names of variables to dataframe
dat.1var$var = as.character(dat.dict[which(dat.dict$metric==metric1),][,2])
dat.2var$var = ifelse(dat.2var$var==1,as.character(dat.dict[which(dat.dict$metric==metric1),][,2]),ifelse(dat.2var$var==2,as.character(dat.dict[which(dat.dict$metric==metric2),][,2]),NA))

# isolate the variable from the single variable parameter dataset from the one with two variables
dat.2var.isolated = subset(dat.2var,var==as.character(dat.dict[which(dat.dict$metric==metric1),][,2]))$mean

# new dataframe with only the mean excess risk
dat = data.frame(single.variable.parameter=dat.1var$mean,two.variable.parameter=dat.2var.isolated)

# calculate r^2 values
lm_eqn <- function(df){
    m <- lm(two.variable.parameter ~ single.variable.parameter, df);
    eq <- substitute(italic(r)~"="~corr,
         list(a = format(coef(m)[1], digits = 2),
              b = format(coef(m)[2], digits = 2),
             r2 = format(summary(m)$r.squared, digits = 3),
             corr = format(cor(df$two.variable.parameter, df$single.variable.parameter),method='pearson')))
    as.character(as.expression(eq));
}

# plot
pdf(paste0(file.loc,'parameter_comparison_',model,'_',year.start,'_',year.end,'_',dname,'_',metric.combined,'_',cause,'.pdf'),paper='a4r',height=0,width=0)
print(
    ggplot(dat) +
    geom_point(aes(x=single.variable.parameter,y=two.variable.parameter)) +
    geom_abline() +
    geom_text(x = 0, y = 0.015, label = lm_eqn(dat), parse = TRUE) +
    xlab(paste0('Excess risk values of ',metric1, ' from single-variable model')) +
    ylab(paste0('Excess risk values of ',metric1, ' from two-variable model')) +
    ggtitle(paste0('Only ',as.character(dat.dict[which(dat.dict$metric==metric1),][,2]),' vs. ',
    as.character(dat.dict[which(dat.dict$metric==metric1),][,2]), ' and ',as.character(dat.dict[which(dat.dict$metric==metric2),][,2]) )) +
    theme_bw() + theme(text = element_text(size = 15),
    panel.grid.major = element_blank(),axis.text.x = element_text(angle=90),
    plot.title = element_text(hjust = 0.5),panel.background = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black"),strip.background = element_blank(),
    legend.position = 'bottom',legend.justification='center',
    legend.background = element_rect(fill="white", size=.5, linetype="dotted"))
)
dev.off()