rm(list=ls())

library(foreign)
library(dplyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
print(year)

# source variables
source('../../data/objects/objects.R')

# read files
dat = read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))

# create directories for output
file.loc <- paste0('../../output/breakdown_cod/',year,'/')
ifelse(!dir.exists(file.loc), dir.create(file.loc,recursive=TRUE), FALSE)

if(year<1999){
    # SOMETHING FOR ICD 9
}
if(year>=1999){

    ##################################################
    ### 1. breakdown of broad groups cod by letter
    ##################################################

    # strip letter from front of cause
    dat$letter = substr(dat$cause,1,1)

    # COD look-up
	cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
								cause.group=c('Other','Other','Cancer','Cancer','Other', # A-E
											'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
											'Other','Other','Other','Other','Other', # K-O
											'Other','Other','Other','External','External', # P-T
											'Other','External','External','External','External', # U-Y
											'External'), # Z
								description=c('Infectious/Parasitic I','Infectious/Parasitic II','Neoplasms','Blood/Immune','Endocrine/Nutritional/Metabolic', # A-E
											'Mental/Behavioural/Neurodevelopmental','Nervous','Eye/Adnexa/Ear/Mastoid','Circulatory','Respiratory', # F-J
											'Digestive','Skin/Subcutaneous','Musculoskeletal/Connective Tissue','Genitourinary','Pregnancy/Childbirth/Puerperium', # K-O
											'Perinatal','Malformations/Deformations/Chromosomal','Abnormal Clinical/Lab','Injury','Poisoning', # P-T
											'Unknown','Transport','Slipping/Tripping/Exposure','Exposure/Suicide','War/Terrorism/Medical Misadventures', # U-Y
											'Unknown'))

    # merge cod in ICD 10 coding
    dat.merged = merge(dat,cod.lookup.10[c('letter','cause.group')],by.x='letter',by.y='letter',all.x=1)

    # add agegroup groupings
  	dat.merged$agegroup <-
              	ifelse (dat.merged$age<5,   0,
                ifelse (dat.merged$age<15,  5,
                ifelse (dat.merged$age<25,  15,
                ifelse (dat.merged$age<35,  25,
                ifelse (dat.merged$age<45,  35,
                ifelse (dat.merged$age<55,  45,
                ifelse (dat.merged$age<65,  55,
                ifelse (dat.merged$age<75,  65,
                ifelse (dat.merged$age<85,  75,
                   	85)))))))))

    # establish the percentage of deaths by broad cause of death
    dat.count = as.data.frame(dplyr::count(dat.merged,agegroup,sex,cause.group,letter))
    dat.count = plyr::ddply(dat.count,c('agegroup','sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count$age.long = plyr::mapvalues(dat.count$age,from=sort(unique(dat.count$agegroup)),to=as.character(age.code[,2]))
    dat.count$age.long <- reorder(dat.count$age.long,dat.count$agegroup)

    dat.count.app = as.data.frame(dplyr::count(dat.merged,sex,cause.group,letter))
    dat.count.app = plyr::ddply(dat.count.app,c('sex','cause.group'),mutate,percentage=round(100*n/sum(n),1))
    dat.count.app$age.long = 'All ages'
    dat.count.app$agegroup = -1

    dat.count = rbind(dat.count.app,dat.count)

    dat.count = merge(dat.count,cod.lookup.10,by=c('cause.group','letter'),all.x=1)

    # friendly names for plotting
    dat.count$sex.long = plyr::mapvalues(dat.count$sex,from=sort(unique(dat.count$sex)),to=c('Men','Women'))

    # function to plot absolutely or relatively
    plot = function(age.sel=-1,y.measure=1){

        dat.count.sub = subset(dat.count,agegroup==age.sel)

        p = ggplot(data=dat.count.sub) +
        facet_wrap(~sex.long) +
        xlab('Cause Group') + ylab('Number of deaths') + ggtitle(paste0(year,', ',unique(dat.count.sub$age.long))) +
        theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=30), plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = 'bottom',legend.justification='center', legend.text=element_text(size=8),
        legend.background = element_rect(fill="gray90", size=.4, linetype="dotted"))

        if(y.measure==1){p = p + geom_bar(aes(y = n, x = cause.group, fill=description),stat="identity")+ ylab('Number of deaths')}
        if(y.measure==2){p = p + geom_bar(aes(y = percentage, x = cause.group, fill=description),stat="identity")+ ylab('Percentage')}

        print(p)

    }

    pdf(paste0(file.loc,year,'_absolute.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,1)
    }
    dev.off()

    pdf(paste0(file.loc,year,'_relative.pdf'),height=0,width=0,paper='a4r')
    for(i in c(-1,0,5,15,25,35,45,55,65,75,85)){
        plot(i,2)
    }
    dev.off()

    ##################################################
    ### 2. breakdown of cod by gbd
    ##################################################

    # TO COMPLETE

}

