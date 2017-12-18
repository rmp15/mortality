rm(list=ls())

library(foreign)
library(dplyr)
library(ggplot2)

# arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

# break down arguments from Rscript
year <- as.numeric(args[1])
print(year)

# read files
dat = read.dta(paste0("~/data/mortality/US/state/processed/cod/deathscod",year,'.dta'))

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
    dat.count = as.data.frame(count(dat.merged,agegroup,sex,cause.group,letter))
    dat.count = merge(dat.count,cod.lookup.10,by=c('cause.group','letter'),all.x=1)
    #dat.count$percentage = with(dat.count,(n/sum(n))*100)

    #dat.count.sum = plyr::ddply(dat.count,('cause.group'),summarize,sum=sum(n))
    #dat.count = merge(dat.count,dat.count.sum,by='cause.group')
    #dat.count$percentage = round(100 * (dat.count$n / dat.count$sum),1)

    ggplot(data=dat.count) +
    geom_bar(aes(y = n, x = cause.group, fill=description),stat="identity") +
    facet_wrap(~sex + agegroup,scales='free') +
    theme(text = element_text(size = 15),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=45), plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),strip.background = element_blank(), axis.line = element_line(colour = "black"),legend.position = 'bottom',legend.justification='center',legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))

    ##################################################
    ### 2. breakdown of cod by gbd
    ##################################################


}

