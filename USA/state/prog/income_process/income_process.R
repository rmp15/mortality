rm(list=ls())

library(reshape2)
library(dplyr)
library(ggplot2)

#

# load income data
file_income = '~/git/mortality/USA/state/data/income/CA1_1969_2016__ALL_AREAS.csv'
income <- read.csv(file_income,header=TRUE,stringsAsFactors = FALSE)

income_tot <- subset(income,LineCode==1)[,c(-2,-3,-4,-5,-6,-7)]
income_tot_melt <- melt(income_tot,id.vars = 'GeoFIPS')
colnames(income_tot_melt) <- c('fips','year','total')
income_tot_melt$year <- as.numeric(substr(income_tot_melt$year,2,5))
income_tot_melt <- income_tot_melt[order(income_tot_melt$fips),]

income_pop <- subset(income,LineCode==2)[,c(-2,-3,-4,-5,-6,-7)]
income_pop_melt <- melt(income_pop,id.vars = 'GeoFIPS')
colnames(income_pop_melt) <- c('fips','year','pop')
income_pop_melt$year <- as.numeric(substr(income_pop_melt$year,2,5))
income_pop_melt <- income_pop_melt[order(income_pop_melt$fips),]

income_pc <- subset(income,LineCode==3)[,c(-2,-3,-4,-5,-6,-7)]
income_pc_melt <- melt(income_pc,id.vars = 'GeoFIPS')
colnames(income_pc_melt) <- c('fips','year','pc')
income_pc_melt$year <- as.numeric(substr(income_pc_melt$year,2,5))
income_pc_melt <- income_pc_melt[order(income_pc_melt$fips),]

income_us <- left_join(income_tot_melt,income_pop_melt,by=c('fips','year'))
income_us <- data.frame(left_join(income_us,income_pc_melt,by=c('fips','year')))
income_us[,c(3,4,5)] <- as.numeric(as.character(unlist(income_us[,c(3,4,5)])))
income_us[,2] <- as.numeric(unlist(income_us[,2]))
income_us$total <- income_us$total*1000

# load population data to check 
# pop <- read.dta("C:/Users/hmt207/data/us_county_data/countyPopulationsnewyears.dta")    #pop_with_sc.dta")
# pop <- subset(pop,age!=99)
# popsum <- data.frame(summarise(group_by(pop,fips,year),popsum=sum(pop)))
load('P:/data/race/nchs_raw_annotated_withag_1990_to_2015')
popsum <- as.data.frame(summarise(group_by(subset(dat_nchs),fips,year),popsum=sum(popsum)))


# pop num is inconsistant 
income_us <- left_join(income_us,popsum,by=c('fips','year'))



#adjust for counties 
# !!! for kyle pop
#income_us$popsum[income_us$fips=='12086' & income_us$year < 1990 & income_us$year > 1981] <- popsum$popsum[popsum$fips=='12025' & popsum$year < 1990 & popsum$year > 1981]
#income_us$popsum[income_us$fips=='46102' & income_us$year < 2012 & income_us$year > 1981] <- popsum$popsum[popsum$fips=='46113' & popsum$year < 2012 & popsum$year > 1981]
# !!! for NCHS pop
income_us$popsum[income_us$fips=='46102' & income_us$year < 2010 & income_us$year >= 1990] <- popsum$popsum[popsum$fips=='46113' & popsum$year < 2010]



##############

# assign same pc to split counties in statefips 55 and 51

separatecty <- function(income_us,pop,fipssplit) {
  test <- subset(income_us,fips%in%fipssplit[1])
  #testrep <- data.frame(summarise(group_by(subset(pop,fips%in% fipssplit[-1]),fips,year),popsum=sum(pop)))
  testrep <- subset(popsum,fips %in% fipssplit[-1])
  test_join <- left_join(testrep,test,by='year')
  test_join <- test_join[,c(1,2,5,6,7,3)]
  colnames(test_join) <- c('fips','year','total','pop','pc','popsum')
  #test2 <- rbind(subset(income_us,fips!=fipssplit[1]),test_join)
  income_us <- rbind(subset(income_us,fips!=fipssplit[1]),test_join)
}










##############

# separatecty <- function(income_us,pop,fipssplit) {
#   test <- subset(income_us,fips%in%fipssplit[1])
#   test_join <- left_join(test,subset(pop,fips%in% fipssplit[-1]),by='year')
#   test_popsumsplit <- summarise(group_by(test_join,year),popsumcty=sum(popsum.y))
#   test_join <- left_join(test_join,test_popsumsplit)
#   test_join$pcsplit <- test_join$pc/test_join$popsumcty*test_join$popsum.y
#   test_attach <- test_join[,c('fips.y','year','total','pop','pcsplit','popsum.y')]
#   colnames(test_attach) <- c('fips','year','total','pop','pc','popsum')
#   test_attach <- test_attach[order(test_attach$fips),]
#   test_attach <- subset(test_attach,is.na(pc)==FALSE)
#   #test_attach <- test_attach[order(test_attach$year),]
#   income_us <- rbind(subset(income_us,fips!=fipssplit[1]),test_attach)
#   return(income_us)
# }

#subset(income_us,fips=='55901')$pc


## remove 55901 and write part of data to 55078 and 55115 
income_us_plot <- subset(income_us,fips %in% c('55078','55115','55901'))
ggplot(income_us_plot,aes(x=year,y=pc)) + geom_point(aes(color=fips))
source('P:/code/functions/ggsave_a4.R')
ggsave_a4('P:/plot/us_counties/personal_income_missing_data_nchspop.pdf')

income_fill <- subset(income_us,fips=='55901')
income_fill <- subset(income_fill,is.na(pc)==FALSE)

income_us[income_us$fips=='55078' & income_us$year %in% income_fill$year,c('total','pop','pc')] <- income_fill[,c('total','pop','pc')]
income_us[income_us$fips=='55115' & income_us$year %in% income_fill$year,c('total','pop','pc')] <- income_fill[,c('total','pop','pc')]
income_us <- subset(income_us,fips!='55901')


# fipssplit <- c('55901','55115','55078')
# income_us <- separatecty(income_us,popsum,fipssplit)


# test2 <- separatecty(income_us,popsum,fipssplit)
# test2trim <- subset(test2,fips%in%fipssplit[-1])
# test2trim <- subset(test2trim,is.na(pc)==FALSE)
# test2trim <- test2trim[order(test2trim$fips,test2trim$year),]
# income_us <- rbind(test2,test2trim)



fipssplit <- c('51901','51540','51003')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51903','51005','51580')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51907','51015','51790','51820')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51911','51031','51680')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51913','51035','51640')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51918','51053','51570','51730')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51919','51059','51600','51610')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51921','51069','51840')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51923','51081','51595')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51929','51089','51690')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51931','51095','51830')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51933','51121','51750')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51939','51143','51590')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51941','51670','51149')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51942','51153','51683','51685')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51944','51161','51775')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51945','51163','51530','51678')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51947','51165','51660')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51949','51175','51620')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51951','51177','51630')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51953','51191','51520')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51955','51195','51720')
income_us <- separatecty(income_us,popsum,fipssplit)
fipssplit <- c('51958','51199','51735')
income_us <- separatecty(income_us,popsum,fipssplit)

income_us$totnew <- income_us$pc*income_us$popsum



# income_us_nan <- subset(income_us,year >= 1982 & year <= 2013 & substr(income_us$fips,3,5)!='000')
# income_us_nan <- subset(income_us_nan,is.na(income_us_nan$pc)==TRUE)
# income_us_nan_fix <- subset(income_us_nan,fips %in% unique(income_us_nan$fips)[substr(unique(income_us_nan$fips),1,2)!='02'])



## look at differences in population between dataset and kyle pop
income_us_plot_pop <- subset(income_us,substr(income_us$fips,3,5)!='000')
popdiff <- subset(income_us_plot_pop,pop/popsum > 2)
colnames(income_us_plot_pop)[c(4,6)] <- c('pop_bae','pop_kyle')
colnames(popdiff)[c(4,6)] <- c('pop_bae','pop_kyle')

ggplot(income_us_plot_pop,aes(x=pop_bae,y=pop_kyle)) + geom_point(na.rm = TRUE) + geom_point(data=popdiff,aes(colour='red'))
ggsave_a4('P:/plot/us_counties/personalincome_pop_comparison_nchspop.pdf')

# load sc info 
file_sclist_orig <- 'P:/data/us_county_data/scfips_correctid_v2.dta'
scloc.sc <- read.dta(file_sclist_orig)
scloc.df.sc <- data.frame(lapply(scloc.sc, as.character), stringsAsFactors=FALSE)

# merge income to supercounties 
for (nr in 1:nrow(scloc.df.sc)) {
  getfips <- scloc.df.sc[nr,scloc.df.sc[nr,]!=""]
  colnamefips <- colnames(getfips)
  findfips <- getfips[grep('fips.*',colnamefips)]
  findfips[] <- lapply(findfips,as.character)
  income_us$fips[income_us$fips %in% findfips] <- as.character(unlist(getfips[1]))
}
income_us_sc <- data.frame(summarise(group_by(income_us,fips,year),inc_pc_sc=sum(totnew,na.rm=TRUE)/sum(popsum,na.rm=TRUE)))



#### checking output ###





# save output 

save(income_us_sc,file='P:/data/income/income_with_sc_nchspop_v2_2015incl')


####### read in SAIPE data #######
# adjust for supercounties
# adjust for merged counties 

# compare data with BEA data 





