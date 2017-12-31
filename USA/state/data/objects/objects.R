# sex lookups
sex.filter <- c('male','female')
sex.lookup <- c('Men','Women')
sex.lookup2 <- c('Men','Women')

# age lookups
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
age.filter <- c(0,5,15,25,35,45,55,65,75,85)

# month lookups
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')

# fips lookups
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# INLA model versions
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0')

# lookups for temperature units
temp = c("10percc3", "90percc3", "meanc3")
episodes = c("number_of_min_3_day_above_+5_jumpupwaves_2", "number_of_min_3_day_above_nonnormal_90_upwaves_2", "number_of_min_3_day_below_+5_jumpdownwaves_2", "number_of_min_3_day_below_nonnormal_90_downwaves_2")
#unit.name = ifelse(metric %in% temp, paste0('°C'), ifelse(metric %in% episodes, ' episode(s)','error'))

# set color ramps
gr <- colorRampPalette(c("darkgreen","green","lightgreen"))(200)
bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
pr <- colorRampPalette(c("plum","orchid","darkmagenta"))(200)
yl <- colorRampPalette(c("lightgoldenrod", "gold","darkorange"))(200)
sm <- colorRampPalette(c("tan1","salmon2","salmon4"))(200)

# create dictionary for variables
dat.dict = data.frame(metric=c('meanc3','number_of_min_3_day_below_nonnormal_90_downwaves_2','number_of_min_3_day_above_nonnormal_90_upwaves_2','number_of_min_3_day_below_+5_jumpdownwaves_2','number_of_min_3_day_above_+5_jumpupwaves_2','number_of_days_above_nonnormal_90_2','number_of_days_below_nonnormal_10','number_of_days_above_+5_2','number_of_days_below_-5_2'),
name=c('Mean','RCA','RWA','ACA','AWA','DA90','DB10','DA+5','DB-5'),
order=c(1,2,6,4,5,3,7,8,9))

# wavelet noise lookup
noise.lookup <- noise.lookup <- c('white_noise','red_noise')

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0')
#model <- models[model]

# ICD10 coding for intentional injuries
intentional = c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
'X70','X71','X72','X73','X74','X741','X742','X743','X744','X749',
'X75','X76','X77','X78','X79','X80','X81','X82','X83','X84',
'X85','X86', 'X87','X88','X89','X90','X91','X92','X93','X94',
'X95','X951', 'X952','X953','X954','X959','X96','X97','X98','X99',
'Y00','Y01', 'Y02','Y03','Y04','Y05','Y06','Y060','Y061','Y062',
'Y068','Y069', 'Y07','Y070','Y071','Y072','Y073','Y078','Y079','Y08',
'Y09','Y35', 'Y350','Y351','Y352','Y353','Y354','Y355','Y356','Y357',
'U011','Y36', 'Y360','Y361','Y362','Y363','Y364','Y365','Y366','Y367',
'Y368','Y369', 'Y870','Y871', 'Y890','Y891')