# sex lookups
sex.filter <- c('male','female')
sex.filter2 <- c('Male','Female')
sex.lookup <- c('Men','Women')
sex.lookup2 <- c('Men','Women')

# age lookups
age.print <- as.vector(levels(factor(levels=c('0-4','5-14','15-24','25-34','35-44','45-54','55-64','65-74','75-84','85+'))))
age.code <- data.frame(age=c(0,5,15,25,35,45,55,65,75,85),age.print=age.print)
age.filter <- c(0,5,15,25,35,45,55,65,75,85)

# age colours
age.colours <- c('#FF1493','#B8860B','#808080','#00BFFF','#00CED1')
age.colours <- c(age.colours,'#66CDAA','#9ACD32','#ADFF2F','#9932CC','#FF8C00')
age.colours=c("blue",brewer.pal(9,"BrBG")[c(9:6,4:1)],"grey")

# to correct colours
f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", "#8A7C64", "#599861" )
#to make picking the number of the colour you want easier:
#plot(1:length(mycols),col=mycols[1:length(mycols)],cex=4,pch=20); abline(v=c(10,20,30,40,50,60))


# colors for causes of death
colors.broad.cod = mycols[c(    14,  # Cancer
                                9,  # Cardio
                                24, # Injuries
                                11)] # Other

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
dat.dict = data.frame(metric=c('meanc3','number_of_min_3_day_below_nonnormal_90_downwaves_2',
                                'number_of_min_3_day_above_nonnormal_90_upwaves_2',
                                'number_of_min_3_day_below_+5_jumpdownwaves_2',
                                'number_of_min_3_day_above_+5_jumpupwaves_2',
                                'number_of_days_above_nonnormal_90_2',
                                'number_of_days_below_nonnormal_90_2',
                                'number_of_days_above_+5_2',
                                'number_of_days_below_-5_2',
                                'sd'),
name=c('Mean','RCA','RWA','ACA','AWA','DA90','DB10','DA+5','DB-5','SD'),
order=c(1,2,6,4,5,3,7,8,9,10))

# wavelet noise lookup
noise.lookup <- noise.lookup <- c('white_noise','red_noise')

# models to choose from
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0')
#model <- models[model]

# climate regions
region.lookup=c("Northwest","West_North_Central", "Northeast",
                "Upper_Midwest","East_North_Central", "West",
                "Southwest", "South", "Southeast")

# load intentional and unintentional lookup
dat.injuries.lookup = read.csv('~/git/mortality/USA/state/data/cod/intentional_injuries_lookup.csv')

icd9.in = dat.injuries.lookup$icd9in = gsub("\\.", "", dat.injuries.lookup$icd9in)
icd9.in = icd9.in[icd9.in != ""]
icd9.in = data.frame(cause=icd9.in,cause.group='Intentional')

icd9.un = dat.injuries.lookup$icd9un = gsub("\\.", "", dat.injuries.lookup$icd9un)
icd9.un = icd9.un[icd9.un != ""]
icd9.un = data.frame(cause=icd9.un,cause.group='Unintentional')

icd9.lookup = rbind(icd9.in,icd9.un)
icd9.lookup$cause = gsub(" ", "", icd9.lookup$cause)

icd10.in = dat.injuries.lookup$icd10in = gsub("\\.", "", dat.injuries.lookup$icd10in)
icd10.in = icd10.in[icd10.in != ""]
icd10.in = data.frame(cause=icd10.in,cause.group='Intentional')

icd10.un = dat.injuries.lookup$icd10un = gsub("\\.", "", dat.injuries.lookup$icd10un)
icd10.un = icd10.un[icd10.un != ""]
icd10.un = data.frame(cause=icd10.un,cause.group='Unintentional')

icd10.lookup = rbind(icd10.in,icd10.un)
icd10.lookup$cause = gsub(" ", "", icd10.lookup$cause)

# COD look-up
cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
                            cause.group=c('Other','Other','Cancer','Cancer','Other', # A-E
                                        'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
                                        'Other','Other','Other','Other','Other', # K-O
                                        'Other','Other','Other','External','External', # P-T
                                        'Other','External','External','External','External', # U-Y
                                        'External')) # Z

# ICD10 coding for intentional injuries
#intentional = c('X60','X61','X62','X63','X64','X65','X66','X67','X68','X69',
# 'X70','X71','X72','X73','X74','X741','X742','X743','X744','X749',
# 'X75','X76','X77','X78','X79','X80','X81','X82','X83','X84',
# 'X85','X86', 'X87','X88','X89','X90','X91','X92','X93','X94',
# 'X95','X951', 'X952','X953','X954','X959','X96','X97','X98','X99',
# 'Y00','Y01', 'Y02','Y03','Y04','Y05','Y06','Y060','Y061','Y062',
# 'Y068','Y069', 'Y07','Y070','Y071','Y072','Y073','Y078','Y079','Y08',
# 'Y09','Y35', 'Y350','Y351','Y352','Y353','Y354','Y355','Y356','Y357',
# 'U011','Y36', 'Y360','Y361','Y362','Y363','Y364','Y365','Y366','Y367',
# 'Y368','Y369', 'Y870','Y871', 'Y890','Y891')

# ICD10 coding for unintentional injuries
unintentional = c()

# cod lookups
cod.broad = c('Allcause','Cancer','Cardiopulmonary','External','Other')

# ASDR weightings
# from http://apps.who.int/healthinfo/statistics/mortality/whodpms/definitions/pop.htm
StdPopMF =      c(1822+7033,        # 0-4
                8687+8597,          # 5-14
                8474+8222,          # 15-24
                7928+7605,          # 25-34
                7145+6590,          # 35-44
                6038+5371,          # 45-54
                4547+3723,          # 55-64
                2955+2210,          # 65-74
                1515+905,           # 75-84
                632)                # 85+
StdPopMF = data.frame(age=age.filter,weight=StdPopMF)


