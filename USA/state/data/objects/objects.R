library(RColorBrewer)

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
# plot(1:length(mycols),col=mycols[1:length(mycols)],cex=4,pch=20); abline(v=c(10,20,30,40,50,60))


# colors for broad causes of death
colors.broad.cod = mycols[c(    24,  # Other
                                40,  # Injuries
                                57,  # Cancer
                                62)]   # Cardio

# colors for sub-causes of death
colors.injuries = mycols[c(     25,  # Unintentional
                                53,  # Intentional
                                33)] # Other

colors.subinjuries = mycols[c(
                                6,   # Transport
                                13,  # Falls
                                36,  # Drownings
                                33,  # Other injuries
                                12,  # Assault
                                20)] # Intentional self-harm

colors.cardio = mycols[c(
                                55,   # Ischaemic
                                15,   # Cerebrovascular
                                7,    # Other cardiovascular/heart diseases
                                48,    # COPD
                                62,   # Respiratory infections
                                25)]  # Other respiratory diseases

# original cardio colour scheme
# colors.cardio = mycols[c(
#                                 1,   # Ischaemic
#                                 11,  # Cerebrovascular
#                                 33,  # Other cardiovascular
#                                 44,  # COPD
#                                 10,  # Respiratory infections
#                                 24)] # Other respiratory diseases

# colors for years progression
colors.years = mycols[c(        9,   # Beginning
                                21,  # Middle
                                10)] # End

# colors for months
colors.months = mycols[c(       1,   # Jan
                                2,   # Feb
                                3,   # Mar
                                4,   # Apr
                                5,   # May
                                6,   # Jun
                                7,   # Jul
                                8,   # Aug
                                9,   # Sep
                                10,  # Oct
                                11,  # Nov
                                12)] # Dec

# month lookups
month.lookup <- c('January','February','March','April','May','June','July','August','September','October','November','December')
month.short <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
month.short.2 <- c('Jan','','Mar','','May','','Jul','','Sep','','Nov','')
month.names <- c('January','February','March','April','May','June',
'July','August','September','October','November','December')

# fips lookups
fips.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')
state.lookup <- read.csv('../../data/fips_lookup/name_fips_lookup.csv')

# INLA model versions
models <- c('1','1a','2','2a','3','3a','4','1b','1c','1d','1e','1f','1de','1ef','1g','0','minus1','1d2','1d3','1d4','0a','0b','1d5','1d6','1d7','1d8','1d9','1d10','1d11')

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

# wavelet noise and loglookup
noise.lookup <- c('white_noise','red_noise')
log.lookup = c('no_log','log')

# climate regions
region.lookup=c("Northwest","West_North_Central", "Northeast",
                "East_North_Central","Central", "West",
                "Southwest", "South", "Southeast")

# load intentional and unintentional lookup (with poisoning and unclear variables added to unintentional)
dat.injuries.lookup = read.csv('~/git/mortality/USA/state/data/cod/intentional_injuries_lookup_unintentional_edit.csv')

icd9.in = dat.injuries.lookup$icd9in = gsub("\\.", "", dat.injuries.lookup$icd9in)
icd9.in = gsub("E", "", icd9.in)

icd9.in = icd9.in[icd9.in != ""]
icd9.in = data.frame(cause=icd9.in,cause.group='Intentional')

icd9.un = dat.injuries.lookup$icd9un = gsub("\\.", "", dat.injuries.lookup$icd9un)
icd9.un = gsub("E", "", icd9.un)
icd9.un = icd9.un[icd9.un != ""]
icd9.un = data.frame(cause=icd9.un,cause.group='Unintentional')

icd9.lookup = rbind(icd9.in,icd9.un)
icd9.lookup$cause = gsub(" ", "", icd9.lookup$cause)
icd9.lookup$cause[nchar(icd9.lookup$cause)==3] = paste0(icd9.lookup$cause[nchar(icd9.lookup$cause)==3],'0')
icd9.lookup$cause[nchar(icd9.lookup$cause)>=5] = substr(icd9.lookup$cause[nchar(icd9.lookup$cause)>=5],1,4)
icd9.lookup = unique(icd9.lookup[,c(1:2)])

icd10.in = dat.injuries.lookup$icd10in = gsub("\\.", "", dat.injuries.lookup$icd10in)
icd10.in = icd10.in[icd10.in != ""]
icd10.in = data.frame(cause=icd10.in,cause.group='Intentional')

icd10.un = dat.injuries.lookup$icd10un = gsub("\\.", "", dat.injuries.lookup$icd10un)
icd10.un = icd10.un[icd10.un != ""]
icd10.un = data.frame(cause=icd10.un,cause.group='Unintentional')

icd10.lookup = rbind(icd10.in,icd10.un)
icd10.lookup$cause = gsub(" ", "", icd10.lookup$cause)

icd10.lookup$cause = as.character(icd10.lookup$cause)
icd10.lookup$cause[nchar(icd10.lookup$cause)==3] <- paste0(icd10.lookup$cause[nchar(icd10.lookup$cause)==3],'0')

icd10.lookup = unique(icd10.lookup[,c(1:2)])


# COD look-up
cod.lookup.10 <- data.frame(letter=as.character(toupper(letters)),
                            cause.group=c('Other','Other','Cancer','Other','Other', # A-E
                                        'Other','Other','Other','Cardiopulmonary','Cardiopulmonary', # F-J
                                        'Other','Other','Other','Other','Other', # K-O
                                        'Other','Other','Other','External','External', # P-T
                                        'Other','External','External','External','External', # U-Y
                                        'External')) # Z

# cod lookups
cod.broad = c('AllCause','Cancer','Cardiopulmonary','External','Other')
cod.cardio = c('Cardiovascular','Respiratory infections','Chronic respiratory diseases')
cod.injuries = c('Unintentional','Intentional')
cod.other = c('Endocrine disorders','Genitourinary diseases','Maternal conditions','Perinatal conditions','Substance use disorders','Neuropsychiatric disorders')

# ASDR weightings
# from http://apps.who.int/healthinfo/statistics/mortality/whodpms/definitions/pop.htm
# and https://www.who.int/healthinfo/paper31.pdf
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

# fix cause of death names
# cod.print = ifelse(cod.arg=='AllCause','all cause',
#             ifelse(cod.arg=='Cancer', 'cancer',
#             ifelse(cod.arg=='Cardiopulmonary', 'cardiorespiratory',
#             ifelse(cod.arg=='External', 'injuries',
#             ifelse(cod.arg=='Unintentional','unintentional',
#             ifelse(cod.arg=='Intentional','intentional',
#             ifelse(cod.arg=='Other', 'other',
#             ifelse(cod.arg=='Cardiovascular','cardiovascular',
#             ifelse(cod.arg=='Chronic respiratory diseases','chronic respiratory diseases',
#             ifelse(cod.arg=='Respiratory infections',"respiratory infections",
#             ifelse(cod.arg=='Endocrine disorders','endocrine disorders',
#             ifelse(cod.arg=='Genitourinary diseases','genitourinary diseases',
#             ifelse(cod.arg=='Maternal conditions','maternal conditions',
#             ifelse(cod.arg=='Neuropsychiatric disorders', 'neuropsychiatric disorders',
#             ifelse(cod.arg=='Perinatal conditions','perinatal conditions',
#             ifelse(cod.arg=='Substance use disorders','substance use disorders'))))))))))))))))
#

