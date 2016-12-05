# using poisson glm output to give se of maxmonth/minmonth rate ratio
# simulate some data
popsJan <- c(rnorm(30,100000,10000))
deathrateJan <- c(rnorm(30,0.005,0.0005))
par(mfrow=c(3,2)); hist(popsJan); hist(deathrateJan)

popsAug <- popsJan+c(rnorm(30,0,100))
deathrateAug <- c(rnorm(30,0.004,0.0005))
hist(popsAug); hist(deathrateAug)

hist(deathrateJan/deathrateAug)

#create the actual death counts, pops and marker for max or min month
deaths <- c(popsJan*deathrateJan, popsAug*deathrateAug)
pops <- c(popsJan,popsAug)
maxmonth <- c(rep(1,30),rep(0,30))

##########
# put into single dataframe for easier subsetting
dat <- data.frame(pops=pops, deaths=deaths, maxmonth=maxmonth)

# create empty vectors for storing results
out_se <- out_coef <- 1:30*NA

# loop over years
# each year, carry out poisson glm with maxmonth as covariate, hence using correct poisson error structure and exponential of
# the maxmonth coeffiecient gives the rate ratio which you would have had before
for(im in 1:30){
	summary(mod <- glm(deaths ~ maxmonth + offset(log(pops)), family=poisson, data=dat[c(im,30+im),]))
	#store the coefficients and se's - have hardcoded the [2,1] and [2,2] because it's simple and I'm lazy, just have to test your own version to make sure correct
	out_coef[im] <- summary(mod)$coefficients[2,1]
	out_se[im] <- summary(mod)$coefficients[2,2]
}

#now carry out lm with 30 max/min ratios and use associated se's so that those with wider se will have less influence in the lm. 
#note you have to exponentiate the coef to get the rate ratios, otherwise they are on the log scale.
#there is a slight dodginess in that the se's are still on the log scale - se's should only be used on the "scale" they were generated on, so best to leave them unlogged here.
#since they are close to 0 it's not going to make much/any difference.
#using 1/var as weights seems to be the most common, did see a few where they used 1/se but definitely less common.
years <- 1:30
summary(mod2 <- lm(exp(out_coef)  ~ years, weights=1/(out_se*out_se)))

#the coefficient and p-value for years is what you plot and what gives you whether the slope is significant.
#the position of the points should definitely be very very close to what you already have.















