# function to find centre of mass of seasonality
circular.age.mean <- function(age.selected,sex.selected) {

    print(paste0('Working on national inverse COM method 1 for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat <- subset(dat,age==age.selected & sex==sex.selected)

    # take months column and repeat death column times
    dat <- rep(dat$month,round(dat$deaths.inv))

    # convert months -> radians
    conv <- 2*pi/12

    # find circular mean, where 1 is January and 0 is December.
    dat.mean <- circ.mean(conv*(dat))/conv
    dat.mean <- (dat.mean + 12) %% 12

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat, replace = T)
        dat.temp.mean <- circ.mean(conv*sample)/conv
        dat.temp.mean <- (dat.temp.mean + 12) %% 12
        print(dat.temp.mean)
        dat.sample[i] <- dat.temp.mean
    }


    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # calculate bootstrap std. error
    std.error <- sqrt(var(COM.bootstrap))

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_1/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}

# function to find centre of mass of seasonality
circular.age.mean.2 <- function(age.selected,sex.selected) {

    print(paste0('Working on national inverse COM method 2 for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected))

    return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.1 <- function(age.selected,sex.selected) {

    print(paste0('Working on national inverse COM first period method 2 for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

    # filter for years
    dat.temp <- subset(dat.temp,year %in% year.group.1)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc.split,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected,'_part1'))

    return(dat.frame)
}

# function to find centre of mass of seasonality for first period of split years
circular.age.mean.split.2 <- function(age.selected,sex.selected) {

    print(paste0('Working on national inverse COM second period method 2 for ',sex.lookup[sex.selected],' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat,age==age.selected & sex==sex.selected)

    # filter for years
    dat.temp <- subset(dat.temp,year %in% year.group.2)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,round(dat.temp$deaths.inv))

    print(length(dat.temp))

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc.split,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_',sex.lookup[sex.selected],'_',age.selected,'_part2'))

    return(dat.frame)
}

# function to find centre of mass of seasonality
circular.age.mean.rate.2 <- function(age.selected,sex.selected,cod='Allcause') {

    print(paste0('Working on national inverse COM rate method 2 for ',sex.lookup[sex.selected],' ',cod,' ',age.selected))

    # take dates as subset
    dat.temp <- subset(dat.national,age==age.selected & sex==sex.selected)

    # take months column and repeat death column times
    dat.temp <- rep(dat.temp$month,dat.temp$rate.inv)

    # convert months -> radians
    conv <- 2*pi/12
    dat.conv <- dat.temp*conv

    # find circular mean in circular world
    dat.mean <- (circ.mean(dat.conv)) %% (2*pi)

    # centre dataset around dat.mean
    dat.conv.cent <- dat.conv - dat.mean

    # create 1000 bootstrap samples
    dat.sample <- vector()
    for(i in 1:1000){
        sample <- sample(dat.conv.cent, replace = T)
        dat.temp.mean <- circ.mean(sample)
        print(dat.temp.mean/conv)
        dat.sample[i] <- dat.temp.mean
    }

    # calculate COM for each bootstrap sample
    COM.bootstrap <- sort(dat.sample)
    COM.bootstrap.5 <- COM.bootstrap[25]
    COM.bootstrap.95 <- COM.bootstrap[975]

    # decentre data and convert back to months units
    dat.mean <- (dat.mean)/conv
    COM.bootstrap.5 <- (COM.bootstrap.5/conv) + dat.mean
    COM.bootstrap.95<- (COM.bootstrap.95/conv) + dat.mean

    # compile information for output of function
    dat.frame <- data.frame(age=age.selected,sex=sex.selected,COM.mean=dat.mean,COM.5=COM.bootstrap.5,COM.95=COM.bootstrap.95)

    # output value for data processing
    file.loc.temp <- paste0(file.loc.entire,'method_2/')
    ifelse(!dir.exists(file.loc.temp), dir.create(file.loc.temp,recursive=TRUE), FALSE)
    saveRDS(dat.frame,paste0(file.loc.temp,'anti_com_rate_cod_',sex.lookup[sex.selected],'_',age.selected,'_',cod))

    return(dat.frame)
}