rm(list=ls())

require(WaveletComp)

# create output directories
ifelse(!dir.exists("../../output/wavelet/test"), dir.create("../../output/wavelet/test",recursive=TRUE), FALSE)

# 1. test with different constant amplitudes
x <- periodic.series(start.period = 50, length = 200)

my.data = data.frame(x = x)
my.w = analyze.wavelet(my.data, "x",
loess.span = 0,
dt = 1, dj = 1/250,
lowerPeriod = 16,
upperPeriod = 128,
make.pval = T, n.sim = 10)

# set up grid and plot
pdf(paste0('../../output/wavelet/test/attenuation_test_1.pdf'),paper='a4r',height=0,width=0)
layout(rbind(c(1,1,4),c(3,3,2)))
plot(x)
wt.avg(my.w)
wt.image(my.w, color.key = "quantile", n.levels = 250,
legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()

# 2. again with different amplitude
x <- periodic.series(start.period = 50, length = 200)
x <- 2 * x

my.data = data.frame(x = x)
my.w = analyze.wavelet(my.data, "x",
loess.span = 0,
dt = 1, dj = 1/250,
lowerPeriod = 16,
upperPeriod = 128,
make.pval = T, n.sim = 10)

# set up grid and plot
pdf(paste0('../../output/wavelet/test/attenuation_test_2.pdf'),paper='a4r',height=0,width=0)
layout(rbind(c(1,1,4),c(3,3,2)))
plot(x)
wt.avg(my.w)
wt.image(my.w, color.key = "quantile", n.levels = 250,
legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()

# 3. test with attenuating amplitude over time
x <- periodic.series(start.period = 50, length = 1000)
for(i in seq(length(x))){ x[i] = exp(-i/50)*x[i]}
x=x[c(1:200)]

my.data = data.frame(x = x)
my.w = analyze.wavelet(my.data, "x",
loess.span = 0,
dt = 1, dj = 1/250,
lowerPeriod = 16,
upperPeriod = 128,
make.pval = T, n.sim = 10)

# set up grid and plot
pdf(paste0('../../output/wavelet/test/attenuation_test_3.pdf'),paper='a4r',height=0,width=0)
layout(rbind(c(1,1,4),c(3,3,2)))
plot(x)
wt.avg(my.w)
wt.image(my.w, color.key = "quantile", n.levels = 250,
legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()

# 4. test with attenuating amplitude over time but with different starting amplitude
x <- periodic.series(start.period = 50, length = 1000)
x <- 2 * x
for(i in seq(length(x))){ x[i] = exp(-i/50)*x[i]}
x=x[c(1:200)]

my.data = data.frame(x = x)
my.w = analyze.wavelet(my.data, "x",
loess.span = 0,
dt = 1, dj = 1/250,
lowerPeriod = 16,
upperPeriod = 128,
make.pval = T, n.sim = 10)

# set up grid and plot
pdf(paste0('../../output/wavelet/test/attenuation_test_4.pdf'),paper='a4r',height=0,width=0)
layout(rbind(c(1,1,4),c(3,3,2)))
plot(x)
wt.avg(my.w)
wt.image(my.w, color.key = "quantile", n.levels = 250,
legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()
