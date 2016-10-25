rm(list=ls())

require(WaveletComp)

# test with attenuating amplitude over time
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

# set up grid plot
pdf(paste0('../../output/wavelet/attenuation_test.pdf'),paper='a4r',height=0,width=0)
layout(matrix(c(1,2,1,2), 2, 2, byrow = TRUE),widths=c(3,1), heights=c(1,2))
wt.avg(my.w)
wt.image(my.w, color.key = "quantile", n.levels = 250,
legend.params = list(lab = "wavelet power levels", mar = 4.7))
dev.off()
