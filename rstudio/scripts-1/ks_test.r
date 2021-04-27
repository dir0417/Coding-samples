#RProject8_2_ks_test.r
set.seed(0)

par(mfcol=c(2,2))

#   2x2 Panel of Plots

#  Generate random sample from N(0,1) distribution
# 
x=rnorm(50)

# 1.1 Index plot of sample
plot(x)

# 1.2 Normal QQ Plot of sample
qqnorm(x)

# 1.3 Empirical CDF of sample

plot.ecdf(x)

# 1.4 Theoretical and empirical CDF together
par(mfcol=c(1,1))
x=rt(50,df=15)
plot.ecdf(x) 

x.grid=seq(-3,3,.001)
x.grid.pnorm<-pnorm(x.grid)
lines(x.grid,x.grid.pnorm,col='green',type="l")

# Compute Kolmogorov Smirnov Test Statistic
x.ecdf<-ecdf(x)
x.ecdf(x)
KSstat=max(abs(x.ecdf(x)-pnorm(x)))
x.KSstat=x[KSstat==abs(x.ecdf(x)-pnorm(x))]
abline(v=x.KSstat, col='gray')
title(paste("KS-stat = ",as.character(round(KSstat,digits=3)),sep=""),adj=1)

# 2.  Apply R function ks.test() to sample
ks.test(x, y="pnorm")
