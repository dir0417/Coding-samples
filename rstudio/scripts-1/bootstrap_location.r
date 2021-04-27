# RProject8_3_bootstrap_location.r
#	Problem 10.26 of Rice
#	Hampson and Walker data on heats of sublimation of
#		platinum,iridium, and rhodium

# To install these packages, uncomment the next two lines
#install.packages("MASS")
#install.packages("boot")

library(MASS)
library(boot)

x.platinum=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/platinum.txt")
x.iridium=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/iridium.txt")
x.rhodium=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/rhodium.txt")

# Parts (a)-(d)
x=x.platinum
hist(x)
stem(x)
boxplot(x)
plot(x)

# (e)
# Measurements 8,9,10 are all very high as are measurements 14 and 15
# The data do not appear indepenent

# (f) Measures of location
mean(x); mean(x,trim=.1);mean(x,trim=.2);median(x); huber(x,k=1.5)[[1]]

# (g) Standard error of the sample mean and approximate 90 percent conf. interval
x.stdev=sqrt(var(x))
x.mean.sterr=x.stdev/sqrt(length(x))

print(x.mean.sterr)

alpha=0.10
z.upperalphahalf=qnorm(1-alpha/2)
x.mean=mean(x)
x.mean.ci<-c(-1,1)*z.upperalphahalf* x.mean.sterr + x.mean
print(x.mean); print(x.mean.ci)


# parts (i), (j), (k).
# Bootstrap confidence intervals of different location measures


fcn.median<- function(x, d) {
  return(median(x[d]))
}
fcn.mean<- function(x, d) {
  return(mean(x[d]))
}

fcn.trimmedmean <- function(x, d, trim=0) {
  return(mean(x[d], trim/length(x)))
}

fcn.huber<-function(x,d){
  x.huber=huber(x[d],k=1.5)
  return(x.huber[[1]])
}

# Bootstrap analysis of sample mean:
x.boot.mean= boot(x, fcn.mean, R=1000)      
plot(x.boot.mean)
print(x.boot.mean$t0)
print(sqrt(var(x.boot.mean$t)))
boot.ci(x.boot.mean,conf=.95, type="basic")
title(paste("Sample Mean:  StDev=",as.character(round(sqrt(var(x.boot.mean$t)),digits=4)),sep=""),
      adj=1)


# Bootstrap analysis of sample median:
x.boot.median= boot(x, fcn.median, R=1000)      
plot(x.boot.median)
title(paste("Median:  StDev=",as.character(round(sqrt(var(x.boot.median$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.median$t0)
print(sqrt(var(x.boot.median$t)))
boot.ci(x.boot.median,conf=.95, type="basic")


# Bootstrap analysis of sample trimmed mean:
x.boot.trimmedmean= boot(x, fcn.trimmedmean,trim=.2, R=1000)      
plot(x.boot.trimmedmean)
title(paste("Trimmed Mean:  StDev=",as.character(round(sqrt(var(x.boot.trimmedmean$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.trimmedmean$t0)
print(sqrt(var(x.boot.trimmedmean$t)))
boot.ci(x.boot.trimmedmean,conf=.95, type="basic")




# Bootstrap analysis of Huber M estimate:
x.boot.huber= boot(x, fcn.huber, R=1000)      
plot(x.boot.huber)
title(paste("Huber M Est:  StDev=",as.character(round(sqrt(var(x.boot.huber$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.huber$t0)
print(sqrt(var(x.boot.huber$t)))
boot.ci(x.boot.huber,conf=.95, type="basic")

