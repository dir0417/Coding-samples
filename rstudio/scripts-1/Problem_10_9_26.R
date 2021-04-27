# Problem_10_9_26.r
#	Problem 10.26 of Rice
#	Hampson and Walker data on heats of sublimation of
#		platinum,iridium, and rhodium

# To install these packages, uncomment the next two lines
#install.packages("MASS")
#install.packages("boot")

library(MASS)
library(boot)
# 
x.platinum=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/platinum.txt")
x.iridium=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/iridium.txt")
x.rhodium=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/rhodium.txt")

# Parts (a)-(d)
x=x.platinum
par(mfcol=c(2,2))
hist(x,main="Platinum")
stem(x)
boxplot(x)
plot(x)


x=x.rhodium
par(mfcol=c(2,2))
hist(x,main="Rhodium")
stem(x)
boxplot(x)
plot(x)

x=x.iridium
par(mfcol=c(2,2))
hist(x,main="Iridium")
stem(x)
boxplot(x)
plot(x)
plot(c(10:length(x)), x[10:length(x)], main="Iridium")
# (e) For Platinum, observations 8,9,10, and 14,5 seem much higher than the rest.
#     They do not seem iid

# (e) For Rhodium,  the first 2 observations are very variable (low then high).
#  and the observations after about number 25 seem different from those before.
#  These data do not seem iid either.

# (e) For Iridium,  the first 4 observations steadily increase and observaton 8 seems much higher
# than the rest.  These data do not appear iid. Also, plotting the observations from 10 on,
# there seems to be time series dependence -- closer observations in the sequence have more similar
# values.



# (f) Measures of location
#   For Rhodium
x=x.rhodium
mean(x); mean(x,trim=.1);mean(x,trim=.2);median(x); huber(x,k=1.5)[[1]]
# All the estimates are very close
# The mean is the lowest, it is affected by the lowest value
# The trimmed means exclude the extreme values and are similar to the median.

#   For Iridium
x=x.iridium
mean(x); mean(x,trim=.1);mean(x,trim=.2);median(x); huber(x,k=1.5)[[1]]
# The mean is much lower than the other estimates
# The mean is the lowest, it is affected by the lowest value
# The trimmed means exclude the extreme values and are similar to the median.


# (g) Standard error of the sample mean and approximate 90 percent conf. interval
# First for rhodium: 
x=x.rhodium
x.stdev=sqrt(var(x))
x.mean.sterr=x.stdev/sqrt(length(x))

print(x.mean.sterr)

alpha=0.10
z.upperalphahalf=qnorm(1-alpha/2)
x.mean=mean(x)
x.mean.ci<-c(-1,1)*z.upperalphahalf* x.mean.sterr + x.mean
print(x.mean); print(x.mean.ci)

stats1.rhodium=c(x.mean, x.mean.ci)

# Second for iridium
x=x.iridium
x.stdev=sqrt(var(x))
x.mean.sterr=x.stdev/sqrt(length(x))

print(x.mean.sterr)

alpha=0.10
z.upperalphahalf=qnorm(1-alpha/2)
x.mean=mean(x)
x.mean.ci<-c(-1,1)*z.upperalphahalf* x.mean.sterr + x.mean
print(x.mean); print(x.mean.ci)

stats1.iridium=c(x.mean, x.mean.ci)


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

#  First for rhodium
set.seed(1)
x=x.rhodium

# Bootstrap analysis of sample 20% trimmed mean:
x.boot.trimmedmean.2= boot(x, fcn.trimmedmean,trim=.2, R=1000)      
par(mfcol=c(1,2))
plot(x.boot.trimmedmean.2)
title(paste("Trimmed Mean:  StDev=",as.character(round(sqrt(var(x.boot.trimmedmean.2$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.trimmedmean.2$t0)
print(sqrt(var(x.boot.trimmedmean.2$t)))
stats2.trim20.rhodium<-c(x.boot.trimmedmean.2$t0,(sqrt(var(x.boot.trimmedmean.2$t))))
#
boot.ci(x.boot.trimmedmean.2,conf=.90, type="basic")

# Bootstrap analysis of sample 10% trimmed mean:
x.boot.trimmedmean.1= boot(x, fcn.trimmedmean,trim=.1, R=1000)      
par(mfcol=c(1,2))
plot(x.boot.trimmedmean.1)
title(paste("Trimmed Mean:  StDev=",as.character(round(sqrt(var(x.boot.trimmedmean.1$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.trimmedmean.1$t0)
print(sqrt(var(x.boot.trimmedmean.1$t)))

stats2.trim10.rhodium<-c(x.boot.trimmedmean.1$t0,(sqrt(var(x.boot.trimmedmean.1$t))))
#
#
boot.ci(x.boot.trimmedmean.1,conf=.90, type="basic")

# Bootstrap analysis of sample median:
x.boot.median= boot(x, fcn.median, R=1000)      
plot(x.boot.median)
title(paste("Median:  StDev=",as.character(round(sqrt(var(x.boot.median$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.median$t0)
print(sqrt(var(x.boot.median$t)))
boot.ci(x.boot.median,conf=.90, type="basic")
stats2.median.rhodium<-c(x.boot.median$t0,(sqrt(var(x.boot.median$t))))
                         
stats1.rhodium
# The bootstrap estimate and standard errrors are given by:
stats2.trim20.rhodium
stats2.trim10.rhodium
stats2.median.rhodium
# For Rhodium these are all about the same

#  (k). The approximate 90% confidence intervals are:


boot.ci(x.boot.trimmedmean.2,conf=.90, type="basic")

boot.ci(x.boot.trimmedmean.1,conf=.90, type="basic")

boot.ci(x.boot.median,conf=.90, type="basic")

# These are all essential equal for rhodium

# These intervals are all about the same as that based on part (g)
stats1.rhodium

# 
##############
# Second for iridium

set.seed(1)
x=x.iridium

# Bootstrap analysis of sample 20% trimmed mean:
x.boot.trimmedmean.2= boot(x, fcn.trimmedmean,trim=.2, R=1000)      
par(mfcol=c(1,2))
plot(x.boot.trimmedmean.2)
title(paste("Trimmed Mean:  StDev=",as.character(round(sqrt(var(x.boot.trimmedmean.2$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.trimmedmean.2$t0)
print(sqrt(var(x.boot.trimmedmean.2$t)))
stats2.trim20.iridium<-c(x.boot.trimmedmean.2$t0,(sqrt(var(x.boot.trimmedmean.2$t))))
#
boot.ci(x.boot.trimmedmean.2,conf=.90, type="basic")

# Bootstrap analysis of sample 10% trimmed mean:
x.boot.trimmedmean.1= boot(x, fcn.trimmedmean,trim=.1, R=1000)      
par(mfcol=c(1,2))
plot(x.boot.trimmedmean.1)
title(paste("Trimmed Mean:  StDev=",as.character(round(sqrt(var(x.boot.trimmedmean.1$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.trimmedmean.1$t0)
print(sqrt(var(x.boot.trimmedmean.1$t)))

stats2.trim10.iridium<-c(x.boot.trimmedmean.1$t0,(sqrt(var(x.boot.trimmedmean.1$t))))
#
#
boot.ci(x.boot.trimmedmean.1,conf=.90, type="basic")

# Bootstrap analysis of sample median:
x.boot.median= boot(x, fcn.median, R=1000)      
plot(x.boot.median)
title(paste("Median:  StDev=",as.character(round(sqrt(var(x.boot.median$t)),digits=4)),sep=""),
      adj=1)
print(x.boot.median$t0)
print(sqrt(var(x.boot.median$t)))
boot.ci(x.boot.median,conf=.90, type="basic")
stats2.median.iridium<-c(x.boot.median$t0,(sqrt(var(x.boot.median$t))))

stats1.iridium
stats2.trim20.iridium
stats2.trim10.iridium
stats2.median.iridium

##############
# The bootstrap estimate and standard errrors are given by:
stats2.trim20.iridium
stats2.trim10.iridium
stats2.median.iridium
# For iridium the median has much lower st error than the trimmed means

#  (k). The approximate 90% confidence intervals are:

boot.ci(x.boot.trimmedmean.2,conf=.90, type="basic")

boot.ci(x.boot.trimmedmean.1,conf=.90, type="basic")

boot.ci(x.boot.median,conf=.90, type="basic")

# The interval using the median is much smaller than that for the trimmed means
# These intervals are all smaller than that based on part (g)
stats1.iridium