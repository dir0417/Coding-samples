# Problem_14_9_39.r

# 1.0 Read in data ----
#       See Problem 14.9.39
# Data from Knafl et al. (1984)
#

tankvolume=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 14/tankvolume.txt",
  sep=",",stringsAsFactors = FALSE,
  header=TRUE)

Volume=tankvolume$Volume
Pressure=tankvolume$Pressure

# (a). Plot pressure versus volume.  The relationship appears linear

plot(Volume, Pressure)
#summary(Volume)

# (b). Calculate the linear regression of pressure on volume
lmfit1=lm( Pressure~ Volume)
summary(lmfit1)
abline(lmfit1,col='green')
#   Plot the residuals versus volume

plot(Volume, lmfit1$residuals)
#
# The residuals plot shows a non-linear relationship with volume
#
# (c). Fit Pressure as a quadratic function of volume.
VolumeSq=Volume*Volume

lmfit2=lm(Pressure ~ Volume + VolumeSq)
summary(lmfit2)

plot(Volume, lmfit2$residuals)
abline(h=0,col='gray')

# The fit looks much better, but the residuals at specific volume 
# levels tend to be all positive or all negative together.

# There is variability within given Volume level which is smaller
# than variability across Volume levels.

# There appears to be two sources of varability: across volume levels and within.
