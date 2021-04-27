# RProject7_3_ProbabilityPlots_Comparisons_QQPlots.r

#   2.0 Uniform distribution of Probability Integral Transform
#
set.seed(3)
# Repeat Create
n.simulations=10
for (j in c(1:n.simulations)){
par(mfrow=c(3,4))
n.samplesize=25
#   2.1 Normal Distribution 
x=rnorm(n=n.samplesize)

#plot(x, main=paste("Index Plot\nN(0,1) Sample","\n n=", as.character(length(x)),sep=""))

#  hist(x, main=paste("Histogram \nN(0,1) Sample","\n n=", as.character(length(x)),sep=""))
hist(x, probability=TRUE,
     main=paste("Histogram \n N(0,1) Sample","\n n=", as.character(length(x)),sep=""))
abline(h=1,col='blue')
hist(pnorm(x), probability=TRUE,
     main=paste("Histogram \n Prob. Integ. Transf.\n N(0,1) Sample","\n n=", as.character(length(x)),sep=""))
abline(h=1,col='blue')
plot(ppoints(length(x)), sort(pnorm(x)), main="Probability Plot")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0)

plot(qnorm(ppoints(length(x))), sort(x), main="QQ Plot",
     xlab="Theoretical Quantile")

abline(a=0,b=1) # Plot line with slope=0 and intercept=0)
abline(v=qnorm(c(.50,.90)),col=c('green','red'))
#   2.2 Chi-square 1  Distribution 
x2=(rnorm(n=n.samplesize))^2

#plot(x, main=paste("Index Plot\n Chisq(1) Sample","\n n=", as.character(length(x)),sep=""))

#  hist(x, main=paste("Histogram \nChisq(1) Sample","\n n=", as.character(length(x)),sep=""))
hist(x2, probability=TRUE,
     main=paste("Histogram \n Chisq(1) Sample","\n n=", as.character(length(x)),sep=""))

hist(pchisq(x2,df=1), probability=TRUE,
     main=paste("Histogram \n Prob. Integ. Transf.\n Chisq(1)  Sample","\n n=", as.character(length(x)),sep=""))
abline(h=1,col='blue')
plot(ppoints(length(x2)), sort(pchisq(x2,df=1)), main="Probability Plot")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0)

plot(qchisq(ppoints(length(x2)), df=1), sort(x2),
     main="QQ Plot",
     xlab="Theoretical Quantile")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0)
abline(v=qchisq(c(.50,.90),df=1),col=c('green','red'))
#   2.  Gamma(5,1) Distribution 
gamma.shape=5
gamma.scale=1
dist.name=paste("Gamma(", gamma.shape, ",", gamma.scale,")", sep="")

x3=rgamma(n=n.samplesize, shape=gamma.shape, scale=gamma.scale)

#plot(x3, main=paste("Index Plot\n", dist.name," Sample","\n n=", as.character(length(x3)),sep=""))

hist(x3, probability=TRUE,
     main=paste("Histogram \n ", dist.name," Sample","\n n=", as.character(length(x)),sep=""))

hist(pgamma(x3,shape=gamma.shape,scale=gamma.scale),
     probability=TRUE,
     xlab="pgamma(x3,...)",
     main=paste("Histogram \n Prob. Integ. Transf.\n", dist.name," Sample","\n n=", as.character(length(x)),sep=""))
abline(h=1,col='blue')
plot(ppoints(length(x3)), sort(pgamma(x3,shape=gamma.shape, scale=gamma.scale)),
     xlab="Expected Order Statistics\nU(0,1) Sample",
     ylab="Yj=F(X[j])",
     main="Probability Plot")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0)


plot(qgamma(ppoints(length(x3)), shape=gamma.shape, scale=gamma.scale), sort(x3),
     xlab="Theoretical Quantile", main="QQ Plot")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0)
abline(v=qgamma(c(.50,.95),shape=gamma.shape, scale=gamma.scale),col=c('green','red'))
}