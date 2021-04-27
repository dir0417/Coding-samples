  # RProject7_1_ProbabilityPlots_Uniform.r

# Simulate samples of U(0,1) random varaibles
#   Plot sample, histogram, and probability plot for different-sized samples

# 1.0 Uniform(0,1) Samples
set.seed(1)
par(mfrow=c(3,4))
n.simulations=10
for (j in c(1:n.simulations)){
for (n.samplesize in c(25, 100, 400)){
x=runif(n=n.samplesize)

plot(x, main=paste("U(0,1) Sample","\n n=", as.character(length(x)),sep=""))

hist(x, main=paste("Histogram U(0,1) Sample","\n n=", as.character(length(x)),sep=""))
hist(x, probability=TRUE,
     main=paste("Histogram U(0,1) Sample","\n n=", as.character(length(x)),sep=""))
abline(h=1,col='blue')
plot(ppoints(length(x)), sort(x), main="Probability Plot")
abline(a=0,b=1) # Plot line with slope=0 and intercept=0
}


} # end loop j
