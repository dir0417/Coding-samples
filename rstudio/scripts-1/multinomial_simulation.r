# Rproject3_script1_multinomial_simulation.r
#
# Parametric Bootstrap simulation of sampling distributions for alternate estimators

# Multinomial Counts:  Hardy Weinberg Equilibrium

# 1.1 Trinomial data----
#     from Example 8.5.1.A, p. 273 of Rice.
#
# x=(X1,X2,X3)  # counts of multinomial cells (1,2,3)

# Erythrocyte antigen blood types of n=1029 Hong Kong population in 1937.
x<-c(342,500,187)

# 1.2 Two estimators:  Multinomial MLE and Binomial(X3) MLE ----

x.n=sum(x)
x.theta.mle=(2*x[3] + x[2])/(2*sum(x))
x.theta.binomialx3=sqrt(x[3]/sum(x))

print(x.theta.mle)
print(x.theta.binomialx3)


# 2.0 Simulate sampling distribution of estimators ----

#   1000 trials (the sampling distribution) of the two estimators

# For each trial, generate  sample  of size n=1027 from the multinomial distribution
#     w.samplespace=c(1,2,3); the outcomes/cells of the multinomial 
#     prob=fcn.probs.hardyweinberg(x.theta.mle); the cell probilities
#   
#   Compute estimates of the Hardy-Weinberg theta parameter 
#     by Multinomial MLE and Binomial(X3) MLE

# 2.1 R functions for the simulation ----

# function computing cell probabilities given Hardy-Weinberg theta parameter
fcn.probs.hardyweinberg<-function(theta0){
  probs=c((1-theta0)**2, 2*theta0*(1-theta0), theta0**2)
  return(probs)
}

# function computing cell counts given w.sample, a sample of single-outcome multinomial
# random variables (comparable to Bernoulli outcomes underlying a binomial)
fcn.w.sample.counts<-function(w.sample, w.samplespace){
  result=0*w.samplespace
  for (j.outcome in c(1:length(w.samplespace))){
    result[j.outcome]<-sum(w.sample==w.samplespace[j.outcome])
    }
  return(result)
}

# 2.2 Validate functions for single trial example ----

args(sample)
w.samplespace=c(1,2,3)

w.sample= sample(x=w.samplespace,size=x.n,replace=TRUE, 
        prob=fcn.probs.hardyweinberg(x.theta.mle))
w.sample.counts<-fcn.w.sample.counts(w.sample, w.samplespace)

par(mfcol=c(1,1))
plot(w.sample,main=paste("Single Multinomial Trial (n=", as.character(x.n),")",sep=""))

print(table(w.sample))
print(w.sample.counts)

# 2.3 Conduct Simulation ----

n.simulations=20000

data.simulations<-matrix(NA,nrow=n.simulations, ncol=2)
dimnames(data.simulations)[[2]]<-c("MLE","Alternate")
for (j.simulation in c(1:n.simulations)){
  
  j.w.sample= sample(x=w.samplespace,size=x.n,replace=TRUE, 
                   prob=fcn.probs.hardyweinberg(x.theta.mle))
  j.w.sample.counts<-fcn.w.sample.counts(j.w.sample, w.samplespace)
  
  x.j=j.w.sample.counts
  # print(x.j)
  
x.j.theta.mle=(2*x.j[3] + x.j[2])/(2*sum(x.j))
x.j.theta.binomialx3=sqrt(x.j[3]/sum(x.j))
  data.simulations[j.simulation,1]=x.j.theta.mle
  data.simulations[j.simulation,2]=x.j.theta.binomialx3
}

# 3.  Simulation Results ----


# 3.1 Histogram of estimators sampling distributions -----
par(mfcol=c(2,1))
print(simulations.means<-apply(data.simulations,2,mean))
print(simulation.stdevs<-sqrt(apply(data.simulations,2,var)))

hist(data.simulations[,1], main="Multinomial MLE")
abline(v=simulations.means[1] +c(-1,0,1)*simulation.stdevs[1], col=c(3,3,3))

hist(data.simulations[,2], main="Binomial(X3) MLE")
abline(v=simulations.means[2] +c(-1,0,1)*simulation.stdevs[2], col=c(2,2,2))

# 3.2 Histograms of estimation errors for each estimator ----
data.simulations.error=data.simulations-x.theta.mle

par(mfcol=c(2,1))
hist(data.simulations.error[,1] , main="Error of Multinomial MLE")
print(simulations.error.means<-apply(data.simulations.error,2,mean))
print(simulations.error.stdevs<-sqrt(apply(data.simulations.error,2,var)))
abline(v=simulations.error.means[1] +c(-1,0,1)*simulations.error.stdevs[1], col=c(3,3,3))


hist(data.simulations.error[,2], main="Error of Binomial(X3) MLE")
abline(v=simulations.error.means[2] +c(-1,0,1)*simulations.error.stdevs[2], col=c(2,2,2))

# 3.3 Boxplot comparing sampling distributions ----
par(mfcol=c(1,1))
boxplot(data.simulations, 
        main="Simulated Sampling Distribution\nHardy-Weinberg Parameter")

# 3.4 Scatterplot of joint distribution of estimates ----
plot(data.simulations[,1], data.simulations[,2],xlab="Multinomial MLE", ylab="Binomial(X3) MLE")
abline(v=simulations.means[1] +c(-1,0,1)*simulation.stdevs[1], col=c(3,3,3))
abline(h=simulations.means[2] +c(-1,0,1)*simulation.stdevs[2], col=c(2,2,2))




# 4. Bootstrap Confidence Interval ----

head(data.simulations.error)
quantile(data.simulations.error[,1],probs=c(.05,.95))
quantile(data.simulations.error[,2],probs=c(.05,.95))


x.theta.mle
x.theta.binomialx3

# 4.1 Approximate 90% confidence interval based on MLE ----

approx.CI.limits.90percent.mle=x.theta.mle +c(- quantile(data.simulations.error[,1], probs=.95),
                                              - quantile(data.simulations.error[,1],probs=.05))
approx.CI.limits.90percent.mle


# 4.2 Approximate 90% confidence interval based on BINOMIALX3 ----

approx.CI.limits.90percent.binomialx3=x.theta.binomialx3 +c(- quantile(data.simulations.error[,2], probs=.95),
                                                            - quantile(data.simulations.error[,2],probs=.05))
approx.CI.limits.90percent.binomialx3