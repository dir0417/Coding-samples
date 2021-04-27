# Rproject4_Bayesian_Poisson.r

# Example 8.4.A:  Counts of asbestos fibers on filters
#       Steel et al. 1980

# 1.  Data ----
x=c(31,29,19,18,31,28,
    34,27,34,30,16,18,
    26,27,27,18,24,22,
    28,24,21,17,24)


# 2.  Parameter Estimation of Poisson----

# Suppose x is a sample of size n=23 from a Poisson(lambda) distribution
#
#   2.1 Estimate of lambda (MOM and MLE are the same)

lambda.hat=mean(x)

print(lambda.hat)
# 

par(mfcol=c(1,1))
#   2.2 Plot histograms of sample data

#     Vary argument nclass= for number of bins
hist(x, xlim=c(0,1.5*max(x)), probability=TRUE)

hist(x,nclass=15, xlim=c(0,1.5*max(x)), probability=TRUE)

hist(x,nclass=10, xlim=c(0,1.5*max(x)), probability=TRUE)
# Add plot of pmf function for fitted Poisson distribution
x.grid=seq(0,1.5*max(x),1)
x.probs=dpois(x.grid, lambda=lambda.hat)

lines(x.grid,x.probs, type="h", col='blue',lwd=4)

# 
lambda.hat.sterror=sqrt(lambda.hat/length(x))

print(lambda.hat.sterror)

# 3. Approximate Confidence Interval for lambda ----
#
#   Confidence Level: 90%

lambda.CI.Limits=lambda.hat + c(-1,1)*qnorm(.95)*lambda.hat.sterror
print(lambda.CI.Limits)

# Note:
qnorm(.95)
qnorm(.05) #= -qnorm(.95)

# 4.  Bayesian Approach ----
#
#   4.1 Prior distribution for lambda ----
#   Gamma(shape=alpha0, rate=nu0) distribution with
#     prior mean = 15
#     prior variance = 5*5
#
#       For a Gamma distribution
#         mean = mu = alpha0/nu0
#         variance =sigsq= alpha0/(nu0*nu0)
#
#       Solving for Gamma parameters
#         nu0 = mu/sigsq
nu0= 15/25
#     alpha0=mu*nu0
alpha0=nu0*15

lambda.grid=seq(0.1,30,.1)
lambda.grid.priorpmf=dgamma(lambda.grid, shape=alpha0, rate=nu0)
priorpmf.grid =dgamma(lambda.grid, shape=alpha0, rate=nu0)

#   4.2 Plot prior, likelihood, and posterior ----
#par(mfcol=c(3,1))
# Plot Prior Density 

plot(lambda.grid, priorpmf.grid,
     col='red', type="l", 
     main="Prior Density")

# Plot Likelihood
# Sum(x) is Poisson(lambda.sum) where lambda.sum=n*lambda
likelihood.grid=dpois(sum(x),lambda=length(x)*lambda.grid)

plot(lambda.grid,likelihood.grid,
     main="Likelihood of lambda", col='green',type="l")


# Plot Posterior
# Posterior parameters

alpha1= alpha0 + sum(x)
nu1 = nu0 + length(x)

posteriorpmf.grid=dgamma(lambda.grid,shape=alpha1, rate=nu1)


plot(lambda.grid, posteriorpmf.grid,
    main="Posterior Density", col='blue', type="l")

#     Plot of all densities together ----
par(mfcol=c(1,1))

plot(lambda.grid, posteriorpmf.grid,ylab="density",
     main="Densities \n Gamma Prior/Posterior (red/blue)\n",
  type="n")

lines(lambda.grid, priorpmf.grid, col='red')

lines(lambda.grid, posteriorpmf.grid, col='blue')

#     Add case of Uniform prior ----
lines(lambda.grid, (1+0*priorpmf.grid)*.1/30, col='orange')
posteriorpmf.uniformprior.grid=dgamma(lambda.grid,shape=1+ sum(x), rate= length(x))

lines(lambda.grid, posteriorpmf.uniformprior.grid, col='green')

title(main="\n\nUniform Prior/Posterior (orange/green)")

      
# 5. Point and Interval Estimates ----

#   5.1  First posterior distribution

#     Parameters/attributes
posterior.mean=alpha1/nu1
posterior.stdev=sqrt(alpha1/(nu1*nu1))
posterior.mode= (alpha1-1)/nu1
#     90% posterior predictive interval
posterior.llimit=qgamma(.05, shape=alpha1, rate=nu1)
posterior.ulimit=qgamma(.95, shape=alpha1, rate=nu1)
#   Summary table
bayes1.attributes=data.frame(
  mode=posterior.mode,
  mean=posterior.mean,
  stdev=posterior.stdev,
  ulimit=posterior.ulimit,
  llimit=posterior.llimit)


#   5.2  Second posterior distribution

#     Parameters/attributes
# Reset posterior parameters corresponding to uniform prior
alpha1=sum(x)
nu1=length(x)

posterior.mean=alpha1/nu1
posterior.stdev=sqrt(alpha1/(nu1*nu1))
posterior.mode= (alpha1-1)/nu1
#   90% posterior predictive interval
posterior.llimit=qgamma(.05, shape=alpha1, rate=nu1)
posterior.ulimit=qgamma(.95, shape=alpha1, rate=nu1)
#   Summary table
bayes2.attributes=data.frame(
  mode=posterior.mode,
  mean=posterior.mean,
  stdev=posterior.stdev,
  ulimit=posterior.ulimit,
  llimit=posterior.llimit)

#   5.3 MLE Estimates/Confidence Interval
mle.attributes=data.frame(
  mode=lambda.hat,
  mean=NA,
  stdev=sqrt(lambda.hat/length(x)),
  ulimit=lambda.hat + qnorm(.95)*sqrt(lambda.hat/length(x)),
  llimit=lambda.hat + qnorm(.05)*sqrt(lambda.hat/length(x)))

estimates.table=data.frame(cbind(Bayes1=t(bayes1.attributes), Bayes2=t(bayes2.attributes),
                                 MLE=t(mle.attributes))
)
dimnames(estimates.table)[[2]]<-c("Bayes 1", "Bayes 2", "Maximum Likelihood")

print(estimates.table)
