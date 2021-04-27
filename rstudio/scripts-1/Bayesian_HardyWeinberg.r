# Project4_Bayesian_HardyWeinberg.r

# Multinomial Counts:  Hardy Weinberg Equilibrium

# 1.0 Trinomial data from Example 8.5.1.A, p. 273 of Rice.
#
# x=(X1,X2,X3)  # counts of multinomial cells (1,2,3)

# Erythrocyte antigen blood types of n=1029 Hong Kong population in 1937.
x<-c(342,500,187)

# Two estimators:  Multinomial MLE and Binomial(X3) MLE
x.n=sum(x)
x.theta.mle=(2*x[3] + x[2])/(2*sum(x))
x.theta.binomialx3=sqrt(x[3]/sum(x))

print(x.theta.mle)
print(x.theta.binomialx3)

# 2.1 R functions
# function computing cell probabilities given Hardy-Weinberg theta parameter
fcn.probs.hardyweinberg<-function(theta0){
  probs=c((1-theta0)**2, 2*theta0*(1-theta0), theta0**2)
  return(probs)
}

# 3.  Bayesian inference with uniform prior

dgrid=.001
theta.grid=seq(0,1,dgrid)

# Compute likelihood of x 

# First, compute matrix with multinomial probabilities (3 columns) for each theta (row)
probs.grid=t(apply(as.matrix(theta.grid),1, fcn.probs.hardyweinberg))


plot(theta.grid, probs.grid[,1],type="l",col='red', xlab="theta",
     ylab="Cell Probability",
     main="Hardy Weinberg Cell Probabilities \n 1 (Red), 2 (Green), 3 (Blue)")


lines(theta.grid, probs.grid[,2],type="l",col='green')

lines(theta.grid, probs.grid[,3],type="l",col='blue')

#  Compute likelihood function given x at theta.grid values

# Issue: scaling of likelihood; use log scale and normalize 
loglike.grid<-( log(probs.grid) %*% as.matrix(x))
plot(theta.grid, loglike.grid)
loglike.grid.norm0<-loglike.grid - max(loglike.grid)
plot(theta.grid, loglike.grid.norm0)
#
# Convert from Log scale to Original Scale of Likelihood
like.grid.norm0 =exp(loglike.grid.norm0) 
like.grid.norm0<-(1/dgrid)*like.grid.norm0/sum(like.grid.norm0)
plot(theta.grid,like.grid.norm0, type="l")

length(like.grid.norm0)
length(theta.grid)

# For uniform prior, the posterior is the normalized likelihiood
plot(theta.grid, like.grid.norm0[,1],type="l", 
     xlab="theta",
     ylab="Density", 
     xlim=c(.3,.6),
     main="Figure 8.10 Posterior Density\nHardy-Weinberg Model theta" )

# Compute 95% posterior predictive interval (numerically)
index.quantile.025=which(cumsum(like.grid.norm0*dgrid) >.025)[1]
posterior.llimit=theta.grid[index.quantile.025]
index.quantile.975=which(cumsum(like.grid.norm0*dgrid) >.975)[1]
posterior.ulimit=theta.grid[index.quantile.975]

abline(v=c(posterior.llimit, posterior.ulimit), col='blue')
abline(v=x.theta.mle, col='green')

print(x.theta.mle)
print(c(posterior.llimit, posterior.ulimit))
