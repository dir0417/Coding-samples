# Rproject1_script1.r
#
# Distributions derived from the normal distribution
# See Chapter 6 of Rice for distribution theory

# 1. R functions for Normal distribution ----

# random number generator of normal distribuion
help(rnorm)

# Family of functions
#   dnorm()   density function
#   pnorm()   cumulative distribution function
#   qnorm()   quantile function
#   rnorm()   random  number generator

#   Syntax of functions -- from help(rnorm) output
# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# rnorm(n, mean = 0, sd = 1)

#   1.1 rnorm() ----
#         random normals
#       Generate vector x of 1000 iid N(0,1)  random values 
x=rnorm(1000)

# NOTE: Top right panel of Rstudio, Environment tab lists  x

#   Plot the series of values in x
plot(x, main="1.1 1000 Random N(0,1) Values")


#   Plot a histogram of the values in x
hist(x, main="1.2a Histogram of 1000 Random N(0,1) Values")
#
# help(hist) #command to display help file on r function hist()
#   Replot histogram using probability density scale
hist(x,probability="TRUE", main="1.2b Histogram of 1000 Random N(0,1) Values\n (density scale)")

#   1.2 dnorm() #----
#       density function of Normal distribution

#   Create vector of x values
x.grid=seq(-3,3,.01)

#   Compute vector of corresponding density values
density.grid=dnorm(x.grid,mean=0, sd=1)

#   use R function lines() to add to current plot
lines(x.grid,density.grid,col='blue')

#   1.3 pnorm()  -----
#     cdf (cumulative distribution function) of Normal distribution

cdf.grid=pnorm(x.grid,mean=0,sd=1)

plot(x.grid,cdf.grid,type="l",col="blue",
     main="1.3 CDF Fof N(0,1)")

#   1.4 qnorm() ----
#     compute quantiles (percentiles)

qnorm(.95)
qnorm(.995)
qnorm(.9995)

qnorm(.05)
qnorm(.005)
qnorm(.0005)


# 2.  R functions for Chi-square distribution ----

# random number generator of chisquare  distribuion
# See:  help(rchisq)
#   Syntax of functions -- from help(qchisq) output
# dchisq(x, df, ncp = 0, log = FALSE)
# pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
# rchisq(n, df, ncp = 0)

#   2.1 rchisq() ----
#         random chisquare 
#       Generate vector v of 1000 chisquare (df=3) random values 
v=rchisq(1000,df=3)

# NOTE: Top right panel of Rstudio, Environment tab lists  v

#   Plot the series of values in v
plot(v, main="2.1 1000 Random Chisquare(df=3) Values")

#   Plot a histogram of the values in v
hist(v, main="2.2a Histogram of 1000 Random\n Chisquare(df=3) Values", nclass=50)

#
#   Replot histogram using probability density scale
#   Plot a histogram of the values in v
hist(v,probability="TRUE", nclass=50,
     main="2.2b Histogram of 1000 Random\n Chisquare(df=3) Values\n (density scale)")

#   2.2 dchisq() #----
#       density function of chisquared  distribution

#   Create vector of x values
v.grid=seq(0, max(v),.01)

#   Compute vector of corresponding density values
density.v.grid=dchisq(v.grid,df=3)

#   use R function lines() to add to current plot
lines(v.grid,density.v.grid,col='blue')

#   2.3 pchisq()  -----
#     cdf (cumulative distribution function) of chisquared distribution

cdf.v.grid=pchisq(v.grid,df=3)

plot(v.grid,cdf.v.grid,type="l",col="blue",
     main="2.3 CDF of Chisquare Dist (df=3)")

#   2.4 qchisq() ----
#     compute quantiles (percentiles)

qchisq(.95,df=3)
qchisq(.995,df=3)
qchisq(.9995,df=3)

qchisq(.05,df=3)
qchisq(.005 ,df=3)
qchisq(.0005,df=3)
#   


# 3.  Simulate Chisquare r.v.s from normal r.v.s ----

#   3.1 A chisquare(df=3) r.v. is sum of 3 squared N(0,1) r.v.s

v.sim=(rnorm(1000))^2 + (rnorm(1000))^2 + (rnorm(1000))^2

plot(v.sim, main="3.1 1000 Random Chisquare(df=3) Values \n (Derived from N(0,1) r.v.s)")
hist(v.sim,nclass=50, main="3.2 Histogram of 1000 Random Chisquare(df=3) Values\n (Derived from N(0,1) r.v.s)")


#
#   Replot histogram using probability density scale
hist(v.sim,probability="TRUE", nclass=50,
     main="3.2 Histogram of 1000 Random Chisquare(df=3) Values\n (Derived from N(0,1) r.v.s)\n (density scale)")
   
#   use R function lines() to add to current plot
lines(v.grid,density.v.grid,col='blue')


# 4.  Simulate t distribution from Normal r.v.s----
#       (t with 3 degrees of freedom)

#   4.1  Apply theorem of t dist ----
#               = N(0,1)/sqrt(chisq/df))

t.sim = x / sqrt(v.sim/3)

#   Plot the series of values in t.sim
plot(t.sim, main="4.1 1000 Random t (df=3) Values")


#   Plot a histogram of the values in v
hist(t.sim, main="4.2a Histogram of 1000 Random t(df=3) Values",nclass=50)
#
#   Replot histogram using probability density scale
hist(t.sim,probability="TRUE", nclass=50,
     main="4.2b Histogram of 1000 Random t(df=3) Values\n (density scale)")

## R functions relating to t distribution
# See help(rt)
#
# dt(x, df, ncp, log = FALSE)
# pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
# qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
# rt(n, df, ncp)
#   Note: ncp is non-centrality parameter
#           corresponds to mean of normal distribution in numberator of t
#   
#   4.2 dt() #----
#       density function of t distribution

#   Create vector of x values
t.grid=seq(min(t.sim),max(t.sim),.01)

#   Compute vector of corresponding density values
density.t.grid=dt(t.grid,df=3)

#   use R function lines() to add to current plot
lines(t.grid,density.t.grid,col='blue')

#   4.3 pt()  -----
#     cdf (cumulative distribution function) of t dist with df degrees of freedom

cdf.t.grid=pt(t.grid,df=3)

plot(t.grid,cdf.t.grid,type="l",col="blue",
     main="4.3 CDF Fof Chisquare Dist (df=3)")

#   4.4 qt() ----
#     compute quantiles (percentiles)

qt(.95,df=3)
qt(.995,df=3)
qt(.9995,df=3)

qt(.05,df=3)
qt(.005 ,df=3)
qt(.0005,df=3)
#   

