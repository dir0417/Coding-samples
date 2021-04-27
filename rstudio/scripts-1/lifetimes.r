# Rproject7_6_lifetimes.r


# 1.0 Read in data ----
#       See Example 10.2.1 A, data from White, Riethof, and Kushnir (1960)
#
#  Lifetime in days of animals who died within 2 years
#   Groups 1-5, 72 animals per group
#   Control group, 107 animals

gpigs1=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigs1.txt",sep=",")
gpigs2=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigs2.txt",sep=",")
gpigs3=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigs3.txt",sep=",")
gpigs4=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigs4.txt",sep=",")
gpigs5=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigs5.txt",sep=",")
gpigscontrol=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/gpigscontrol.txt",sep=",")

#     Plot the data (check for errors)
plot(gpigs1)
plot(gpigs2)
plot(gpigs3)
plot(gpigs4)
plot(gpigs5)
plot(gpigscontrol)


#  2.  Compute empirical survival functions for each group ---

#   2.1 Define fcn.ecdf 
#         empirical cdf of time-to-failure
#   Inputs
#         x   (times of failures)
#         n   (total number of items/individuals)
#   For jth smallest failure, set ecdf = j/(n+1)
fcn.ecdf<-function(x, n=72){
  if (sum(1*(diff(x)<0))==0){
    x.0=x
  x.0.ecdf=c(1:length(x.0))/(n+1)
  return(x.0.ecdf)}else{return(NULL)}
}

gpigs1.esf<-1-fcn.ecdf(gpigs1)
gpigs2.esf<-1-fcn.ecdf(gpigs2)
gpigs3.esf<-1-fcn.ecdf(gpigs3)
gpigs4.esf<-1-fcn.ecdf(gpigs4)
gpigs5.esf<-1-fcn.ecdf(gpigs5)
gpigscontrol.esf<-1-fcn.ecdf(gpigscontrol,n=107)

# 3.0  Plot Empirical Survival Functions ---

xlim0=c(0,800.)
ylim0=c(0,1.)

plot(gpigscontrol,gpigscontrol.esf, 
     xlim=xlim0,
     ylim=ylim0,
     main="Empirical Survival Functions \n Figure 10.2 (Rice)",
     xlab="Days Elapsed",
     ylab="Proportion of live animals",
     type="l", col='black')
cols.groups<-rainbow(5)

lines(gpigs1, gpigs1.esf, lty=1, col=cols.groups[1])
lines(gpigs2, gpigs2.esf, lty=1, col=cols.groups[2])
lines(gpigs3, gpigs3.esf, lty=1, col=cols.groups[3])
lines(gpigs4, gpigs4.esf, lty=1, col=cols.groups[4])
lines(gpigs5, gpigs5.esf, lty=1, col=cols.groups[5])

legend(x=550,y=1.0, legend=c("Control",paste("Group ", c("I","II","III","IV","V"),sep="")),
       lty=rep(1,times=6),
       col=c('black', cols.groups), 
       cex=.8)


# Redo plot with log scale

plot(gpigscontrol,log(gpigscontrol.esf), 
     xlim=xlim0,
     ylim=c(log(.005),log(1.0)),
     main="Log Empirical Survival Functions \n Figure 10.2 (Rice)",
     xlab="Days Elapsed",
     ylab="Log Proportion of live animals",
     type="l", col='black')
cols.groups<-rainbow(5)

lines(gpigs1, log(gpigs1.esf), lty=1, col=cols.groups[1])
lines(gpigs2, log(gpigs2.esf), lty=1, col=cols.groups[2])
lines(gpigs3, log(gpigs3.esf), lty=1, col=cols.groups[3])
lines(gpigs4, log(gpigs4.esf), lty=1, col=cols.groups[4])
lines(gpigs5, log(gpigs5.esf), lty=1, col=cols.groups[5])

legend(x=0,y=-3.0, legend=c("Control",paste("Group ", c("I","II","III","IV","V"),sep="")),
       lty=rep(1,times=6),
       col=c('black', cols.groups), 
       cex=.8)
