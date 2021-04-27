# Rproject6_LRTest_Poisson.r

# Example 8.4.A:  Counts of asbestos fibers on filters
#       Steel et al. 1980

# 1.  Data ----
x=c(31,29,19,18,31,28,
    34,27,34,30,16,18,
    26,27,27,18,24,22,
    28,24,21,17,24)

# x= rpois(n=100, lambda=24.91)
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

# 3.  Likelihood Ratio Test Based on Histogram


nclass0=10
hist.0<-hist(x,nclass=nclass0)
print(hist.0$counts)
n.bins=length(hist.0$counts)

print(hist.0$breaks)

# cdf values at the breaks

hist.0.breaks.MLEFITTED.cdf=ppois(hist.0$breaks, lambda=lambda.hat)

# bin probabilities are the differences
hist.0.pbin=diff(hist.0.breaks.MLEFITTED.cdf)

####
#   3.1 Construct vectors of Observed and Expected counts

counts.Observed=hist.0$counts


probs.mle=hist.0.pbin
counts.sum=sum(counts.Observed)

counts.Expected=counts.sum*probs.mle

#labels.BloodType=c("M","MN","N")

#   3.2 Compute LRStat 
#       Conduct level alpha=0.05 test
#       Compute P-Value
component.LRStat=2*counts.Observed*log(counts.Observed/counts.Expected)
LRStat=sum(component.LRStat)
print(LRStat)
#
# Under Null Hypothesis, LRStat is a chi-square r.v. with
#   q=(m-1) -1  degrees of freedom
#     where m=number of bins
# For level alpha test, determine critical value of LRStat
alpha=.05
m=n.bins
q=m-1-1
print(q)

chisq.criticalValue=qchisq(p=1-alpha,df=q)
print(chisq.criticalValue)

#   Test rejected  at alpha level (LRStat > chisq.criticalValue)
#
# Compute P-value of LRStat

LRStat.pvalue=1-pchisq(LRStat, df=q)

print(LRStat.pvalue)


# 4. Pearson ChiSquare Test ----
#   4.1 Construct vectors of Observed and Expected counts


counts.Observed=hist.0$counts


probs.mle=hist.0.pbin
counts.sum=sum(counts.Observed)

counts.Expected=counts.sum*probs.mle


#   4.2 Compute Pearson ChiSqStat
#       Conduct level alpha=0.05 test
#       Compute P-Value


component.ChiSqStat=((counts.Observed-counts.Expected)^2 )/counts.Expected
ChiSqStat=sum(component.ChiSqStat)
print(ChiSqStat)

#
# Under Null Hypothesis, LRStat is a chi-square r.v. with
#   q=(m-1) -1  degrees of freedom
#     where m=number of bins
# For level alpha test, determine critical value of LRStat
alpha=.05
m=n.bins
q=m-1-1
print(q)

chisq.criticalValue=qchisq(p=1-alpha,df=q)
print(chisq.criticalValue)

#   Test accepted at alpha level (ChiSqStat < chisq.criticalValue)
#
# Compute P-value of ChiSqStat

ChiSqStat.pvalue=1-pchisq(ChiSqStat, df=q)

print(ChiSqStat.pvalue)

############################
# 5.  Create Table for Both Tests ----
table.lrtests<-data.frame(
  Observed=counts.Observed,
  Expected=counts.Expected,
  LRStat.j=component.LRStat,
  ChiSqStat.j=component.ChiSqStat)


#   Add last row equal to sums of columns
#     Use r function apply(X=, MARGIN=, FUN=)
table.lrtests.0<-rbind(
  table.lrtests,
  t(as.matrix(apply(X=table.lrtests,MARGIN=2,FUN=sum))))
labels.tablerows<-paste("Bin_",c(1:n.bins),"[",
                        hist.0$breaks[1:(n.bins)],",",
                        hist.0$breaks[2:(n.bins+1)],"]",sep="")
dimnames(table.lrtests.0)[[1]]<-c(labels.tablerows,"Total/Sum")



print(table.lrtests.0)

print(data.frame(cbind(LRStat.pvalue=LRStat.pvalue, 
                       ChiSqStat.pvalue=ChiSqStat.pvalue),
                 row.names=c("P-Value")))
