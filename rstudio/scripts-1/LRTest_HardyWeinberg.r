# Project6_LRTest_HardyWeinberg.r

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

# 3. Generalized Likelihood Ratio Test ----
#   3.1 Construct vectors of Observed and Expected counts

counts.Observed=x

probs.mle=fcn.probs.hardyweinberg(x.theta.mle)
counts.sum=sum(x)

counts.Expected=counts.sum*probs.mle

labels.BloodType=c("M","MN","N")

#   3.2 Compute LRStat 
#       Conduct level alpha=0.05 test
#       Compute P-Value
component.LRStat=2*counts.Observed*log(counts.Observed/counts.Expected)
LRStat=sum(component.LRStat)
print(LRStat)
#
# Under Null Hypothesis, LRStat is a chi-square r.v. with
#   q=(m-1) -1 = (3-1)-1=1 degrees of freedom
#   
# For level alpha test, determine critical value of LRStat
alpha=.05
q=3-1-1

chisq.criticalValue=qchisq(p=1-alpha,df=q)
print(chisq.criticalValue)
 
#   Test accepted at alpha level (LRStat < chisq.criticalValue)
#
# Compute P-value of LRStat

LRStat.pvalue=1-pchisq(LRStat, df=q)

print(LRStat.pvalue)


# 4. Pearson ChiSquare Test ----
#   4.1 Construct vectors of Observed and Expected counts

counts.Observed=x

probs.mle=fcn.probs.hardyweinberg(x.theta.mle)
counts.sum=sum(x)

counts.Expected=counts.sum*probs.mle

labels.BloodType=c("M","MN","N")

#   4.2 Compute Pearson ChiSqStat
#       Conduct level alpha=0.05 test
#       Compute P-Value

component.ChiSqStat=((counts.Observed-counts.Expected)^2 )/counts.Expected
ChiSqStat=sum(component.ChiSqStat)
print(ChiSqStat)

#
# Under Null Hypothesis, ChiSqStat is a chi-square r.v. with
#   q=(m-1) -1 = (3-1)-1=1 degrees of freedom
#   
# For level alpha test, determine critical value of ChiSqStat
alpha=.05
q=3-1-1

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
dimnames(table.lrtests.0)[[1]]<-c(labels.BloodType,"Total/Sum")



print(table.lrtests.0)

print(data.frame(cbind(LRStat.pvalue=LRStat.pvalue, 
            ChiSqStat.pvalue=ChiSqStat.pvalue),
      row.names=c("P-Value")))
