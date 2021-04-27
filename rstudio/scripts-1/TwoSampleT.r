# Rproject11_Tablets_TwoSampleT.r
# 1.0 Read in data ----
#       See Example 12.2A of Rice
# 
#   Measurements of chlorpheniramine maleate in tablets made by seven laboratories
#     Nominal dosage equal to 4mg
#     10 measurements per laboratory
#
#     Sources of variability
#         within labs
#         between labs


# Note: read.table has trouble parsing header row
if (FALSE){tablets1=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 12/tablets1.txt",
                   sep=",",stringsAsFactors = FALSE, quote="\'",
                   header=TRUE)
}
# Read in matrix and label columns
tablets1=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 12/tablets1.txt",
                    sep=",",stringsAsFactors = FALSE, skip=1,
                    header=FALSE)
dimnames(tablets1)[[2]]<-paste("Lab",c(1:7),sep="")

tablets1
#   Replicate Figure 12.1 of Rice
boxplot(tablets1)

#   2. Comparing Two Independent Samples ----
boxplot(tablets1[,c("Lab4","Lab7")])


x=tablets1$Lab4
y=tablets1$Lab7

#   2.1 Define a function to implement Two-sample t test ---- 
fcn.TwoSampleTTest<-function(x,y, conf.level=0.95, digits0=4){
  # conf.level=0.95; digits0=4
x.mean=mean(x)
y.mean=mean(y)
x.var=var(x)
y.var=var(y)
x.n=length(x)
y.n=length(y)
sigmasq.pooled=((x.n-1)*x.var + (y.n-1)*y.var)/(x.n + y.n -2)
# Print out statistics for each sample and pooled estimate of standard deviation
cat("Sample Statistics:\n")
print(t(data.frame(
  x.mean,
  x.n,
  x.stdev=sqrt(x.var),
  y.mean,
  y.n,
  y.stdev=sqrt(y.var),
  sigma.pooled=sqrt(sigmasq.pooled)
  )))

cat("\n t test Computations")
mean.diff=x.mean-y.mean
mean.diff.sterr=sqrt(sigmasq.pooled)*sqrt( 1/x.n + 1/y.n)
mean.diff.tstat=mean.diff/mean.diff.sterr
tstat.df=x.n + y.n -2
mean.diff.tstat.pvalue=2*pt(-abs(mean.diff.tstat), df=tstat.df)

print(t(data.frame(
  mean.diff=mean.diff,
  mean.diff.sterr=mean.diff.sterr,
  tstat=mean.diff.tstat,
  df=tstat.df,
  pvalue=mean.diff.tstat.pvalue)))

# Confidence interval for difference
alphahalf=(1-conf.level)/2.
t.critical=qt(1-alphahalf, df=tstat.df)

mean.diff.confInterval=mean.diff +c(-1,1)*mean.diff.sterr*t.critical
cat(paste("\n", 100*conf.level, " Percent Confidence Interval:\n "))
cat(paste(c(
  "[", round(mean.diff.confInterval[1],digits=digits0), 
  ",", round(mean.diff.confInterval[2],digits=digits0),"]"), collapse=""))

#    invisible(return(NULL))
}

#  2.2 Apply function fcn.TwoSampleTTest ----


# Compare Lab7 to Lab4
fcn.TwoSampleTTest(tablets1$Lab7, tablets1$Lab4)

#

#  2.3 Compare to built-in r function t.test() ----
t.test(tablets1$Lab7, tablets1$Lab4, var.equal=TRUE)


#  2.4 Compare to Linear Regression ----

x=tablets1$Lab4
y=tablets1$Lab7

#   Create yvec and  xmat matrix
yvec=c(x,y)
xmat.col1=0*yvec +1
xmat.col2=c(0*x, (1 + 0*y))
xmat=cbind(xmat.col1, xmat.col2)

plot(xmat.col2, yvec)
#   2.4.1 Fit linear regression
lmfit=lm(yvec ~ xmat.col2,x=TRUE, y=TRUE)
abline(lmfit, col='green')
# x matrix used by lm() is same as xmat
#   print(abs(xmat-lmfit$x))

print(summary(lmfit))

# t value / P-value for xmat.col2 estimate same as two-sample t test

# F-statistic in lm()  equal to square of t value
# (p-values of F and t are same)
