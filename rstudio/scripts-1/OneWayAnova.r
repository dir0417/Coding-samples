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

# Compare output to t.test
t.test(tablets1$Lab7, tablets1$Lab4, var.equal=TRUE)




#   3.1 Conduct One-Way ANOVA ----
#       Analysis of Variance with R function aov()

#   Create vector variables
#     yvec = Dependent variable 
yvec=as.vector(data.matrix(tablets1))

#     xvec.factor.Lab = Independent variable (of factor type in R)

xvec.factor.Lab=as.factor(as.vector(col(data.matrix(tablets1))))
table(xvec.factor.Lab)

plot(as.numeric(xvec.factor.Lab), yvec)

# One-way ANOVA using r function aov()
tablets1.aov<-aov(yvec ~ xvec.factor.Lab)
tablets1.aov.summary<-summary(tablets1.aov)

# Replicate Table in Example 12.2.A, p. 483 of Rice
print(tablets1.aov.summary)

# Construct model tables

tablets1.aov.model.tables<-model.tables(tablets1.aov, type="means",se=TRUE)
print(tablets1.aov.model.tables)
# Validate standard error for difference of means
ResidualsMeanSq=0.003673
J=nrow(tablets1)
sqrt(ResidualsMeanSq/J) # standard error of each mean
sqrt(ResidualsMeanSq/J)*sqrt(2) # standard error of difference of two means

#tablets1.aov.model.tables<-model.tables(tablets1.aov, type="effects",se=TRUE)
#print(tablets1.aov.model.tables)

#   3.2 Confidence intervals for pairwise differences ----
# Create confidence intervals on differences between means
#   Studentized range statistic
#   Tukey's 'Honest Significant Difference' method
# Apply R function TukeyHSD(): 
TukeyHSD(tablets1.aov)
#
# Compare Tukey HSD confidence interval for Lab7 vs Lab4
fcn.TwoSampleTTest(tablets1$Lab7,tablets1$Lab4)

# Note P-value from t-test is 0.0144 while TukeyHSD is 0.076

# 4. Implement One-Way Anova with lm() ----
lmfit.oneway.Lab=lm(yvec ~ xvec.factor.Lab,x=TRUE, y=TRUE)
lmfit.oneway.Lab.summary<-summary(lmfit.oneway.Lab)
print(lmfit.oneway.Lab.summary)

# Compare regression output from lm() with anova output from aov()
print(tablets1.aov.summary)

# Residual standard error equals root (Mean Sq for Residuals)
print(lmfit.oneway.Lab.summary$sigma)
print((lmfit.oneway.Lab.summary$sigma)^2)
# Compare to Mean Sq for Residuals in tablets1.aov.summary

# F-statistic/degrees of freedom/P-values are same

