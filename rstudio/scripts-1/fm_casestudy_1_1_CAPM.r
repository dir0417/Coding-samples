# fm_casestudy_1_rcode_CAPM.r

#Execute the R-script ``fm_casestudy_1_0_DownloadData.r'' 
#   creates the time-series matrix $casestudy1.data0.00$ 
#     and saves it in R-workspace ``casestudy_1_0.Rdata'.

#source("fm_casestudy_1_0.r")
# 0.1 Load libraries ----

library("zoo")
library("quantmod")
library("coefplot")
library ("graphics")
library("ggplot2")

#####

# 0.2 Load R dataset: casestudy_1_0.Rdata ----
load("casestudy_1_0.RData")
dim(casestudy1.data0.00)
names(casestudy1.data0.00)
head(casestudy1.data0.00)
tail(casestudy1.data0.00)

# 0.3 Define functions ----

# function to compute daily returns of a symbol
fcn.compute.r.daily.symbol0<-function(
  symbol0="SP500",
  rdbase0=casestudy1.data0.00, scaleLog=FALSE){
  
  indexcol.symbol0<-match(symbol0, dimnames(rdbase0)[[2]],nomatch=0)
  if (indexcol.symbol0==0){ return(NULL)}
  if(scaleLog==FALSE){
  r.daily.symbol0<-zoo(
    x=exp(as.matrix(diff(log(rdbase0[,indexcol.symbol0]))))-1,
    order.by=as.Date(time(rdbase0)[-1]))
    dimnames(r.daily.symbol0)[[2]]<-paste("r.daily.",symbol0,sep="")
    return(r.daily.symbol0)
  }

  if(scaleLog==TRUE){
    r.daily.symbol0<-zoo(
      x=(as.matrix(diff(log(rdbase0[,indexcol.symbol0])))),
      #order.by=time(rdbase0)[-1])
      order.by=as.Date(time(rdbase0)[-1]))
      dimnames(r.daily.symbol0)[[2]]<-paste("dlog.daily.",symbol0,sep="")
      return(r.daily.symbol0)
  }
  return(NULL)

}

fcn.compute.r.daily.riskfree<-function(rdbase0=casestudy1.data0.00){
  r.daily.riskfree<-(.01*coredata(rdbase0[-1,"DGS3MO"]) *
                     diff(as.numeric(time(rdbase0)))/360)
  dimnames(r.daily.riskfree)[[2]]<-"r.daily.riskfree"
  r.daily.riskfree0<-zoo(
    x=as.matrix(r.daily.riskfree),
    #order.by=time(rdbase0)[-1])
    order.by=as.Date(time(rdbase0)[-1]))
  return(r.daily.riskfree0)
}

# function to compute submatrix of kperiod returns
fcn.rollksub<-function(x,kperiod=2,...){
  x.0<-filter(as.matrix(coredata(x)), f=rep(1,times=kperiod), sides=1)
  n0=floor(length(x)/kperiod)
  indexsub0<-seq(kperiod,length(x),kperiod)
  x.00<-x.0[indexsub0]
  return(x.00)
  
}

# 1.1 Define list.symbol.00 
list.symbol.00<-c("GE","BAC","XOM","JDSU")

# 2.0 Setup CAPM for symbol0 ----
symbol0<-list.symbol.00[1]
#symbol0<-list.symbol.00[2]
#symbol0<-list.symbol.00[3]
#symbol0<-list.symbol.00[4]

#   2.1 Plot time series of symbol0, SP500 and risk-free asset -----
#   symbol0, SP500, and the risk-free interest rate

opar<-par()
# set graphics parameter to 3 panels
par(mfcol=c(3,1))
plot(casestudy1.data0.00[,symbol0],ylab="Price",
     main=paste(symbol0, " Stock",sep=""))

plot(casestudy1.data0.00[,"SP500"], ylab="Value",main="S&P500 Index")

plot(casestudy1.data0.00[,"DGS3MO"], ylab="Rate" ,
     main="3-Month Treasury Rate (Constant Maturity)")
# reset graphics parameter to 1 panel
par(mfcol=c(1,1))

#   2.2 Compute the returns series ----

r.daily.symbol0<-fcn.compute.r.daily.symbol0(symbol0, casestudy1.data0.00)
r.daily.SP500<-fcn.compute.r.daily.symbol0("SP500", casestudy1.data0.00)
r.daily.riskfree<-fcn.compute.r.daily.riskfree(casestudy1.data0.00)


# Note for returns time series of riskfree asset 
#   holding periods vary from 1-3 days corresponding
#   to mid-week and over-weekend returns)
#   plot(r.daily.riskfree)

# Compute excess returns of symbol0 and SP500
r.daily.symbol0.0 <-r.daily.symbol0 - r.daily.riskfree
 dimnames(r.daily.symbol0.0)[[2]]<-paste("r.daily.",symbol0,".0",sep="")

r.daily.SP500.0<-r.daily.SP500 - r.daily.riskfree
  dimnames(r.daily.SP500.0)[[2]]<-"r.daily.SP500.0"

dim(r.daily.SP500.0)

#   2.3 Create r.daily.data0 = merged series ----
#     and display first and last sets of rows
dimnames(r.daily.symbol0)[[2]]
# Note: r.daily.symbol0 has names that use symbol0
r.daily.data0<-merge(r.daily.symbol0, r.daily.SP500, r.daily.riskfree, 
                     r.daily.symbol0.0, r.daily.SP500.0)

head(r.daily.data0)
tail(r.daily.data0)

#   2.4 Excess Returns plot:  symbol0 vs SP500 ----
par(mfcol=c(1,1))
plot(r.daily.SP500.0, r.daily.symbol0.0, main=symbol0)
abline(h=0,v=0)


# 3. Linear Regression for CAPM ----

#The linear regression model is fit using the R-function lm():

options(show.signif.stars=FALSE)
# (by default the output from summary.lm() uses stars (*) 
#   to indicate significant coefficients; 
#   automated processing of the output encounters errors
#   with  asterisks so the option to not show them is necessary)

lmfit0<-lm(r.daily.symbol0.0 ~ r.daily.SP500.0, x=TRUE, y=TRUE)

#   3.1 Apply lm(), summary.lm() ----
# The components of lmfit0:
names(lmfit0)

lmfit0.summary<-summary.lm(lmfit0)
# The components of lmfit0.summary:
names(lmfit0.summary)

print(lmfit0.summary)

# Under CAPM, the intercept should be zero
tstat.intercept<-round(lmfit0.summary$coefficients["(Intercept)", "t value"],digits=4)
pvalue.intercept<-round(lmfit0.summary$coefficients["(Intercept)", "Pr(>|t|)"],digits=4)

# The $t$-statistic for the intercept is:
print(tstat.intercept)

# The p-value for testing whether the intercept is zero is
print(pvalue.intercept)


#   3.2  R-squared plot ----
lmfit0.rsquared<-lmfit0.summary$r.squared
lmfit0.rsquared.pvalue<-pf(
  q=lmfit0.summary$fstatistic[["value"]],
  df1=lmfit0.summary$fstatistic[["numdf"]],
  df2=lmfit0.summary$fstatistic[["dendf"]],
  lower.tail=FALSE)

length(as.numeric(lmfit0$fitted.values))
length(as.numeric(lmfit0$y))

modelname0<-"CAPM"

plot(x=lmfit0$fitted.values,
     y=lmfit0$y,
     xlab="Fitted Values",
     ylab="Actual Values",
     main=paste(c(modelname0,
                  "\n Plot: Actual vs Fitted Values\n",
                  "(R-Squared = ",round(lmfit0.rsquared,digits=4),
                  ")"),collapse=""))
abline(a=0,b=1,col='green',lwd=2)

#   3.3 Coefficients Plot ----
library("coefplot")
par(mfcol=c(1,1))
coefplot(lmfit0, lwdInner=4, lwdOuter=1, 
         title=paste(modelname0,
                     "\n Coefficients Plot",sep=""),
         plot=TRUE)

#   3.4a Residuals Analysis:  Histogram/Gaussian Fits----
std.residuals<-sort(as.numeric(lmfit0$residuals))

par(mfcol=c(1,1))
hist(std.residuals, freq=FALSE, nclass=100,
     main=paste(c(modelname0 ,
                  "\n Histogram of Std. Residuals \n",
                  "Normal Fits:  MLE(Green) and Robust(Blue)"),
                collapse=""))

std.residuals.mean=mean(std.residuals)
std.residuals.stdev.mle=sqrt(mean(std.residuals^2) - (mean(std.residuals))^2)
std.residuals.stdev.robust=IQR(std.residuals)/1.3490

std.residuals.density.mle<-dnorm(std.residuals, mean=std.residuals.mean, sd=std.residuals.stdev.mle)
std.residuals.density.robust<-dnorm(std.residuals, mean=std.residuals.mean, sd=std.residuals.stdev.robust)

lines(std.residuals, std.residuals.density.mle, col='green',lwd=2)
lines(std.residuals, std.residuals.density.robust, col='blue',lwd=2)

#   3.4b Residuals Analysis:  QQPlot/Gaussian Fits----
par(mfcol=c(1,1))
qqnorm(std.residuals,
       main=paste(modelname0,
                  "\n Normal QQ Plot of Std. Residuals",sep=""))

abline(a=0,b=sqrt(var(std.residuals)), col='green', lwd=3)
abline(a=0,b=IQR(std.residuals)/1.3490, col='blue', lwd=3)
title(sub=paste("Residual Sigma Fits: MLE(Green) and Robust(Blue)"))

#   3.4c Residuals Analysis:  MLE-Percentile Histogram ----
par(mfcol=c(1,1))
#
nclass0=100
hist(100.*pnorm(std.residuals,mean=std.residuals.mean, sd=std.residuals.stdev.mle), nclass=nclass0,
     xlab="Fitted Percentile",
     main=paste(c(modelname0,
                  "\nHistogram of Residuals\nMLE-Fitted Percentiles"),
                collapse=""))
abline(h=length(std.residuals)/nclass0, col="green",lwd=2)

#   3.4c Residuals Analysis:  Robust-Fitted Percentile Histogram ----
par(mfcol=c(1,1))
hist(100.*pnorm(std.residuals,mean=std.residuals.mean, sd=std.residuals.stdev.robust), nclass=nclass0,
     xlab="Fitted Percentile",
     main=paste(c(modelname0,
                  "\nHistogram of Residuals\nRobust-Fitted Percentiles"),
                collapse=""))

abline(h=length(std.residuals)/nclass0, col="blue",lwd=2)

#   3.5 Regression Diagnostics: Leverage/Hat Values ----
# For the functions hatvalues() and influence.measures()
# the input argument lmfit0 must handle indexes properly

y00=coredata(r.daily.symbol0.0)
x00=coredata(r.daily.SP500.0)
lmfit00<-lm(y00~x00, x=TRUE, y=TRUE)
# This replacement code snippet eliminates warnings and errors returned from
#   the functon influence.measures(lmfit0) below

#
lmfit0.hat<-zoo(x=hatvalues(lmfit00), 
                           order.by=as.Date(names(lmfit00$fitted.values)))
par(mfcol=c(1,1))
plot(lmfit0.hat, ylab="Leverage/Hat Value",
     main=paste(c(modelname0,
                  "\nLeverage/Hat Values"),
                collapse=""))

# Note the cases are time points of the time series data
#   The financial crisis of 2008 is evident 


#   3.6 Regression Diagnostics Plot----
#     The R function $plot.lm()$ generates a 
#     2x2 display of plots for various regression diagnostic statistics:
oldpar=par(no.readonly=TRUE)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lmfit00)
par(oldpar,no.readonly=TRUE)


#######ADDITION 
#   Some useful R functions

#   anova.lm():  conduct an Analysis of Variance for the linear regression model, detailing the computation of the F-statistic for no regression structure.
#                 #anova.lm(lmfit0)

#   influence.measures():  compute regression diagnostics evaluating case influence for the linear regression model; includes `hat' matirx, case-deletion statistics for the regression coefficients and for the residual standard deviation.



#   3.7 Apply influence.measures()  ----
# Compute influence measures (case-deletion statistics)
lmfit0.inflm<-influence.measures(lmfit00)
# Table counts of influential/non-influential cases
# as measured by the hat/leverage statistic.
table(lmfit0.inflm$is.inf[,"hat"])

#   3.8 Plot data, fitted model, influential cases ----
#      selective highlighting of influential cases

plot(r.daily.SP500.0, r.daily.symbol0.0,
     main=paste(symbol0,"  vs SP500 Data \n OLS Fit (Green line)\n High-Leverage Cases (red points)\n High Cooks Dist (blue Xs)",sep=""),
     cex.main=0.8)
abline(h=0,v=0)
abline(lmfit0, col=3, lwd=3)

# Plot cases with high leverage as red (col=2) "o"s
index.inf.hat<-which(lmfit0.inflm$is.inf[,"hat"]==TRUE)
points(r.daily.SP500.0[index.inf.hat], r.daily.symbol0.0[index.inf.hat], 
        col=2, pch="o")

# Plot cases with high cooks distance as big (cex=2) blue (col=4) "X"s
index.inf.cook.d<-which(lmfit0.inflm$is.inf[,"cook.d"]==TRUE)
dim(r.daily.SP500.0)
dim(r.daily.SP500.0)

points(r.daily.SP500.0[index.inf.cook.d], r.daily.symbol0.0[index.inf.cook.d], 
       col=4, pch="X", cex=2.)
