# Rproject2_script2_gamma_MOMwithMLE.r
# 0.0 Load library
library("MASS")

# 1.0 Read in data ----
#       LeCam and Neyman Precipitation Data from Rice 3e Datasets
#       From Rice, p. 414: 
#   rainfall of summer storms, in inches, measured by network of rain gauges
#     southern Illinois for the years 1960-1964
#     measurements are average amount of rainfall from each storm
#   
#C:\PseudoFdrive\MIT\mit18443\rstudio18443\Rice 3e Datasets\ASCII Comma\Chapter 10
file0.60<-"Rice 3e Datasets\\ASCII Comma\\Chapter 10\\illinois60.txt"
file0.60data<-scan(file=file0.60,sep=",")
file0.61<-"Rice 3e Datasets\\ASCII Comma\\Chapter 10\\illinois61.txt"
file0.61data<-scan(file=file0.61,sep=",")
file0.62<-"Rice 3e Datasets\\ASCII Comma\\Chapter 10\\illinois62.txt"
file0.62data<-scan(file=file0.62,sep=",")
file0.63<-"Rice 3e Datasets\\ASCII Comma\\Chapter 10\\illinois63.txt"
file0.63data<-scan(file=file0.63,sep=",")
file0.64<-"Rice 3e Datasets\\ASCII Comma\\Chapter 10\\illinois64.txt"
file0.64data<-scan(file=file0.64,sep=",")
data.precipitation<-c(file0.60data, file0.61data, file0.62data, file0.63data, file0.64data)

# 2.  Display the data ----
#   2.1 Histograms with different bin-counts (nclass) ----
par(mfcol=c(1,1))
hist(data.precipitation,nclass=15,
     main="Precipitation Data (nclass=15)\n(Le Cam and Neyman)")

hist(data.precipitation,nclass=50,
     main="Precipitation Data (nclass=50)\n(Le Cam and Neyman)")

#   2.2 Index plot ----

plot(data.precipitation, main="Precipitation Data")


# 3.0 Parameter Estimation of Gamma Distribution ----

#   3.1 Method of moments estimates ----

#     Compute first moment (mean) and variance (second moment minus square of first moment)

data.precipitation.xbar=mean(data.precipitation)
data.precipitation.var=mean(data.precipitation^2) - (mean(data.precipitation))^2

#     Compute MOM estimates per theory
lambdahat.mom=(data.precipitation.xbar)/data.precipitation.var
alphahat.mom=(data.precipitation.xbar^2)/data.precipitation.var

print(lambdahat.mom)
print(alphahat.mom)

#   3.2 Maximum Likelihood Estiamtes

options(warn=-1)
data.precipitation.fitdistr.mle<-fitdistr(data.precipitation,densfun="gamma")
print(data.precipitation.fitdistr.mle)


#   3.2 Simulation of MOM Estimates ----

#     3.2.1 Define function used in simulation ----
# Define function computing MOM estimates of the rate and shape parameters
fcn.fitdistr.mom.gamma<-function(x){
  x.mean=mean(x)
  x.var=mean(x^2) - (x.mean^2)
  rate.mom=x.mean/x.var
  shape.mom=(x.mean^2)/x.var
  result=c(shape=shape.mom, rate=rate.mom)
  return(result)
}

data.precipitation.mom.gamma<-fcn.fitdistr.mom.gamma(data.precipitation)

print(data.precipitation.mom.gamma)

shape.mom=data.precipitation.mom.gamma["shape"]
rate.mom=data.precipitation.mom.gamma["rate"]

data.precipitation.fitdistr.mle<-fitdistr(data.precipitation,densfun="gamma")
print(data.precipitation.fitdistr.mle)
shape.mle=data.precipitation.fitdistr.mle$estimate["shape"]
rate.mle=data.precipitation.fitdistr.mle$estimate["rate"]

#     3.2.2 Set up simulation objects ----
nsimulation=1000
n0=length(data.precipitation)
output.mom.simulation<-matrix(nrow=nsimulation, ncol=2)
dimnames(output.mom.simulation)[[2]]<-c("shape","rate")

output.mle.simulation<-matrix(nrow=nsimulation, ncol=2)
dimnames(output.mle.simulation)[[2]]<-c("shape","rate")
options(warn=-1)

#     3.2.3 Conduct simulation ----
for (j in c(1:nsimulation)){
  #j=1
  data.simulation=rgamma(n0,shape=shape.mle, rate=rate.mle)

  data.simulation.fitdistr.mom<-fcn.fitdistr.mom.gamma(data.simulation)
  output.mom.simulation[j, 1]<-data.simulation.fitdistr.mom["shape"]
  output.mom.simulation[j, 2]<-data.simulation.fitdistr.mom["rate"]
# 
# For MLE use function fitdistr from the library MASS
#   turn off warnings
  options(warn=-1)
#  data.simulation.fitdistr.mle<-fitdistr(data.simulation,densfun="gamma")
data.simulation.fitdistr.mle<-suppressWarnings(fitdistr(data.simulation,densfun="gamma"))


  output.mle.simulation[j, "shape"]<-data.simulation.fitdistr.mle$estimate["shape"]
  output.mle.simulation[j, "rate"]<-data.simulation.fitdistr.mle$estimate["rate"]
  
  
  #  print(j)
#  print(output.mom.simulation[j,])
}

# 4 Display/Compare  sampling distributions of MOMs and MLEs----

par(mfcol=c(2,1))
#   4.1.1 Displays for MOM of shape parameter ----
hist(output.mom.simulation[,"shape"],nclass=50)

simulation.shape.mom.Mean=mean(output.mom.simulation[,"shape"])
simulation.shape.mom.StandardError=sqrt(var(output.mom.simulation[,"shape"]))
print(shape.mom)
print(simulation.shape.mom.Mean)
print(simulation.shape.mom.StandardError)

# Add red vertical(v) lines at simulation mean +/- 1 standard error
abline(v=simulation.shape.mom.Mean + c(-1,0,1)*simulation.shape.mom.StandardError,
       col=c("red","red","red"),lwd=c(2,2,2))
# Add blue vertical(v) line at true shape parameter for simulation
abline(v=shape.mle,col="blue",lwd=2)


#   4.1.2 Displays for MLE of shape parameter
hist(output.mle.simulation[,"shape"],nclass=50)

simulation.shape.mle.Mean=mean(output.mle.simulation[,"shape"])
simulation.shape.mle.StandardError=sqrt(var(output.mle.simulation[,"shape"]))
print(shape.mle)
print(simulation.shape.mle.Mean)
print(simulation.shape.mle.StandardError)

# Add red vertical(v) lines at simulation mean +/- 1 standard error
abline(v=simulation.shape.mle.Mean + c(-1,0,1)*simulation.shape.mle.StandardError,
       col=c("green","green","green"),lwd=c(3,3,3))
# Add blue vertical(v) line at true shape parameter for simulation
abline(v=shape.mle,col="blue",lwd=2)
##########
#
# 
#   4.2.1 Displays of MOM for Rate parameter ----
par(mfcol=c(2,1))
hist(output.mom.simulation[,"rate"],nclass=50)

simulation.rate.mom.Mean=mean(output.mom.simulation[,"rate"])
simulation.rate.mom.StandardError=sqrt(var(output.mom.simulation[,"rate"]))
print(rate.mom)
print(simulation.rate.mom.Mean)
print(simulation.rate.mom.StandardError)

# Add red vertical(v) lines at simulation mean +/- 1 standard error
abline(v=simulation.rate.mom.Mean + c(-1,0,1)*simulation.rate.mom.StandardError,
       col=c("red","red","red"),lwd=c(2,2,2))
# Add blue vertical(v) line at true rate parameter for simulation
abline(v=rate.mle,col="blue",lwd=2)


#   4.2.2 Displays of MLE for Rate parameter ----
# 
hist(output.mle.simulation[,"rate"],nclass=50)

simulation.rate.mle.Mean=mean(output.mle.simulation[,"rate"])
simulation.rate.mle.StandardError=sqrt(var(output.mle.simulation[,"rate"]))
print(rate.mle)
print(simulation.rate.mle.Mean)
print(simulation.rate.mle.StandardError)

# Add red vertical(v) lines at simulation mean +/- 1 standard error
abline(v=simulation.rate.mle.Mean + c(-1,0,1)*simulation.rate.mle.StandardError,
       col=c("red","red","red"),lwd=c(2,2,2))
# Add blue vertical(v) line at true rate parameter for simulation
abline(v=rate.mle,col="blue",lwd=2)

#   4.3. Boxplot comparisons of estimates

#help(boxplot) 
par(mfcol=c(1,2))
boxplot(cbind(MOM=output.mom.simulation[,"shape"],MLE= output.mle.simulation[,"shape"]),
        main="Shape Parameter\nSampling Distributions\n(simulated)")
boxplot(cbind(MOM=output.mom.simulation[,"rate"],MLE= output.mle.simulation[,"rate"]),
        main="Rate Parameter\nSampling Distributions\n(simulated)")


par(mfcol=c(1,1))
plot(y=output.mom.simulation[,"shape"],x= output.mle.simulation[,"shape"],
     xlab="MLE",ylab="MOM",
     main="Shape Parameter\nSampling Distributions\n(simulated)")

# Add green vertical(v) lines at MLE simulation mean +/- 1 standard error
abline(v=simulation.shape.mle.Mean + c(-1,0,1)*simulation.shape.mle.StandardError,
       col=c("green","green","green"),lwd=c(3,3,3))
# Add blue vertical(v) line at true shape parameter for simulation
abline(v=shape.mle,col="blue",lwd=2)

# Add red horizontal (h) lines at MOM simulation mean +/- 1 standard error
abline(h=simulation.shape.mom.Mean + c(-1,0,1)*simulation.shape.mom.StandardError,
       col=c("red","red","red"),lwd=c(2,2,2))
# Add blue horizontal(h) line at true shape parameter for simulation
abline(h=shape.mle,col="blue",lwd=2)
#
par(mfcol=c(1,1))
plot(y=output.mom.simulation[,"rate"],x= output.mle.simulation[,"rate"],
     xlab="MLE",ylab="MOM",
     main="Rate Parameter\nSampling Distributions\n(simulated)")

# Add green vertical(v) lines at MLE simulation mean +/- 1 standard error
abline(v=simulation.rate.mle.Mean + c(-1,0,1)*simulation.rate.mle.StandardError,
       col=c("green","green","green"),lwd=c(3,3,3))
# Add blue vertical(v) line at true rate parameter for simulation
abline(v=rate.mle,col="blue",lwd=2)

# Add red horizontal (h) lines at MOM simulation mean +/- 1 standard error
abline(h=simulation.rate.mom.Mean + c(-1,0,1)*simulation.rate.mom.StandardError,
       col=c("red","red","red"),lwd=c(2,2,2))
# Add blue horizontal(h) line at true rate parameter for simulation
abline(h=rate.mle,col="blue",lwd=2)
#
