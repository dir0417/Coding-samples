# Rproject2_script1_gamma_MOM.r

# 1.0 Read in data ----
#       LeCam and Neyman Precipitation Data from Rice 3e Datasets
#       From Rice, p. 414: 
#   rainfall of summer storms, in inches, measured by network of rain gauges
#     southern Illinois for the years 1960-1964
#     measurements are average amount of rainfall from each storm
#   
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

#     3.2.2 Set up simulation objects ----
nsimulation=1000
n0=length(data.precipitation)
output.mom.simulation<-matrix(nrow=nsimulation, ncol=2)
dimnames(output.mom.simulation)[[2]]<-c("shape","rate")

#     3.2.3 Conduct simulation ----
for (j in c(1:nsimulation)){
  #j=1
  data.simulation=rgamma(n0,shape=shape.mom, rate=rate.mom)

  data.simulation.fitdistr<-fcn.fitdistr.mom.gamma(data.simulation)
  output.mom.simulation[j, 1]<-data.simulation.fitdistr["shape"]
  output.mom.simulation[j, 2]<-data.simulation.fitdistr["rate"]
#  print(j)
#  print(output.mom.simulation[j,])
}

#     3.2.4 Display MOM estimates sampling distributions ----

#       Displays for shape parameter
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
abline(v=shape.mom,col="blue",lwd=2)



#       Displays for rate parameter
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
abline(v=rate.mom,col="blue",lwd=2)
