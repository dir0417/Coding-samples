# Rproject3_script1_chromatin.r

# 1.0 Read in data ----
#       See problem 8.10.45, p 321 of Rice.
#
#   Three experiments where 100-200 measurements of 2-dimensional distances were determined.
#   
file0.short<-"Rice 3e Datasets\\ASCII Comma\\Chapter 8\\chromatin\\data32.txt"
data.short<-scan(file=file0.short,sep=",")

file0.medium<-"Rice 3e Datasets\\ASCII Comma\\Chapter 8\\chromatin\\data05.txt"
data.medium<-scan(file=file0.medium,sep=",")

file0.long<-"Rice 3e Datasets\\ASCII Comma\\Chapter 8\\chromatin\\data33.txt"
data.long<-scan(file=file0.long,sep=",")


# 2.  Display the data ----
#   2.1 Histograms with different bin-counts (nclass) ----
nclass0=15
par(mfcol=c(3,1))
hist(data.short,nclass=nclass0, 
     main=paste(c("Chromatin/short Data \n (nclass=",as.character(nclass0),")"),
                     collapse=""))
hist(data.medium,nclass=nclass0, 
     main=paste(c("Chromatin/medium Data \n (nclass=",as.character(nclass0),")"),
                collapse=""))

hist(data.long,nclass=nclass0, 
     main=paste(c("Chromatin/long Data \n (nclass=",as.character(nclass0),")"),
                collapse=""))
#   2.2 Index plots ----
par(mfcol=c(3,1))


plot(data.short, main="Chromatin/short Data")
plot(data.medium, main="Chromatin/medium Data")
plot(data.long, main="Chromatin/long Data")

# 3.0 Functions computing MLE and MOM estimates ----

fcn.rayleigh.mle<-function(x){
  theta.mle=sqrt(mean(.5*x*x))
  return(theta.mle)
}

fcn.rayleigh.mom<-function(x){
  theta.mom=sqrt(2./pi)*mean(x)
  return(theta.mom)
}

# 4. Analyse Chromatin/Short data


#   4.1 Compute MLE and MOM Estimates ----
fcn.rayleigh.mle(data.short)

fcn.rayleigh.mom(data.short)



# 4.2   Plot density function of Rayleigh -----
data0=data.short
main.line1="Chromatin/short Data"

x.grid<-seq(0,max(data0)*1.1,max(data0)/1000)
theta0.mle=fcn.rayleigh.mle(data0)
theta0.mom=fcn.rayleigh.mom(data0)

x.grid.density.mle<- (x.grid/(theta0.mle^2))* exp( - (x.grid^2)/(2*theta0.mle^2))
x.grid.density.mom<- (x.grid/(theta0.mom^2))* exp( - (x.grid^2)/(2*theta0.mom^2))

par(mfcol=c(1,1))
hist(data0, nclass=15, xlim=c(0, 1.1*max(data0)), probability=TRUE,
     main=paste(main.line1,"\n MLE (Green) and MOM (Red)",sep=""))

lines(x.grid, x.grid.density.mle, type="l",col="green")
lines(x.grid, x.grid.density.mom, type="l",col="red")
