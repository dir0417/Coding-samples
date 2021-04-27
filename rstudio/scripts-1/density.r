# RProject8_4_density.r
require(graphics)
set.seed(0)
  x=ifelse(runif(100)<.5, rnorm(100) +5,rnorm(100))
  #  x=ifelse(runif(100)<.5, rgamma(100,shape=3,scale=2),rnorm(100))
  #  x=ifelse(runif(100)<.5, (rnorm(100) +5)^2,(rnorm(100)^2))
  
  
    par(mfcol=c(2,2))


x.density1<-density(x,bw="sj")
hist(x,nclass=50,probability=TRUE,
     main=paste("Density Estimate (bw='sj')",
                "\nN = ", as.character(length(x)),"  Bandwidth=", as.character(round(x.density1$bw,digits=3)),
               collapse=""))
lines(x.density1$x, x.density1$y, col="green")
rug(x)

x.density1<-density(x,bw="nrd0")

hist(x,nclass=50,probability=TRUE,
     main=paste("Density Estimate (bw='nrd0')",
                "\nN = ", as.character(length(x)),"  Bandwidth=", as.character(round(x.density1$bw,digits=3)),
                collapse=""))
lines(x.density1$x, x.density1$y, col="green")
rug(x)


x.density1<-density(x,bw="nrd0",adjust=.2)

hist(x,nclass=50,probability=TRUE,
     main=paste("Density Estimate (bw='nrd0 x .2')",
                "\nN = ", as.character(length(x)),"  Bandwidth=", as.character(round(x.density1$bw,digits=3)),
                collapse=""))
lines(x.density1$x, x.density1$y, col="green")
rug(x)

x.density1<-density(x,bw="nrd0",adjust=4.)

hist(x,nclass=50,probability=TRUE,
     main=paste("Density Estimate (bw='nrd0 x 4')",
                "\nN = ", as.character(length(x)),"  Bandwidth=", as.character(round(x.density1$bw,digits=3)),
                collapse=""))
lines(x.density1$x, x.density1$y, col="green")
rug(x)

### Alternate Kernels:
par(mfcol=c(1,1))
(kernels <- eval(formals(density.default)$kernel))

## show the kernels in the R parametrization
plot (density(0, bw = 1), xlab = "",
      main = "R's density() kernels with bw = 1")
for(i in 2:length(kernels))
  lines(density(0, bw = 1, kernel =  kernels[i]), col = i)
legend(1.5,.4, legend = kernels, col = seq(kernels),
       lty = 1, cex = .8, y.intersp = 1)

# 
stem(x)
args(stem)
#help(stem)
stem(x,scale=2)
stem(x,scale=3)
boxplot(x)

