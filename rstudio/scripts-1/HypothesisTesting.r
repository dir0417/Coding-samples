# RProject5_HypothesisTesting.r

# 1.0   Two coins:  coin0 and coin1
#
#   P(Head | coin0)=0.5 and P(Head | coin1) =0.7
prob.coin0=0.5
prob.coin1=0.7

#help(sample)
# 2.0 Choose a coin at random, count number of heads in 10 tosses
prob.coin.random=sample(c(prob.coin0,prob.coin1), size=1)

x=rbinom(n=1, size=10, prob=prob.coin.random)

x

# 3.  Likelihood Table

#     For each outcome of X (column 1) 
#       report Likelihood of coin0 (column2) and of coin1 (column 3)
# X= number of heads on 10 tosses of coin
list.x=0:10

pdf.coin0=dbinom(list.x, size=10,prob=prob.coin0)
pdf.coin1=dbinom(list.x, size=10,prob=prob.coin1)

likelihood.table=cbind(
  x=list.x, like.coin0=pdf.coin0, like.coin1=pdf.coin1,
  likeratio= pdf.coin0/pdf.coin1)

likelihood.table

# Plot pmf function of X under H0 and H1
par(mfcol=c(1,1))
plot(list.x, pdf.coin0, xlab="x", ylab="p(x | theta)", 
     ylim=c(0, max(c(pdf.coin0, pdf.coin1))))

points(list.x, pdf.coin1, col='red')
title(main="p(x | theta) for H0 (Black) and H1 (Red)")
title(sub=paste(
  "H0: theta = ", prob.coin0, ",  H1: theta = ", prob.coin1, collapse=""))

# Plot Likelihood Ratio H0:H1 as a function of X

par(mfcol=c(1,1))
plot(likelihood.table[,"x"], likelihood.table[,"likeratio"], xlab="x", ylab= "LikeRatio",
     main="Likelihood Ratio  H0:H1")
list.levels=c(1/10, 1,10)
abline(h=1, col='grey')
abline(h=list.levels, col=c(2,3,4))

plot(likelihood.table[,"x"], log(likelihood.table[,"likeratio"]), xlab="x", ylab= "Log LikeRatio",
     main="Likelihood Ratio  HO:H1")
abline(h=0,col='gray')
abline(h=log(list.levels), col=c(2,3,4))

paste(list.levels, collapse=", ")
title(sub=paste("LR Levels:  ", paste(list.levels, collapse=", "), sep=""))

# 3.0 Bayes Rule accepts H0 if Likelihood Ratio > c
#     where c=P(H1)/P(H0);   the prior odds of H1 to H0

# 4.0 Consider decision rule for each value of x*=0,1, ...,10
#
# d.x*=1 if x >=x*

#Create table of probability of errors
list.xstar=c(-1:10)

table.errorProbs<-matrix(NA, nrow=length(list.xstar), ncol=3)
for (j.xstar in c(1:length(list.xstar))){
  
  xstar=list.xstar[j.xstar]
  table.errorProbs[j.xstar,1]=xstar
  prob.rejectH0.given.H0=1-pbinom(xstar,size=10,prob=prob.coin0)
  table.errorProbs[j.xstar, 2]=prob.rejectH0.given.H0
  prob.acceptH0.givenH1 = pbinom(xstar,size=10,prob=prob.coin1)
  table.errorProbs[j.xstar, 3]=prob.acceptH0.givenH1
}
dimnames(table.errorProbs)[[2]]<-c("xstar", "P(Reject H0 | H0)", "P(Accept H0 | H1)")

print(table.errorProbs)

#


plot(table.errorProbs[,1], table.errorProbs[,2], 
     xlab="x* (Reject H0 if X > x*)",
    ylab="P(Reject H0 | H0)",
    main="P(Type I Error)")


par(mfcol=c(1,1))

plot(table.errorProbs[,1], table.errorProbs[,3], 
     xlab="x* (Reject H0 if X > x*)",
     ylab="P(Accept H0 |H1)",
     main="P(Type II Error)")



par(mfcol=c(1,1))

plot(table.errorProbs[,2], table.errorProbs[,3], 
     xlab="P(Reject H0 | H0)",
     ylab="P(Accept H0 | H1)",
     main="Risk Points of Decision Rules\nP(Type II Error) vs P(Type I Error)"
)

# Add Risk Points of decision rules d2.x* (opposite of rules d.x*)

# for constants c in list.xstar

# d.xstar=1 if x >=xstar
# d2.xstar=1 if x < xstar

table.errorProbs2<-matrix(NA, nrow=length(list.xstar), ncol=3)
for (j.xstar in c(1:length(list.xstar))){
  
  c=list.xstar[j.xstar]
  table.errorProbs2[,1]=c
  prob.rejectH0.given.H0=pbinom(c,size=10,prob=prob.coin0)
  table.errorProbs2[j.xstar, 2]=prob.rejectH0.given.H0
  prob.acceptH0.givenH1 = 1-pbinom(c,size=10,prob=prob.coin1)
  table.errorProbs2[j.xstar, 3]=prob.acceptH0.givenH1
}

points(table.errorProbs2[,2], table.errorProbs2[,3],pch="x",col="red")

