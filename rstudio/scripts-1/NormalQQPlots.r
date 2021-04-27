  # RProject7_4_NormalQQPlots.r

set.seed(1)

# 1.0 Normal QQ Plots 

#     Illustrating how the Normal QQ Plot 
#     changes when the data is mean-shifted re-scaled

#n.samplesize=100
for (n.samplesize in c(25,100,400)){
par(mfcol=c(2,2))
# Random sample 
x=rnorm(n.samplesize)

qqnorm(x, main="Normal Q-Q Plot\n x ~ N(0,1)")
title(sub=paste("Sample Size = ", as.character(n.samplesize),sep=""))

qqnorm(5+x, main="Normal Q-Q Plot\n 5+ x \nx ~ N(0,1)")

qqnorm(+3*x, main="Normal Q-Q Plot\n  3*x \nx ~ N(0,1)")


qqnorm(5+3*x, main="Normal Q-Q Plot\n 5+ 3*x \nx ~ N(0,1)")
}