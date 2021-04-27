# Rproject3_script3_Poisson_PrussianCavalry.r

table.counts<-matrix(
  c(0,109,
    1,65,
    2,22,
    3,3,
    4,1), nrow=5,ncol=2,byrow=2,dimnames=list(NULL,c("Number","Count")
                                              ))

table.counts
n0=sum(table.counts[,"Count"])
lambda.mle=sum(table.counts[,"Count"]*table.counts[,"Number"] ) / n0
lambda.mle.sterr=sqrt(lambda.mle/n0)

alpha0=.05
z.alpha0.half=qnorm(1-alpha0/2)
lambda.CI.lower=lambda.mle - z.alpha0.half*lambda.mle.sterr
lambda.CI.upper=lambda.mle + z.alpha0.half*lambda.mle.sterr

print(c(lambda.mle, lambda.mle.sterr, z.alpha0.half, lambda.CI.lower, lambda.CI.upper)
)
sum(table.counts[,"Count"])
