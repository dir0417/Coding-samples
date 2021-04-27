# RProject12_ChisquareTest.r

# De Veaux, Velleman and Bock (2014) Example

# 1.  Tattoo/HepC  Two-Way Table ----
tableA=data.frame(
  HepC=rbind(TattooParlor=17,
             TattooElsewhere=8,
             NoTattoo=22),
  NoHepC=rbind(TattooParlor=35,
             TattooElsewhere=53,
             NoTattoo=491)
)
print(tableA)

# 
# 2. Conduct ChiSquare Test of Independence ----

# Custom function implementing chisqtest
fcn.chisqtest<-function(tableA){
  
cat("\n Two-Way Table: \n")
print(tableA)

n.total=sum(as.vector(tableA))
cat("\n Total Counts in Table:  ", n.total,"\n")

# Compute marginal probabilities of
# TattooStatus and of HepCStatus
probs.TattooStatus=rowSums(tableA)/n.total
probs.HepCStatus=colSums(tableA)/n.total
cat("\n  MLEs of  row level probabilities\n")
print(probs.TattooStatus)
cat("\n  MLEs of  column level probabilities\n")
print(probs.HepCStatus)

# Compute table of fitted cell probabilities and
#   expected counts assuming independence of two factors
tableA.fittedprobs=as.matrix(probs.TattooStatus)%*% t(
  as.matrix(probs.HepCStatus) )
cat("\n Fitted cell probabilities assuming independence\n")
print(tableA.fittedprobs)

tableA.expected=n.total* tableA.fittedprobs
cat("\n Expected Counts assuming independence \n")
print(tableA.expected)


# Compute standardized residuals fitted table
tableA.chisqresiduals=((tableA - tableA.expected))/sqrt(tableA.expected)
cat("\n Table of Chi-Square Residuals  by cell\n")
print(tableA.chisqresiduals)

# Compute table of chi-square test statistic contributions
tableA.chisqterms=((tableA - tableA.expected)^2)/tableA.expected
cat("\n Table of Chi-Square statistic terms by cell\n")
print(tableA.chisqterms)

tableA.chisqStatistic=sum(as.vector(tableA.chisqterms))
cat("\n Chi-Square Statistic: ",tableA.chisqStatistic,"\n")
df.tableA=(nrow(tableA)-1)*(ncol(tableA)-1)
cat("\n degrees of freedom: ", df.tableA, "\n")
tableA.chisqStatistic.pvalue=1-
  pchisq(tableA.chisqStatistic, df=df.tableA)
cat("\n P-Value :  ", tableA.chisqStatistic.pvalue, "\n\n")

}

fcn.chisqtest(tableA)


# 3.  Apply built-in R function chisq.test() ----
print(chisq.test(tableA, correct=FALSE))


# 
# 4. Specify Two-Way Table aggregating Tattoo  ----
tableB=data.frame(
  HepC=rbind(Tattoo=25,
             NoTattoo=22),
  NoHepC=rbind(Tattoo=88,
               NoTattoo=491)
)
print(tableB)

#   Apply fcn.chisqtest() and chisq.test() ----
fcn.chisqtest(tableB)
chisq.test(tableB)
chisq.test(tableB,correct=FALSE)


# 5. Specify Recidivism Study Two-Way Table ---- 

tableC=data.frame(
  ReOffended=rbind(FGC=46, Control=77),
  NoReOffence=rbind(FGC=186, Control=149))
print(tableC)


#   Apply fcn.chisqtest() and chisq.test() ----

fcn.chisqtest(tableC)

chisq.test(tableC, correct=FALSE)
