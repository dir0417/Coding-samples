# Rproject10_flow_occ_regressions.r
# 1.0 Read in data ----
#       See Problem 10.9.50, 
#       data from:  http://pems.eecs.berkeley.edu
# For each of three lanes, the 
#   flow (number of cars)
#   occupancy (percentage of time a car was over the loop)
#
#     1740  5-minute intervals
#     Lane 1 farthest left lane, lane 2 center, lane 3 farthest right


flowocc=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 10/flow-occ.txt",
                   sep=",",stringsAsFactors = FALSE,
                   header=TRUE)

Timestamp2 = strptime(flowocc$Timestamp, "%m/%d/%Y %H:%M:%S")
#plot(Timestamp2, flowocc$Lane.1.Occ)
#plot(flowocc$Lane.1.Occ)
flowocc$Timestamp=Timestamp2

lmfit1=lm(Lane.3.Occ ~ Lane.1.Occ, data=flowocc)
plot(flowocc$Lane.1.Occ, flowocc$Lane.3.Occ)

lmfit1=lm(Lane.3.Occ ~ Lane.1.Occ, data=flowocc)
abline(lmfit1,col="green")

plot(flowocc$Lane.1.Occ, lmfit1$residuals)
abline(h=0,col="gray")

qqnorm(lmfit1$residuals)

# Consider two subsets

ind.subset1=(flowocc$Lane.1.Occ < .18)

ind.subset2=(flowocc$Lane.1.Occ > .18)

# For first subset:

plot(flowocc$Lane.1.Occ[ind.subset1], flowocc$Lane.3.Occ[ind.subset1])

lmfit1.subset1=lm(Lane.3.Occ ~ Lane.1.Occ, data=flowocc, weight=1*ind.subset1)
abline(lmfit1.subset1,col="green")

plot(flowocc$Lane.1.Occ[ind.subset1], lmfit1.subset1$residuals[ind.subset1])

abline(h=0,col="gray")
qqnorm(lmfit1.subset1$residuals[ind.subset1])

# For second subuset:

plot(flowocc$Lane.1.Occ[ind.subset2], flowocc$Lane.3.Occ[ind.subset2])

lmfit1.subset2=lm(Lane.3.Occ ~ Lane.1.Occ, data=flowocc, weight=1*ind.subset2)
abline(lmfit1.subset2,col="green")

plot(flowocc$Lane.1.Occ[ind.subset2], lmfit1.subset2$residuals[ind.subset2])

abline(h=0,col="gray")

qqnorm(lmfit1.subset2$residuals[ind.subset2])

# For second subuset:

