# Rproject9_1_flow_occ.r

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
# (a). For each lane, plot flow and occupancy versus time

par(mfcol=c(2,1))
plot(flowocc$Timestamp, flowocc[,"Lane.1.Occ"], main="Lane 1: Occupancy")
plot(flowocc$Timestamp, flowocc[,"Lane.1.Flow"],main="Lane 1: Flow")

par(mfcol=c(2,1))
plot(flowocc$Timestamp, flowocc[,"Lane.2.Occ"], main="Lane 2: Occupancy")
plot(flowocc$Timestamp, flowocc[,"Lane.2.Flow"],main="Lane 2: Flow")

par(mfcol=c(2,1))
plot(flowocc$Timestamp, flowocc[,"Lane.3.Occ"], main="Lane 3: Occupancy")
plot(flowocc$Timestamp, flowocc[,"Lane.3.Flow"],main="Lane 3: Flow")

# Using the time-stamp formatting, the days of the week are indicated on the plots
# Without these time stamps, we see that the Sat-Sun period had consistently low occupancy
# for every lane


# (b). Compare flows in the three lanes by making parallel boxplots
par(mfcol=c(1,1))
boxplot(flowocc[,c("Lane.1.Flow","Lane.2.Flow","Lane.3.Flow")],ylab="Count")
# From the boxplot Lane 2 has the highest flow followed by Lane 1 and Lane 3
# This is based on the medians (middle line of boxplots) as well as each of the 
# upper and lower quartiles.


# (c). Examine relationship in flows by making satter plots.

# Pairs plot of all flows

pairs(flowocc[,c("Lane.1.Flow","Lane.2.Flow","Lane.3.Flow")])

# Individual scatter plots

plot(flowocc$Lane.1.Flow, flowocc$Lane.2.Flow)
abline(a=0,b=1) # plot line with slope 1 and 0 intercept
# For the lower range of Flows (Lane 1 < 75), the Lane 2 Flow is greater than Lane 1 Flow
# At higher range of Flows, Lane 2 Flows fall less than Lane 1 Flows.



plot(flowocc$Lane.1.Flow, flowocc$Lane.3.Flow)
abline(a=0,b=1) # plot line with slope 1 and 0 intercept

# For the lower range of Flows (Lane 1 < 50), the Lane 3 Flow is about the same as Lane 1 Flow.
# At higher range of Flows, Lane 3 Flows fall to be much less than Lane 1 flows, 
# but with high variability.



plot(flowocc$Lane.2.Flow, flowocc$Lane.3.Flow)
abline(a=0,b=1) # plot line with slope 1 and 0 intercept
abline(a=0,b=.7,col='green') # plot line with slope 1 and 0 intercept
abline(a=0,b=.6,col='green') # plot line with slope 1 and 0 intercept

# For most of the range of Flows, Lane 3 Flow is about  60%-70% of Lane 2 Flow.
# The variability in flows gets high with the magnitude of the flow.

# (d). Occupancy can be viewed as a measure of congestion. Find the mean
# and median occupancy in each of the three lanes.


vec.medians=c(median(flowocc$Lane.1.Occ),median(flowocc$Lane.2.Occ), median(flowocc$Lane.3.Occ))
vec.means=c(mean(flowocc$Lane.1.Occ),mean(flowocc$Lane.2.Occ), mean(flowocc$Lane.3.Occ))

print(cbind(vec.medians, vec.means))
# For each lane, the mean occupancy is greater than the median.
# This suggests that the each distribution is skew (not symmetric) to the right.
# The median ranks the lanes (high to low) as Lane 2, Lane 1, Lane 3, which conforms
# with the discussion above in the parallel boxplots.


#(e).  Make histograms of the occupancies, varying the number of bins
par(mfcol=c(2,2))

hist(flowocc$Lane.1.Occ,nclass=25)
hist(flowocc$Lane.1.Occ,nclass=50)
hist(flowocc$Lane.1.Occ,nclass=100)
hist(flowocc$Lane.1.Occ,nclass=200)

# Of these plots, both the 50-bin and 100-bin case looks good.
# Such a high number of bins is possible when the sample size is large; here n=1740.

# Unusual features: there seems to be declining distribution of occupancies over the
# range 0.0 to 0.18; then above 0.19, there is a unimodal distribution.
# The percentage time occupancy is above .19 is 6.8%
mean(flowocc$Lane.1.Occ>.19)
par(mfcol=c(1,1))
hist(flowocc$Lane.1.Occ[flowocc$Lane.1.Occ>.19],nclass=25)

# This range perhaps corrsponds to 5-minute periods during rush hours over the analysis period.

par(mfcol=c(2,2))

hist(flowocc$Lane.2.Occ,nclass=25)
hist(flowocc$Lane.2.Occ,nclass=50)
hist(flowocc$Lane.2.Occ,nclass=100)
hist(flowocc$Lane.2.Occ,nclass=200)
# The 100-bin histogram might be better at capturing the far left range of the distribution.
# It appears that some modest flow is more frequent than no flow.

# Unusual features:  The same feature as for Lane 1 above applies concerning the 
# upper range above 0.15
# Below 0.15, there appears to be a bimodal distribution.



par(mfcol=c(2,2))

hist(flowocc$Lane.3.Occ,nclass=25)
hist(flowocc$Lane.3.Occ,nclass=50)
hist(flowocc$Lane.3.Occ,nclass=100)
hist(flowocc$Lane.3.Occ,nclass=200)
# Like Lane 2, the 100-bin histogram might be better at capturing the far left range of the distribution.
# It appears that some modest flow is more frequent than no flow.
# Also, there may be a bi-mo3a1 part of the distribution in the range less than 0.1.

# Unusual features: The features noted above for Lane 2 appear for Lane 3 too.

#(f).  Make plots to support or refute the statement, when one lane is congested, the others are too.


# Individual scatter plots
par(mfcol=c(1,3))
plot(flowocc$Lane.1.Occ, flowocc$Lane.2.Occ)
abline(a=0,b=1) # plot line with2slope 1 and 0 intercept

plot(flowocc$Lane.1.Occ, flowocc$Lane.3.Occ)
abline(a=0,b=1) # plot line with slope 1 and 0 intercept


plot(flowocc$Lane.2.Occ, flowocc$Lane.3.Occ)
abline(a=0,b=1) # plot line with slope 1 and 0 intercept

# (g).

# Flow vs Occupancy for Lane 1
par(mfcol=c(1,1))
plot(flowocc[,"Lane.1.Occ"], flowocc[,"Lane.1.Flow"],
     main="Lane 1:  Flow vs Occupancy")

par(mfcol=c(1,1))
plot(flowocc[,"Lane.2.Occ"], flowocc[,"Lane.2.Flow"],
     main="Lane 2:  Flow vs Occupancy")


par(mfcol=c(1,1))
plot(flowocc[,"Lane.3.Occ"], flowocc[,"Lane.3.Flow"],
     main="Lane 3:  Flow vs Occupancy")

# For each lane, the relationship of Flow to Occupancy is a nice
# linear relationship for the lower range of Occupancy
# Above occupancy threshholds that range from about .1, .08, .07
# respectively for Lanes 1,2, and 3, the flow tends to decrease, and the variability increases.

# The general relationship appears the same in all 3 lanes, but the threshholds and amount
# of variability is different.
