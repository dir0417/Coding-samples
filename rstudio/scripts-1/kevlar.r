# Rproject7_script3_kevlar.r


# 1.0 Read in data ----
#       See Problem 10.9.43, data from Barlow, Toland, and Freeman (1984)
#
#  Times to failure (in hours) of strands of kevlar tested at
#   stress levels of 70%, 80%, and 90%.
#   (The space shuttle uses  spherical vessels of kevlar 
#   which are in an environment of sustained pressure)

kevlar70=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/kevlar70.txt",sep=",")
kevlar80=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/kevlar80.txt",sep=",")
kevlar90=scan(file="Rice 3e Datasets/ASCII Comma/Chapter 10/kevlar90.txt",sep=",")

#     Plot the data (check for errors)
plot(kevlar70)
plot(kevlar80)
plot(kevlar90)

#  2.  Compute empirical survival functions for each group ---

#   2.1 Define fcn.ecdf 
#         empirical cdf of time-to-failure
#   Inputs
#         x   (times of failures)
#         n   (total number of items/individuals)
#   For jth smallest failure, set ecdf = j/(n+1)
fcn.ecdf<-function(x, n=72){
  if (sum(1*(diff(x)<0))==0){
    x.0=x
  x.0.ecdf=c(1:length(x.0))/(n+1)
  return(x.0.ecdf)}else{return(NULL)}
}

kevlar70.esf<-1-fcn.ecdf(kevlar70,n=length(kevlar70))
kevlar80.esf<-1-fcn.ecdf(kevlar80,n=length(kevlar80))
kevlar90.esf<-1-fcn.ecdf(kevlar90,n=length(kevlar90))

# 3.0  Plot Empirical Survival Functions ---

plot(kevlar70, kevlar70.esf)
plot(kevlar80, kevlar80.esf)
plot(kevlar90, kevlar90.esf)
#
#   Plots of Log Empirical Survival Functions

plot(kevlar70, log(kevlar70.esf))
plot(kevlar80, log(kevlar80.esf))
plot(kevlar90, log(kevlar90.esf))
# Re-do plot to focus on data range
plot(kevlar90, log(kevlar90.esf),xlim=c(0, 2.2))

