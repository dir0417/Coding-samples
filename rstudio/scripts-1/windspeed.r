# Rproject8_1_windspeed.r

# 1.0 Read in data ----
#       See Problem 10.9.39
#       data from Simiu and Filliben (1975), analysis of extreme winds

windspeed=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 10/windspeed.txt",
  sep=",",stringsAsFactors = FALSE)
windspeed.0=t(windspeed[,-1])
dimnames(windspeed.0)<-list(c(1:nrow(windspeed.0)), windspeed[,1])
head(windspeed.0)
boxplot(windspeed.0)
#boxplot(windspeed.0, horizontal=TRUE)
boxplot(windspeed.0, horizontal=FALSE,las=2,cex.lab=.5)
