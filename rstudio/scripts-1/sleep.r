# Rproject8_5_sleep.r

# Reproduce Figure 10.16 of Rice
#   Sleep versus logarithm of brain weight for a collection of mammals.
# 1.0 Read in data ----

sleep.0=read.table(file="Rice 3e Datasets/ASCII Comma/Chapter 10/sleep.txt",
  sep=",",stringsAsFactors = FALSE,
  header=TRUE)

names(sleep.0)
plot(sleep.0$brain_weight, sleep.0$total_sleep)
plot(log(sleep.0$brain_weight), sleep.0$total_sleep)
#help(text)

text(log(sleep.0$brain_weight), sleep.0$total_sleep,labels=sleep.0$animal, adj=c(.2,.1),cex=.5,
     pos=4)
