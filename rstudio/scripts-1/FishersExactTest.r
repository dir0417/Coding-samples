# RProject12_FishersExactTest.r

# Rosen and Jerdee (1974) Data. Rice Section 13.1

table1=data.frame(
  Male=rbind(Promote=21,HoldFile=3),
  Female=rbind(Promote=14,HoldFile=10))

# Print two-way table
table1
# Print rowSums and colSums

colSums(table1)
rowSums(table1)


fisher.test(table1)

# For hypergeometric distribution
#   Urn with m white balls and n black balls
#   X = number of white balls in sample 
# k balls selected without replacement
m=rowSums(table1)["Promote"]
n=rowSums(table1)["HoldFile"]
k=colSums(table1)["Male"]

x.max=min(k,m)
x.min=max(k-n,0)

vec.x=seq(x.min,x.max,1)

vec.x.pdf=dhyper(vec.x, m,n,k)

print(data.frame(x=vec.x,pdf=vec.x.pdf))

plot(vec.x, vec.x.pdf,type="h",
     xlab="x",
     ylab="p(x)",
     main="PDF of Hypergeometric\n m=35, n=13, k=24\n(observed value X=21)")

x.observed=21

text(x=x.observed, 0.,  "X")
x.observed.pdf=dhyper(x.observed,m,n,k)

# Values as extreme than x.observed
#   are x >= x.observed and
#       x <= x.min + (x.max-x.observed)

x.observed.pvalue = sum(vec.x.pdf[ vec.x >= x.observed]) +
    sum(vec.x.pdf[vec.x <= x.min + (x.max-x.observed)])


print(x.observed.pvalue)

# Note: x.observed.pvalue equals:


print(2*(1-phyper(x.observed-1, m,n,k)))

