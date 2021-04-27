# Problem 9.11.7

# X1, ..., Xn are iid Poisson(lambda)
#

# Test H0: lambda=lambda0 vs H1: lambda=lambda1

# Consider likelihood ratio 
# LR=  f(x1,...,xn | lambda0)/f(x1,...,xn | lambda1).
#
# The LR decreases with sum(x) if lambda1>lambda0
#     and decreases with -sum(x) if lambda1<lambda0

# Consider lambda0=3., lambda1=4., and n=10
#
# Find level alpha=0.05 test

# Rejection region is sum(x)>k.
# Determine k such that
#   P(sum(x)>k)<=alpha under null hypothesis
#
# sum(x) ~ Poisson(n*lambda), so under null hypothesis
# sum(x) ~ Poission(10*3.=30.)
alpha=0.05
k=qpois(1-alpha, lambda=30.)
print(k)
print(ppois(k,lambda=30)) # cdf at k

# Actual alpha is
1-ppois(k,lambda=30)
