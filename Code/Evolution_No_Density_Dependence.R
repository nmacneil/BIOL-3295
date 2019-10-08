# EVOLUTION WITH NO DENSITY DEPENDENCE IN POPULATION DYNAMICS

# Remove all objects - always start with this
rm(list=ls())

# times
t = seq(0,100)
# intrinsic growth rates for genotype 1 and 2
r1 = 0.1
r2 = 0.2
# initial populatin size for genotypes 1 and 2
n10 = 1
n20 = 1
# population size for genotypes 1 and 2
n1 = n10*exp(r1*t)
n2 = n20*exp(r2*t)

par(mfrow = c(2,1), mar = c(4,4,1,1))
# Graph of the population sizes for genotypes 1 and 2
plot(t,n1, col = "red", typ="l", xlab = "time, t", ylab = "population size")
lines(t,n2, col = "blue")

# Graphs of the relative abundance for genotypes 1 and 2
plot(t,n1/(n1+n2), col = "red", typ="l", ylim = c(0,1), xlab = "time, t", ylab = "relative abundance")
lines(t,n2/(n1+n2), col = "blue")