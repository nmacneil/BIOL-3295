require(deSolve)
# Remove all objects - usually start with this
rm(list=ls())

# Evolution when both alleles compete for finite resourses since population dynamics
# are density-dependent.

# This model formulation is eqns. 2 in Travis et al. 2013 Density-dependent selection or
# density-dependent fitness?

EvolutionEqns <- function(t,y,p){
  N1 = y[1]
  N2 = y[2]
  dN1 = N1*(a1 - b11*N1 - b12*N2)
  dN2 = N2*(a2 - b21*N1 - b22*N2)
  return(list(c(dN1,dN2)))
}
# CASE 1. Genotype 2 is the superior competitor
# Intrinsic growth rate of genotypes 1 and 2
a1<-1
a2<-2

# b_ij represents the strength of intraspecific competition for species i from
# species j.
b11<-1
b22<-1
b12<-1 
# Genotype 2 is relatively unaffected by the presence of genotype 1, and is therefore a
# better competitor
b21<-.1

# The times we are solving for
times=seq(0,100,.1)

# Performing the numerical integration
out = ode(y=c(N1=1,N2=1),times=times,func=EvolutionEqns,p=NULL, method = "ode45")

# Genotype 2 has the higher instrinsic growth rate and is also less 
# sensitive to competition: not surprisingly genotype 2 is more fit and
# goes to fixation while genotype 1 goes extinct.

plot(out[,1], out[,2], ylim = c(0, 2), typ="l", col = "red", xlab = "time",ylab = "no. of individuals with genotype 1 or 2", main = "Genotype 2 (red) sup. competitor & larger r")
lines(out[,1], out[,3], col = "blue")

# CASE 2. Making genotype 1 the better competitor, while genotype 2 still has
# higher instrinsic growth, i.e. trade-off between competitive ability and reproductive
# output.
# b_ij represents the strength of intraspecific competition for species i from
# species j.
b11<-1
b22<-1
b21<-1.8
# Genotype 1 is relatively unaffected by species 2.
b12<-2-b21


# Performing the numerical integration
out = ode(y=c(N1=1,N2=1),times=times,func=EvolutionEqns,p=NULL, method = "ode45")

plot(out[,1], out[,2], ylim = c(0, 2), typ="l", col = "red", xlab = "time",ylab = "no. of individuals with genotype 1 or 2", main = "Better competitor (red), higher fecundity (blue)")
lines(out[,1], out[,3], col = "blue")