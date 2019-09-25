# SOLVING MODELS FOR DENSITY-DEPENDENT POPULATION GROWTH
#----------
# Models:
# 1) Continuous time logistic growth
# 2) Discrete time logistic map
# 3) Altervative version of the discrete time logistic map
# 4) Ricker model (discrete time)
# 5) Beverton-Holt model (discrete time)

# Remove all objects - always start with this
rm(list=ls())

# load the package to numerically solve the continuous time
# model
require(deSolve)

#####
# DEFINE ALL THE MODELS:
#####

####
# Continuous time logistic growth
####
# The function returns dN/dt
LogisticGrowth <- function(t,N,p){
  dN = r*N*(1-N/K)
  # Note that the function called by ode() is required to
  # return a list.
  return(list(dN))
}

######
# May's logistic growth function
#####
MayLogisticGrowth = function(Nstart,lambda,K,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = lambda*N*(1-N/K)
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

######
# Alternative logistic growth function
######
AltLogisticGrowth = function(Nstart,lambda,K,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = N+lambda*N*(1-N/K)
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

########
# Ricker
########
Ricker = function(Nstart,lambda,K,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = N*exp(r*(1-N/K))
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

#######
# Beverton-Holt
#######
BevertonHolt = function(Nstart,lambda,K,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = lambda*N/(1+N*(lambda - 1)/K)
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

####### 
# CALL THE FUNCTIONS AND MAKE GRAPHS FOR SPECIFIC PARAMETER
# VALUES
########

## Continuous time logistic growth
# Parameters
r = 1
K = 100
# Initial population size
N0 = 10
# Times
times = seq(0,50,.1)

# Performing the numerical integration
out = ode(N0,times,LogisticGrowth,p=c(r=r,K=K), method = "ode45")

# Make the graph
plot(out[,1], out[,2], typ="l", xlab = "time, t", ylab = "population size, N(t)")


