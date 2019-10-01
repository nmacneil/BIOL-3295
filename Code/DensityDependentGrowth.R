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
Ricker = function(Nstart,r,K,tstart,tend){
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

####### Continuous time logistic growth
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
par(mfrow=c(2,1), mar=c(4,4,2,2))
# Panel 1
plot(out[,1], out[,2], typ="l", xlab = "time, t", ylab = "population size, N(t)", main = "CT logistic growth with r=1")

# Panel 2
r = -1
N0 = 80
# Performing the numerical integration
out = ode(N0,times,LogisticGrowth,p=c(r=r,K=K), method = "ode45")
plot(out[,1], out[,2], typ="l", xlab = "time, t", ylab = "population size, N(t)", main = "CT logistic growth with r=-1")

par(mfrow = c(2,2), mar = c(4,4,2,2))
# Discrete time logistic growth (May)
# Consider 4 different parameter values
May = MayLogisticGrowth(.1, 2.7, 0.6, 0, 30)
plot(May$time, May$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "May logistic, lambda = 2.7")
# Panel 2
May = MayLogisticGrowth(.1, 3.4, 0.6, 0, 30)
plot(May$time, May$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "May logistic, lambda = 3.4")
# Panel 3
May = MayLogisticGrowth(.1, 3.8, 0.6, 0, 30)
plot(May$time, May$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "May logistic, lambda = 3.8")
# Panel 4, note that May's logistic growth is a poor model because it predicts negative population sizes
May = MayLogisticGrowth(.1, 8, 0.6, 0, 30)
plot(May$time, May$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "May logistic, lambda = 4.1")

######## Ricker population growth
# Consider 4 different parameter values
Ricker.output = Ricker(1, 1.2, 20, 0, 30)
plot(Ricker.output$time, Ricker.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "Ricker, r = 1.2")

Ricker.output = Ricker(1, 2.2, 20, 0, 30)
plot(Ricker.output$time, Ricker.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "Ricker, r = 2.2")

Ricker.output = Ricker(1, 4.5, 20, 0, 30)
plot(Ricker.output$time, Ricker.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "Ricker, r = 4.5")

Ricker.output = Ricker(1, 0.9, 20, 0, 30)
plot(Ricker.output$time, Ricker.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "Ricker, r = 0.9")

######## Beverton-Holt population growth
# Consider 4 different parameter values
BH.output = BevertonHolt(1, 1.3, 20, 0, 30)
plot(BH.output$time, BH.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "BH, lambda = 1.3, K=20")

BH.output = BevertonHolt(1, 1.5, 20, 0, 30)
plot(BH.output$time, BH.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "BH, lambda = 1.5, K=20")

BH.output = BevertonHolt(1, 1.3, 10, 0, 30)
plot(BH.output$time, BH.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "BH, lambda = 1.3, K=10", ylim = c(0,20))

BH.output = BevertonHolt(1, 0.9, 20, 0, 30)
plot(BH.output$time, BH.output$Popn.Size, typ="l", xlab = "time, t", ylab = "Population Size, N_t", main = "BH, lambda = 0.9, K=20", ylim=c(0,20))

