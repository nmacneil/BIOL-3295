# EVOLUTIONARY SIMULATION: Evolution of body size in Drosphila
# Based on the section beginning on p107 of Fox et al.

# Remove all objects - usually start with this
rm(list=ls())

# Load a package that provides a color palette
require(viridis)

# The number of time steps to run the simulation
total.time = 500
# Larval mortality rate
l0 = 1
# Adult mortality rate
l1 = 1

# Record the directory of this file. It is hard to automate
# this step: you will need to write it in.
wd = "/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 6/"

# Set the working directory to the directory of the current file
setwd(wd)

# Create a list of all the strategies in the "Body_Size_Strategies" folder
strategies.list = list.files('Body_Size_Strategies')

# The number of strategies
Number.Strategies = length(strategies.list)

# set the working directory to the subfolder: "Body_Size_Strategies", note
# that all the strategies you want to load in should be in a subfolder
# located in the same directory as this file.
setwd("Body_Size_Strategies")  

source(strategies.list[1])
Strategy = data.frame(Strategy=1,L=L)

# Each strategy is named by its position in strategies.list.
for(i in seq(2,Number.Strategies)){
  source(strategies.list[i])
  Strategy = rbind(Strategy,c(Strategy = i,L=L))
}

# Change back the working directory
setwd(wd)



# The development function
Development = function(L){
  d0 =1
  d1 = 1
  d2 = 1
  D = d0*L^d1 + d2
  return(D)
}

Develop.time = Development(Strategy$L)
Strategy = cbind(Strategy,Develop.time)

# Define the characteristics of all indviduals in the population
strategy = NULL
develop.time = NULL
L = NULL
age = NULL
for(i in seq(1,Number.Strategies)){
  strategy = c(strategy,rep(i,1000/Number.Strategies))
  develop.time=c(develop.time,rep(Strategy$Develop.time[i],1000/Number.Strategies))
  L = c(L,rep(Strategy$Develop.time[i],1000/Number.Strategies))
  age=c(age,rep(0,1000/Number.Strategies))
}

Popn = data.frame(cbind(strategy=strategy,age=age,L=L,develop.time))

# The fecundity function
Fecundity = function(a,L){
  m1 = 1
  m2 = 1
  m3 = 1
  m4 = 1
  m5 = 1
  m0 = m4*L^m5
  m = m0*(1-exp(-m1*(a-m2))*exp(-m3*a))
  return(m)
}

dscore = age
bscore = age
Number = matrix(0,total.time+1,1+Number.Strategies)
Number[1,]=c(0,t(unname(table(Popn$strategy))))

# Determine the temporal dynamics
for(t in seq(1,total.time)){
# Each strategy has a corresponding d-score, based on the death
# rate for that strategy and the number of individuals of that strategy
dscore[Popn$age>=Popn$develop.time] = l1
dscore[Popn$age<Popn$develop.time] = l0
# cum.dscore: divide up a number line into intervals proportional to the
# d-score for each strategy
cum.dscore = cumsum(dscore)
# r1: choose a random number to decide an individual of which strategy
# will die
r1 = runif(1, 0, length(Popn[,1]))
# strategy.d: the strategy of the individual that will die
ind.d = min(which(cum.dscore>r1))
Popn = Popn[-ind.d,]
# bscore
bscore =rep(0,length(Popn[,1]))
bscore[Popn$age>=Popn$develop.time]=Fecundity(Popn$age[Popn$age>=Popn$develop.time],Popn$L[Popn$age>=Popn$develop.time])
Popn$age = Popn$age+1
# cum.bscore: break-up a number line so that intervals are proportional to
# the bscore
cum.bscore = cumsum(bscore)
if(cum.bscore[length(Popn[,1])]>0){
# Select that strategy that reproduces
r1 = runif(1, 0, cum.bscore[length(Popn[,1])])
ind.b = min(which(cum.bscore>r1))
Popn = rbind(Popn,c(strategy = Popn$strategy[ind.b], age=0,L=Popn$L[ind.b]),develop.time=Popn$develop.time[ind.b])
}
index(Popn, seq(1,length(Popn[,1])))
Number[t+1,] = c(unname(t),t(unname(table(Popn$strategy))))
}

#Plot the results
#Fix the color palette so that we can make a legend that will be
# correct for a variable number of strategies
Colours = viridis(Number.Strategies)
plot(seq(1,total.time), Number[,1], typ="l", col = Colours[1], ylim = c(min(Number),max(Number)), xlab = "time", ylab = "population size", main = "Evolution of maturation rate")
for(i in seq(2,Number.Strategies)){
lines(seq(1,total.time), Number[,i], typ="l", col = Colours[i])
}
legend("topleft", legend = strategies.list, col=Colours, lty = rep(1,Number.Strategies),box.lwd = 0)
