# EVOLUTIONARY SIMULATION: Evolution of body size in Drosphila
# Based on the section beginning on p107 of Fox et al.

# Remove all objects - usually start with this
rm(list=ls())

# Load a package that provides a color palette
require(viridis)

# Record the directory of this file. It is hard to automate
# this step: you will need to write it in.
wd = "/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 6/"

# Set the working directory to the directory of the current file
setwd(wd)

# Create a list of all the strategies in the "Body_Size_Strategies" folder
strategies.list = list.files('Body_Size_Strategies')

# The number of strategies
Number.Strategies = length(strategies.list)

# Strategy is a dataframe listing all the strategies. The line below
# is preallocation
Strategy = NULL

# The initial number of individuals with each strategy
# Preallocation
Number = matrix(0,total.time,Number.Strategies)
# Begin with equal abundance of each strategy
Number[1,1:Number.Strategies] = 1000/Number.Strategies

Development = function(L){
  d0 =1
  d1 = 1
  d2 = 1
  D = d0*L^d1 + d2
}

Fecundity = function(b){
  d0 =1
  d1 = 1
  d2 = 1
  D = d0*L^d1 + d2
}

Mortality = 

  # set the working directory to the subfolder: "Body_Size_Strategies", note
  # that all the strategies you want to load in should be in a subfolder
  # located in the same directory as this file.
  setwd("Body_Size_Strategies")  
  
# Each strategy is named by its position in strategies.list.
for(i in seq(1,Number.Strategies)){
source(strategies.list[i])
  D = Development(L)
Strategy = rbind(Strategy,c(Name=i, L=L, D=D))
}

# Change back the working directory
setwd(wd)

# Strategy is converted into a dataframe.
Strategy = as.data.frame(Strategy)

# The number of time steps to run the simulation
total.time = 500



# Determine the temporal dynamics
for(t in seq(1,total.time-1)){
# Each strategy has a corresponding d-score, based on the death
# rate for that strategy and the number of individuals of that strategy
dscore = Strategy$d*Number[t,]
# cum.dscore: divide up a number line into intervals proportional to the
# d-score for each strategy
cum.dscore = cumsum(dscore)
# r1: choose a random number to decide an individual of which strategy
# will die
r1 = runif(1, 0, cum.dscore[Number.Strategies])
# strategy.d: the strategy of the individual that will die
strategy.d = min(which(cum.dscore>r1))
# At t+1 the number of individuals for each strategy is unchanged...
Number[t+1,] = Number[t,]
# ... except, for "strategy.d" the index of the strategy that experiences
# the death, for this strategy the number of individuals is reduced by 1.
Number[t+1,strategy.d] = Number[t,strategy.d]-1
# bscore: A birth score associated with each strategy based on the value
# of "b" and the number of indiviuals of that strategy
bscore = Strategy$b*Number[t+1,]
# cum.bscore: break-up a number line so that intervals are proportional to
# the bscore
cum.bscore = cumsum(bscore)
# Select that strategy that reproduces
r1 = runif(1, 0, cum.bscore[Number.Strategies])
strategy.b = min(which(cum.bscore>r1))
# Increase the number of individuals of the strategy that gave birth by 1.
Number[t+1,strategy.b] = Number[t+1,strategy.b]+1
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
