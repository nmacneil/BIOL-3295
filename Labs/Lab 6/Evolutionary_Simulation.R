# Remove all objects - usually start with this
rm(list=ls())

# Strategy characteristics:
# Load in strategies.
Strategy = NULL
source('/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 6/AmyStrategy1.R')
Strategy = rbind(Strategy,c(Name=1, b=b, d=d))
source('/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 6/AmyStrategy2.R')
Strategy = rbind(Strategy, c(Name=2, b=b, d=d))
Strategy = as.data.frame(Strategy)
Total.Strategies = length(Strategy[,1])
total.time = 200
Number = matrix(0,total.time,Total.Strategies)
Number[1,1:2] = 1000/Total.Strategies
for(t in seq(1,total.time-1)){
# Choose an individual to die
dscore = Strategy$d*Number[t,]
cum.dscore = cumsum(dscore)
r1 = runif(1, 0, cum.dscore[Total.Strategies])
strategy.d = min(which(cum.dscore>r1))
Number[t+1,] = Number[t,]
Number[t+1,strategy.d] = Number[t,strategy.d]-1

# Choose an individual to give birth
bscore = Strategy$b*Number[t+1,]
cum.bscore = cumsum(bscore)
r1 = runif(1, 0, cum.bscore[Total.Strategies])
strategy.b = min(which(cum.bscore>r1))
Number[t+1,strategy.b] = Number[t+1,strategy.b]+1
}

plot(seq(1,total.time), Number[,1], typ="l", col = "red", ylim = c(min(Number),max(Number)), xlab = "time", ylab = "Popn Size", main = "Moran model - DB process")
lines(seq(1,total.time), Number[,2], typ="l", col = "blue")