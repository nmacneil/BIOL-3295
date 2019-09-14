rm(list=ls())
library(vioplot)
library(sm)
library(beeswarm)
library(boot)
# to install vioplot, check "install dependencies". When asked do you
# want to use sources that require compilation answer "n".

# Create data
summer.perunit <-rpois(100,35)
winter.perunit <-rpois(100,33)
data <-data.frame(summer.perunit, winter.perunit)

# graph variability in counts as a histogram
par(mfrow=c(1,2))
hist(data$summer.perunit, main = "summer", xlab = "count in one sampling unit")
hist(data$winter.perunit, main = "winter", xlab = "count in one sampling unit")

# calculate the bootstrapped estimates of the population size
# function to obtain R-Squared from the data
abundance.est <- function(K,data,indices) {
  d <- data[indices]
  abundance <- K*mean(d)
  return(abundance)
}

# bootstrapping with 100 replications
results.winter <- boot(data=winter.perunit, statistic=abundance.est,
                       R=100, K=1000)
# bootstrapping with 100 replications
results.summer <- boot(data=summer.perunit, statistic=abundance.est,
                       R=100, K=1000)

# view results of the 100 replicated population abundance calculations
# for winter
results.winter$t

season = c(rep('summer',100), rep('winter',100))
popn.est=c(results.summer$t,results.winter$t)
data2 = data.frame(season = season, popn.est = popn.est)


par(mfrow= c(1,3))
boxplot(popn.est~season, ylab = "population estimate", col = "dodgerblue", main = "box plot", xlab = "season")
vioplot(results.summer$t, results.winter$t, names=c("summer", "winter"), col="gold", main = "violin plot", xlab = "season")
beeswarm(popn.est~season, pch=16, main = "beeswarm plot")

# get 95% confidence interval
boot.ci(results.summer, type="bca")
boot.ci(results.winter, type="bca")

