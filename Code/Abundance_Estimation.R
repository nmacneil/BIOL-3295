# VISUALIZING UNCERTAINTY IN ABUNDANCE ESTIMATES: Alternatives to parameteric 95%
# confindence intervals
# clear already stored varaibles 
rm(list=ls())
# load this library which enables violin plots
library(vioplot)
# load this library which is a dependency for "vioplot"
library(sm)
# load this library for beeswarm plots
library(beeswarm)
# load this library to perform a non-parameteric bootstrap.
library(boot)

# to install vioplot, check "install dependencies". When asked do you
# want to use sources that require compilation answer "n".

# if "sm" does not load you may need to install X11 at https://www.xquartz.org/

# Create data for sampling unit counts for summer and winter
# we create counts for 100 sampling units and assume the counts are
# Poisson distributed with a mean as given by the second argument of rpois().
summer.perunit <-rpois(100,35)
winter.perunit <-rpois(100,33)
# The data are combined into a dataframe.
data <-data.frame(summer.perunit, winter.perunit)

# graph variability in counts as a histogram
par(mfrow=c(1,2))
hist(data$summer.perunit, main = "summer", xlab = "count in one sampling unit")
hist(data$winter.perunit, main = "winter", xlab = "count in one sampling unit")

# To calculate the bootstrapped estimates of the population size
# we need a function that calculates abundance for each resample
# under the bootstrapping procedure:
abundance.est <- function(K,data,indices) {
  # this line is neccessary as per the "boot" package
  d <- data[indices]
  # abundance = K times the mean of the sample counts
  abundance <- K*mean(d)
  return(abundance)
}

# bootstrapping the winter data with R=100 replications, for a landscape with K = 1000 sampling units
# The bootstrapping procedure resamples the data with replacement to create a new dataset. The abundance
# is calculated, and the procedure is repeated 100 times.
# See https://en.wikipedia.org/wiki/Bootstrapping_(statistics)#Approach
results.winter <- boot(data=winter.perunit, statistic=abundance.est,
                       R=100, K=1000)
# bootstrapping with summer data
results.summer <- boot(data=summer.perunit, statistic=abundance.est,
                       R=100, K=1000)

# print out the results of the bootstrapping.
results.winter$t

# Below is a reformatting of the data so that we can call the
# boxplot and beeswarm functions
season = c(rep('summer',100), rep('winter',100))
popn.est=c(results.summer$t,results.winter$t)
data2 = data.frame(season = season, popn.est = popn.est)


par(mfrow= c(1,3))
boxplot(popn.est~season, ylab = "population estimate", col = "dodgerblue", main = "box plot", xlab = "season")
vioplot(results.summer$t, results.winter$t, names=c("summer", "winter"), col="gold", main = "violin plot", xlab = "season")
beeswarm(popn.est~season, pch=16, main = "beeswarm plot", ylab = "")

# print out the 95% confidence intervals estimated by the non-parameteric bootstrapping
# routine
boot.ci(results.summer, type="bca")
boot.ci(results.winter, type="bca")

# Sometimes an error is generated if the data contains outliners.

# THINGS TO TRY:
# 1) have the means for the summer and winter data be equal to each other.
# 2) Consider fewer samples, i.e. k < 100 or a smaller landscape K < 1000, but bigger than k.

