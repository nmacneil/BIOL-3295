rm(list=ls())
library(vioplot)
library(sm)
library(beeswarm)
library(boot)
# install vioplot and check "install dependencies". When asked do you
# want to use sources that require compilation answer "n".

# Create data
summer.perunit <-rpois(100,35)
winter.perunit <-rpois(100,22)
data <-data.frame(summer.perunit, winter.perunit)

par(mfrow=c(1,2))
hist(data$summer.perunit, main = "summer", xlab = "count in one sampling unit")
hist(data$winter.perunit, main = "winter", xlab = "count in one sampling unit")

season = c(rep('summer',100), rep('winter',100))
count.perunit=c(summer.perunit,winter.perunit)
data2 = data.frame(season = season, count.perunit = count.perunit)



par(mfrow= c(1,3))
boxplot(count.perunit~season, ylab = "count per sampling unit", col = "dodgerblue", main = "box plot", xlab = "season")
vioplot(summer.perunit, winter.perunit, names=c("summer", "winter"), col="gold", main = "violin plot", xlab = "season")
beeswarm(count.perunit~season, pch=16, main = "beeswarm plot")


# # function to obtain R-Squared from the data
# rsq <- function(formula, data, indices) {
#   d <- data[indices,] # allows boot to select sample
#   fit <- lm(formula, data=d)
#   return(summary(fit)$r.square)
# }
# # bootstrapping with 1000 replications
# results <- boot(data=mtcars, statistic=rsq,
#                 R=1000, formula=mpg~wt+disp)
# 
# # view results
# results
# plot(results)
# 
# # get 95% confidence interval
# boot.ci(results, type="bca")

# function to obtain R-Squared from the data
abundance.calc <- function(K,data,indices) {
  d <- data[indices]
  abundance <- K*mean(d)
  return(abundance)
}
# bootstrapping with 1000 replications
results <- boot(data=winter.perunit, statistic=abundance.calc,
                R=100, K=75)

# view results
results$t


# get 95% confidence interval
boot.ci(results, type="bca")
