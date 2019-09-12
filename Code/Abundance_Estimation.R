rm(list=ls())
library(vioplot)
library(sm)
library(beeswarm)
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


