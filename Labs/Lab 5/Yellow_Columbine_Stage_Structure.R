library(popbio)
## Demography census data from Aquilegia chrysantha in Fillmore Canyon,
## Organ Mountains, New Mexico, 1996-2003
## Data owned by Brook Milligan, Chris Stebben and Alan Strand.
## Code is taken from the documentation for the popbio package

## Load the Aquilegia data
data(aq.trans)

## To understand the meaning of the rows in the dataframes, click the Packages tab,
## click popbio and then scroll down to aq.trans and click on that.

# For the analysis use only the data from 1998 and plot 909.
sf=subset(aq.trans, year==1998 & plot==909, c(year, plant, stage, fruits, fate))

# Consult the data in 1999 to determine how many recruits (seedlings) were
# produced by the 1998 population
seedlings=nrow(subset(aq.trans, plot==909 & year==1999 & stage=="recruit"))

# Information on seed bank size and survivorship was unavailable. Therefore,
# we make the assumptions as described below
seed.survival = 0.4
seed.bank.size = 1000
seeds.per.fruit = 50

# The expected number of seeds that germinate into a recruit (seedling)
seeds.from.plants = sum(sf$fruits)*seeds.per.fruit
recruitment.rate = seedlings/(seed.bank.size + seeds.from.plants)

# The expected number of recruits produced by each plant
sf$recruit = sf$fruits*seeds.per.fruit*recruitment.rate

# The expected number of surviving seeds produced by each plant
sf$seed = sf$fruits * seeds.per.fruit * seed.survival

# The projection matrix
A = projection.matrix(sf, add=c(1,1,seed.survival, 2,1,recruitment.rate))

# An initial condition of forecast the future population size from
n = c(10000,100,100,100,100)

# The predicted future population size
nt=pop.projection(A,n,20)

# A plot of the future number of seeds
plot(nt$stage.vectors[1,], type = "l", xlab = 'time', ylab = "number", main = "seeds")

# A plot of the future number of each plant stage
# A plot of the number of recruits - the solid orange line
plot(nt$stage.vectors[2,], type = "l", col = "orange", ylim = c(0,200), xlab = "time", ylab = "number")
# A plot of the number of small plants - the dashed red line
lines(nt$stage.vectors[3,], lty = 2, col = "red")
# A plot of the number of large plants - the blue dotted line
lines(nt$stage.vectors[4,], lty = 3, type = "l", col = "blue")
# A plot of the number of flowering plants - the green dot-dashed line
lines(nt$stage.vectors[5,], lty=4, type = "l", col = "green")
