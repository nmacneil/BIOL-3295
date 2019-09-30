# IS FLOUR BEETLE POPULATION GROWTH DENSITY-DEPENDENT?
# ----------------------------------------------------

# Let's start by removing any variables from our workspace
rm(list=ls())
# Install the package bbmle. Use the "Packages" tab and click "install"
# or type install.packages("bbmle") at the Console prompt.
require(bbmle)

# IMPORTING THE FLOUR BEETLE DATA
# ----------------------------------
# You will need to edit the path to Tribolium.csv to tell R where you
# saved this file. Entering the command getwd() in the console may help you to figure out the path to
# Triboium.csv on your computer. If you are having trouble, try a workaround like
# moving Tribolium.csv to a very easy place to find, like your current working directory
# or to the Desktop.
Tribolium = read.csv('/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 4/Tribolium.csv')

# The flour beetles are raised in different habitat types
# We will plot the number of adult flour beetles for the 
# adult habitat type W5.
Tribolium=Tribolium[Tribolium$Adult.habitat=="W5",]

# Type head(Tribolium) in the Console to see what the data look like.

# Eight measurements are recorded during the 133 day experiment, taken at the times
# listed below.
time = c(1,19, 38, 57, 76, 95, 114, 133)

Table1 = NULL
# Here we are reformatting that data such that column 1 are times
# and column 2 are observations of population size for adult flour beetles.
for(i in seq(1,length(Tribolium$day.1),1)){
  # Columns 6 to 13 are the columns of the data that contain
  # data on population size. There are 7 replicates of the experiment and the data is
  # reformatted so that additional replicates are appended in the rows below.
  points(time, Tribolium[i,6:13])
  popn = as.numeric(Tribolium[i,6:13])
  Table1 <- rbind(Table1,data.frame(time = time, measured=popn))
}

# Enter head(Table1) into the Console to see how the data has been reformatted.
# Now we have finished 'cleaning' the data. It is now in a format that we can use for
# model fitting and graphing.

# Plot the data
plot(Table1$time, Table1$measured, pch=19)
# Some better axes labels would be great, though - can you remember how to do this?
# xlab = "", ylab = ""

# FITTING THE BEVERTON-HOLT AND GEOMETERIC GROWTH MODELS
#-------------------------------------------------------
# In the Protection Island exercise we estimated the birth and death rate
# parameters from independent observations. In contrast, here we'll fit these parameters
# from the data.

BevertonHolt = function(lambda,K){
  N=NULL
  # All replicates start with six adult Tribolium on day 1.
  N[1]=6
  # We solve the Beverton-Holt equation up until day 133 by
  # iterating the recursion.
  for(t in seq(1,132,1)){
    N[t+1] = lambda*N[t]/(1+N[t]*(lambda - 1)/K)
  } 
  # And then presenting the results in a nice dataframe.
  Result=data.frame(time=seq(1,133,1), Popn.Size=N)
  return(Result)
}

# This is a function that calculates the likelihood of combinations of
# lambda and K values given the measured Tribolium population sizes.
# This function will be called by mle2 which will then using an optimization
# routine to find the combination of lambda and K values which have the maximum
# likelihood given the data.
LLBH <- function(lambda, K){
  R=NULL
  # The data shows and increase so the best fit lambda value will not be < 1.
  # Similarly, when K <=0, the Beverton-Holt model doesn't make sense. This if
  # statement was added to help out the optimization routine by offering some guidance
  # on the parameter space not to look at.
  if(lambda>1 & K>0){
    # We need to consider every data point: every row in Table 1.
    for(i in seq(1,length(Table1$time))){
      # time.cutoff sets a time beyond which stop the model fitting.
      # if time.cutoff >= 133 then all the data are considered. We will use this
      # to see if Tribolium growth is geometric early on.
      if(Table1$time[i]<=time.cutoff){
        # We call the Beverton-Holt function to calculate the population size for these
        # specific lambda and K values
      BH = BevertonHolt(lambda,K)
      # The data are only recorded each week, so we have no data for day 2, 3, 4, etc.
      # Below we are extracting the prediction of the Beverton-Holt model on the day
      # that the data correspond to.
      BH.Pred=BH$Popn.Size[BH$time==Table1$time[i]]
      # Below we have assumed that the probability of a measured population size falling a given distance
      # from the Beverton-Holt predicted value of the population size follows a Poisson distribution.
      R1 = dpois(Table1$measured[i], BH.Pred)
      # We make all the R1 values for a particular lambda and K combination into a long list.
      R =rbind(R,R1) 
      }}
    # The negative log-likelihood is the negative sum of the log of the probabilities calculated for
    # each observation for this particular lambda and K combination.
  Rout=-sum(log(R))}
  else {Rout = 10e10}
  # If you choose to uncomment the print line below, you can see the different lambda and K values that the optimization
  # routine is trying and the associated negative log-likelihood values. The negative log-likelihoods are a measure
  # of the model fit for a given combination of parameters. The best fit parameters have the largest negative log-likelihood.
  # i.e., since these number are negative a good fit is indicated by values close to zero.
  #print(c(lambda=lambda,K=K, Rout=Rout))
 return(Rout)
}

# This function is similar to LLBH, but the function below, calculates the negative log-likelihood for a given
# lambda value assuming geometeric growth.
LLGeo <- function(lambda){
  R=NULL
  # The lambda values need to be tightly constrained otherwise the prediction can be really off due
  # to the exponential growth
  if(lambda>=.99 & lambda<=1.037){
    # We compare the model prediction to the measured population size for each data point.
  for(i in seq(1,length(Table1$time))){
    # all measurements taken after time.cutoff days are ignored.
  if(Table1$time[i]<=time.cutoff){
    # t is the time recorded in the Tribolium experiment
  t = Table1$time[i]
  # Below is the formula for geometric population growth. The population size of day 1 is 6. This
  # reformulation ensures that when t=1 the predicted population size is 6 and that on each successive
  # day the population size increases by a factor of lambda from its previous day's size (i.e., as assumed by
  # geometric growth)
  Geo.Pred = 6*lambda^(t-1)
  # Assume the residuals follow a Poisson distribution. Residuals are the difference between the measured and
  # model predicted values
  R1 = dpois(Table1$measured[i], Geo.Pred)
  # Make all the calculated probabilities into a list.
  R = rbind(R,R1)}}
    # Calculate the negative log likelihood for all observed data for a particular lambda value.
  Rout=-sum(log(R))}
  else
  {Rout = 10e6}
  return(Rout)
}

# Initially we will use all the data so we set time.cutoff to a number bigger than 133 days.
# Note we have used "<-" instead of "=" to assign the value of time.cutoff. This is because we
# want time.cutoff to having global scope, which means that the value of time.cutoff should
# be known inside of functions: note that the time.cutoff value is needed to calculate the values
# of LLBH and LLGeo.
time.cutoff<-150
# The function mle2 is part of the bbmle package. This function will determine the best fit
# values of the parameters given the data. The optimation algorith will evaluate the LLBH and LLGeo
# functions for a range of lambda (and K) values to find the values that maximize the likelihood
# (i.e. the best fit values)
# The numbers below are guess as to the best fit parameter values to help out the optimization
# algorithm.
# The line of code calculation "LLBH.Result" will take some time to complete its calculations.
LLBH.Result = mle2(LLBH, start = list(lambda = 1.03, K=150),method = "Nelder-Mead")
LLGeo.Result = mle2(LLGeo, start = list(lambda = 1.02))

# Let's see our results:
LLBH.Result
# You should see that the best fit estimate of lambda is 1.0758 and the best fit estimate of K is 116.5.
# Let's see how this fit compares with the data
BestFitBH = BevertonHolt(1.0758, 116.48)
lines(BestFitBH$time, BestFitBH$Popn.Size)
# Note also that the log-likehood for the best fit values was 2146.056.
LL.Bestfit.BH = -2146.056

# Let's also graph the best fit geomteric growth function
LLGeo.Result
# You should see that lambda = 1.027 is the best fit value and 6405.41 is the log likelihood.
# Let's set
LL.Bestfit.Geo = -6405.41
# Let's plot this:
times=seq(1,133)
lines(times, 6*1.027^(times-1))

# Note also that we didn't try any lambda values bigger than 1.037 for the geometric growth
# function. Let's see what this looks like:
lines(times, 6*1.037^(times-1))
# Note that this one shoots out the top of our graph. On day 133 the agreement between the predicted
# and observed values is so terrible, the computer starts to have trouble doing the calculations.
# Therefore, even though lambda = 1.037 doesn't sound that different from lambda = 1.027, on day
# 133 the predicted population sizes are hugely different!

# THE LIKELIHOOD RATIO TEST
#---------------------------
# We can statistically test which model geometric or Beverton-Holt population growth fits
# the data best using the likelihood ratio test.

# The likelihood ratio test statistic is calulated as:
Lik.Test.Stat = -2*(LL.Bestfit.Geo-LL.Bestfit.BH)

# The critical value of the test statistic at the 0.05 alpha significance level
qchisq(0.95,1)

# The p-value for this test statistic is:
pval = pchisq(Lik.Test.Stat,1)
