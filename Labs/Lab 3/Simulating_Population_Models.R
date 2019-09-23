# SIMULATING POPULATION MODELS
## --------------------------

# This lab will cover using computers to solve a discrete-time population
# growth model.

# The geometeric population growth model is

#   eqn (1): N_{t+1}= lambda*N_t 

# To find N_{t+1} where t is far into the future, we might have to
# solve this model multiple times, which is time consuming on paper,
# but can be completely very quickly on a computer.

# Other models of population growth, are continuous time:

# eqn (2): dN(t)/dt = r*N(t)

# These can also be solved on a computer, using "numerical methods" that
# will calculate the population size, N(t), at particular times.

# This lab involves practicing using computers to solve these population
# models (i.e. calculate the future population size). For geometric and exponential
# growth, it is quite easy to calculated N(t) or N_t for any value of t, given
# initial population size information, so these computational methods are
# over-kill for these simple models, however, the skills we develop will be useful
# for more complicated models of population growth that we will see later in class.

#------------
## 1. CALCULATING FUTURE POPULATION SIZE USING LISTS AND FOR-LOOPS
# Remove all objects - start with this in case your Console
# has old values saved to it that you may have forgotten about.
rm(list=ls())

# Let's suppose lambda = 1.2, and N0 = 1 for a geometric growth model (eqn 1). We might try
# to find the future population size many years into the future like this:
N0=1
lambda=1.2

N1 = lambda*N0
N2 = lambda*N1
N3 = lambda*N2
N4 = lambda*N3
N5 = lambda*N4
print(N5)

# This is a correct approach, it's just more typing than needed. Rather than define
# N1, N2, ... , N5, as separate variables, we can store them as elements in a data
# frame, and rather than repeatedly writing the formula for every different time values
# we can just use a for-loop and write the code to loop across different time values

# Let's try the above again, this time using a loop:
for(t in seq(1,5,1)){
N = lambda*N
}

# Did you get an error? Oh, we didn't give a starting value of N.
N<-1
for(t in seq(1,5,1)){
  N = lambda*N
}
print(N)

# Okay, that was good, but we've only calculated the population size
# at time 5 and we've lost the values for t=1,2,3,4... Let's
# create a list of our population size values

N[1]=1
for(t in seq(1,5,1)){
  N[t+1] = lambda*N[t]
}
print(N)

# This is good. Now, what is the length of N?
length(N)
# Which element of the list is the population size at time, t=0?
# Which element of the list is the population size at time, t=5?
# It would be helpful to have a list of times that correspond to
# the population sizes we have calculated.

N[1]=1
time[1]=0
for(t in seq(1,5,1)){
  N[t+1] = lambda*N[t]
  time[t+1] = time[t]
}
print(N)
print(time)

# Oh no, this has gone horribly wrong! What do you learn when you type:
?time

# Let's try again with a better choice of variable name:
N[1]=1
Time[1]=0
for(t in seq(1,5,1)){
  N[t+1] = lambda*N[t]
  Time[t+1] = t
}
print(N)
print(Time)

# Still problems. Let's try just this one line:
Time[1]=0

# This problem is related to something called "preallocation".
# In my mind, "Time" is going to be a list of times, but the computer
# doesn't know this. The computer wonders is "Time" just a single
# number (a scalar)? Is Time a function? The computer would like more
# information. Therefore, I will try this:
Time=NULL
Time[1]=0

# When I tell the computer "Time = NULL", I am specifying that Time is not
# defined properly yet, but to just wait and we'll flush things out better
# later.
?NULL

# Preallocation
N=NULL
Time=NULL
# Initial values
N[1]=1
Time[1]=0
for(t in seq(1,5,1)){
  N[t+1] = lambda*N[t]
  Time[t+1] = t
}
print(N)
print(Time)

# Now let's stitch time and population size together into a dataframe:
PopnSize = data.frame(time = Time, Popn.Size = N)
print(PopnSize)

## 2. MAKING A FUNCTION FOR GEOMETRIC GROWTH
# In section 1. we made some nice code for calculating future population sizes,
# however, now we want to make it a bit more general. If we wanted to instead set
# lambda = 1.5 we could cut and paste our previous code, but if we want to do the
# same set of calculations again, but for different parameters, a better idea is to
# make a function.

GeoGrowth = function(Nstart,b,d,tstart,tend){
  N=NULL
  Time=NULL
  N[1]=Nstart
  Time[1]=tstart
  for(t in seq(1,tend-tstart,1)){
    lambda = 1+b-d
    N[t+1] = lambda*N[t]
    Time[t+1] = tstart+t
  }
  Popn.Size = data.frame(time = Time, Popn.Size = N)
  return(Popn.Size)
}

GeoGrowth(1,1.5,.3,1,5)

# What do each of the arguments of the user defined function GeoGrowth correspond to?
# (i.e, what do the numbers 1, 1.5, .3, etc correspond to?)

# The function GeoGrowth is a little different than the code from Section 1.
# Try this:

GeoGrowth(1,1.5,.3,2001,2005)

# In the user defined function, GeoGrowth(), what values of t does the loop
# run over?
seq(1,tend-tstart,1)

# Opps, that gives an error because we didn't say tstart = 2001 and tend = 2005,
# rather we just set the appropriate function arguments to 2001 and 2005.
# Try this instead:
seq(1,2005-2001,1)

# Note that the first element of Popn.Size is NStart and when
# t = 4, the fifth element of Popn.Size is calculated because the value
# is assigned to the (t+1)th position in the list.

## PROTECTION ISLAND
# These questions pertain to the Handouts: Protection Island.pdf and Protection Island
# Solutions.pdf.

# First, we would like to plot the data.
# May is the 5th month
# October is the 10th month.
# We will that May is 5 months before the Oct-November census date.
Data = data.frame(time = c(-5/12, 1-5/12, 1,2), Popn.Size = c(10,35,110,400))
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size")

# Let's change the axes limits to try to guess the population size at t=0.
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size", typ="l", xlim = c(-.5,.5), ylim=c(10,30))
# typ=l changes the plot to a line graph rather than a scatter plot
# as outlined in "Protection Island - SOLUTIONS.pdf" an approximate
# value of N_0 is 20.

# Let's use the estimated values from "Protection Island - SOLUTIONS.pdf" to
# predict the population size from t =0 to t = 2 (Oct 1936 to 1938)
ProtectionIsland = GeoGrowth(20,2.63,17/110,0,2)
print(ProtectionIsland)

# Let's plot the geometric growth predictions to compare with the data.
# plot the data:
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size")
# Now add in lines for the predicted values assuming geometric growth:
lines(ProtectionIsland$time, ProtectionIsland$Popn.Size)
# The predicted values, are the lines and we see that they're a bit of an under estimate.

# Let's try some other versions of the model. Let's suppose May is our census point.
plot(c(0,1,1+5/12, 2+5/12), c(10,35,110,400), ylab = "Popn size", xlab = "years since May 1936", xlim = c(0,3), ylim = c(0,400))

# Let's predict the May population sizes and compare to just the May data:
ProtectionIsland.May = GeoGrowth(10,2.65,17/110,0,3)
lines(ProtectionIsland.May$time, ProtectionIsland.May$Popn.Size)

# Interesting... the model predicts May to May well, but is thrown-off for the Oct-Nov predictions.
# Let's now try to derive a better model for the Pheasant population dynamics by considering
# the timing of the breeding season. We know that pheasants breed during the summer, so no pheasants are
# born over the winter. Let's assume that by May all the adult pheasants that were going to die by
# Novemeber are already dead (no adult deaths over the summer). We'll also assume that all chicks
# have hatched by November. Let's make the function:
                  
SeasonalGeoGrowth = function(May.Start,b,d,year.start,year.end){
  N.May =NULL
  N.Oct = NULL
  Year=NULL
  N.May[1]=May.Start
  Year[1]=year.start
  # Calculate the October values for the first year only.
  N.Oct[1] = N.May[1] + b*N.May[1] # this says there is no adult
  # mortality over the summer, and adds the births to the May popn size
  for(t in seq(1,year.end-year.start,1)){
    # Now let's do the calculations for other years:
    Year[t+1] = year.start+t
    # The May population size is the previous year's October population
    # size minus all the mortality that is assumed to have occured over the winter
    N.May[t+1] = N.Oct[t] - d*N.Oct[t]
    # The October population size is the May population size (for the same year)
    # plus all the births from the chicks.
    N.Oct[t+1] = N.May[t+1] + b*N.May[t+1]
  }
  Popn.Size = data.frame(year = Year, May = N.May, Oct = N.Oct)
  return(Popn.Size)
}

# Let's try out our geometric growth function that specifically attributes births and deaths
# to particular times of the year:
Seasonal.ProtectionIsland = SeasonalGeoGrowth(10,2.65, 17/110,1936,1938)
print(Seasonal.ProtectionIsland)

# Now let's plot the results versus the data again.
# Let's set Jan 1936 to t = 0.
# plot the data:
plot(c(5/12, 1+5/12, 1+10/12, 2+11/12), c(10,35,110,400), xlab = "years since Jan 1936", ylab = "population size")
# for the above 10/12 indicates October.
# now let's plot in the predictions from Seasonal.ProtectionIsland:
lines(c(5/12, 10/12, 1+5/12, 1+10/12, 2+5/12, 2+10/12), c(Seasonal.ProtectionIsland[1,2:3], Seasonal.ProtectionIsland[2,2:3], Seasonal.ProtectionIsland[3,2:3]))
# If you aren't sure what the below code corresponds to, just run that line below:
c(Seasonal.ProtectionIsland[1,2:3], Seasonal.ProtectionIsland[2,2:3], Seasonal.ProtectionIsland[3,2:3])
# (basically, we've just made a list with May values followed by October values)


# Wow! That looks like some pretty good predictions (aside from a bit of an underestimate in October 1938)
# Let's plot our non-seasonal version of the geometric growth function as a dotted line. Let's just remind
# ourselves about the format of the predictions:
ProtectionIsland
# .... where here time = 0 meant October 1936. On our current plot October 1936 is time = 10/12, and so we
# need to add 10/12 to all the values of time in ProtectionIsland if we want to add these to our
# current plot where t=0 means January 1936.
ProtectionIsland$time = ProtectionIsland$time+10/12
print(ProtectionIsland)
# do not run ProtectionIsland$time = ProtectionIsland$time+5/12 another time or you'll add
# another 5/12 which we don't want!
# Now lets add the non-seasonal geometeric growth to the plot
lines(ProtectionIsland$time, ProtectionIsland$Popn.Size, lty=2)
# Note: lty=2 is giving us the dashed line

# Okay, now that helps us see that our non-seasonal geometric growth model was actually not
# so bad! One reason that the predicted values may have been an underestimate is
# because the population size is close to its maximum value in November. Therefore, our
# model predictions were averaging more across a whole year.

# Next, I would like YOU to add a legend to you the figure
legend("topleft", legend=c("Thing 1", "Thing 2", "Thing 3"), lwd=c(2,2,NA), lty = c(1,2,NA), pch = c(NA,NA,1), box.lwd = 0)

