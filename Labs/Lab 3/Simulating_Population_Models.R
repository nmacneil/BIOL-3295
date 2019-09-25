# SIMULATING POPULATION MODELS
## --------------------------

# This lab will cover using computers to solve a discrete-time population
# growth model.

# The geometric population growth model is:

# eqn (1): N_{t+1}= lambda*N_t 

# To find N_{t+1} where t is far into the future, we might have to
# solve this model multiple times, which is time consuming on paper,
# but can be completed very quickly on a computer.

# Other models of population growth, are continuous time:

# eqn (2): dN(t)/dt = r*N(t)

# These can also be solved on a computer, using "numerical methods" that
# will calculate the population size, N(t), at particular times.

# This lab involves practicing using computers to solve these population
# models (i.e. calculate the future population size). For geometric and exponential
# growth, it is quite easy to calculate N(t) or N_t for any value of t, given
# initial population size information, so these computational methods are
# over-kill for these simple models, however, the skills we develop will be useful
# for more complicated models of population growth that we will see later in class.

# In each section will come across questions that you will need to answer and hand-in
# as your lab write up:
#Section 1: Q1.1-1.6
#Section 2: None
#Section 3: Q3.1-3.5
#Section 4: Q4.1-4.2

# You will need to install this package:
# It is used in Section 3.
require(deSolve)

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
# N1, N2, ... , N5, as separate variables, we can store them as elements in a list or 
# dataframe, and rather than repeatedly writing the formula for every different time value,
# we can just use a for-loop and write the general equation.

# Let's try the above again, this time using a loop and a general formula:
for(t in seq(1,5,1)){
N = lambda*N
}

# Did you get an error? Oh, we didn't give a starting value of N.
N=1
for(t in seq(1,5,1)){
  N = lambda*N
}
print(N)

# Okay, that was good, but we've only calculated the population size
# at time 5 and we've lost the values for t=1,2,3,4... Let's
# create a list of our population size values so that the old values aren't
# forgotten:

N[1]=1
for(t in seq(1,5,1)){
  N[t+1] = lambda*N[t]
}
print(N)

# This is good.
# Q1.1: Now, what is the length of N?
length(N)
# Q1.2: Which element of the list is the population size at time, t=0?
# Q1.3: Which element of the list is the population size at time, t=5?
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

# Oh no, this has gone horribly wrong!
# Q1.4: What do you learn when you type:
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

#Q1.5 If you are writing code that involves a for-loop that adds
#entries to a list and you get the error message:
#"Error in Result[1] = 0 : object 'Result' not found"
# What could be a possible solution?

#Q1.6 This code in section 1 contains a lot of errors, which is by design
#so that you can learn about common errors that you might encounter if
#you were doing this on your own. However, it would also be useful for you
#to have a version of 'working' code, without all the tangential comments
#and with just the version of the code that works. For this question,
# -you should create a new .R file that is a minimal version of the working code for Section 1.
# -you should still add comments that describe the purpose of each line.
# -you don't want to include unnecessary sections. For example, lines 44-49
#  are later replaced with 'better' code, so don't include these lines in your
#  "minimal" version.


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

# What do each of the arguments of the user-defined function GeoGrowth called on line 188 correspond to?
# (i.e, what do the numbers 1, 1.5, .3, 1, and 5 correspond to?)

# The function GeoGrowth is a little different than the code from Section 1.
# Try this:

GeoGrowth(1,1.5,.3,2001,2005)

# In the user-defined function, GeoGrowth(), what values of t does the loop
# run over?
seq(1,tend-tstart,1)

# Opps, that gives an error because we didn't say tstart = 2001 and tend = 2005,
# rather we had just set the appropriate function arguments to 2001 and 2005.
# Try this instead:
seq(1,2005-2001,1)

# Note that the first element of Popn.Size is NStart, and when
# t = 4, the fifth element of Popn.Size is calculated because the value
# is assigned to the (t+1)th position in the list.

# There are no questions to hand in from Section 2.

## 3. PROTECTION ISLAND
# These questions pertain to the Handouts: Protection Island.pdf and Protection Island
# Solutions.pdf.

# First, we would like to plot the data.
# May is the 5th month
# October is the 10th month.
# We note that May is 5 months before the Oct-November census date.
Data = data.frame(time = c(-5/12, 1-5/12, 1,2), Popn.Size = c(10,35,110,400))
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size")

# Let's change the axes limits to try to guess the population size at t=0.
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size", typ="l", xlim = c(-.5,.5), ylim=c(10,30))
# typ=l changes the plot to a line graph rather than a scatter plot
# as outlined in "Protection Island - SOLUTIONS.pdf" an approximate
# value of N_0 is 20.

# Let's use the estimated parameter (b and d) values from "Protection Island - SOLUTIONS.pdf" to
# predict the population size from t =0 to t = 2 (Oct 1936 to 1938)
ProtectionIsland = GeoGrowth(20,2.63,17/110,0,2)
print(ProtectionIsland)

# Let's plot the geometric growth predictions to compare with the data.
# plot the data:
plot(Data$time, Data$Popn.Size, xlab = "Years since October 1936", ylab = "Popn size")
# Now add in lines for the predicted values assuming geometric growth:
lines(ProtectionIsland$time, ProtectionIsland$Popn.Size)
# The predicted values are the lines, and we see that they're a bit of an under estimate
# at t=1 and t=2.

# Let's try some other versions of the model. Let's suppose May is our census point.
plot(c(0,1,1+5/12, 2+5/12), c(10,35,110,400), ylab = "Popn size", xlab = "years since May 1936", xlim = c(0,3), ylim = c(0,400))

#Q3.1 What is the difference between the x-axis label on this figure and the previous one?
# i.e. how is t=0 defined differentily in either case?

# Let's predict the May population sizes:
ProtectionIsland.May = GeoGrowth(10,2.65,17/110,0,3)
lines(ProtectionIsland.May$time, ProtectionIsland.May$Popn.Size)

# Interesting... the model predicts May to May well, but is thrown-off for the Oct-Nov data.
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

#Q3.2 Why is the formula for the population size in May: N.May[t+1] = N.Oct[t] - d*N.Oct[t]?
#Q3.3 Will the population size in May ever be larger than the population size
# in October of the previous year?

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
unname(c(Seasonal.ProtectionIsland[1,2:3], Seasonal.ProtectionIsland[2,2:3], Seasonal.ProtectionIsland[3,2:3]))
# (basically, we've just made a list with May values followed by October values)


# Wow! That looks like some pretty good predictions (aside from a bit of an underestimate in October 1938)
# Let's plot our non-seasonal version of the geometric growth function as a dotted line. Let's just remind
# ourselves about the format of the predictions:
ProtectionIsland
# .... where here time = 0 meant October 1936. On our current plot October 1936 is time = 10/12, and so we
# need to add 10/12 to all the values of time in ProtectionIsland if we want to add these to our
# current plot where t=0 means January 1936.
time2 = ProtectionIsland$time+10/12
print(ProtectionIsland)

# Now let's add the non-seasonal geometric growth to the plot
lines(time2, ProtectionIsland$Popn.Size, lty=2)
# Note: lty=2 is giving us the dashed line

# Okay, now that helps us see that our non-seasonal geometric growth model was actually not
# so bad! One reason that the predicted values may have been an underestimate is
# because the population size is close to its maximum value in November. Therefore, our
# model predictions were averaging more across a whole year.

# Q3.4 Modify the code below to add a descriptive legend to the figure
legend("topleft", legend=c("Thing 1", "Thing 2", "Thing 3"), lwd=c(2,2,NA), lty = c(1,2,NA), pch = c(NA,NA,1), box.lwd = 0)

# Q3.5 Export and save the figure and copy and paste the figure into a text editing program. Add a figure
# caption that:
# - Begins with a 1 sentence describing the main point of the figure.
# - Continue with a few sentences that provide specific information relevant to understanding the figure.
# - For the model predictions, you need to describe the model (give its name) and give the parameter values
#   and initial population sizes.

#_______
# 4. Solving the continuous time population model

# For the continous time population growth model (eqn 2), we will be using the deSolve
# package that we loaded in initially. By solving the model, we mean that
# we want to find the population size at any time, N(t) since eqn 2
# is currently only phrased as a change in population size, dN/dt.

# Below we define our exponential growth function. This function
# calcuates dN/dt, the change in population size.
# t is time, N is the current population size, and p are parameters
# in this case b and d. Note that if this function is to be used by the
# function ode(), as we would like, there is a very specific order for
# the arguments of the dN/dt function.
# First argument: t, the independent variable, time.
# Second argument: N, the dependent variables(s), N. If there is more than one
#   variable these will need to be supplied as a list.
# Third argument: p, the parameters (quantities that do not change over time). If
#   there are more than one parameter, as in this example, when the function is called
#   the parameters will need to be supplied as a list.
ExpGrowth <- function(t,N,p){
  dN = (b - d)*N
  # Note that the function called by ode() is required to
  # return a list.
  return(list(dN))
}

# To solve the model we need to provide specific values of the parameters:
b = 1
d = 0.95
# .. and the initial population size
N0 = 10
# ... and the times we want to calculate the solutions for:
times = seq(0,50,1)

# We will use the ode() function from the deSolve package to solve the
# exponential growth model.
# Let's see what arguments are required for the ode() function.
?ode
out = ode(N0,times,ExpGrowth,p=c(b=b,d=d), method = "ode45")
# Note that the third argument of ExpGrowth is p, therefore, we need
# to tell R which of the numbers is b and which is d: On the left hand
# side of the ='s we are specifying the name "b" and on the right hand
# side we are supplying the numerical value b = 1.
c(b=b,d=d)
head(out)

# Let's plot our output (the ode help file gives examples of how to do this too)
# The code below specifies that column 1 of "out" is on the x-axis
# and column 2 is on the y-axis to give us a graph of population size versus time.
plot(out[,1], out[,2], xlab = "time", ylab = "population size", typ="l", col="red", lwd=2)
# From class we saw that it was possible to solve the exponential growth model
# using calculus. Let's add points that shows the solution using calculus:
points(times, N0*exp((b-d)*times))
# We plotted the calculus solution as points, because plotted as lines, the two lines
# fall exactly on top of each other.

# We note that geometric growth (discrete time) gives exact predictions to
# exponential growth when exp(r) = lambda.
# Now let's test this.
lambda = exp(b-d)

# Let's slightly modify the old geometric growth function so that
# lambda can be supplied as an argument without requiring b and d.
GeoGrowth.lambda = function(Nstart,lambda,tstart,tend){
  N=NULL
  Time=NULL
  N[1]=Nstart
  Time[1]=tstart
  for(t in seq(1,tend-tstart,1)){
    N[t+1] = lambda*N[t]
    Time[t+1] = tstart+t
  }
  Popn.Size = data.frame(time = Time, Popn.Size = N)
  return(Popn.Size)
}

DT = GeoGrowth.lambda(N0,lambda,0,50)
head(DT)

# To add points to an existing plot use:
points(DT$time, DT$Popn.Size, pch=4)

# Q4.1 Modify the code below to add a descriptive legend to the figure
legend("topleft", legend=c("Thing 1", "Thing 2", "Thing 3"), lwd=c(2,2,NA), lty = c(1,2,NA), pch = c(NA,NA,1), box.lwd = 0)

# Q4.2 Export and save the figure and copy and paste the figure into a text editing program. Add a figure
# caption that:
# - Begins with a 1 sentence describing the main point of the figure.
# - Continue with a few sentences that provide specific information relevant to understanding the figure.
# - For the model predictions, you need to describe the model (give its name) and give the parameter values
#   and initial population sizes.
