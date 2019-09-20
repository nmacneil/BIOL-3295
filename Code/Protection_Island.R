# GEOMETERIC GROWTH PROGRAMMED AS A FUNCTION

# Remove all objects - always start with this
rm(list=ls())

# If we want to use the geometric growth routine multiple times (possibly for
# different parameter values) a compact way of doing this is to program it as a
# function.

# COMPARE THE CODE WITHIN THE {} WITH THE CODE IN THE FILE GeoGrowth1.R.
# DO YOU KNOW WHAT EACH OF THE ARGUMENTS IN THE IN FUNCTION ARE? (b,d,etc)
GeoGrowth = function(Nstart,b,d,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = N + b*N - d*N
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

# TYPE: Popn1 = GeoGrowth(490,0.5,0.95,2015,2080) INTO YOUR CONSOLE. YOU WILL
# HAVE TO HAVE RUN THE CODE FOR THE GeoGrowth FUNCTION FOR THIS TO WORK. NEXT
# TYPE Popn1 INTO THE CONSOLE. TYPE Popn1$time AND THEN Popn1$Popn.Size.

# We would like to make a graph corresponding to the geometric growth of the
# Protection Island pheasant population.
# Let NStart = 10
# tstart = 1936
# b = 2.65
# d = 0.15
# tend = whatever you would like it to be.

# REPLACE THE NUMBERS IN THE GeoGrowth FUNCTION BELOW WITH THE NUMBERS
# CORRESPONDING TO PROTECTION ISLAND

ProtectionIsland = GeoGrowth(490,0.5,0.95,2015,2080)

# TYPE ProtectionIsland into your console
# TYPE ProtectionIsland$time into your console
# TYPE ProtectionIsland$Popn.Size into your console

# MAKE A GRAPH OF YOUR RESULTS - ADD DESCRIPTIVE LABELS TO YOUR AXES:
# (YOU WILL NEED TO REPLACE 'name of variable on x axis' ETC)
#plot('name of variable on x axis', 'name of variable on y axis',  typ = 'l', lwd=2, ylab = 'y axis label', xlab = "x axix label")

# Next we would like to add the data to the figure. 
Data = data.frame(time = c(-5/12,0, 1-5/12, 1, 2-5/12, 2, 3-5/12), Popn.Size = c(10, 35*(1+17/110),35,110,110*(1-17/110), 400, 400*(1-17/110)))
plot(Data$time, Data$Popn.Size)
lines(c(0,1,2), c(20.4, 71, 247))
#TYPE Data
#TYPE Data$time
#TYPE Data$Popn.Size

#ADD THE DATA POINTS TO YOUR FIGURE USING
#points('name of variable on x axis', 'name of variable on y axis', pch=16)

# Suppose that after the population reaches near 1500, that the island farmers would like to
# shoot the pheasants to get population down to around 1000 in 10 years time (from when the
# population was 1500).

# USING THE FUNCTION GeoGrowth, WHEN IS THE POPULATION SIZE AROUND 1500?

# We will define a new function GeoGrowth2, where h corresponds to the
# fraction of the population that is killed through hunting.

GeoGrowth2 = function(Nstart,b,d,tstart,tend,h){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = N + b*N - d*N
    N = (1-h)*N
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

# tstart FOR GeoGrowth2 SHOULD BE THE YEAR WHEN THE POPULATION SIZE IS AROUND 1500 (WHICH
# YOU HAD CALCULATED ABOVE). IN GeoGrowth2 KEEP b=2.65 AND d=0.15. LET tend BE tstart+10.
# Nstart SHOULD BE THE VALUE OF THE POPULATION SIZE AT tstart.
# EXPERIMENT WITH DIFFERENT VALUES OF h TO FIND ONE SO THAT IN 10 YEARS THE POPULATION SIZE
# IS DOWN TO 1000. USE NO MORE THAT 3 DECIMAL PLACES FOR h.

Hunting = GeoGrowth2(10,2.65,0.15,1936,1940,0.5)

# ADD IN THE HUNTING SCENARIO TO YOUR PLOT AND ADD A LEGEND
#lines('x axis variable', 'y axis variable' , lty = 2, lwd=2)
#legend("topleft", legend=c("Thing 1", "Thing 2"), lwd=2, lty = c(1,2), box.lwd = 0)

# ONCE YOU HAVE A GRAPH YOU ARE HAPPY WITH PLEASE PRINT IT TO HAND IN WITH
# YOUR LAB REPORT.
