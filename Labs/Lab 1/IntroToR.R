#SL:10/01/2014 An introduction to R
#Created by Shawn J. Leroux & Amy Hurford  sleroux@mun.ca  ahurford@mun.ca
#Revised by AH on 09/10/2019
#Good resources for R basics: http://www.statmethods.net/   www.r4all.org   http://brianmcgill.org (notes tab)

#In class you will be running scripts that are working or mostly working. However,
#to understand the code, and as a step toward eventually learning to program yourself
#it is helpful practice running some commands yourself.

#To start make sure you know:
# -the difference between R and R studio.
# -the purpose of the 'Editor' and of the 'Console'
# -the difference between 'Source' and 'Run'
# -how to execute a Source command either with the button or via a console
# -the lines beginning with # are comments. The other black/blue lines are actual
# commands.
# -'Source' will run the script with each of the lines run sequentinally.

# 1. R remembers only the last value of an object, and it remembers that
# until you clear it.
#####
# Run these two lines only:
r<-2
r<-5000
# type 'r' into the console, to learn what is the value of r. What is r?
# Now run this line:
r<-4
# ... What is the value of 'r' now? despite 'r' having had 3 different values,
# the current value of 'r' is the last value of 'r' and old values will not be
# remembered unless you use a different procedure.

# Because R remembers, it's a good idea to clean out old values before
# starting:
rm(list=ls())
# now what is the value of 'r'?

#At its simplest R is a calculator
# (run the lines below and subsequently step through this code by reading
# the comments and running the blue/black code lines)
2+2
2*4
1/4

# 2. Loading in data
########
#If you have data, you can import it into R and run calculations on your data.
#We will import a sample of the data used in Leroux & Kerr 2013 Land Development
# in and around protected areas at the wilderness frontier. Conservation Biology
# 27:166-176.

MyData <-read.csv("PA_Data.csv") 

#Check to see if your data are correct by visualizing it in R
#Note the type of data you have; numeric, integer, factor, etc.
str(MyData)

# read.csv() is a built in R function. This means that it is a function and we
# didn't make it up it was already there. If you would like to understand how
# the read.csv function works try:
?read.csv
??read.csv
# or you could even use a search engine and type in 'read.csv AND R'.

#In the above expression str() is another built-in function
# and it is applied to the object in parenthesis - the data, to reveal its
# STRucture.

#This outputs the first 6 lines of your data. head is another built in function.
head(MyData)

# What if we try this?
headache(MyData)
# ... we get an error because unlike str() and head(), there is no built-in
# function called headache. The error message gives us a clue about what the
# problem is. Whenever you are stuck and your code isn't working a good idea
# is to copy your error message into a search engine - it's common to find that
# others may have already had and solved the same problem you are having.

#... on the other hand tail() is a built-in function that shows the last 6 lines
# of your data
tail(MyData)

# What about this?
tail(MyNewData)
# ... nice try, but you've not defined anything named 'MyNewData'. On what line
# of this script did you define 'MyData'?

# 3. Getting the path to a file
#######
# 'PA_Data.csv' loaded in with just the file name, but if you'd saved 'PA_Data.csv'
# in another folder, this might not have been so easy.
getwd()
# This will tell you the path to 'IntroToR.R'. Using this as a guide you can
# probably figure out the path to fine 'PA_Data.csv'. To test out this method
# try to load in 'PA_Data.csv' using the comelte path like this:
MyNewData <-read.csv("/Users/amyhurford/Desktop/BIOL-3295/Labs/Lab 1/PA_Data.csv") 
# Did it work? What commands might you write to check that MyNewData loading in ok?

# 4. Assigning values to x and y.
#####
#You can generate data or numbers in many ways in R
#Create a sequence of numbers from 1 to 100 by 1
seq(1:100)

#Or have it increment by 0.5 instead
seq(1,100,0.5)

#If you're wondering what are appropriate arguments of seq(), i.e.,
#what is okay to put inside the (), you might try this:
?seq

#You can store this sequence as a vector by creating an object named x that stores the sequence
x <- seq(1:100)

#Type x to see that you created this vector correctly.
x
# if you just do seq(1:100) but don't give it a name (e.g. 'x'), then R won't remember it.

#Take median of x
median(x)
#... median() is another example of a built-in function that comes with the standard
# R installation.

#You can generate random numbers in R. runif() is the function to generate random numbers from a uniform distribution
#How about 100 random numbers drawn from a uniform distribution with minimum = 0 and maximum = 10
y <- runif(100, min = 0, max = 10)
y

# 5. Let's make a plot
########
#Now, we have two objects with length 100. We can create a plot of these data.
#Intuitively, the primary function for plotting is called 'plot'.
#Let's see what arguments 'plot' takes so we know how to make our figure of x vs y

help(plot)

# What if I try to make a plot and the 'x' and 'y' arguments have different lengths?
plot(y,1)
# opps, that's an error:
length(1)
length(y)
# y has length 100 but 1 has length only equal to 1. Let's try it like this:
plot(y,rep(1,100))
# okay, that worked, but what is rep(1,100)? Try below and see
rep(1,100)
# and what is it's length?
length(rep(1,100))
length(y)
# okay, plot(y,rep(1,100)) is a working command because both the x and y arguments
# have length 100.

#Type help(par) to find out everything you can do with this simple plotting function.
# par describes the different arguments you can provide to plot to change
# the appearance of your plot.
plot(x,y)
#Your plot would look better if you added some labels and a plot title, increase data point size and font size
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5)
#You can also control the margins (in inches) of your plot by
#Set the margins for your plot (in inches). 
par(mai=c(0.85,0.9,0.25,0.05))
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5)
#The settings are (bottom, left, top, right). Zero values for all make the plot box flush to the viewing screen.

#You can make side by side plots with the function par(mfrow=c(1,1)) where the numbers are # of rows, # of columns in your figure set
par(mfrow=c(1,2))#this will give you 1 row with 2 plots.

#create another random data set to make another plot
w <- runif(100, min = 0, max = 10)
w

#Make side-by-side plots
par(mfrow=c(1,2))
#First plot
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers 1", cex=1.5, cex.lab=1.5)
#Second plot
plot(x,w,main="My Figure", xlab="Sequence", ylab="Random Numbers 2", cex=1.5, cex.lab=1.5)

#Alternatively, you could just add the new points to your existing plot but if you do this, you will want to use different colours to represent the two different random number sets your are plotting
#Tell R you want to use only one plotting window
par(mfrow=c(1,1))
#Base plot with blue data points
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5, col="blue")
#Second data points added in red with the function points()
points(x,w, cex=1.5, col="red")

#Let's add a legend to the above plot
par(mfrow=c(1,1))
#Base plot with blue data points
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", ylim=c(0,12), cex=1.5, cex.lab=1.5, col="blue")
#Second data points added in red
points(x,w, cex=1.5, col="red")
legend("topleft", legend=c("Random Numbers 1", "Random Numbers 2"), pch=21, pt.bg=c("Blue", "Red"), pt.cex=1.5)

# 6. Working with dataframes
#############
# If you want to make a list use the built-in function c()
c(2,4,6,8)

# If you want to make a list of words (called 'strings') you need to use ''
c('cold','warm','hot')

# What if we do:
c(cold,warm,hot)
#... that doesn't work because R is looking for objects named cold, warm, and hot
# Try this:
cold<-1
warm<-2
hot<-1000
c(cold,warm,hot)
# The '' are needed so that R knows there are words and not the names of objects
# or variables.

# A dataframe is a list of lists with names at the top to help us remember
# what each column means
DataData <- data.frame(Monday=c(2,4,6,8), Tuesday=seq(1:4), Wednesday=c(8,6,4,2))
DataData
#... in the case of the data.frame function we didn't need the '' on the names
# of the columns, because these are already assumed to be strings for that specific
# function.

# What might happen if the columns don't have the same length?
Data2 <- data.frame(Monday=1, Tuesday=2, Wednesday=c(1,2,3))
Data2
# Surprisingly that did work!
Data3 <- data.frame(Monday=c(1,2), Tuesday=2, Wednesday=c(1,2,3))
Data3
# ... not so lucky this time!

#Now, let's add a column named 'Thursday' to the data frame. You can call
# columns from data frames by listing them after a $ as below.
DataData$Thursday <- c(0.1,0.2,0.3,0.4)
DataData

# What is the point of all this? Sometimes you might load in data as a .csv file
# as we did above with PA_Data above, but other times you might make your own
# data by running a simulation (we will see this below) or you might want to add
# a column to the data that you loaded in. For example, lets go back to
head(MyData)
# ... I ask for just the first 6 rows because it is much too much to look at all
# the data at once
MyData
# So let's just do the first 6 rows:
head(MyData)
# Let me add a random number in a new 5th column
# In this case it is helpful to define a name for the number of columns in
# MyData. It gets called 'numcols'.
numcols <-length(MyData[,1])
MyData$RAND_NUM <- sample(seq(1:numcols),numcols)
head(MyData)

# I can access the value of my dataframe at a particular row (r1) and column (c1)
# like this:
r1<-1
c1<-1
MyData[r1,c1]
# Try changing r1 and c1 to access different data. How many columns in MyData?
# Why doesn't this work?
MyData[1,8]

# What if we did round brackets by mistake?
MyData(1,8)
#... the error message indicates that round brackets are for functions, so now R
# is confused because there is no built-in or user defined function called MyData.

# 7. For loops
###########
# Often in class we want recursively evaluate a function. For example, we want to use
# the value generated after one iteration of the loop for the next go around.

# First we need to pre-allocate our data structure: we give it a name so that we can
# add to it later. Right now it's just empty.
Data_Loop <-NULL

# We would like make a sequence where each prior result is doubled
# and with a starting value of 1.

# Outside the loop we define the start value.
loop_1 <-1

# The loop will be run for 7 iterations number 1 to 7. This is stated as
# seq(1:7) and 'j' will be the name of the iteration number. Inside the () brackets
# is the information about for what different values to run the loop for (here, it's 1 to 7)
# After giving the name ('j') and the values of j to loop across (1 to 7 by 1) then
# inside {} are a list of commands. Therefore, you might want to select the entire
# code block starting at for() and ending at } and run it all together.
for(j in seq(1:7)){

#this states the new value of loop_1 is double the old value
loop_1 <- loop_1*2

#Put your data in a data frame. Don't forget to enclose this within the loop!
#Here I am telling R to place the results of our loop - 'loop_1' in a data frame along with the values of j.
#The data frame has two columns; "iteration_number" and "Result". The data to put in these columns come after the =. In column "iteration_number" we are placing j values (this is the parameter we are looping over) and in column "Result" we are placing the values of loop_1 evaluated for different values of j.
#Here we use rbind to add an additional row of data for every iteration of j. If we did not do this, our data frame would only have one row. When populating a data frame within a loop be sure to use rbind (or cbind - look it up!)
Data_Loop <- rbind(Data_Loop, data.frame(iteration_number=j, Result=loop_1))
#Close the loop
}

Data_Loop

#You can easily extract elements of a data frame by using []
#Give me the value of Data_Loop at row = 3 in column = 2
Data_Loop[3,2]

#Or give me the second column of Data_Loop  where Result is the name of the second column of the data frame Data_Loop 
Data_Loop$Result

# Loops are used to do calculations where the next calculated value depends on the previous
# one.

# 8. User defined functions
############
# Let's look at how to create your own functions. Why would you want to do this?
# R may not have a built-in function that does what you want to do or maybe
# you want to apply a certain function frequently in your analysis. Is there
# already a function called FOO?
FOO(1,1)
#.. no, we get an error saying there isn't.

# Let's make one. This is my function called FOO. It has two parameters s and q.
# Here I define my function to solve a very simple equation but the equation
# can be as complex as you like.
# The first part defines the parameters of the function (s,q) and
# the function itself is enclosed in {} (so that you can define more than
# line of code to comprise your function).
# Observe that function() is the function to create a function!
FOO <- function(s,q){
	(2*s + q)}

#When you call the function, you have to give it a value for s and q
#This line says compute FOO for s = 1 and q = 2
FOO(1,2)
#How about
FOO(2,1)
#... why is FOO(1,2) not equal to FOO(2,1)?
# Why do these give errors?
FOO(1)
FOO(1,1,1)

# 9. Loading packages to access more built in functions
##############
# Many times, however, someone else has already written a function to do what you
# want to do.
# How many days from Jan 1 to Oct 31? This is called the julian day and the function
# to figure this out is part of the chron package.

# Let's try this:
julian(10,31,1970)
#... did you get there there is no function called julian?

# We need to install the chron package:
install.packages("chron")
# ... but you can also do this via the "Packages" tab in the right panel.

# Now we have installed "chron", but we also need to tell R we would like to
# use it right now. Scroll through your package list. Is chron checked? No?
# run the command below:
library("chron")
#... now is it checked?

# Now let's use the julian function
julian(1,1,1970)
julian(10,31,1970)

# 10. Finding your mistakes
#########
# a) R is case sensitive and spelling matters. These will give errors:
Read.csv('PA_Data.csv')
read.csv('P_Data.csv')
# In the first instance there is no function called 'Read.csv()' defined just
# 'read.csv()'. In the second instance, there is no file 'P_Data.csv' just
# 'PA_Data.csv'. R isn't going to 'guess' what you meant to say!

# b) R has a huge user community. You can most likely find solutions to any problems
# you have by typing your question about R or you error message into a search engine.

# c) Try R help (help() or ?topic or ??topic where topic is what you want to learn about.

# d) Another common error is forgetting a closing bracket or having too may closing
# brackets. Run your cursor over these - it can help with clues.
(1+1))
(2+1
  #... the + in your console means R is waiting for the rest of the code block.
  
# e) If a script is calling a missing function, it could be that the package isn't
# loaded.
  
# f) The error messages might give you clues about what is wrong, but it can take
# some time to get used to these.
  
# g) Is the problem specific to your function? Are you providing too many or the
  # wrong kind of arguments to your function.
  read.csv(1)
#... the argument of read.csv should be a file name not a number so we get an error.
  # Try using ?read.csv or help(read.csv)
  
# h) "Step and Query". There is an error in your code. Particularly, if you are running
  # the entire script via "source()", your first job is to find the line of code that
  # generates the error. Run the first line of code and then query the result in your
  # console. Is it what you expected? No? Then fix it! Yes, try the next line and query
  # Is it what you expected. Step through everything sequentially making sure everything
  # is going to plan.
  
# i) "Source and print". Another version of h) is to print messages to your console to
  # get updates on what the values of your variables are at different points in the script.
  # Remember that the code runs from top to bottom.
  r<-1
  print(r)
  r<-3
  print(r)
  r<-2
  print(r)
  print('We made to line 413!')
  # This is another way to track what is happening.
  
  # 11. Writing your own code
  # a) A lot of the time, I copy and paste parts of old codes that I've written and
  # modify them for the new task. I almost never remember how to add a legend to
  # a plot, so I just look for some old code that I have where I have a legend on
  # my plot and copy it.
  
  # b) For future labs, you might want to refer back to this file to remember how to,
  # for example, make a plot. That's perfectly fine!
  
  # c) Alternatively, the R help files and vignettes sometimes have examples. I will copy and paste
  # the example into my script and check that it runs, and then just change one piece
  # at a time until I've changed the example, into what I wanted to do.
  
  ####### LAB 1.
  # To hand in for next week:
  #1a. Give three different examples of R code that won't work.
  #1b. In each of the three instances, write the error messages that are generated.
  #1c. Describe why the codes won't work.
  #1d. Give the corrected version of the code.
  
  #2. What is the hardest thing, for you, about reading code or learning to code?
  
  #3. Look at 'TwoCaribouHerdExample.R'.
  #3a. On what lines is there a user-defined function DEFINED?
  #3b. What is the name of the user-defined function?
  #3c. Give one of the lines where a user-defined function is CALLED.
  #3d. Name a built-in function that is used.