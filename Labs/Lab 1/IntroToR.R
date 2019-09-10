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

# 1. R remembers only the last value of a parameter, and it remembers that
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

#... on the otherhand tail() is a built-in function that shows the last 6 lines
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
# as we did above with


###########
# FOR LOOPS
###########
#In this course, you will frequently solve equations for a range of values. One way to do this in R is with for loops. Two other ways of doing this are with built in functions like sapply and tapply or by creating your own function. I encourage you to look into the 'apply' functions. Below we will go over how to create your own function. But first, let's look at creating for loops.
#First step: you want to create an empty data frame to place your data. I will create an empty data frame called 'Data_Loop'
Data_Loop <-NULL
#I would like to find the solution to the equation i + j for i = 0.5 and j = 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9. 1.0
#set the i parameter value
i <- 0.5
#Start your loop. You will be looping over values of j between 0.1 and 1 by steps of 0.1. The loop is applied to things inside the squiggly brackets { }
for(j in seq(0.1,1,0.1)){

#this is the equation you will be calculating for every value of j
loop_1 <- i + j

#Put your data in a data frame. Don't forget to enclose this within the loop!
#Here I am telling R to place the results of our loop - 'loop_1' in a data frame along with the values of j.
#The data frame has two columns; "Parameterj" and "Result". The data to put in these columns come after the =. In column "Parameterj" we are placing j values (this is the parameter we are looping over) and in column "Result" we are placing the values of loop_1 evaluated for different values of j.
#Here we use rbind to add an additional row of data for every iteration of j. If we did not do this, our data frame would only have one row. When populating a data frame within a loop be sure to use rbind (or cbind - look it up!)
Data_Loop <- rbind(Data_Loop, data.frame(Parameterj=j, Result=loop_1))

#print the j value so you can track progress. Good for longer loops
print(j)
#Close the loop
}

Data_Loop

#You can easily extract elements of a data frame by using []
#Give me the value of Data_Loop at row = 3 in column = 2
Data_Loop[3,2]

#Or give me the first column of Data_Loop  where j is the name of the first column of the data frame Data_Loop 
Data_Loop$Parameterj


############
# FUNCTIONS
############
#Finally, let's look at how to create your own functions. Why would you want to do this? R may not have a built-in function that does what you want to do or maybe you want to apply a certain function frequently in your analysis.
#This is my function. It is called FOO. It has two parameters s and q.  Here I define my function to solve a very simple equation - as above - but the equation can be as complex as you like.
#The first part defines the parameters of the function (s,q) and the function itself is enclosed in {}
#function is the function to create a function! Say that one 10 times fast!
FOO <- function(s,q){
	(s + q)}

#When you call the function, you have to give it a value for s and q
#This line says compute FOO for s = 1 and q = 1
FOO(1,1)

#Let's use this function to create a table as above
Data_Loop2 <-NULL
#Here I am iterating the function FOO over s = 0.5 for q between 0.1 to 1 by steps of 0.1...just as above
Data_Loop2 <- data.frame(Parameterq=seq(0.1,1,0.1), Result=FOO(0.5,seq(0.1,1,0.1)))
Data_Loop2


#############################################################################
# LAB #1 - useful code
#############################################################################
#For question 1 of the lab you are to reproduce the figures from the textbook.

#Here I reproduce Figure P1.2 for three different slope (b) values
#This is my linear function f where b is the slope and c is the intercept (i.e. value of f when x = 0)
f <- b*x + c

#I want to plot f for c = 1, x = 0 to 10, and b = 1, 5, 10
# For your assignment, you will need to try different parameter values (i.e., b and c values) to find the ones that most closely match the textbook figures
#Create a blank data frame
Linear <- NULL
#set the c parameter value
c <- 1
#create a vector of x values
x<-seq(0,10,.1)

######
# This sets the label x as the header of your dataframe (named 'Linear') and then places the numerical values of x into the column of the dataframe. At the moment, Linear has just one column of data
Linear <- data.frame(x=x)

#Start your loop. You will be looping over values of values of b = 1, 5, and 10. The loop is applied to things inside the squiggly brackets { }.
for(b in c(1,5,10)){
#Below is the equation you will be calculating for every value of b. On the first iteration of the loop b is 1.
#Note that since x is a vector (i.e., it is not just a single number, but rather x consists of multiple entries), then f is also a vector where each value of f corresponds to the value of x at the same position.
f <- b*x + c

#Now we add the results of our calculations to our data frame, Linear, using the function cbind to 'bind' the existing columns of Linear with columns containing the new results from this iteration of the loop. The size of the dataframe grows with each iteration of the loop. After the first iteration, Linear consists of 3 colums, x, b, and f where all the b values are 1. After the second iteration of the loop, Linear consists of 5 columns: x, b, f, b, f, where for the second occurance of b all the b values are 5 and the second occurance of f corresponds to the values of f when b is 5.
Linear <- cbind(Linear, data.frame(b=b, f=f))

#print the b value so you can track progress.
print(b)
#Close the loop
}

#Now lets take a look at the dataframe 'Linear' that we have created.
Linear
#The dataframe has 101 rows for each different value of x and 7 columns.
#To extract an entire column of the dataframe, for example column 1 of 'Linear', we can type Linear[,1]


#Now plot your linear function for different slopes
par(mfrow=c(1,1))
#Base plot with straight black line. The x-axis values are taken from the dataframe 'Linear' and are the x values for b = 1. The y-axis values are taken from the data frame 'Linear' and are the f values for b =1. We will make the plot for y-axis 0 to 110.
plot(Linear[,1],Linear[,3], xlab="x", ylab="f(x)", type="l", ylim=c(0,110), cex.lab=1.5, cex.axis=1.5, lwd=2)
#Add a line where the slope is 5 (ie b = 5) and make this line dashed
lines(Linear[,1],Linear[,5], lty="dashed", lwd=2)
#Add a line where the slope is 10 (ie b = 10) and make this line dotted
lines(Linear[,1],Linear[,7], lty="dotted", lwd=2)
#Add a legend in the top-left corner
legend("topleft", legend=c("b=1", "b=5", "b=10"), lty=c("solid","dashed","dotted"), lwd=2, cex=1.5)
#Beauty!

####################################################################
#Iterating over the different values of b was a tidy and efficient way to make
#graphs of the SAME function with different parameter values, but for question 3 you are required
#to plot several different functions: constant, linear, and quadratic on the same axis. In this case
#it may be easier to work without a loop because the equations are different on each iteration.
####################################################################
#The equation we are using is 
g <- x*sin(x)
#remember R recalls the last values of x which we used above. If you want new x values for this function, you need to assign them here
#sin is a built-in function in R
#We want to calculate x for values of 0 to 10 (which is consistent with how x is already defined above).

#Type in the equations for your approximations below:
#g0 <- write the equation here and uncomment
#g1<- write the equation here
#g2<- write the equation here

#Plot the function g
par(mfrow=c(1,1))
plot(x,g, xlab="x", ylab="g(x)", type="l", cex.lab=1.5, cex.axis=1.5, lwd=2)
#Add your approximation lines here by using the 'lines' function
#lines(x,g0,lty="dashed", lwd=2)
#Some basic things to remember
# 1) anything that comes after a # is a comment that will not be computed by R. This is a good way to insert comments in your code. For assignments that require you to hand in code, we expect you to comment your code so that we can follow your procedures - ie make your code and results easily reproducible.
# 2) R is case sensitive. If your code is not running first check to see if there are any spelling mistakes or case mistakes.
# 3) R has a huge user community. You can most likely find solutions to any problems you have by typing your question about R in google. We expect you to first try to solve your own problems by troubleshooting using google or R help (help() or ?topic or ??topic where topic is what you want to learn about). 
# 4) R has a large number of built-in functions. For example read.csv is a function to read .csv files into R. Sometimes you will have to download R packages to access additional functions. For example the package 'lattice' has nice functions to create 3D plots. To download this package you can use the function install.packages() or you can use the menu 'Packages & Data' - Get list and scroll to the 'lattice' package. Once you have downloaded the package, to use it you must open the package by typing 'library(lattice)'. It is useful to list the libraries you plan to use in your code at the top of your code file.
# 5) Always type your code in some kind of text editor like Rstudio or notepad. R for Mac has a decent built-in text editor. This will allow you to reproduce your analysis easily.
# 6) To run your code you can a) copy and paste from your text editor to the R console, b) highlight your code and type apple-return on macs or shift-return on pc
# 7) you can create an object named pretty much anything - a, b, HgW, HJ6, Data1, etc. R will overwrite values of your objects as you use them. So if you want to use an object throughout, it is best not to name any new objects with the same name. ie only have one object named a, one named b, etc.

