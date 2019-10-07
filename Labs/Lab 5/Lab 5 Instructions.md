# Lab 5: Compuational analysis of yellow columbine (_Aquilegia chrysantha_) stage-structured population dynamics
(This file is best viewed on the Github website - and other .md files too)

This report is due Wednesday Oct 16 at 2pm

<img src="https://upload.wikimedia.org/wikipedia/commons/9/9d/Aquilegia_chrysantha_2.jpg" width="50%">
Photo by Stan Shebs [CC BY-SA 3.0 (https://creativecommons.org/licenses/by-sa/3.0)]

1. Open `RStudio` and install the package `popbio`.
2. Download and run the file `Yellow_Columbine_Stage_Structure.R`. This code will produce 2 graphs: 1) the number of seeds over time, and 2) the number of the different types of plants over time.

3.	Into the R console type:
```
> head(aq.trans)
```

This command lists the first six row of data entries along with their headings. To understand the meaning of the columns in the dataframe, click the `Packages` tab in `RStudio`, click `popbio` and then scroll down to `aq.trans` and click on that. Does the dataset contain any information on the number of seeds produced each year?

To determine how many different plots are recorded in this dataset, into the `R Studio console` type:
```
> table(aq.trans$plot)
```
The first row of the output is the plot number, and the second row is the number of plants recorded in each plot. Modify this command to generate summary statistics for other variables that you are interested in. For example,
```
> table(aq.trans$stage)
```
will summarize the number of plants in each stage across all plots.

4.	The code uses a subset of the data: only year 1998 and plot 909. To see the data subset, type:
```
> sf
```
into the `R Studio console`. Does every flowering plant produce fruit?

5.	The dataset does not contain any information on the number of seeds. In the code, what assumptions are made to estimate the size of the seed bank in 1999?

6.	Type,
```
> A
```
into the `R Studio console`. This is the projection matrix. Where _i_ is an index for the row number (top to bottom) and _j_ is an index for the column (left to right), the elements of the projection matrix are the number of _i_ that will be produced by each _j_ over the course of a year.

Try it out – the number of seeds produced per seed is 0.4. This means that each seed has a 0.4 probability of surviving a year. How many small plants is each small plant expected to produce (i.e., the probability a small plant survives and remains in the small class)? How many large? How many with flowers?

If a small plant, next year, can only be a small plant, a large plant, a flowering plant, or dead, what must be the probability that the small plant dies? What is the probability a large plant dies? A flower?
```
> 1-sum(A[,3])
> 1-sum(A[,4])
> 1-sum(A[3:5,5])
```
Note, 100% survival is not realistic, however, this is what the data has implied and here, we are just trying to understand the projection matrix.
7.	Type
```
> nt
```

into the `R Studio console`. The intrinsic rate of growth of the population is the dominant eigenvalue of the projection matrix, `A`. If the dominant eigenvalue is bigger than 1, the population will grow. If the dominant eigenvalue is less than 1, the population will eventually go extinct. The dominant eigenvalue of our projection matrix, `A`, is calculated as,
```
> nt$lambda
```
Will the population of yellow columbine in plot 909 grow (based on the data from 1999)?

8.	Type the command,
```
> nt=pop.projection(A,n,20)
```
into the `R Studio console`. This command starts with an initial population size of `n` and uses the projection matrix `A`, to calculate the size of the population for the next 19 years. Note that in the code `n = c(10000, 100, 100, 100, 100)` means that we start with 10,000 seeds and 100 of each type of plant. The predicted number of yellow columbine in each stage each year is,

 ```
> nt$stage.vectors
```
After 19 years, which stages have increased in prevalence? Consult the graphs (be sure to figure out which lines correspond to which stages – you will need to consult the R code). Does it look like the population will persist? Increase the number of years that you predict the size of the population for. Were you correct that the population will go extinct?

9.	For this model, eventually the fraction of the population that is in each stage will be unchanging. The relative abundance of each stage (providing that sufficient time has passed) is predicted by the right eigenvalue of the projection matrix, `A`. Type,
```
> stable.stage(A)
```

into the `R Studio console`. Reset the number of years into the future that you predict the population abundance to 19 (i.e., the command from 8.) and re-run `Yellow_Columbine_Stage_Structure.R` (using the `Source` button). Now, we wish to test whether the predicted relative of abundance of each stage is achieved on the 19th year into the future. Type,
```
> nt$stage.vectors[,19]/sum(nt$stage.vectors[,19])
```

The stable stage distribution is dominated by the abundance of seeds. To calculate the relative abundance of just the plant stages type,
```
>nt$stage.vectors[2:5,19]/sum(nt$stage.vectors[2:5,19])
```

How does the result compare to `stage.structure(A)`. About what percentage of plants would we expect to be flowering?

If the population does not start at the stable stage distribution it may take some time to reach the stable distribution. Is the stable stage distribution reached on the 5th year? Modify the commands from above to determine this - note that 19 above refers to year 19.

10.	Suppose it were possible to increase any non-zero entry of the projection matrix by a fixed amount. Increasing the value of which entry by a fixed amount would have the biggest affect the intrinsic growth rate, `lambda`?

To answer this question type,
```
> sensitivity(A, zero=TRUE)
```
The results of this command tells us that increasing the fraction of seeds that germinate to recruits would most substantially increase the intrinsic growth rate. How would this information help us to figure out how best to promote the growth of yellow columbine?

11.	Currently, the population of yellow columbine is destined to go extinct. Change the value of `seed.survival` until the population is just able to persist. In other words, edit your `R` script by changing `seed.survival` and `Source` the code. Find the miminum value of `seed.survival` such that we would expect the yellow columbine population to grow in size.

## To hand in:

Write a report describing the results of your analysis of the yellow columbine population. Your report should include the following:

-	In the _Introduction_ section, write one sentence (with a citation) describing yellow columbine (_Aquilegia chrysantha_). The citation may be from the Internet or from the peer-reviewed literature.

-	In the _Data_ section, describe the data that was used to parameterize the projection matrix. Specifically, you should mention that only a subset of the data was used in the analysis. Be sure to mention the assumptions that were made regarding the seed bank.

- In the _Discussion_ section, you should address the following questions:
  * How might your conclusions change if your assumptions were different?
  * What was a limitation of your analysis?
  * What are the implications of the sensitivity analysis results for suggested measures to help protect yellow columbine?
  
- In the _Discussion_ section, you should address the following questions:
  * How might your conclusions change if your assumptions were different?
  * What was a limitation of your analysis?
  * What are the implications of the sensitivity analysis results for suggested measures to help protect yellow columbine?
  
  

What was the intrinsic rate of growth?
Write out the projection matrix and explain what the elements of the projection matrix mean.
What is the stable stage distribution? What does it mean?
Include a graph of the population dynamics of the different plant stages of yellow columbine. In the figure caption, make sure you mention what each of the different line types represents.

-	In the _Results_ section, describe the findings of your analysis:

