# Model-Building - Determining factors that lead to divorce

It is a common belief that 50% percent of all marriages will end getting a divorce. However, there can be many reasons for a married couple to get a divorce both in their control and not and their control. The dataset divusa from faraway package in R, contains data that involves married and divorced couples from 77 years. The data has macro-economic indicators that can help predict divorce. Attached below is a sample of the divusa dataset.  The data is labeled as followed:
1.	Year: year from 1920-1996
2.	Divorce: divorce per 1000 women age 15 or higher
3.	Unemployed: unemployment rate
4.	Femlab: percent of female participation in labor force
5.	Marriage: marriages per 1000 unmarried women age 16 or higher
6.	Birth: births per 1000 women age 15-44
7.	Military: military personnel per 1000 population

# This report will include a rigorous analysis on determining the best model to predict divorce rates from the divusa dataset. 

**A full model will consist of the following equation.**
![alt-text](https://github.com/JaimeGoB/Model-Building/blob/master/Equations/fullModel.png)


In order to see if the model needs to undergo a transformation of variables y or x, the residual plot of full model needs to show that the errors are random, do not follow any pattern(cone, circle or parabola), have a mean of zero and are normally distributed. A residual plot shows if the error follows the Normality assumptions. From the residual plot shown below we can see that the model needs a transformation. 

![alt-text](https://github.com/JaimeGoB/Model-Building/blob/master/Equations/transformations.png)


Since lambada = -1 is still within the 95% confidence interval of Box-Cox we will use this transformation because it is easier to interpret than a lambada = -.92. **So, after performing a 1/y transformation the new fitted model becomes:**

![alt-text](https://github.com/JaimeGoB/Model-Building/blob/master/Equations/transformedModel.png)


Using an exhaustive search, we will get the most optimal subsets of regression coefficients.

All four different tests from exhaustive search show that dropping the military coefficient will yield a better model. So, we will drop this variable thus; 
**The final fitted model becomes:**

![alt-text](https://github.com/JaimeGoB/Model-Building/blob/master/Equations/finalModel.png)


