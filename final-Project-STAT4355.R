library(faraway)
library(MASS)
library(car)
library(reshape2)
library(ggplot2)
library(utility)
library(leaps)
data(divusa)


#Perform Model Building using the 7 step process. 

#1) Fit the full model
#the effect of year, umployment, females in labor force, marriages, births and miliary personal
#on divorce (77 observations and 7 predictors)
full_model <- lm(divorce ~ unemployed + femlab + marriage + birth + military, data = divusa)
summary(full_model)

############################################################################
#2 Perform Residual Analysis

#We will standarized residual values
stdres(full_model)
#We will standarized residual values
studres(full_model)

#we will use the rstudent to watch out for residual values
rStudent <-rstudent(full_model)
rStudent

#plot standardized, studentized and rstudent residuals
par(mfrow=c(1,3))
barplot(height =stdres(full_model), names.arg = 1:77,
        main = "Standardized Residuals\nFull Model", xlab = "Index",
        ylab = "Standardized Resid", ylim=c(-3,3))
abline(h=2, col = "Red", lwd=2)
abline(h=-2, col = "Red", lwd=2)

barplot(height = studres(full_model), names.arg = 1:77,         
        main = "Studentized Residuals\nFull Model", xlab = "Index",        
        ylab = "Studentized Resid", ylim=c(-5,5))
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)

barplot(height = rStudent, names.arg = 1:77,         
        main = "R Student Residuals\nFull Model", xlab = "Index",         
        ylab = "R Student Resid", ylim=c(-5,5))
abline(h=qt(0.05/(2*77), 68, lower.tail=F) , col = "Red", lwd=3)
abline(h=-qt(0.05/(2*77), 68, lower.tail=F) , col = "Red", lwd=3)

#range of the values in rstudent (min and max)
range(rStudent)
#range for threshold of rstudent postivie and negative 3.7
qt(0.05/(2*77), 31, lower.tail=F)


#checking for influential observsations on the final model
myInf <-influence.measures(full_model)
summary(myInf)

influenceIndexPlot(final_model, vars=c("Cook", "Studentized", "hat"))
############################################################################
#3 Do you need transformation. Yes because the residual plot is not linear

par(mfrow=c(1,1))
residualPlot(full_model, main = "Residual Plot \nFull Model", type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

############################################################################
#4 Transform if Needed
#Part 1) check for log transformation on x
div_log <- data.frame(log_x= (log(divusa[,3:7]) ),
                      y=divusa[,2])

full_log <- lm(y ~ log_x.unemployed + log_x.femlab + log_x.marriage + log_x.birth + log_x.military, 
               div_log)

par(mfrow=c(1,2))
residualPlot(full_model, main = "Residual Plot\nFull Model", type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
residualPlot(full_log , main = "Residual Plot of Log(x) \nTransformation",type = "rstudent",quadratic = F, col = "black", 
             pch = 16, cex =  1.5,cex.axis = 1.5, cex.lab = 1.5)


#Part 2) Since tranformation on x did not work we will do it on y.
#a) Check if a square root y tranformation will help
div_sqrt <- data.frame(x=divusa[,3:7], 
                       sqrt_y = sqrt(divusa[,2]) )

fullModel_sqrt <- lm(sqrt_y ~ x.unemployed + x.femlab + x.marriage + x.birth + x.military,
                     div_sqrt)

#b) check if reciprical of y helps
div_rcpl <- data.frame(x=divusa[,3:7], 
                       rcpl_y =  ( 1 / (divusa[,2]))) 

fullModel_rcpl <- lm(rcpl_y ~ x.unemployed + x.femlab + x.marriage + x.birth + x.military, 
                     div_rcpl)

#c)Box Cox transformation
par(mfrow=c(1,1))
boxcox_Div = boxCox(divusa$divorce ~ divusa$unemployed + divusa$femlab + divusa$marriage + divusa$birth + divusa$military, 
                    lambda = seq(-4,4,1/10))
lambda <- boxcox_Div$x[which.max(boxcox_Div$y)] #gives -.92

div_BoxTran <- data.frame(x=divusa[,3:7], 
                          box_y = ((divusa[,2])^ lambda ) )

fullModel_BoxTran = lm(box_y ~ divusa$unemployed + divusa$femlab + divusa$marriage + divusa$birth + divusa$military,
                       div_BoxTran)


#plotting different transformations to compare residual plots
par(mfrow=c(2,2))
residualPlot(full_model, main = "Residual Plot\nFull Model", type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
residualPlot(fullModel_sqrt, main = "Residual Plot of\n sqrt(y) transformation",type = "rstudent",quadratic = F, col = "green", 
             pch = 16, cex =  1.5,cex.axis = 1.5, cex.lab = 1.5)
residualPlot(fullModel_rcpl, main = "Residual Plot of\n  1/y transformation",type = "rstudent",quadratic = F, col = "red", 
             pch = 16, cex =  1.5,cex.axis = 1.5, cex.lab = 1.5)
residualPlot(fullModel_BoxTran , main = "Residual Plot of Box-Cox of\n y ^ -.92. ",type = "rstudent",quadratic = F, col = "black", 
             pch = 16, cex =  1.5,cex.axis = 1.5, cex.lab = 1.5)

#Fit new transformed model with 1/y transformation
fit <- lm(rcpl_y ~ x.unemployed + x.femlab + x.marriage + x.birth + x.military, 
          data = div_rcpl)
summary(fit)

#Performing Residual Analysis for transformed model
rStudent2 <-rstudent(fit)
rStudent2

par(mfrow=c(1,3))
barplot(height =stdres(fit), names.arg = 1:77,
        main = "Standardized Residuals \nTransformed Model", xlab = "Index",
        ylab = "Standardized Resid", ylim=c(-3,3))
abline(h=2, col = "Red", lwd=2)
abline(h=-2, col = "Red", lwd=2)


barplot(height = studres(fit), names.arg = 1:77,         
        main = "Studentized Residuals \nTransformed Model", xlab = "Index",        
        ylab = "Studentized Resid", ylim=c(-5,5))
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)


barplot(height = rStudent2, names.arg = 1:77,         
        main = "R Student Residuals \nTransformed Model", xlab = "Index",         
        ylab = "R Student Resid", ylim=c(-5,5))
abline(h=qt(0.05/(2*77), 68, lower.tail=F) , col = "Red", lwd=3)
abline(h=-qt(0.05/(2*77), 68, lower.tail=F) , col = "Red", lwd=3)

par(mfrow=c(1,1))
residualPlot(fullModel_BoxTran , main = "Residual Plot of\nTransformed Model",type = "rstudent",quadratic = F, col = "black", 
             pch = 16, cex =  1.5,cex.axis = 1.5, cex.lab = 1.5)

#5 Perform all possible regressions
# load leaps package with regsubsets
n <- (nrow(divusa))
all.possible <- regsubsets(rcpl_y ~ x.unemployed + x.femlab + x.marriage + x.birth + x.military, data = div_rcpl)
summary(all.possible)
summary(all.possible)$which


# create the table of selection criteria
ap.mse <-summary(all.possible)$rss/(n-(2:6))
ap.adjr2 <-summary(all.possible)$adjr2
ap.cp <-summary(all.possible)$cp
ap.bic <-summary(all.possible)$bic
ap.criteria <-cbind(ap.mse, ap.adjr2, ap.cp, ap.bic)
colnames(ap.criteria) <-c("MSE", "Adj R2", "Cp", "BIC")
rownames(ap.criteria) <- 2:6
ap.criteria

#visualizing selection criteria
par(mfrow=c(2,2))
plot(all.possible, scale="r2", main = "Exhaustive: R2 \nFitted Model")
plot(all.possible, scale="adjr2", main = "Exhaustive: Adjusted R2\nFitted Model")
plot(2:6, ap.mse, col = "blue", type = "l", xlab = "p", ylab = "MSE")
plot(2:6, ap.adjr2, col = "blue", type = "l", xlab = "p", ylab = "Adj R2")

par(mfrow=c(2,2))
plot(all.possible, scale="Cp", main = "Exhaustive: Cp \nFitted Model")
plot(all.possible, main="Exhaustive: BIC \nFitted Model")
plot(2:6, ap.cp, col = "blue", xlab = "p", ylab = "Cp", pch=16, cex=1)
abline(a=0,b=1, col = "red")
plot(2:6, ap.bic, col = "blue", type = "l", xlab = "p", ylab = "BIC")

