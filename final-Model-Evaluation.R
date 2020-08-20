#6 Select models for further analysis
#All criteria for model selection clearly show that 
#the best model that consists of 4 predictor variables
#unemployed, femlab, marriage and birth. So we can drop year and military.

#Fit the final model
final_model <- lm(rcpl_y ~ x.unemployed + x.femlab + x.marriage + x.birth,
                  data = div_rcpl)
summary(final_model)


#Ploting normality for final model
par(mfrow = c(1, 2))
hist(studres(final_model),main ="Histogram of\nStudentized Residuals\nFinal Model", 
                        xlab = "Studentized Residuals", breaks = 10, freq = F, 
                        col = "cornflowerblue", 
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
qqPlot(final_model, main = "Normality Plot \nFinal Model")

#Plotting the residual plot
par(mfrow = c(1, 2))
residualPlot(full_model, main = "Residual Plot \nFull Model", type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)
residualPlot(final_model, main ="Residual Plot \nFinal Model", type="rstudent", quadratic=F, col = "dodgerblue",
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5)

#checking for influential observsations on the final model
myInf <-influence.measures(final_model)
summary(myInf)

influenceIndexPlot(final_model, vars=c("Cook", "Studentized", "hat"))

rstudent3 <- rstudent(final_model)

par(mfrow=c(1,3))
barplot(height =stdres(final_model), names.arg = 1:77,
        main = "Standardized Residuals \nFinal Model", xlab = "Index",
        ylab = "Standardized Resid", ylim=c(-3,3))
abline(h=2, col = "Red", lwd=2)
abline(h=-2, col = "Red", lwd=2)

barplot(height = studres(final_model), names.arg = 1:77,         
        main = "Studentized Residuals \nFinal Model", xlab = "Index",        
        ylab = "Studentized Resid", ylim=c(-5,5))
abline(h=3, col = "Red", lwd=3)
abline(h=-3, col = "Red", lwd=3)

barplot(height = rstudent3, names.arg = 1:77,         
        main = "R Student Residuals \nFinal Model", xlab = "Index",         
        ylab = "R Student Resid", ylim=c(-5,5))
abline(h=qt(0.05/(2*77), 71, lower.tail=F) , col = "Red", lwd=3)
abline(h=-qt(0.05/(2*77), 71, lower.tail=F) , col = "Red", lwd=3)



