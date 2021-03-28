rm(list=ls())

#load libraries for data, plotting and testing
library(OECD)
library(ggplot2)
library(lmtest)
library(ggiraphExtra)
library(tseries)
library(mctest)
library(scatterplot3d)

#get data from OECD library
inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1983, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1983, end_time=(2019))

#extract relevant data
unemployment=unemployment_data$obsValue[39:74]
inflation=inflation_data$obsValue[2:37]
inflation_previous_period=inflation_data$obsValue[1:36]
summary(lm(inflation ~ unemployment+inflation_previous_period))

#reduce variance
unemployment <- log(unemployment)

model <- lm(inflation ~ unemployment+inflation_previous_period)

#Then we have estimated the variance
res <- resid(model)
y_hat <- fitted(model)
(variance_hat = sum(res**2)/(length(res)-3))

#multiple regression plot for given values of inflation_previous_period so to see how close possible regression lines are to
#data points
ggPredict(model,se=TRUE,interactive=TRUE) 

#3-Dplot of the regression
scatterplot3d(unemployment,inflation_previous_period,inflation)
model_coefficients=coef(model)
plot3d=scatterplot3d(unemployment,inflation_previous_period,inflation,angle=30,scale.inflation=0.7,pch=16,color='orange',main="Regression Plane")
plot3d$plane3d(model,lty.box="solid",col='blue')

#then we plotted the fitted and the real values to asses the goodness of the model.
#We also plotted errors to have broader picture
plot(inflation, type='l',main="Actual (black),Fitted (green) and Residuals (orange)",asp=1.3)
lines(y_hat, col='green')
lines(res,col='orange')

# Test for linearity:

#The Ramsey Regression Equation Specification Error Test (RESET) tests whether non-linear combinations of 
#the fitted values help explain the dependent variable. 
#The intuition behind the test is that if non-linear combinations of the explanatory variables have any 
#power in explaining the dependent variable, the model is mis-specified in the sense that the data generating 
#process might be better approximated by a polynomial or another non-linear functional form.
#If the null-hypothesis that all coefficients are zero is rejected, then the model suffers from misspecification.
reset(model)

# Test for normality of errors:

#The Jarque-Bera tests test H0: errors are N(0,sigma^2) against H1: errors are not normal
jarque.bera.test(res)

# Heteroscedasticity tests:

#The Goldfeld-Quandt test compares the variances of two submodels divided by a specified breakpoint 
#and rejects H0: homoscedasticity in favour of H1: vari=cXi^2 if the variances differ
gqtest(model, order.by=unemployment)
gqtest(model, order.by=inflation_previous_period)

# The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression 
#model and rejects if too much of the variance is explained by the additional explanatory variables. 
bptest(model, studentize = FALSE)

# Error correlation tests:

#The Breusch-Godfrey test tests H0: errors are uncorrelated against H1: errors are correlated up to degree m. 
#To do this test a statistic with the residuals is built.
bgtest(model, order=3)

# zero mean of residuals
mean(res)

#test for multicollinearity
omcdiag(model)
