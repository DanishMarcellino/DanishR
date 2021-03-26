rm(list=ls())
library(OECD)
library(ggplot2)
library(lmtest)
library(ggiraphExtra)
library(tseries)
library(mctest)
library(scatterplot3d)

inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1983, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1983, end_time=(2019))


unemployment=unemployment_data$obsValue[39:74]
inflation=inflation_data$obsValue[2:37]
inflation_previous_period=inflation_data$obsValue[1:36]
summary(lm(inflation ~ unemployment+inflation_previous_period))

# Decreases the variance:
unemployment <- log(unemployment)

model <- lm(inflation ~ unemployment+inflation_previous_period)
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

#then we plotted the fitted and the real values 
plot(inflation, type='l',main="Actual (black),Fitted (green) and Residuals (orange)",asp=1.3)
lines(y_hat, col='green')
lines(res,col='orange')

# Test for linearity:
reset(model)

# Test for normality of errors:
jarque.bera.test(res)

# Heteroscedasticity tests:
gqtest(model, order.by=unemployment)
gqtest(model, order.by=inflation_previous_period)

bptest(model, studentize = FALSE)

# Error correlation tests:
bgtest(model, order=3)

# zero mean of residuals
mean(res)

#test for multicollinearity
omcdiag(model)
