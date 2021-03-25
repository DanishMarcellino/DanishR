rm(list=ls())

#load libraries for data, plotting and testing
library(OECD)
library(ggplot2)
library(lmtest)
library(ggiraphExtra)
library(tseries)
library(mctest)

#get data from OECD library
inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1998, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1998, end_time=(2019))
protection_data=get_dataset("EPL_CD", filter="DNK.EPC_V2", start_time=1998, end_time=(2019))
wage_data=get_dataset("AV_AN_WAGE", filter="DNK.USDPPP", start_time=1998, end_time=(2019))

#estimation of the mark-up
real_wage=wage_data$obsValue[2:22]
markup=1/real_wage-1 

#extract relevant data
protection=protection_data$obsValue[2:22]
unemployment=unemployment_data$obsValue[24:44]
inflation=inflation_data$obsValue[2:22]
inflation_previous_period=inflation_data$obsValue[1:21]

#compute relevant differences
un=(protection+markup) #natural rate of unemployment (off by a constant alpha)
u_difference=unemployment-un 
infl_difference=inflation-inflation_previous_period

#We have then regressed inflation over u_difference and the inflation of the previous period
model=lm(inflation~u_difference+inflation_previous_period)
(summary(model))

#multiple regression plot for given values of inflation_previous_period so to see how close possible regression lines are to
#data points
ggPredict(lm(inflation~u_difference+inflation_previous_period),se=TRUE,interactive=TRUE) 


#We have then plotted the regression between the difference in inflation and u_difference 
#(we have used the difference in inflation rather than the inflation alone (as we did for the model), in order
#to have a 2-D plot)
graph=ggplot(data=NULL,aes(x=u_difference,y=infl_difference))
graph+geom_point()+geom_smooth(method=lm, se=FALSE) 


#Then we have estimated the variance
(res=residuals(model))
(res2=sum(res**2))
(variance=res2/(21-3))



#To check whether our assumptions about the linear regression model were respected we decided to run some test.
#In particular, we carried out:


# Homoscedasticity tests:

#The Goldfeld-Quandt test compares the variances of two submodels divided by a specified breakpoint 
#and rejects if the variances differ. In our case we called the test on our model obtained through the 
#linear regression and we got a p-value of 0.3736, meaning that the hypothesis that the variance is 
#homoscedastic is not rejected with a very high confidence.
gqtest(model)

# The Studentized Breusch-Pagan test fits a linear regression model to the residuals of a linear regression 
#model and rejects if too much of the variance is explained by the additional explanatory variables. 
#So also this test was useful to understand whether or not homoscedasticity was respected also by the error term. 
#In this case we got a p-value of 0.2126 so we can conclude that variances are indeed homoscedastic. 
bptest(model)

#Is residuals mean equal zero?

#As we can see from the following calculation the mean of the residuals is approximately zero, which is 
#correct for the OLS model
(mean(res))

# Error correlation tests:
bgtest(model,order=3)

#test for normality
jarque.bera.test(res)


# Test for linearity:

#The Ramsey Regression Equation Specification Error Test (RESET) tests whether non-linear combinations of 
#the fitted values help explain the dependent variable. 
#The intuition behind the test is that if non-linear combinations of the explanatory variables have any 
#power in explaining the dependent variable, the model is mis-specified in the sense that the data generating 
#process might be better approximated by a polynomial or another non-linear functional form.
#If the null-hypothesis that all coefficients are zero is rejected, then the model suffers from misspecification.
#In our case we get a p-value of 0.6536 so H0 is not rejected and therefore we can conclude that linearity
#is plausible.
reset(model)

# Test for multicollinearity

#Since all diagnostics return 0, there is no evidence of multicollinearity.
omcdiag(model)

