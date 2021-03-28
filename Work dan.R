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
inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1998, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1998, end_time=(2019))
protection_data=get_dataset("EPL_CD", filter="DNK.EPC_V2", start_time=1998, end_time=(2019))
wage_data=get_dataset("AV_AN_WAGE", filter="DNK.USDPPP", start_time=1998, end_time=(2019))
#CPI_data=get_dataset("PRICES_CPI",filter="DNK.CPALTT01.GP.A",start_time=1998, end_time=(2019))
#MEI_data=get_dataset("MEI_PRICES_PPI",filter="PIEAMP02+PIEAMP01+PIEATI02+PIEATI01+PIEAMI02+PIEAMI01+PIEAFD02+PIEAFD01+PIEAEN02+PIEAEN01+PITGVG02+PITGVG01+PITGIG02+PITGIG01+PITGCG02+PITGCG01+PITGCD02+PITGCD01+PITGND02+PITGND01+PISPPR02+PISPPR01+PISPIG02+PISPIG01+PISPFG02+PISPFG01+WPIATT01+WPIAMP01+WPOTFD01.DNK.GP+GY+IXOB.A",start_time=1998, end_time=(2019))

#estimation of the mark-up
real_wage=wage_data$obsValue[2:22]
#CPI_real=CPI_data$obsValue[2:22]
#MEI_real=MEI_data$obsValue[1238:1258]
#markup=CPI_real/real_wage-1 
markup=1/real_wage -1

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

#3D plot of the regression
scatterplot3d(u_difference,inflation_previous_period,inflation)
model_coefficients=coef(model)
plot3d=scatterplot3d(u_difference,inflation_previous_period,inflation,angle=30,scale.inflation=0.7,pch=16,color='orange',main="Regression Plane")
plot3d$plane3d(model,lty.box="solid",col='blue')

#Then we have estimated the variance
(res=residuals(model))
(res2=sum(res**2))
(variance_hat=res2/(21-3))

#then we plotted the fitted and the real values for inflation so to understand the goodness of the model. 
#We also plotted the residuals to have a even clearer idea of the goodness of the model
y_hat <- fitted(model)
plot(inflation, type='l',main="Actual,Fitted and Residuals",asp=1.2)
lines(y_hat, col='green')
lines(res,col='orange')


#To check whether our assumptions about the linear regression model were respected we decided to run some test.
#In particular, we carried out:


# Homoscedasticity tests:

#The Goldfeld-Quandt test compares the variances of two submodels divided by a specified breakpoint 
#and rejects H0: homoscedasticity in favour of H1: vari=cXi^2 if the variances differ
gqtest(model, order.by=u_difference)
gqtest(model, order.by=inflation_previous_period)

# The Breusch-Pagan test fits a linear regression model to the residuals of a linear regression 
#model and rejects if too much of the variance is explained by the additional explanatory variables. 
bptest(model, studentize=FALSE)


#Is residuals mean equal zero?

#As we can see from the following calculation the mean of the residuals is approximately zero, which is 
#correct for the OLS model
(mean(res))

# Error correlation tests:

#The Breusch-Godfrey test tests H0: errors are uncorrelated against H1: errors are correlated up to degree m. 
#To do this test a statistic with the residuals is built.
bgtest(model,order=3)

#test for normality:

#The Jarque-Bera tests test H0: errors are N(0,sigma^2) against H1: errors are not normal
jarque.bera.test(res)


# Test for linearity:

#The Ramsey Regression Equation Specification Error Test (RESET) tests whether non-linear combinations of 
#the fitted values help explain the dependent variable. 
#The intuition behind the test is that if non-linear combinations of the explanatory variables have any 
#power in explaining the dependent variable, the model is mis-specified in the sense that the data generating 
#process might be better approximated by a polynomial or another non-linear functional form.
#If the null-hypothesis that all coefficients are zero is rejected, then the model suffers from misspecification.
reset(model)

# Test for multicollinearity

#Since all diagnostics return 0, there is no evidence of multicollinearity.
omcdiag(model)

