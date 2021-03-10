install.packages("lmtest")


rm(list=ls())
library(OECD)
library(ggplot2)
library(lmtest)
library(tseries)

inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1983, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1983, end_time=(2019))


unemployment=unemployment_data$obsValue[39:74]
inflation=inflation_data$obsValue[2:37]
inflation_previous_period=inflation_data$obsValue[1:36]
# difference=inflation-inflation_previous_period
# summary(lm(difference ~ unemployment))
# e=ggplot(data=NULL,aes(x=unemployment,y=difference))
# e+geom_point()+geom_smooth(method=lm, se=FALSE)
summary(lm(inflation ~ unemployment+inflation_previous_period))

# Decreases the variance:
unemployment <- log(unemployment)

model <- lm(inflation ~ unemployment+inflation_previous_period)
res <- resid(model)
y_hat <- fitted(model)
(variance_hat = sum(res**2)/(length(res)-3))

plot(inflation, type='l')
lines(y_hat, col='green')

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


