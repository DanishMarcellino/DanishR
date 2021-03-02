rm(list=ls())

#load libraries for data, plotting and testing
library(OECD)
library(ggplot2)
library(lmtest)

#get data from OECD library
inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1998, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1998, end_time=(2019))
protection_data=get_dataset("EPL_CD", filter="DNK.EPC_V2", start_time=1998, end_time=(2019))
wage_data=get_dataset("AV_AN_WAGE", filter="DNK.USDPPP", start_time=1998, end_time=(2019))

#estimation of the mark-up
real_wage=wage_data$obsValue[2:22]
real_wage_rescaled=real_wage/real_wage[1]  #rescale to a reference year
markup=1/real_wage_rescaled-1 

#extract relevant data
protection=protection_data$obsValue[2:22]
unemployment=unemployment_data$obsValue[24:44]
inflation=inflation_data$obsValue[2:22]
inflation_previous_period=inflation_data$obsValue[1:21]

#compute relevant differences
un=(protection+markup) #natural rate of unemployment (off by a constant alpha)
u_difference=unemployment-un 
infl_difference=inflation-inflation_previous_period

#linear regression summary and plotting
model=summary(lm(inflation~u_difference+inflation_previous_period))
ggPredict(lm(inflation~u_difference+inflation_previous_period)) #multiple regression plot for given values of infl(t-1)
graph=ggplot(data=NULL,aes(x=u_difference,y=infl_difference))  
graph+geom_point()
graph+geom_point()+geom_smooth(method=lm, se=FALSE) #plot of regression between infl-infl(t-1) and ut-un


res=residuals(model)
res2=sum(res**2)
variance=res2/(21-3)

# Heteroscedasticity tests:
gqtest(model)
bptest(model)

# Error correlation tests:
dwtest(model)

# Test for linearity:
reset(model)
