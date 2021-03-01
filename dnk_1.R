rm(list=ls())
library(OECD)
library(ggplot2)
inflation_data=get_dataset("KEI", filter="CP+CPALTT01.DNK.GP.A", start_time=1998, end_time=(2019))
unemployment_data=get_dataset("STLABOUR", filter="DNK.LRHUTTFE+LRHUTTTT.ST.A", start_time=1998, end_time=(2019))
protection_data=get_dataset("EPL_CD", filter="DNK.EPC_V2", start_time=1998, end_time=(2019))
wage_data=get_dataset("AV_AN_WAGE", filter="DNK.USDPPP", start_time=1998, end_time=(2019))

wage=wage_data$obsValue[2:22]
markup=wage/wage[1]
protection=protection_data$obsValue[2:22]
unemployment=unemployment_data$obsValue[24:44]
inflation=inflation_data$obsValue[2:22]
inflation_previous_period=inflation_data$obsValue[1:21]
un=(protection+markup)
un_diff=unemployment-un
difference=inflation-inflation_previous_period

summary(lm(inflation~inflation_previous_period+un_diff))
e=ggplot(data=NULL,aes(x=un_diff,y=difference))
e+geom_point()
e+geom_point()+geom_smooth(method=lm, se=FALSE)
