# import libraries and dataset
library(readr)
library(xts)
monthly_factors=read_csv("F-F_Research_Data_5_Factors_2x3.CSV")
#clean and convert the data into zoo obj
colnames(monthly_factors)=monthly_factors[3,1:8]
monthly_factors=monthly_factors[4:680,2:7]
monthly_factors=as.data.frame(monthly_factors)
monthly_factors=sapply(monthly_factors,FUN = as.numeric)
monthly_factors=monthly_factors/100
monthly_factors=ts(monthly_factors,frequency = 12,start = c(1963,7))
monthly_factors=as.zoo(monthly_factors)

#assign values to rmw and plot the series
rmw=monthly_factors$RMW
plot.zoo(rmw)

#compute the annualized arithmatic mean standard deviation 
annulized_mean=12*mean(rmw)
annulized_std=12^0.5*sd(rmw)
rmw_performance=list('annulized_mean'=annulized_mean,
                     'annulized_std'=annulized_std)
rmw_performance
# plot the 60-th order autocorrlation 
sixty_order_acf=acf(rmw,lag.max = 60)
# compute the cumsum of the autocorrelation
cum_autocor=cumsum(sixty_order_acf$acf)
plot(cum_autocor,type = 'line',main = 'cumulative autocorrelation of rmw')


#perform box-lijung test 

box_ljing=Box.test(x=rmw,lag = 6,type = 'Ljung-Box')
box_ljing
#extract the p-value
box_ljing$p.value

# we first hypothesized a AR(1) model 
AR_1=lm(formula = rmw~lag(rmw))
summary(AR_1)
#plot the acf of the residuals 
acf(AR_1$residuals)
