# import libraries and dataset
library(readr)
library(xts)
library(Hmisc)
library(DataAnalytics)
monthly_factors=read_csv("F-F_Research_Data_5_Factors_2x3.CSV")
#clean and convert the data into zoo obj
colnames(monthly_factors)=monthly_factors[3,1:8]
monthly_factors=monthly_factors[4:680,2:7]
monthly_factors=as.data.frame(monthly_factors)
monthly_factors=sapply(monthly_factors,FUN = as.numeric)
#monthly_factors=monthly_factors/100
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
lag_rmv=back(as.vector(rmw))
lag_rmv[1]=as.numeric(rmw[1])
AR_1=lm(formula = as.vector(rmw)~lag_rmv)
summary(AR_1)
#plot the acf of the residuals 
acf(AR_1$residuals)


##compute the OLS and robust standard errors
#build the design matrix X

X=matrix(data = 1,nrow = length(lag_rmv),ncol = 2)
X[,2]=lag_rmv
#compute the OLS variance
sigma_sq=var(AR_1$residuals)
Sigma_OLS=solve(t(X)%*%X)*sigma_sq
Var_OLS=diag(Sigma_OLS)
Var_OLS
# compute the robust variance 
lambda_hat=diag(AR_1$residuals^2) # create lambdat-hat
Sigma_white=solve(t(X)%*%X)%*%t(X)%*%lambda_hat%*%X%*%solve(t(X)%*%X)
Var_white=diag(Sigma_white)
Var_white


#problem 2.1
## configurate the simulation settings 
mu=0.005
sigma=0.04
T=600
N=10000
betas=rep(0,N)
for (t in 1:N){
  r1=mu+sigma*rnorm(600)
  r2=mu+sigma*rnorm(600)
  model=lm(formula = r1~r2)
  betas[t]=model$coefficients[2]
}
mean(betas)
sd(betas)
hist(betas)

#Q2
mu=0.005
sigma=0.04
T=600
N=10000
betas=rep(0,N)
for (t in 1:N){
  r1=mu+sigma*rnorm(600)
  r2=mu+sigma*rnorm(600)
  p1=cumsum(r1)
  p2=cumsum(r2)
  model=lm(formula = p1~p2)
  betas[t]=model$coefficients[2]
}
mean(betas)
sd(betas)
hist(betas)

(1/(600-1))^0.5
