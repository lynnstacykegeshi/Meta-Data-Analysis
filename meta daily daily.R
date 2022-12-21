library(dplyr)
library(lubridate)
library(xts)
library(tseries)
library(forecast)
library(seastests)
library(FinTS)
library(tidyverse)
library(ggplot2)

metad<-read.csv("META.csv", header = TRUE,  sep = ",")


metad$New_date=ymd(metad$Date)
str(metad)
metad

closingtsd<- msts(metad$Close,start=c(2012,5), end=c(2022,5), seasonal.periods=c(24*2, 24*2*5))         
plot.ts(closingtsd)
str(closingtsd)
head(closingtsd)
#Decomposing the data
closingtsd_dec<-decompose(closingtsd, "multiplicative")
closingtsd_dec
plot(closingtsd_dec)

#Plot the original data ACF and PACF
ggtsdisplay(closingtsd)

#???is the data seasonal
isSeasonal(closingtsd, test="wo")

adf.test(closingtsd)
#pvalue>0.05: 0.07992>0.05, Fail to reject the null hypothesis and conclude that the time series is non stationary

kpss.test(closingtsd)
#p-value is 0.01. 0.01<0.05, Reject null hypothesis and conclude that the time series is non stationary.
ndiffs(closingtsd)
#####################
#Data has a strong trend
#Take the first difference of the data to remove the trend
dfclosingd<-diff(closingtsd)
autoplot(dfclosingd, main="Change in closing price per month",xlab="Time in years", ylab="Closing price", col="blue")
ggtsdisplay(dfclosingd)
#This is not the best strategy as we have not log transformed the time series
##Ignore
######################

#Now test if TS is stationary
ndiffs(dfclosingd)
adf.test(dfclosingd)
kpss.test(dfclosing)
#0.1>0.05 fail to reject null hypothesis and conclude that the time series is stationary


#data is not stationary under variance so we get the log transform 

closingts_lineard<-log(closingtsd)

ndiffs(closingts_linear)
plot.ts(closingts_linear, main="Closing Stock Prices(log)", ylab="Closing Price", col=4)

#We then get the first difference to remove the trend
diffclosed<-diff(closingts_lineard)
isSeasonal(diffclose)
adf.test(diffclose)
ndiffs(diffclose)

#######
#Conducting normal test on log transformed data shows the the resulting time series is not normallly distributed???
hist(closingtsd)
hist(diffclosed)
qqnorm(closingts)
qqline(closingts)
qqnorm(diffclose)
qqline(diffclose)
shapiro.test(diffclose)
############

#ARIMA model
ARIMAfitmetad <- auto.arima(diffclosed, trace=TRUE,
                           ic= c("bic"), #c("aicc","aic","bic"),
)
ARIMAfitmetad


diffclosed_ar<- arima(diffclosed, order=c(1,0,0))
diffclosed_ar
AIC(diffclosed_ar)#Extract AIC values  

#Check for residuals
diffclosed_residuals<-residuals(diffclosed_ar)
diffclosed_residuals
acf(diffclosed_residuals, main="Correlation of residuals ")
#Residuals are independent since there are no lags.

plot.ts(diffclosed_residuals)

hist(diffclosed_ar$residuals)
#Histogram is bell shaped- normally distributed. The residuals are thus not correlated.
isSeasonal(diffclose)
#Ljung box test

ljung_resd <- Box.test(diffclosed_ar$residuals, lag=1, type="Ljung-Box")
ljung_resd

#0.7777>0.05. Thus, we fail to reject the null hypothesis of the test and conclude that the data values are independent.The model is a good fit for the data

#####
#Estimate the model
#Forecasting using original time series

meta_forecast<-forecast(diffclose_ar,level = c(95), h=10*12)
meta_forecast
ts.plot(meta_forecast)
metattttt<-predict(diffclose_ar, n.ahead = 120)
metattttt
diffclose_ar

ARIMAfit_m <- auto.arima(closingts, trace=TRUE)

myforecastm <-forecast(ARIMAfit_m, h=10*12)
myforecastm
dev.new()


ARIMAfit_closedd <- auto.arima(closingtsd, trace=TRUE)

myforecastdd <-forecast(ARIMAfit_closedd, h=3650)
myforecastdd
dev.new()
plot(myforecastdd)
plot(myforecastm)

#predict(object = closingtsd,n.ahead = 365)
