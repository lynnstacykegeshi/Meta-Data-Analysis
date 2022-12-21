library(dplyr)
library(lubridate)
library(xts)
library(tseries)
library(forecast)
library(seastests)
library(FinTS)
library(tidyverse)
library(ggplot2)

#get the data
metam<-read.csv("META monthly historical prices data.csv", header = TRUE,  sep = ",")
View(metam)
str(metam)

colnames(metam)

metam$New_date=ymd(metam$Date)#changing date type to date from character
metam$monthhh=month(ymd(metam$New_date), label = TRUE)#extracting month from date
metam$monthhh

#declare data as timeseries and plot
closingts<-ts(metam$Close,start=c(2012,6),end=c(2022, 5), frequency = 12)
plot.ts(closingts,main = "Closing price TS", xlab = "Time in years", ylab="Closing price" )


###############
###Preliminary Analysis
#Boxplot the data
boxplot(Close~monthhh, 
        data=metam, 
        main="Box plot showing META closing price for each month",
        xlab="Months",
        ylab="Closing price in US Dollars",
        col="deepskyblue",
        border="darkblue")


#check how many times we need to differentiate the data in order to make it stationary
ndiffs(closingts)

#Decomposing the data
closing_dec<-decompose(closingts, "multiplicative")
closing_dec
plot(closing_dec)


#Plot the original data ACF and PACF
ggtsdisplay(closingts)

#if you notice that in the acf chart there are a few significant spikes at the beginning but the significant spikes quickly decrease to zero line in the middle at the same time in the pacf chart you only have a one or two significant spike at that moment you should use ar model

#ACF is geometric 
#PACF is significant till pth lag
#Conclusion : AR (1) model

#Seasonality in the data
isSeasonal(closingts, test="wo")
no_diff_close <- nsdiffs(closingts)#not even necessary since the isSeasonal test tells us the data has no seasonality.
no_diff_close

#There is no seasonality in the data

#Test for stationarity
# If p<5%, adf (non-stationary) and kpss test (stationary)
adf.test(closingts)
#pvalue>0.05: 0.07992>0.05, Fail to reject the null hypothesis and conclude that the time series is non stationary

kpss.test(closingts)
#p-value is 0.01. 0.01<0.05, Reject null hypothesis and conclude that the time series is non stationary.
ndiffs(closingts)
#####################
#Data has a strong trend
#Take the first difference of the data to remove the trend
dfclosing<-diff(closingts)
autoplot(dfclosing, main="Change in closing price per month",xlab="Time in years", ylab="Closing price", col="blue")
ggtsdisplay(dfclosing)
#This is not the best strategy as we have not log transformed the time series
##Ignore
######################

#Now test if TS is stationary
ndiffs(dfclosing)
adf.test(dfclosing)
kpss.test(dfclosing)
#0.1>0.05 fail to reject null hypothesis and conclude that the time series is stationary


#data is not stationary under variance so we get the log transform 

closingts_linear<-log(closingts)

ndiffs(closingts_linear)
plot.ts(closingts_linear, main="Closing Stock Prices(log)", ylab="Closing Price", col=4)

#We then get the first difference to remove the trend
diffclose<-diff(closingts_linear)
isSeasonal(diffclose)
adf.test(diffclose)
ndiffs(diffclose)

#######
#Conducting normal test on log transformed data shows the the resulting time series is not normallly distributed???
hist(closingts)
hist(diffclose)
qqnorm(closingts)
qqline(closingts)
qqnorm(diffclose)
qqline(diffclose)
shapiro.test(diffclose)
############

#ARIMA model
ARIMAfitmeta <- auto.arima(diffclose, trace=TRUE,
                       ic= c("bic"), #c("aicc","aic","bic"),
)
ARIMAfitmeta


diffclose_ar<- arima(diffclose, order=c(0,1,0))
diffclose_ar
AIC(diffclose_ar)#Extract AIC values  

#Check for residuals
diffclose_residuals<-residuals(diffclose_ar)
diffclose_residuals
acf(diffclose_residuals, main="Correlation of residuals ")
#Residuals are independent since there are no lags.

plot.ts(diffclose_residuals)

hist(diffclose_ar$residuals)
#Histogram is bell shaped- normally distributed. The residuals are thus not correlated.
isSeasonal(diffclose)
#Ljung box test

ljung_res <- Box.test(diffclose_ar$residuals, lag=2, type="Ljung-Box")
ljung_res

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
plot(myforecastm)
