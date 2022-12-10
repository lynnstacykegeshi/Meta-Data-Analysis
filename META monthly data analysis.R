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
autoplot(closingts,main = "Closing price TS", xlab = "Time in years", ylab="Closing price" )


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


#Data has a strong trend
#Take the first difference of the data to remove the trend
dfclosing<-diff(closingts)
autoplot(dfclosing, main="Change in closing price per month",xlab="Time in years", ylab="Closing price", col="blue")
ggtsdisplay(dfclosing)

#Now test if TS is stationary
ndiffs(dfclosing)
adf.test(dfclosing)
kpss.test(dfclosing)
#0.1>0.05 fail to reject null hypothesis and conclude that the time series is stationary