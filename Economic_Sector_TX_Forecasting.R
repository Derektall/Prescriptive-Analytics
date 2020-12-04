library(pillar)
library(data.table)
library(tidyverse)
library(ggplot2)
library(readxl)
library(tseries)
library(forecast)
library(forecastML)
library(smooth)
library(Mcomp)
library(zoo)
library(TTR)

Manufacturing <- read_excel("alldata_sa.xls")
Retail <- read_excel("TROS_alldata_SA.xls")
Service <- read_excel("TSSOS_alldata_SA.xls")
ManuGDP <- read_excel("C:\\Users\\Zoidb\\Downloads\\Manufacture_quarterly.xlsx")
RetailGDP <- read_excel("C:\\Users\\Zoidb\\Downloads\\Retail_quarterly.xlsx")
ServiceGDP <- read_excel("C:\\Users\\Zoidb\\Downloads\\Service_quarterly.xlsx")

Manufacturemonth <- ts(Manufacturing,start=c(2004,1),end=c(2020,7),frequency=12)
Manufacturemonth2 <- window(Manufacturemonth,start=c(2004,1),end=c(2020,7),frequency=12)
ManufactureQ <- aggregate(Manufacturemonth2,nfrequency=4,mean)

Retailmonth <- ts(Retail,start=c(2007,1),end=c(2020,7),frequency=12)
Retailmonth2 <- window(Retailmonth,start=c(2007,1),end=c(2020,7),frequency=12)
RetailQ <- aggregate(Retailmonth2,nfrequency=4,mean)

Servicemonth <- ts(Service,start=c(2007,1),end=c(2020,7),frequency=12)
Servicemonth2 <- window(Servicemonth,start=c(2007,1),end=c(2020,7),frequency=12)
ServiceQ <- aggregate(Servicemonth2,nfrequency=4,mean)

Manufactureoutlook <- (Manufacturemonth[,116])
Retailoutlook <- (Retailmonth[,92])
Serviceoutlook <- (Servicemonth[,68])

Manufacturebac <- (Manufacturemonth[,124])
Retailbac <- (Retailmonth[,100])
Servicebac <- (Servicemonth[,76])

Manufacturerev <- (Manufacturemonth[,76])
Retailrev <- (Retailmonth[,52])
Servicerev <- (Servicemonth[,4])

## Performing Dickey Fuller Tests to see if the time series are stationary or not


adf.test(Manufactureoutlook, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Manufacturebac, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Manufacturerev, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Retailoutlook, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Retailbac, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Retailrev, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Serviceoutlook, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Servicebac, alternative = c("stationary", "explosive"),
         k = 0)

adf.test(Servicerev, alternative = c("stationary", "explosive"),
         k = 0)

## All are stationary except for Manufacture rev, so we'll first difference that time series
## and do Holt Winters forecasting models on the rest. This is because HoltWinters model
## gives higher weight to recent observations which makes more sense since COVID is
## going to still have an effect in the near future.

Manrevdif <- diff(Manufacturerev,differences=1)

##First I fit the time series using HoltWinters 

Manbachw <- HoltWinters(Manufacturebac)
Manouthw <- HoltWinters(Manufactureoutlook)
Manrevhw <- HoltWinters(Manufacturerev)

Retailbachw <- HoltWinters(Retailbac)
Retailrevhw <- HoltWinters(Retailrev)
Retailouthw <- HoltWinters(Retailoutlook)

Servicebachw <- HoltWinters(Servicebac)
Servicerevhw <- HoltWinters(Servicerev)
Serviceoutlookhw <- HoltWinters(Serviceoutlook)

Manbacfore <- predict(Manbachw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Manbachw, Manbacfore)

Manrevfore <- predict(Manrevhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Manrevhw, Manrevfore)

Manoutfore <- predict(Manouthw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Manouthw, Manoutfore)

Mangdpfore <- predict(Mangdphw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Manouthw, Manoutfore)

Servbacfore <- predict(Servicebachw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Servicebachw, Servbacfore)

Servrevfore <- predict(Servicerevhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Servicerevhw, Servrevfore)

Servoutfore <- predict(Serviceoutlookhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Serviceoutlookhw, Servoutfore)

Retailbacfore <- predict(Retailbachw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Retailbachw, Retailbacfore)

Retailrevfore <- predict(Retailrevhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Retailrevhw, Retailrevfore)

Retailoutfore <- predict(Retailouthw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(Retailouthw, Retailoutfore)

## I then do all the same steps above but this time with the amount of dollars (in millions)
## each industry contributes to the Texas GDP

ManufactureGDP <- ts(ManuGDP,start=c(2005,1),end=c(2020,7),frequency=4)
RetailGDP <- ts(RetailGDP,start=c(2005,1),end=c(2020,7),frequency=4)
ServiceGDP <- ts(ServiceGDP,start=c(2005,1),end=c(2020,7),frequency=4)

ManufactureGDPuv <- ManufactureGDP[,51]
RetailGDPuv <- RetailGDP[,51]
ServiceGDPuv <- ServiceGDP[,51]


adf.test(ManufactureGDPuv, alternative = c("stationary", "explosive"),
         k = 0)
adf.test(RetailGDPuv, alternative = c("stationary", "explosive"),
         k = 0)
adf.test(ServiceGDPuv, alternative = c("stationary", "explosive"),
         k = 0)

ManGDPdif <- diff(ManufactureGDPuv,differences=1)
RetailGDPdif <- diff(RetailGDPuv,differences=1)
ServiceGDPdif <- diff(ServiceGDPuv,differences=1)

adf.test(ManGDPdif, alternative = c("stationary", "explosive"),
         k = 0)
adf.test(RetailGDPdif, alternative = c("stationary", "explosive"),
         k = 0)
adf.test(ServiceGDPdif, alternative = c("stationary", "explosive"),
         k = 0)

ManGDPhw <- HoltWinters(ManufactureGDPuv)
RetailGDPhw <- HoltWinters(RetailGDPuv)
ServiceGDPhw <- HoltWinters(ServiceGDPuv)

ManGDPfore <- predict(ManGDPhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(ManGDPhw, ManGDPfore)

RetailGDPfore <- predict(RetailGDPhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(RetailGDPhw, RetailGDPfore)

ServiceGDPfore <- predict(ServiceGDPhw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(ServiceGDPhw, ServiceGDPfore)

##Although I suspect HoltWinters is the best way to forecast, I will build other forecasting
##models to evaluate

##Exponential smoothing forecasts with additive errors


ManbacANN <- (ets(Manufacturebac,model="ANN"))
autoplot(forecast(ets(Manufacturebac,model="ANN")))

ManoutANN <- (ets(Manufactureoutlook,model="ANN"))
autoplot(forecast(ets(Manufactureoutlook,model="ANN")))

ManrevANN <- (ets(Manufacturerev,model="ANN"))
autoplot(forecast(ets(Manufacturerev,model="ANN")))

MangdpANN <- (ets(RetailGDPuv,model="ANN"))
autoplot(forecast(ets(RetailGDPuv,model="ANN")))

RetailbacANN <- (ets(Retailbac,model="ANN"))
autoplot(forecast(ets(Retailbac,model="ANN")))

RetailrevANN <- (ets(Retailrev,model="ANN"))
autoplot(forecast(ets(Retailrev,model="ANN")))

RetailoutANN <- (ets(Retailoutlook,model="ANN"))
autoplot(forecast(ets(Retailoutlook,model="ANN")))

RetailgdpANN <- (ets(RetailGDPuv,model="ANN"))
autoplot(forecast(ets(RetailGDPuv,model="ANN")))

ServicebacANN <- (ets(Servicebac,model="ANN"))
autoplot(forecast(ets(Servicebac,model="ANN")))

ServiceoutANN <- (ets(Serviceoutlook,model="ANN"))
autoplot(forecast(ets(Serviceoutlook,model="ANN")))

ServicerevANN <- (ets(Servicerev,model="ANN"))
autoplot(forecast(ets(Servicerev,model="ANN")))

ServicegdpANN <- (ets(ServiceGDPuv,model="ANN"))
autoplot(forecast(ets(ServiceGDPuv,model="ANN")))

##Gives same forecast value because it doesn't take trend or seasonality into account

##I will then try to use both additive errors and additive trend

ManbacAAN <- (ets(Manufacturebac,model="AAN"))
autoplot(forecast(ets(Manufacturebac,model="AAN")))

ManoutAAN <- (ets(Manufactureoutlook,model="AAN"))
autoplot(forecast(ets(Manufactureoutlook,model="AAN")))

ManrevAAN <- (ets(Manufacturerev,model="AAN"))
autoplot(forecast(ets(Manufacturerev,model="AAN")))

MangdpAAN <- (ets(RetailGDPuv,model="AAN"))
autoplot(forecast(ets(RetailGDPuv,model="AAN")))

RetailbacAAN <- (ets(Retailbac,model="AAN"))
autoplot(forecast(ets(Retailbac,model="AAN")))

RetailoutAAN <- (ets(Retailoutlook,model="AAN"))
autoplot(forecast(ets(Retailoutlook,model="AAN")))

RetailgdpAAN <- (ets(RetailGDPuv,model="AAN"))
autoplot(forecast(ets(RetailGDPuv,model="AAN")))

RetailrevAAN <- (ets(Retailrev,model="AAN"))
autoplot(forecast(ets(Retailrev,model="AAN")))

ServicebacAAN <- (ets(Servicebac,model="AAN"))
autoplot(forecast(ets(Servicebac,model="AAN")))

ServiceoutAAN <- (ets(Serviceoutlook,model="AAN"))
autoplot(forecast(ets(Serviceoutlook,model="AAN")))

ServicegdpAAN <- (ets(ServiceGDPuv,model="AAN"))
autoplot(forecast(ets(ServiceGDPuv,model="AAN")))

ServicerevAAN <- (ets(Servicerev,model="AAN"))
autoplot(forecast(ets(Servicerev,model="AAN")))

##Next I will try using auto.arima to find the best ARIMA forecasts 

Manoutarima <- auto.arima(Manufactureoutlook)
autoplot(forecast(auto.arima(Manufactureoutlook)))

Manbacarima <- auto.arima(Manufacturebac)
autoplot(forecast(auto.arima(Manufacturebac)))

Manrevarima <- auto.arima(Manufacturerev)
autoplot(forecast(auto.arima(Manufacturerev)))

Mangdparima <- auto.arima(ManufactureGDPuv)
autoplot(forecast(auto.arima(ManufactureGDPuv)))

Retailbacarima <- auto.arima(Retailbac)
autoplot(forecast(auto.arima(Retailbac)))

Retailrevarima <- auto.arima(Retailrev)
autoplot(forecast(auto.arima(Retailrev)))

Retailgdparima <- auto.arima(RetailGDPuv)
autoplot(forecast(auto.arima(RetailGDPuv)))

Retailoutarima <- auto.arima(Retailoutlook)
autoplot(forecast(auto.arima(Retailoutlook)))

Servicebacarima <- auto.arima(Servicebac)
autoplot(forecast(auto.arima(Servicebac)))

Servicerevarima <- auto.arima(Servicerev)
autoplot(forecast(auto.arima(Servicerev)))

Servicegdparima <- auto.arima(ServiceGDPuv)
autoplot(forecast(auto.arima(ServiceGDPuv)))

##Moving average models

Manoutsma <- SMA(Manufactureoutlook,n=3)

Manbacsma <- SMA(Manufacturebac,n=3)

Manrevsma <- SMA(Manufacturerev,n=3)

Mangdpsma <- SMA(ManufactureGDPuv,n=3)

Retailoutsma <- SMA(Retailoutlook,n=3)

Retailbacsma <- SMA(Retailbac,n=3)

Retailrevsma <- SMA(Retailrev,n=3)

Retailgdpsma <- SMA(RetailGDPuv,n=3)

Serviceoutsma <- SMA(Serviceoutlook,n=3)

Servicebacsma <- SMA(Servicebac,n=3)

Servicerevsma <- SMA(Servicerev,n=3)

Servicegdpsma <- SMA(ServiceGDPuv,n=3)


## Exponentiatial Weighted Moving Average models (giving more weight to)

Manoutema <- EMA(Manufactureoutlook,n=3)

Manbacema <- EMA(Manufacturebac,n=3)

Manrevema <- EMA(Manufacturerev,n=3)

Mangdpema <- EMA(ManufactureGDPuv,n=3)

Retailoutema <- EMA(Retailoutlook,n=3)

Retailbacema <- EMA(Retailbac,n=3)

Retailrevema <- EMA(Retailrev,n=3)

Retailgdpema <- EMA(RetailGDPuv,n=3)

Serviceoutema <- EMA(Serviceoutlook,n=3)

Servicebacema <- EMA(Servicebac,n=3)

Servicerevema <- EMA(Servicerev,n=3)

Servicegdpema <- EMA(ServiceGDPuv,n=3)



##Now that I have all the models, I will compare the RMSE of each. I chose this over
## MAPE because the farther off predictions will result in a stronger penalty in the RMSE

accuracy(forecast(Manouthw))
accuracy(ManoutANN)
accuracy(ManoutAAN)
accuracy(Manoutarima)
accuracy(forecast(Manoutsma))
accuracy(forecast(Manoutema))
## The smoothing average works best
accuracy(forecast(Manrevhw))
accuracy(ManrevANN)
accuracy(ManrevAAN)
accuracy(Manrevarima)
accuracy(forecast(Manrevsma))
accuracy(forecast(Manrevema))
## SMA
accuracy(forecast(Manbachw))
accuracy(ManbacANN)
accuracy(ManbacAAN)
accuracy(Manbacarima)
accuracy(forecast(Manbacsma))
accuracy(forecast(Manbacema))
## SMA
accuracy(forecast(ManGDPhw))
accuracy(MangdpANN)
accuracy(MangdpAAN)
accuracy(Mangdparima)
accuracy(forecast(Mangdpsma))
accuracy(forecast(Mangdpema))
## SMA
accuracy(forecast(Retailouthw))
accuracy(RetailoutANN)
accuracy(RetailoutAAN)
accuracy(Retailoutarima)
accuracy(forecast(Retailoutsma))
accuracy(forecast(Retailoutema))
##SMA
accuracy(forecast(Retailrevhw))
accuracy(RetailrevANN)
accuracy(RetailrevAAN)
accuracy(Retailrevarima)
accuracy(forecast(Retailrevsma))
accuracy(forecast(Retailrevema))
##SMA
accuracy(forecast(Retailbachw))
accuracy(RetailbacANN)
accuracy(RetailbacAAN)
accuracy(Retailbacarima)
accuracy(forecast(Retailbacsma))
accuracy(forecast(Retailbacema))
##SMA
accuracy(forecast(Retailgdphw))
accuracy(RetailgdpANN)
accuracy(RetailgdpAAN)
accuracy(Retailgdparima)
accuracy(forecast(Retailgdpsma))
accuracy(forecast(Retailgdpema))
##SMA 
accuracy(forecast(Servicebachw))
accuracy(ServicebacANN)
accuracy(ServicebacAAN)
accuracy(Servicebacarima)
accuracy(forecast(Servicebacsma))
accuracy(forecast(Servicebacema))
##SMA
accuracy(forecast(Servicerevhw))
accuracy(ServicerevANN)
accuracy(ServicerevAAN)
accuracy(Servicerevarima)
accuracy(forecast(Servicerevsma))
accuracy(forecast(Servicerevema))
##SMA
accuracy(forecast(Serviceouthw))
accuracy(ServiceoutANN)
accuracy(ServiceoutAAN)
accuracy(Serviceoutarima)
accuracy(forecast(Serviceoutsma))
accuracy(forecast(Serviceoutema))
##SMA























