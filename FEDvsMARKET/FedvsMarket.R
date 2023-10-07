#BY HARSH RAJ
library(vars)
library(tseries)
library(tidyverse)
library(forecast)
library(astsa)
#SELECTING THE DATA SET
DataF=read.csv(file.choose())
DataF

plot(DataF$Fed.Funds,DataF$Close)
#CONVERTING THE DATA SET INTO TIME SERIES OF FREQUENCY 12(MONTHLY)
Market=ts(DataF$Close,start(c(1992,1),frequency=12))
Market
Fed=ts(DataF$Fed.Funds,start=c(1992,1),frequency = 12)
Fed
#FINDING THE OUTLIERS
tsoutliers(Fed)
tsoutliers(Market)
#PLOTTING THE GRPAHS
plot(Fed)
plot(Market)

#DOING THE ADF TEST TO CHECK FOR STATIONARITY 
Market
adfy1<-adf.test(Market)
adfy1
adfy2<-adf.test(Fed)
adfy2
# FINDING THE STATIONARY DATA SET AND DOING THE FORECASTING ON IT 
ts_fed=diff(Fed,7)
ts_market<-diff(Market,7)
val22=auto.arima(ts_market)
val22
f<-forecast(val22)
plot(f)
val23=auto.arima(ts_fed)
f2<-auto.arima(ts_fed)
plot(f2)
### PERFORMING ACF AND PACF TEST ON THE DATA SET
acf(Market,lag.max = 50)
pacf(Market)
acf(Fed)
pacf(Fed)
## VAR MODEL SELCTION 
lag<-VARselect(DataF,lag.max=30)
lag$selection
lag# Best at lag 8
#Estimate
estim=VAR(DataF,p=8,type = "const")
summary(estim)
#PERFORMING SERIAL AND ARCH TEST ON THE DATASET
Serial1<-serial.test(estim,lags.pt = 8,type="PT.asymptotic")
Arch1<-arch.test(estim,lags.multi = 8,multivariate.only = TRUE)
Arch1
Serial1

#PREDICTION FROM THE VAR MODEL
var.pred = predict(estim,n.ahead = 12,ci=0.95)
fanchart(var.pred)
accuracy(estim,var.pred)
#Stable value - we check if are eigen value is less than one 
roots(estim,)
#All the eigen values comes less than one

#Granger Casualty Test
grangery1<-causality(estim,cause="Close")
grangery1$Granger# Having a small P value we reject the null hypothesis
grangery2<-causality(estim,cause="Fed.Funds")
grangery2$Granger
#IRFs impulse response 
irf1<-irf(estim,impulse="Close",response="Fed.Funds",n.ahead = 25,boot = TRUE,run=200,ci=0.95)
plot(irf1,ylab="FED RATE",main="FED Response to Market")

irf2<-irf(estim,impulse="Fed.Funds",response="Close",n.ahead = 25,boot = TRUE,run=200,ci=0.95)
plot(irf1,ylab="DOW-JONES CLOSING",main="MARKET Response to FED")
