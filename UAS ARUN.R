getwd()
setwd("C:/Users/USER/Documents/R/Time series")

# UAS TIME SERIES
# DATA CCTV SIMPANG WARDHANI

library(TTR)
library(forecast)

# INPUT DATA
cctv.data <- read.csv("Kelompok 6 Data CCTV Simpang Wardhani View Timur.csv", skip = 7, header = TRUE, sep = ",")
cctv.ts <- ts(cctv.data[,2])

# PLOT DATA
plot(cctv.data[,2],type="o",pch=16,cex=.5,xlab="hari",ylab="Jumlah Kendaraan")

# KESTASIONERAN DATA
cctv.SMA3 <- SMA(cctv.ts,n=3)
plot.ts(cctv.SMA3)

cctv.SMA8 <- SMA(cctv.ts,n=8)
plot.ts(cctv.SMA8)

acf(cctv.ts,main="ACF Sebelum differencing")

pacf(cctv.ts, main="PACF Sebelum differencing")
ndiffs(cctv.ts)
#DIFFERENCING (d=1)
wt.cctv <- diff(cctv.ts,differences=1)
plot.ts(wt.cctv)

# ACF PACF DIFFERENCING
par(mfrow=c(1,2))

acf(wt.cctv,lag.max=20,type="correlation",main="ACF w(t)")
pacf(wt.cctv,lag.max=20,main="PACF w(t)")

# MODEL MA(1)
model.ma1 <- arima(wt.cctv,order=c(0,0,1))
model.ma1
forecast.ma1 <- forecast(model.ma1,h=7)
acf(model.ma1$residuals,lag.max=20)
pacf(model.ma1$residuals,lag.max=20)
Box.test(model.ma1$residuals,lag=20,fitdf=1,type="Ljung-Box")
hist(model.ma1$residuals,freq=FALSE,breaks=15)
curve(dnorm(x, mean=mean(model.ma1$residuals),
            sd=sd(model.ma1$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL MA(2)
model.ma2 <- arima(wt.cctv,order=c(0,0,2))
model.ma2
forecast.ma2 <- forecast(model.ma2,h=7)
acf(model.ma2$residuals,lag.max=20)
pacf(model.ma2$residuals,lag.max=20)
Box.test(model.ma2$residuals,lag=20,fitdf=2,type="Ljung-Box")
hist(model.ma2$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.ma2$residuals),
            sd=sd(model.ma2$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL AR(1)
model.ar1 <- arima(wt.cctv,order=c(1,0,0))
model.ar1
forecast.ar1 <- forecast(model.ar1,h=7)
acf(model.ar1$residuals,lag.max=20)
pacf(model.ar1$residuals,lag.max=20)
Box.test(model.ar1$residuals,lag=20,fitdf=1,type="Ljung-Box")
hist(model.ar1$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.ar1$residuals),
            sd=sd(model.ar1$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL AR(2)
model.ar2 <- arima(wt.cctv,order=c(2,0,0))
model.ar2
forecast.ar2 <- forecast(model.ar2,h=7)
acf(model.ar2$residuals,lag.max=20)
pacf(model.ar2$residuals,lag.max=20)
Box.test(model.ar2$residuals,lag=20,fitdf=2,type="Ljung-Box")
hist(model.ar2$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.ar2$residuals),
            sd=sd(model.ar2$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARMA(1,1)
model.arma11 <- arima(wt.cctv,order=c(1,0,1))
model.arma11
forecast.arma11 <- forecast(model.arma11,h=7)
acf(model.arma11$residuals,lag.max=20)
pacf(model.arma11$residuals,lag.max=20)
Box.test(model.arma11$residuals,lag=20,fitdf=2,type="Ljung-Box")
hist(model.arma11$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arma11$residuals),
            sd=sd(model.arma11$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARMA(1,2)
model.arma12 <- arima(wt.cctv,order=c(1,0,2))
model.arma12
forecast.arma12 <- forecast(model.arma12,h=7)
acf(model.arma12$residuals,lag.max=20)
pacf(model.arma12$residuals,lag.max=20)
Box.test(model.arma12$residuals,lag=20,fitdf=3,type="Ljung-Box")
hist(model.arma12$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arma12$residuals),
            sd=sd(model.arma12$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARMA(2,1)
model.arma21 <- arima(wt.cctv,order=c(2,0,1))
model.arma21
forecast.arma21 <- forecast(model.arma21,h=7)
acf(model.arma21$residuals,lag.max=20)
pacf(model.arma21$residuals,lag.max=20)
Box.test(model.arma21$residuals,lag=20,fitdf=3,type="Ljung-Box")
hist(model.arma21$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arma21$residuals),
            sd=sd(model.arma21$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARMA(2,2)
model.arma22 <- arima(wt.cctv,order=c(2,0,2))
model.arma22
forecast.arma22 <- forecast(model.arma22,h=7)
acf(model.arma22$residuals,lag.max=20)
pacf(model.arma22$residuals,lag.max=20)
Box.test(model.arma22$residuals,lag=20,fitdf=4,type="Ljung-Box")
hist(model.arma22$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arma22$residuals),
            sd=sd(model.arma22$residuals)),col=2,lty=2,lwd=2,add=TRUE)
#ARIMA MODELING 
# MODEL ARIMA(0,1,1)
model.arima011 <- arima(wt.cctv,order=c(0,1,1))
model.arima011
forecast.arima011 <- forecast(model.arima011,h=7)
acf(model.arima011$residuals,lag.max=20)
pacf(model.arima011$residuals,lag.max=20)
Box.test(model.arima011$residuals,lag=20,fitdf=1,type="Ljung-Box")
hist(model.arima011$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arima011$residuals),
            sd=sd(model.arima011$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARIMA(1,1,1)
model.arima111 <- arima(wt.cctv,order=c(1,1,1))
model.arima111
forecast.arima111 <- forecast(model.arima111,h=7)
acf(model.arima111$residuals)
pacf(model.arima111$residuals)
Box.test(model.arima111$residuals,lag=20,fitdf=2,type="Ljung-Box")
hist(model.arima111$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arima111$residuals),
            sd=sd(model.arima111$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARIMA(2,1,1)
model.arima211 <- arima(wt.cctv,order=c(2,1,1))
model.arima211
forecast.arima211 <- forecast(model.arima211,h=7)
acf(model.arima211$residuals)
pacf(model.arima211$residuals)
Box.test(model.arima211$residuals,lag=20,fitdf=3,type="Ljung-Box")
hist(model.arima211$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arima211$residuals),
            sd=sd(model.arima211$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARIMA(1,1,2)
model.arima112 <- arima(wt.cctv,order=c(1,1,2))
model.arima112
forecast.arima112 <- forecast(model.arima112,h=7)
acf(model.arima112$residuals)
pacf(model.arima112$residuals)
Box.test(model.arima112$residuals,lag=20,fitdf=3,type="Ljung-Box")
hist(model.arima212$residuals,freq=FALSE,breaks=15)
curve(dnorm(x,mean=mean(model.arima112$residuals),
            sd=sd(model.arima112$residuals)), col=2,lty=2,lwd=2,add=TRUE)

# MODEL ARIMA(2,1,2)
model.arima212 <- arima(wt.cctv,order=c(2,1,2))
model.arima212
forecast.arima212 <- forecast(model.arima212,h=7)
acf(model.arima212$residuals)
pacf(model.arima212$residuals)
Box.test(model.arima212$residuals,lag=20,fitdf=4,type="Ljung-Box")
hist(model.arima212$residuals,freq=FALSE,breaks=15)
curve(dnorm(x, mean=mean(model.arima212$residuals),
            sd=sd(model.arima212$residuals)),col=2,lty=2,lwd=2,add=TRUE)

# PERBANDINGAN AIC
AIC(model.ar1)
AIC(model.ar2)
AIC(model.ma1)
AIC(model.ma2)
AIC(model.arma11)
AIC(model.arma12)
AIC(model.arma21)
AIC(model.arma22)
AIC(model.arima011)
AIC(model.arima111)
AIC(model.arima211)
AIC(model.arima112)
AIC(model.arima212)

# MODEL TERBAIK
best.model <- model.arma21

# FITTED MODEL TERBAIK
fit.best <- as.vector(fitted(best.model))
plot(cctv.data[,2],type="p",pch=16,cex=.5,xlab="hari",ylab="Jumlah Kendaraan")
lines(fit.best)
legend("topright",c("Data Aktual","Fitted"),pch=c(16,NA),lwd=c(NA,1))

# FORECAST MODEL TERBAIK
best.forecast <- forecast(best.model,h=7)
best.forecast
plot(best.forecast)
