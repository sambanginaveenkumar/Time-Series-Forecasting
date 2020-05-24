library(forecast)
library(lubridate)
library(fpp2)
library(tseries)
library(MLmetrics)
setwd("E:/r direct/TimeSeries/Assignment")
Sales <- read.csv("Demand-1.csv",header = TRUE )
str(Sales)
dim(Sales)
summary(Sales)
Sales_ItemA <- ts(Sales[,3], start=c(2002,1),end=c(2017,7),frequency=12)
plot(Sales_ItemA)
plot(Sales_ItemA, col = "Green",  main = "Montly Sales Of Item A")
Sales_Itemb <- ts(Sales[,4], start=c(2002,1),end=c(2017,7),frequency=12)
plot(Sales_Itemb, col = "Green",  main = "Montly Sales Of Item B")

ts.plot(Sales_ItemA, Sales_Itemb, gpars = list(col = c("Skyblue", "Orange")),
        xlab="year", ylab="Sales Demand",main = "Montly Sales Of Item A & B") 
legend("topleft", colnames(Sales[3:4]), col=1:ncol(Sales), lty=1.9, cex=.45)
monthplot(Sales_ItemA,col='Green', main = "Montly Plot Of Item A")
monthplot(Sales_Itemb,col='Green', main = "Montly Plot Of Item B")
Itema_Sea<-stl(Sales_ItemA, s.window='p') 
plot(Itema_Sea,col = "Green",main = "Decompostion Of Time Series Components for Item A")
Itemb_Sea<-stl(Sales_Itemb, s.window='p') 
plot(Itemb_Sea,col = "Green",main = "Decompostion Of Time Series Components for Item B")

Deseason_ItemA <- (Itema_Sea$time.series[,2]+Itema_Sea$time.series[,3]) 
ts.plot(Sales_ItemA, Deseason_ItemA, col=c("Skyblue", "Orange"), main="ItemA Demand vs Deseasoned Demand")
Deseason_ItemB <- (Itemb_Sea$time.series[,2]+Itema_Sea$time.series[,3]) 
ts.plot(Sales_Itemb, Deseason_ItemB, col=c("Skyblue", "Orange"), main="ItemA Demand vs Deseasoned Demand")

DataATrain <- window(Sales_ItemA, start=c(2002,1), end=c(2015,10), frequency=12) 
DataATest <- window(Sales_ItemA, start=c(2015,11), frequency=12) 

DataBTrain <- window(Sales_Itemb, start=c(2002,1), end=c(2015,10), frequency=12) 
DataBTest <- window(Sales_Itemb, start=c(2015,11), frequency=12)
ItemATrn <- stl(DataATrain, s.window="p") 
ItemBTrn <- stl(DataBTrain, s.window="p")

fcst.IteA.stl <- forecast(ItemATrn, method="rwdrift", h=21) 
fcst.IteB.stl <- forecast(ItemBTrn, method="rwdrift", h=21) 

ItemA<- cbind(DataATest,fcst.IteA.stl$mean) 
ItemB<- cbind(DataBTest,fcst.IteB.stl$mean)

ts.plot(ItemA, col=c("blue", "green"),xlab="year", ylab="demand",
        main="Quarterly Demand Of Item A: Actual vs Forecast")

MAPE_ItemA <- mean(abs(ItemA[,1]-ItemA[,2])/ItemA[,1])
MAPE_ItemA

ts.plot(ItemB, col=c("blue", "green"),xlab="year", ylab="demand",
        main="Quarterly Demand Of Item B: Actual vs Forecast")

MAPE_ItemB <- mean(abs(ItemB[,1]-ItemB[,2])/ItemB[,1])
MAPE_ItemB

Box.test(fcst.IteA.stl$residuals, lag=30, type="Ljung-Box") 
Box.test(fcst.IteB.stl$residuals, lag=30, type="Ljung-Box") 

hawA <- HoltWinters(as.ts(DataATrain),seasonal="additive") 
hawA

plot(hawA,col = "Green",main = "Holt Winter Model Of Item A")

hwAForecast <- forecast(hawA, h=21) 
Item1 <- cbind(DataATest,hwAForecast) 

par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0) 

ts.plot(Item1[,1],Item1[,2], col=c("Green","orange"),xlab="year", ylab="demand", 
        main="Holt Winter Model Demand A: Actual vs Forecast")

Box.test(hwAForecast$residuals, lag=30, type="Ljung-Box")
MAPE(Item1[,1],Item1[,2])

hawB <- HoltWinters(as.ts(DataBTrain),seasonal="additive") 
hawB

plot(hawB,col = "Green",main = "Holt Winter Model Of Item B")

hwBForecast <- forecast(hawB, h=21) 
Item2 <- cbind(DataBTest,hwBForecast) 

ts.plot(Item2[,1],Item2[,2], col=c("Green","orange"),xlab="year", ylab="demand", 
        main="Holt Winter Model Demand B: Actual vs Forecast")

Box.test(hwBForecast$residuals, lag=30, type="Ljung-Box")
MAPE(Item2[,1],Item2[,2])

adf.test(Sales_ItemA)
diff_dem_ItA <- diff(Sales_ItemA) 
plot(diff_dem_ItA,col = "Green",main = "Difference Of Item A")
adf.test(diff(Sales_ItemA))

adf.test(Sales_Itemb)
diff_dem_ItB <- diff(Sales_Itemb) 
plot(diff_dem_ItB,col = "Green",main = "Difference Of Item B")
adf.test(diff(Sales_Itemb))

acf(Sales_ItemA,lag=30)
pacf(Sales_ItemA)
acf(diff_dem_ItA,lag=30)
pacf(diff_dem_ItA)

acf(Sales_ItemA,lag=50)

acf(Sales_Itemb,lag=30)
pacf(Sales_Itemb)
acf(diff_dem_ItB,lag=30)
pacf(diff_dem_ItB)
acf(diff_dem_ItB,lag=50)

ItA.arima.fit.train <- auto.arima(DataATrain, seasonal=TRUE) 
ItA.arima.fit.train

plot(ItA.arima.fit.train$residuals,col = "Green",main = "ARIMA residuals Of Item A")

plot(ItA.arima.fit.train$x,col="Skyblue")
lines(ItA.arima.fit.train$fitted,col="Orange",main="Demand A: Actual vs Forecast")

MAPE(ItA.arima.fit.train$fitted,ItA.arima.fit.train$x)
acf(ItA.arima.fit.train$residuals)
pacf(ItA.arima.fit.train$residuals)
Box.test(ItA.arima.fit.train$residuals, lag = 10, type = c("Ljung-Box"), fitdf = 0)
ArimafcastA <- forecast(ItA.arima.fit.train, h=21) 
ItemA2 <- cbind(DataATest,ArimafcastA) 

par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0) 

ts.plot(ItemA2[,1],ItemA2[,2], col=c("Green","Orange"),xlab="year",
        ylab="demand", main="Demand Item A: Actual vs Forecast")



ItB.arima.fit.train <- auto.arima(DataBTrain, seasonal=TRUE) 
ItB.arima.fit.train

plot(ItB.arima.fit.train$residuals,col = "Green",main = "ARIMA residuals Of Item B")

plot(ItB.arima.fit.train$x,col="Skyblue")
lines(ItB.arima.fit.train$fitted,col="Orange",main="Demand Of Item B: Actual vs Forecast")

MAPE(ItB.arima.fit.train$fitted,ItB.arima.fit.train$x)
acf(ItB.arima.fit.train$residuals)
pacf(ItB.arima.fit.train$residuals)
Box.test(ItB.arima.fit.train$residuals, lag = 10, type = c("Ljung-Box"), fitdf = 0)
ArimafcastB <- forecast(ItB.arima.fit.train, h=21) 
ItemB2 <- cbind(DataBTest,ArimafcastB) 

par(mfrow=c(1,1), mar=c(2, 2, 2, 2), mgp=c(3, 1, 0), las=0) 

ts.plot(ItemB2[,1],ItemB2[,2], col=c("Green","Orange"),xlab="year",
        ylab="demand", main="Demand Item B: Actual vs Forecast")

ItA.arima.fit <- auto.arima(Sales_ItemA, seasonal=TRUE) 
fcastA <- forecast(ItA.arima.fit, h=21) 

plot(fcastA,col = "Green")

ItB.arima.fit <- auto.arima(Sales_Itemb, seasonal=TRUE) 
fcastB <- forecast(ItB.arima.fit, h=21) 

plot(fcastB,col = "Green")
