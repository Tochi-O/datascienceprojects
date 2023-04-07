
install.packages("forecast")
install.packages("tseries")
install.packages("aTSA")
install.packages("fGarch")
install.packages("FinTS")
install.packages("lubridate")
install.packages("backtest")


library(tidyverse)
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(lubridate)
library(dplyr)
library(aTSA)
library(tseries)
install.packages("forecast", dependencies = TRUE)
library(forecast)

salesdata <- read.csv("train.csv")
head(salesdata)

salesdata <- na.omit(salesdata)
head(salesdata)
nrow(salesdata)
ncol(salesdata)
colnames(salesdata)
new_salesdata <- select(salesdata, -c("Row.ID","Order.ID","Ship.Date"    
,"Ship.Mode","Customer.ID","Customer.Name","Segment","Country" ,"Region","Product.ID","Category","Sub.Category" 
,"Product.Name","City","State","Postal.Code" ))

new_salesdata$Sales <- as.numeric(gsub(' ','',new_salesdata$Sales))
new_salesdata$Order.Date <- dmy(new_salesdata$Order.Date)


new_salesdata$logR<- log(lag(new_salesdata$Sales))-log(new_salesdata$Sales)

head(new_salesdata)

head(new_salesdata)

remna <- function(x){
  x[is.na(x)] <-1
  return(x)
}


maxDate <- max(new_salesdata$Order.Date)
minDate <- min(new_salesdata$Order.Date)
test_tseries <- ts(new_salesdata$logR, end=c(year(maxDate), month(maxDate)),start=c(year(minDate), month(minDate)),frequency=6)



logr <- new_salesdata
logr
plot(test_tseries)
plot(stl(test_tseries,s.window = "periodic"))

acf = acf(test_tseries, main='ACF Plot', lag.max=100)
pacf.logr = pacf(test_tseries, main='PACF Plot', lag.max=100)
test_tseries<- na.omit(test_tseries)
head(test_tseries)
#Augmented Dickey-Fuller(ADF) Test
print(adf.test(test_tseries))

m1 <- auto.arima(test_tseries, seasonal = TRUE)


summary(m1)
accuracy(forecast(m1))

checkresiduals(m1)

# P1 = ts(salesdata[,2])
# P2 = ts(salesdata[,3])
# P3 = ts(salesdata[,4])
# P4 = ts(salesdata[,5])
# P5 = ts(salesdata[,6])
# price = ts(salesdata[,7])
# temperature = ts(salesdata[,8])
# 
# 
# plot.ts(P1)
# plot.ts(P2)
# plot.ts(P3)
# plot.ts(P4)
# plot.ts(P5)
# plot.ts(price)
# plot.ts(temperature)
# 
# 
# head(salesdata)

