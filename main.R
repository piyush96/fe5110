# Main program

## load library
library(TTR)
#library(quantmod)
library(Quandl)
Quandl.auth("SWnAfbKWidzuzbtDBG5_") # users'd better run their own Quandl account
library(forecast)
source("lib.R")
###############

## helloworld
#a = 2
#aprime = plusOne(a)
#tDate = as.Date(x = tDate, format = "%m/%d/%Y")

###############

## main
hof2005 = Quandl(code = "CME/HOF2005", type = "xts")
#class(hof2005)
#acf(atr['2003-07-02/'][,"tr"])
hof2005.atr = CalculateATR(hof2005)
hof2005.atr.arima.period = CalculateArimaPeriod(hof2005.atr)
hof2005.atr.arima = ArimaTR(hof2005.atr, hof2005.atr.arima.period)
hof2005.atr.arima
Box.test(hof2005.atr.arima$resid,lag=9,fitdf=8)


hok2002 = Quandl(code = "CME/HOK2002", type = "xts")
hok2002.atr = CalculateATR(hok2002)
hok2002.atr.arima.period = CalculateArimaPeriod(hok2002.atr)
hok2002.atr.arima = ArimaTR(hok2002.atr, hok2002.atr.arima.period)
hok2002.atr.arima

Box.test(hok2002.atr.arima$resid,lag=3,fitdf=2)
