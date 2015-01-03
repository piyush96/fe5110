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
atr_hof2005 = CalculateATR(hof2005)
autoTrHof2005 = ArimaTR(atr_hof2005)
autoTrHof2005
Box.test(autoTrHof2005$resid,lag=9,fitdf=8)


hok2002 = Quandl(code = "CME/HOK2002", type = "xts")
atr_hok2002 = CalculateATR(hok2002)
autoTrHok2002 = ArimaTR(atr_hok2002)
autoTrHok2002

Box.test(autoTrHok2002$resid,lag=3,fitdf=2)
