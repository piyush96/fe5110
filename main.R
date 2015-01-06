# Main program

## load library
library(TTR)
#library(quantmod)
library(Quandl)
Quandl.auth("SWnAfbKWidzuzbtDBG5_") # users'd better run their own Quandl account
library(forecast)
source("helper.R")
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
hof2005.tr.arima.period = CalculateArimaPeriod(hof2005.atr)
hof2005.tr.arima = ArimaTR(hof2005.atr$tr, hof2005.tr.arima.period)
hof2005.tr.arima
Box.test(hof2005.tr.arima$resid,lag=9,fitdf=8)


hok2002 = Quandl(code = "CME/HOK2002", type = "xts")
hok2002.atr = CalculateATR(hok2002)
hok2002.tr.arima.period = CalculateArimaPeriod(hok2002.atr)
# hok2002[index(hok2002)[hok2002.atr.arima.period+1]] # access data at hok2002.atr.arima.period
# hok2002.atr[is.na(hok2002.atr$atr)] # the first 20 atr which are NA
hok2002.tr.arima = ArimaTR(hok2002.atr$tr, hok2002.tr.arima.period)
hok2002.tr.arima

Box.test(hok2002.atr.arima$resid,lag=3,fitdf=2)


# position members
# is.long: TRUE means long pos, FALSE means short pos
# size: position size
# capital: the total amount of money
# N: to be updated every Monday
# entry: a vector of 4 indicating the entry/adding prices
# is.loaded: whether take all 4 units
# underlying: the prices of the future contract
# atr: average true range

# initialize a position on a future contract
pos = list(
        is.long     = NA,
        size        = 0,
        capital     = 0,
        N           = NA,
        entry.price       = c(first = NA, second = NA, third = NA, fourth = NA),
        is.loaded   = FALSE,
        underlying  = NA,
        atr         = NA
    )



