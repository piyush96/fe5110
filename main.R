# Main program

## load library
library(TTR)
#library(quantmod)
library(Quandl)
Quandl.auth("SWnAfbKWidzuzbtDBG5_") # users'd better run their own Quandl account
source("lib.R")
###############

## helloworld
#a = 2
#aprime = plusOne(a)
#tDate = as.Date(x = tDate, format = "%m/%d/%Y")

###############

## main
#hof2013.data <- read.csv(file = "CME-HOF2013.csv", header = TRUE, sep = ",")
#colnames(hof2013.data)[7] = "Close"
#mhof2013 = as.matrix(hof2013.data)
#atr <- ATR(mhof2013[,c("High","Low","Close")], n=14)
hof2005 = Quandl(code = "CME/HOF2005", type = "xts")
class(hof2005)
colnames(hof2005)[6] = "Close"
atr = ATR(hof2005[,c("High","Low","Close")], n=14)
class(atr)
hof2005[,"Close"]
acf(atr['2003-07-02/'][,"tr"])

# coreTr = coredata(tr)
# plot(coreTr[1:372], coreTr[2:373])
# dCoreTr = coreTr[2:373] - coreTr[1:372]
# plot(dCoreTr[1:371], dCoreTr[2:372])

# auto.arima(diff(x),max.p=20,max.q=0,ic="aic")
# data(bmw,package="evir")
# bmw = as.vector(bmw)
# n=length(bmw)
# 
# fitAR1 = arima(bmw, order = c(1,0, 0))
# Box.test(fitAR1$resid,lag=2,fitdf=1)
coreTr = coredata(atr['2003-07-02/'][,"tr"])
acf(x = diff(coreTr))
p = 5
fitTR = arima(as.vector(diff(coreTr)), order = c(p, 0, 0))
Box.test(fitTR$resid,lag=p+1,fitdf=p)
autoTr = auto.arima(coreTr, max.p = 20, max.q = 0, ic = "aic")
autoTr
Box.test(autoTr$resid,lag=10,fitdf=9)
