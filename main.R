# Main program

## load library
library(TTR)
#library(quantmod)
library(Quandl)
Quandl.auth("SWnAfbKWidzuzbtDBG5_") # users'd better run their own Quandl account
source("lib.R")
###############

## helloworld
a = 2
aprime = plusOne(a)
tDate = as.Date(x = tDate, format = "%m/%d/%Y")

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