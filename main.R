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

## helloworld --------------------------------------
#a = 2
#aprime = plusOne(a)
#tDate = as.Date(x = tDate, format = "%m/%d/%Y")

###############

## main ---------------------------------------
#class(hof2005)
#acf(atr['2003-07-02/'][,"tr"])
# hof2005 -----------------------------------------------------------------
hof2005 = Quandl(code = "CME/HOF2005", type = "xts")
hof2005.atr = CalculateATR(hof2005)
hof2005.tr.arima.period = CalculateArimaPeriod(hof2005.atr)
hof2005.tr.arima = ArimaTR(hof2005.atr$tr, hof2005.tr.arima.period)
hof2005.tr.arima
Box.test(hof2005.tr.arima$resid,lag=9,fitdf=8)
# hof2005 -----------------------------------------------------------------


# hok2002 -----------------------------------------------------------------


hok2002 = Quandl(code = "ICE/CCH2000", type = "xts")
hok2002 = hok2002[!is.na(hok2002$Open)]
hok2002.atr = CalculateATR(hok2002)
hok2002.tr.arima.period = CalculateArimaPeriod(hok2002.atr)
# hok2002[index(hok2002)[hok2002.atr.arima.period+1]] # access data at hok2002.atr.arima.period
# hok2002.atr[is.na(hok2002.atr$atr)] # the first 20 atr which are NA
hok2002.tr.arima = ArimaTR(hok2002.atr$tr, hok2002.tr.arima.period)
hok2002.tr.arima

Box.test(hok2002.atr.arima$resid,lag=3,fitdf=2)
# hok2002 -----------------------------------------------------------------


Run <- function (code, c.size=NA){
    if (!is.na(c.size)) {
        CONTRACT.SIZE <<- c.size
    }
    
    # initialize position ---------------------------------------------------
    pos = NewPosition(code)
    #pos$underlying = underlying
    pos = InitPosition(pos)
    #---------------------------------------------------
    
    n = nrow(pos$underlying)
    prd.pos = pos
    
    prd.start.date = CalculateArimaPeriod(prd.pos$atr) + 2
    

    for (i in (BREAKOUT.PERIOD + 1) : n ){
        date = DateFromIndex(pos$underlying, i)
        
#         if (date == "2000-03-16") {
#             browser()
#         }
        
        #on Monday, update N
        if (weekdays(date) == UPDATE.DAY) {
            pos = UpdateN(pos, date)
        }
        pos = TradeStrategy(pos, date)
 
        # predicted position
        # start trading only after ArimaPeriod
        #-----------------------------------------------------
        if (i >= prd.start.date) {
#             prd.pos = PredictN(prd.pos, date)
#             prd.pos = TradeStrategy(prd.pos, date)
        }
        
        #--------------------------------------------------
    }

# browser()
    return (list(
                pnl = pos$pnl,
                pnl.trace = pos$pnl.trace
        ))
}

underlying = Quandl(code = "CME/HOK2002", type = "xts")

#default periods
ho02 = Run("CME/HOK2002", 42000)
cocoa00 = Run("ICE/CCH2000", 10)

#shorter periods
ATR.DAYS <<- 10
BREAKOUT.PERIOD <<- 10
EXIT.PERIOD <<- 5

ho02.s = Run("CME/HOK2002", 42000)
cocoa00.s = Run("ICE/CCH2000", 10)

ho02$pnl
cocoa00$pnl
ho02.s$pnl
cocoa00.s$pnl


par(mfrow=c(2,1))
plot(ho02$pnl.trace[ho02$pnl.trace != 0])
plot(cocoa00$pnl.trace[cocoa00$pnl.trace != 0])
plot(cocoa00.s$pnl.trace[cocoa00.s$pnl.trace != 0])















