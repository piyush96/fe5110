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
hof2005 = Quandl(code = "CME/HOF2005", type = "xts")
#class(hof2005)
#acf(atr['2003-07-02/'][,"tr"])
# hof2005 -----------------------------------------------------------------
hof2005.atr = CalculateATR(hof2005)
hof2005.tr.arima.period = CalculateArimaPeriod(hof2005.atr)
hof2005.tr.arima = ArimaTR(hof2005.atr$tr, hof2005.tr.arima.period)
hof2005.tr.arima
Box.test(hof2005.tr.arima$resid,lag=9,fitdf=8)
# hof2005 -----------------------------------------------------------------


# hok2002 -----------------------------------------------------------------


hok2002 = Quandl(code = "CME/HOK2002", type = "xts")
hok2002.atr = CalculateATR(hok2002)
hok2002.tr.arima.period = CalculateArimaPeriod(hok2002.atr)
# hok2002[index(hok2002)[hok2002.atr.arima.period+1]] # access data at hok2002.atr.arima.period
# hok2002.atr[is.na(hok2002.atr$atr)] # the first 20 atr which are NA
hok2002.tr.arima = ArimaTR(hok2002.atr$tr, hok2002.tr.arima.period)
hok2002.tr.arima

Box.test(hok2002.atr.arima$resid,lag=3,fitdf=2)
# hok2002 -----------------------------------------------------------------


Run <- function (){
    # initialize position ---------------------------------------------------
    pos = NewPosition()
    pos$underlying = underlying
    pos = InitPosition(pos)
    #---------------------------------------------------
    
    
    for (i in (BREAKOUT.PERIOD + 1) : n ){
        date = DateFromIndex(pos$underlying, i)
        
        #on Monday, update N
        if (weekdays(date) == UPDATE.DAY) {
            pos = UpdateN(pos, date)
        }
        
        #stop loss at open price
        if (!is.na(pos$stop.price)) {
            today.price = coredata(pos$underlying$Open[date])[1]
            
            if ((pos$is.long == TRUE && today.price <= pos$stop.price)
                || (pos$is.long == FALSE && today.price >= pos$stop.price)
                ) {
                msg = paste("Day: ", i,
                            "Stop loss, exit position at price:", today.price, 
                            "; size:", pos$size, sep = " ")
                pos = ExitPosition(pos, today.price)
                cat(msg, sep = "\n")
                break 
            }
        }
        
        #Exit at open price
        if (IsExitBreakout(pos, date) == TRUE) {
            today.price = coredata(pos$underlying$Open[date])[1]
            msg = paste("Day: ", i,
                        "Exits, exit position at price:", today.price, 
                        "; size:", pos$size, sep = " ")
            pos = ExitPosition(pos, today.price)
            cat(msg, sep = "\n")
            break 
        }
        
        
        break.out = EntryBreakout(pos$underlying, date)
        
        #entering a position
        if(!is.na(break.out)
           && pos$load < MAX.LOAD) {
    #         browser()
            if (break.out == HIGH.BREAKOUT
                && (pos$load == 0 
                    || (!is.na(pos$is.long) && pos$is.long == TRUE))) {
                
                msg = paste("Day:", i,
                            "High break out on", date, 
                            "with price =", pos$underlying$Open[date], 
                            sep = " ")
                
                #going long now
                if (pos$load == 0){
                    pos$is.long = TRUE    
                }
    #             pos$load = pos$load + 1
    #             pos$size = pos$size + pos$unit.size
    #             pos$entry.price[pos$load] = pos$underlying$High[date]
                
                pos = DoTrade(pos, date)
            }
            
            if (break.out == LOW.BREAKOUT
                && (pos$load == 0 
                    || (!is.na(pos$is.long) && pos$is.long == FALSE))) {
# if (i == 36) {browser()}
                msg = paste("Day:", i, 
                            "Low break out on", date, 
                            "with price =", pos$underlying$Open[date], 
                            sep = " ")
                
                #going short now
                if (pos$load == 0){
                    pos$is.long = FALSE    
                }
    #             pos$load = pos$load + 1
    #             pos$size = pos$size - pos$unit.size
    #             pos$entry.price[pos$load] = pos$underlying$Low[date]
                pos = DoTrade(pos, date)
            }
            cat(msg, sep = "\n")
        }
        
    }

browser()
}

underlying = Quandl(code = "CME/HOK2002", type = "xts")
Run()

