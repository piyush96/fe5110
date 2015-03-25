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
        
        
        #on Monday, update N
        if (weekdays(date) == UPDATE.DAY) {
            pos = UpdateN(pos, date)
        }
        pos = TradeStrategy(pos, date)
 

    }

# browser()
    return (list(
                pnl = pos$pnl,
                pnl.trace = pos$pnl.trace,
                underlying = pos$underlying
        ))
}

underlying = Quandl(code = "CME/HOK2002", type = "xts")

#default periods
ho02 = Run("CME/HOK2002", 42000) # heating oil
cocoa00 = Run("ICE/CCH2000", 10) # cocoa
hg02 = Run("CME/HGG2002", 25000) # copper
si00 = Run("CME/SIH2000", 5000) # silver

#shorter periods
ATR.DAYS <<- 10
BREAKOUT.PERIOD <<- 10
EXIT.PERIOD <<- 5

ho02.s = Run("CME/HOK2002", 42000)
cocoa00.s = Run("ICE/CCH2000", 10)
hg02.s = Run("CME/HGG2002", 25000)
si00.s = Run("CME/SIH2000", 5000) # silver

ho02$pnl
cocoa00$pnl
hg02$pnl
si00$pnl
ho02.s$pnl
cocoa00.s$pnl
hg02.s$pnl
si00.s$pnl

# par(mfrow=c(2,1))
# plot(ho02$pnl.trace[ho02$pnl.trace != 0])
# plot(cocoa00$pnl.trace[cocoa00$pnl.trace != 0])
# plot(cocoa00.s$pnl.trace[cocoa00.s$pnl.trace != 0])


WritePnl2Csv(ho02)
WritePnl2Csv(cocoa00)
WritePnl2Csv(hg02)
WritePnl2Csv(si00)

WritePnl2Csv(ho02.s)
WritePnl2Csv(cocoa00.s)
WritePnl2Csv(hg02.s)
WritePnl2Csv(si00.s)

# for each pair of variables, e.g. ho02 and ho02.s, make a xlsx file which contains 
# the pnl in ho02.pnl.csv and ho02.s.pnl.csv
# and the open price in ho02.underlying.csv. 
# The xlsx file is used to produced all graphs for the report.









