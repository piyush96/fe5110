## Constants
ACCOUNT = 1000000
UNIT.RATIO = 0.01
CONTRACT.SIZE = 42000
UPDATE.DAY = "Monday"
MAX.LOAD = 4

RANGE = 200 # number of days in True Range time series to construct ARiMA
RANGE.RATIO = 2/3 # portion of whole duration to construct ARiMA
ATR.DAYS = 20 # number of days to compute ATR

BREAKOUT.PERIOD = 20
HIGH.BREAKOUT = "HIGH.BREAKOUT"
LOW.BREAKOUT = "LOW.BREAKOUT"

ADDING.UNIT.COEF = 1/2
STOP.LOSS.COEF = 2

## Library
CalculateATR <- function(p.code.data){ # p.code.data is a xts returned from Quandl
    #p.code.data = Quandl(code = _codeName, type = "xts")
    #class(codeData)
    colnames(p.code.data)[6] = "Close"
    p.atr = ATR(p.code.data[,c("High","Low","Close")], n=ATR.DAYS)
    #class(atr)
    #codeData[,"Close"]
    return (p.atr)
}

CalculateArimaPeriod <- function(p.atr){
    p.period = floor(nrow(p.atr)*RANGE.RATIO) #range should be long enough, it is now 2/3 of the entire duration
    #p.period = RANGE
    return (p.period)
}

ArimaTR <- function(p.atr, p.period){ #p.atr is the return of CalculateATR
    p.range = paste(p.period, "days", sep = " ") 
    p.tr = as.vector(first(p.atr[!is.na(p.atr$tr)], p.range)$tr) # exclude the first day which has NA tr-value
    p.autoTR = auto.arima(p.tr, max.p = 20, max.q = 0, ic = "aic")
    return (p.autoTR)
}

# To check whether breakout happens at a particular date
# date is a string of date YYYY-MM-dd
DoBreakout <- function(p.xts, p.date){
    p.i = IndexFromDate(p.xts, p.date)
    if (is.na(p.i)) {
        return (NA)
    }
    
    if (p.i <= BREAKOUT_PERIOD) { # not long enough
        return (NA)
    }
    
    p.high = max(p.xts$High[(p.i - BREAKOUT_PERIOD) : (p.i - 1)]) 
    p.low = min(p.xts$Low[(p.i - BREAKOUT_PERIOD) : (p.i - 1)])
    
    if (p.xts$High[p.date] > p.high) {
        return (HIGH.BREAKOUT)
    }
    
    if (p.xts$Low[p.date] < p.low) {
        return (LOW.BREAKOUT)
    }
    
    return (NA)
}

# AddUnit <- function()

UpdatePosition <- function(p.pos, p.date) {
    p.pos$N = p.pos$atr$atr[p.date]
    p.pos$unit.size = floor(coredata(p.pos$capital * UNIT.RATIO / (p.pos$N * CONTRACT.SIZE))[1])
    return (p.pos)
}