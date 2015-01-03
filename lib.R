## Constants
RANGE = 250 # number of days in True Range time series to construct ARiMA
RANGE_RATO = 2/3 # portion of whole duration to construct ARiMA
ATR_DAYS = 20 # number of days to compute ATR

## Library
CalculateATR <- function(p.code.data){ # p.code.data is a xts returned from Quandl
    #p.code.data = Quandl(code = _codeName, type = "xts")
    #class(codeData)
    colnames(p.code.data)[6] = "Close"
    p.atr = ATR(p.code.data[,c("High","Low","Close")], n=ATR_DAYS)
    #class(atr)
    #codeData[,"Close"]
    return (p.atr)
}

CalculateArimaPeriod <- function(p.atr){
    p.period = floor(nrow(p.atr)*RANGE_RATO) #range should be long enough, it is now 2/3 of the entire duration
    return (p.period)
}

ArimaTR <- function(p.atr, p.period){ #p.atr is the return of CalculateATR
    p.range = paste(p.period, "days", sep = " ") 
    p.tr = as.vector(first(p.atr[!is.na(p.atr$tr)], p.range)$tr) # exclude the first day which has NA tr-value
    p.autoTR = auto.arima(p.tr, max.p = 20, max.q = 0, ic = "aic")
    return (p.autoTR)
}