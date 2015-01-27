## Constants
ACCOUNT = 1e7
UNIT.RATIO = 0.01
CONTRACT.SIZE = 42000
UPDATE.DAY = "Monday"
MAX.LOAD = 4

RANGE = 200 # number of days in True Range time series to construct ARiMA
RANGE.RATIO = 2/3 # portion of whole duration to construct ARiMA
ATR.DAYS = 20 # number of days to compute ATR

BREAKOUT.PERIOD = 20
EXIT.PERIOD = 10
HIGH.BREAKOUT = "HIGH.BREAKOUT"
LOW.BREAKOUT = "LOW.BREAKOUT"

ADDING.UNIT.COEF = 1/2
STOP.LOSS.COEF = 2

## Library
CalculateATR <- function(p.code.data){ # p.code.data is a xts returned from Quandl
    #p.code.data = Quandl(code = _codeName, type = "xts")
    #class(codeData)
    colnames(p.code.data)[colnames(p.code.data) == "Settle"] = "Close"
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

PredictN <- function(p.pos, p.date){ # for AR model only
    p.i = IndexFromDate(p.pos$atr, p.date)
    coef = p.pos$arima.tr$coef
    n = length(coef)
    if (is.na(coef["intercept"])) {
        ar = rev(as.vector(p.pos$atr$tr[(i - n + 1) : i]))
    }
    else {
        ar = c(rev(as.vector(p.pos$atr$tr[(i - n + 2) : i])) , 1)
    }
    prd.tr = coef %*% ar
    
    
    p.pos$N = (coredata(p.pos$atr$atr[p.i - 1]) * (ATR.DAYS - 1) + prd.tr) / ATR.DAYS
    p.pos$unit.size = floor(coredata(p.pos$capital * UNIT.RATIO / (p.pos$N * CONTRACT.SIZE))[1])
    return (p.pos)
}

# To check whether breakout happens at a particular date
# date is a string of date YYYY-MM-dd
EntryBreakout <- function(p.xts, p.date){
    p.i = IndexFromDate(p.xts, p.date)
    if (is.na(p.i)) {
        return (NA)
    }
    
    if (p.i <= BREAKOUT.PERIOD) { # not long enough
        return (NA)
    }
    
    p.high = max(p.xts$High[(p.i - BREAKOUT.PERIOD) : (p.i - 1)]) 
    p.low = min(p.xts$Low[(p.i - BREAKOUT.PERIOD) : (p.i - 1)])
    
    if (p.xts$Open[p.date] > p.high) { # prefer high breakout to low breakout
        return (HIGH.BREAKOUT)
    }
    
    if (p.xts$Open[p.date] < p.low) {
        return (LOW.BREAKOUT)
    }
    
    return (NA)
}

IsExitBreakout <- function(p.pos, p.date){
    if (is.na(p.pos$is.long)) { # empty position
        return (FALSE)
    }
    
    p.i = IndexFromDate(p.pos$underlying, p.date)
    if (is.na(p.i)) { # wrong date
        return (FALSE)
    }
    
    if (p.i <= EXIT.PERIOD) { # not long enough
        return (FALSE)
    }
    
    if (p.pos$is.long == TRUE) {
        p.low = min(p.pos$underlying$High[(p.i - EXIT.PERIOD) : (p.i - 1)])
        if (p.pos$underlying$Open[p.date] <= p.low) {
            return (TRUE)
        }
    }
    else if (p.pos$is.long == FALSE) {
        p.high = max(p.pos$underlying$Low[(p.i - EXIT.PERIOD) : (p.i - 1)])
        if (p.pos$underlying$Open[p.date] >= p.high) {
            return (TRUE)
        }
    }
    
    return (FALSE)
}

# AddUnit <- function()

UpdateN <- function(p.pos, p.date) {
    p.pos$N = p.pos$atr$atr[p.date]
    p.pos$unit.size = floor(coredata(p.pos$capital * UNIT.RATIO / (p.pos$N * CONTRACT.SIZE))[1])
    return (p.pos)
}


DoTrade <- function(p.pos, p.date) {
    
        
    if (p.pos$is.long == TRUE) {
#         browser()
        p.price = coredata(p.pos$underlying$Open[p.date])[1]
        p.unit.add = min(1, floor((p.pos$capital - abs(p.pos$cum.value)) / (p.price * CONTRACT.SIZE * p.pos$unit.size))) #buy maximum 1 unit at a time
        if (p.unit.add != 0){
            p.pos$load = p.pos$load + 1
            p.pos$size = p.pos$size + p.pos$unit.size * p.unit.add
            p.pos$cum.value = p.pos$cum.value + p.pos$unit.size * p.unit.add * p.price * CONTRACT.SIZE
            p.pos$entry.price[p.pos$load] = p.pos$underlying$Open[p.date]
            p.pos$stop.price = p.price - STOP.LOSS.COEF * coredata(p.pos$N)[1]
        }
        
    }
    
    if (p.pos$is.long == FALSE) {
#         browser()
        p.price = coredata(p.pos$underlying$Open[p.date])[1]
        p.unit.add = min(1, floor((p.pos$capital - abs(p.pos$cum.value)) / (p.price * CONTRACT.SIZE * p.pos$unit.size))) #sell maximum 1 unit at a time
        if (p.unit.add != 0) {
            p.pos$load = p.pos$load + 1
            p.pos$size = p.pos$size - p.pos$unit.size * p.unit.add
            p.pos$cum.value = p.pos$cum.value - p.pos$unit.size * p.unit.add * p.price * CONTRACT.SIZE
            p.pos$entry.price[p.pos$load] = p.pos$underlying$Open[p.date]
            p.pos$stop.price = p.price + STOP.LOSS.COEF * coredata(p.pos$N)[1]
        }
        
    }
    
    msg = paste(p.date, "cumulative value:", p.pos$cum.value, sep = " ")
    cat(msg, sep = "\n")
    return (p.pos)
}

TradeStrategy <- function (pos, date) {
    last.date = index(last(pos$underlying, '1 day'))[1]
    #unwind position on last day of contract
    if (!is.na(pos$is.long)
        && (date == last.date)) {
        today.price = coredata(pos$underlying$Open[date])[1]
        pos = ExitPosition(pos, today.price)
        pos$pnl = pos$pnl - pos$cum.value
        
        pos$pnl.trace[date] = pos$pnl
        pos$is.realized = TRUE
#         msg = paste("Day: ", i,
#                     "Date: ", date,
#                     "Unwind position, exit position at price:", today.price, 
#                     "; size:", pos$size, 
#                     "; PNL", pnl,
#                     sep = " ")
#         cat(msg, sep = "\n")

    }
    
    #stop loss at open price
    if (!is.na(pos$stop.price)) {
        today.price = coredata(pos$underlying$Open[date])[1]
        
        if ((pos$is.long == TRUE && today.price <= pos$stop.price)
            || (pos$is.long == FALSE && today.price >= pos$stop.price)
        ) {
            pos = ExitPosition(pos, today.price)
            pos$pnl = pos$pnl - pos$cum.value
            
            pos$pnl.trace[date] = pos$pnl
        
#             msg = paste("Day: ", i,
#                         "Date: ", date,
#                         "Stop loss, exit position at price:", today.price, 
#                         "; size:", pos$size,
#                         "; PNL:", pnl,
#                         sep = " ")
#             cat(msg, sep = "\n")
            pos = ResetPosition(pos)

        }
    }
    
    #Exit at open price
    if (IsExitBreakout(pos, date) == TRUE) {
        today.price = coredata(pos$underlying$Open[date])[1]
        pos = ExitPosition(pos, today.price)
        pos$pnl = pos$pnl - pos$cum.value

        pos$pnl.trace[date] = pos$pnl
#         msg = paste("Day: ", i,
#                     "Date: ", date,
#                     "Exits, exit position at price:", today.price, 
#                     "; size:", pos$size,
#                     "; PNL:", pnl,
#                     sep = " ")
#         cat(msg, sep = "\n")
        pos = ResetPosition(pos)

    }
    
    
    break.out = EntryBreakout(pos$underlying, date)
    
    #entering a position
    if(!is.na(break.out)
       && pos$load < MAX.LOAD) {
        #         browser()
        if (break.out == HIGH.BREAKOUT
            && (pos$load == 0 
                || (!is.na(pos$is.long) && pos$is.long == TRUE))) {
            
#             msg = paste("Day:", i,
#                         "High break out on", date, 
#                         "with price =", pos$underlying$Open[date], 
#                         sep = " ")
            
            #going long now
            if (pos$load == 0){
                pos$is.long = TRUE    
            }                
            pos = DoTrade(pos, date)
        }
        
        if (break.out == LOW.BREAKOUT
            && (pos$load == 0 
                || (!is.na(pos$is.long) && pos$is.long == FALSE))) {
#             msg = paste("Day:", i, 
#                         "Low break out on", date, 
#                         "with price =", pos$underlying$Open[date], 
#                         sep = " ")
            
            #going short now
            if (pos$load == 0){
                pos$is.long = FALSE    
            }
            pos = DoTrade(pos, date)
        }
#         cat(msg, sep = "\n")
    }
    
    
    return (pos)
}

# p.underlying is a quandl contract code
NewPosition <- function(p.underlying = NA) {
    # position members
    # is.long: TRUE means long pos, FALSE means short pos
    # size: position size
    # capital: the total amount of money
    # cum.value: cumulative transaction money
    # N: to be updated every Monday
    # entry: a vector of 4 indicating the entry/adding prices
    # next.breakout.price: price to add more units
    # load: how many units already taken, maximum = 4
    # underlying: the prices of the future contract
    # atr: average true range
    # arima.tr: predicting model of true range
    # unit.size
    # pnl: total pnl
    # pnl.trace: pnl on each date
    # is.realized: TRUE to stop trading the position
    
    # initialize a position on a future contract 
    p.pos = list(
        is.long     = NA,
        size        = 0,
        capital     = 0,
        cum.value   = 0,
        N           = NA,
        entry.price       = c(first = NA, second = NA, third = NA, fourth = NA),
        stop.price  = NA,
        next.breakout.price = NA,
        load        = 0,
        underlying  = NA,
        atr         = NA,
        arima.tr    = NA,
        unit.size = NA,
        pnl         = 0,
        pnl.trace = NA,
        is.realized = FALSE
    )
    
    if (!is.na(p.underlying)) {
        p.pos$underlying = Quandl(code = p.underlying, type = "xts")
        p.pos$underlying = p.pos$underlying[!is.na(p.pos$underlying$Settle)]
    }
    return (p.pos)
}

ResetPosition <- function (p.pos) {
    p.pos$is.long     = NA
    p.pos$size        = 0
    p.pos$cum.value   = 0
    p.pos$entry.price       = c(first = NA, second = NA, third = NA, fourth = NA)
    p.pos$stop.price  = NA
    p.pos$next.breakout.price = NA
    p.pos$load        = 0
    # p.pos$arima.tr = NA
    return (p.pos)
}

InitPosition <- function (p.pos) {
    n = nrow(p.pos$underlying)   
    p.pos$pnl.trace = xts(vector("numeric", n), index(p.pos$underlying))
    
    p.pos$atr = CalculateATR(p.pos$underlying)
    p.pos$arima.tr = ArimaTR(p.pos$atr$tr, CalculateArimaPeriod(p.pos$atr))
    p.pos$capital = ACCOUNT
    p.pos = UpdateN(p.pos, DateFromIndex(p.pos$atr, ATR.DAYS + 1))
    return (p.pos)
}

ExitPosition <- function (p.pos, p.price){
    p.money = p.price * CONTRACT.SIZE * p.pos$size
    p.pos$cum.value = p.pos$cum.value - p.money
#     if (p.pos$is.long == TRUE) {
#         p.pos$cum.value = p.pos$cum.value + p.money
#     }
#     else {
#         p.pos$cum.value = p.pos$cum.value - p.money
#     }
    p.pos$size = 0
    p.pos$stop.price = NA
    
    return (p.pos)
}

