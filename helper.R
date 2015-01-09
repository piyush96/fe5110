## Helper function

# return the date of time series at i
# i is an integer
DateFromIndex <- function(p.xts, p.i) { 
    p.dates = index(p.xts)
    if (p.i < 1 
        || p.i > length(p.dates)) {
        return (NA)
    }
    else {
        return (p.dates[p.i])
    }
} 

# return index of time series on date
# date is a string of date YYYY-MM-dd
IndexFromDate <- function(p.xts, p.date) { 
    p.dates = index(p.xts)
    p.first.date = p.dates[1]
    p.last.date = p.dates[length(p.dates)]
    p.date.conv = as.Date(p.date)
    if (p.date.conv > p.last.date 
        || p.date.conv < p.first.date) {
        return (NA)
    }
    else {
        p.i = nrow(p.xts[paste("/", p.date, sep = "")])
        return (p.i)
    }
}

## ping ping