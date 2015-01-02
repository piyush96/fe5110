# Library
plusOne <- function(x){
    x = x + 1
    return (x)
}

myATR <- function(codeName){
    codeData = Quandl(code = codeName, type = "xts")
    #class(codeData)
    colnames(codeData)[6] = "Close"
    atr = ATR(codeData[,c("High","Low","Close")], n=14)
    #class(atr)
    #codeData[,"Close"]
    return (atr)
}