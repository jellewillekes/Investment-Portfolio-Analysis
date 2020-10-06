library(quantmod)
library(PerformanceAnalytics)
library(stringr)
library(readxl)
library(matrixStats)

setwd("/Users/willekes/R Investing")
#EuroStoxx50 <- read_excel('Stoxx50input.xlsx', sheet = "Sheet1")$Ticker
EuroStoxx600 <- read_excel('Stoxx600input.xlsx', sheet = "Sheet1")$Ticker
SP500 <- read_excel('SP500input.xlsx', sheet = "Sheet1")$Ticker

from = '2019-05-01'
to = '2020-07-31'
periodicity = 'monthly'

stock_sharps <- function(tickers){
  stockPrices <- NULL
  
  for(ticker in tickers) {
    stockPrices <- cbind(stockPrices, getSymbols.yahoo(ticker, from = from, to = to, periodicity = periodicity, auto.assign=FALSE)[,4])
  }
  
  stockReturns <- na.omit(ROC(stockPrices))
  averageReturns <- colMeans(stockReturns, na.rm = TRUE)
  stockSD = colSds(stockReturns)
  sharpe = averageReturns/stockSD
  
  return(sharpe)
}

#sharpsEuroStoxx50 <- stock_sharps(EuroStoxx50)
sharpsEuroStoxx600 <- stock_sharps(EuroStoxx600)
sharpsSP500 <- stock_sharps(SP500)

returnsTotal <- data.frame(c(sharpsEuroStoxx600, sharpsSP500))
colnames(returnsTotal) <- "Return"
tickers <- c(EuroStoxx600, sharpsSP500)
returnsTotal <- data.frame(tickers, returns = returnsTotal$Return)

orderedReturns <- returnsTotal[order(-returnsTotal$returns),]
result <- orderedReturns$tickers
result <- head(result, 20)
write.csv(result, "stockPickingSeptember.csv")

