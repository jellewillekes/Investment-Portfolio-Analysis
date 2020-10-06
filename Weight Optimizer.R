library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(foreach)
library(iterators)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(BatchGetSymbols)
library(readxl)
library(xlsx)
library(plyr)
library(tidyr)

setwd("/Users/willekes/R Investing")


#read_excel('SigmaInput.xlsx', sheet = "Sheet1")$Ticker

#Input Data
tickers <- read.csv('stockPickingSeptember.csv')$x
from <- '2017-01-01'            # Starting date
periodicity <- 'monthly'          # Frequency of Stock price 
SP500 <- "^GSPC"
DAX <- "^GDAXI"  
minWeight <- 0.02               # Minimum Weight of a Stock in optimized Portfolio
maxWeight <- 0.10                # Maximum Weight of a Stock in optimized Portfolio

#Calculate 
stockPrices <- NULL
for(ticker in tickers) {
  stockPrices <- cbind(stockPrices, getSymbols.yahoo(ticker, from=from, periodicity = periodicity,  auto.assign=FALSE)[,4]) 
}

portfolioReturns <- na.omit(ROC(stockPrices))
colnames(portfolioReturns) <- tickers

#Create Portfolio P with the assets in selectedStocks
P <- portfolio.spec(assets = colnames(portfolioReturns))
#Set Constraints and to Optimal Portfolio
P <- add.constraint(P, type="full_investment")
P <- add.constraint(P, type="box", min=minWeight, max=maxWeight) 

#Set Objective for the Optimal Portfolio
P <- add.objective(P, type="return", name="mean")
P <- add.objective(P, type="risk", name="StdDev")
P <- add.objective(P, type="risk_budget", name="StdDev", arguments=list(p=0.95), max_prisk=0.05)

#Calculate Optimal Portfolio
optimalPortfolio <- optimize.portfolio(portfolioReturns, P, optimize_method = "ROI", trace=TRUE)
optimalWeights <- extractWeights(optimalPortfolio)
optimalReturns <- Return.portfolio(portfolioReturns, weights=optimalWeights)
#Create Efficient Frontier of Optimal Portfolio
ef <- extractEfficientFrontier(optimalPortfolio, match.col = "StdDev", n.portfolios = 50, risk_aversion = NULL)

#Create Equally Weighted Portfolio
equalWeights <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
equalReturns <- Return.portfolio(portfolioReturns, weights = equalWeights)
colnames(equalReturns) <- "Equally Weighted Portfolio"

#Create SP500 Benchmark
sp500Prices <- getSymbols.yahoo(Symbols=SP500, from=from, periodicity = periodicity, auto.assign=FALSE)[,4]
sp500Returns <- na.omit(ROC(sp500Prices))
sp500Returns <- as.xts(sp500Returns)

#Create DAX Benchmark
DaxPrices <- getSymbols.yahoo(Symbols=DAX, from=from, periodicity = periodicity, auto.assign=FALSE)[,4]
DaxReturns <- na.omit(ROC(DaxPrices))
DaxReturns <- as.xts(DaxReturns)

#Add Returns in a Dataframe
returns_df <- cbind(optimalReturns, sp500Returns, DaxReturns, equalReturns)

#Plot Results
plot(returns_df)
legend(2020, NULL, legend = c("optimalReturns", "sp500Returns", "DaxReturns", "equalReturns"), col = c("black", "red", "green"), pch = c(4,4))
charts.PerformanceSummary(returns_df, main="Performance Over Time")
chart.Weights(optimalPortfolio)
chart.EfficientFrontier(ef, match.col = "StdDev", n.portfolios = 50, xlim = NULL, ylim = NULL, 
                        cex.axis = 0.8, element.color = "black", main = "Efficient Frontier",
                        RAR.text = "SD", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
plot(stockPrices, legend = tickers)

#Value-at-Risk Calculations
VaR(portfolioReturns, weights = optimalWeights, p=0.95, portfolio_method = "component", method = "modified")
#VaR(portfolioReturns, weights = optimalWeights, p=0.99, portfolio_method = "component", method = "modified")
CVaR(portfolioReturns, weights = optimalWeights, p=0.95, portfolio_method = "component", method = "modified")
#CVaR(portfolioReturns, weights = optimalWeights, p=0.99, portfolio_method = "component", method = "modified")

?VaR

