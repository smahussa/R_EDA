# A quant finance project. Steps outlined below: 

# Import libraries
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

# 1- We will first use the CAPM model to find Beta and Alpha, and compute Sharpe Ratio for a given portfolio. Import data from Yahoo Finance.
tickers <- c("FB", "AAPL", "AMZN", "NFLX")
weights <- c(0.25, 0.25, 0.25, 0.25)

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from = '2016-01-03', periodicity = "daily", auto.assign = F)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

# Lets use the S&P500 as our market returns
benchmarkPrices <- getSymbols.yahoo('^GSPC', from = '2016-01-03', periodicity = "daily", auto.assign = F)[,4]
benchmarkReturns <- na.omit(ROC(benchmarkPrices))

portfolioReturn <- Return.portfolio(portfolioReturns)

CAPM.beta(portfolioReturn, benchmarkReturns, 0.035/252)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, 0.035/252)
SharpeRatio(portfolioReturn, 0.035/252)

table.AnnualizedReturns(portfolioReturn)
table.CalendarReturns(portfolioReturn)


# 2- Lets do portfolio optimization for these stocks.
tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from = '2016-01-03', periodicity = "daily", auto.assign = F)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

# Lets specify a model where we want to maximize our returns and minimze our variance. Our output is the optimal weights for each stock which we should invest.
# Constraints are that the weights should add up to 1 and the individual weights should be between 0.1 and 0.4
portf <- portfolio.spec(assets = colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min = 0.1, max = 0.4)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

# Run the optimizer
optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
print(optPort)

chart.Weights(optPort)

# Plots the efficient frontier for different return and risk combinations. We have return on the y-axis and risk on the x-axis.
ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25, risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)


# 3- Lets do some backtesting for these stocks. Backtesting is a process where we run our algorithm on historical returns to check past performance and validity of our algo.
tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from = '2016-01-03', periodicity = "daily", auto.assign = F)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(assets = colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum = 0.99, max_sum = 1.01)
# Add a constraint for transaction costs as we are rebalancing our weights on a monthly balance for backtesting.
portf <- add.constraint(portf, type = "transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type="box", min = .1, max = .4)
portf <- add.objective(portf, type="return", name="mean")
# Set a constraint for the variance now
portf <- add.objective(portf, type="risk", name="StdDev", target = 0.005)

# Run 10000 different random portfolios
rp <- random_portfolios(portf, 10000, "sample")

# Rebalancing our weights on a monthly balance for backtesting
opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            optimize_method = "random",
                                            rp = rp,
                                            rebalance_on = "months",
                                            training_period = 1,
                                            rolling_window = 10)

# Find performance of the same stocks with equal weightage.
equal_weight <- rep(1/ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

# Find performance of the stock market (S&P500)
sp500prices <- getSymbols.yahoo('SPY', from = '2016-01-03', periodicity = "daily", auto.assign = F)[,4]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)

chart.Weights(opt_rebal, main = "Rebalanced Weights Over Time")

# Plot performance of the optimal portfolio, along with equal weighted portfolio along with the market performance.
rebal_weights <- extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights = rebal_weights)
rets_df <- cbind(rebal_returns, benchmark, sp500Rets)
charts.PerformanceSummary(rets_df, main = "P/L over time")
