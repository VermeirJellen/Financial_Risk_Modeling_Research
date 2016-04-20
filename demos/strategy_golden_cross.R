############################################################################
### EGARCH-Clayton-Gumbel model: Trading strategy VaR targeting backtest  ##
############################################################################
readline(prompt="Press enter to perform golden cross backtest for SPDR funds..")
SPDR <- SnPIndices[,-1]
returns.SPDR <- na.omit((SPDR / lag(SPDR, k= 1) - 1) * 100)
strategyResults.SPDR.GoldenCross <- EMACrossoverStrategyBacktest(assets=SPDR, useScaling=TRUE, refit.every=10, 
                                                                 plotOption=TRUE, scaling.printStats=TRUE,
                                                                 strategyName="Golden Cross (SPDR)", nrCores=detectCores())

readline(prompt="Press enter to perform golden cross backtest for BEL20 stocks..")
BEL20Stocks <- BEL20[,-15]
returns.BEL20Stocks <- na.omit((BEL20Stocks / lag(BEL20Stocks, k= 1) - 1) * 100)
strategyResults.BEL20.GoldenCross <- EMACrossoverStrategyBacktest(assets=BEL20Stocks, useScaling=TRUE, refit.every=10, 
                                                                  plotOption=TRUE, scaling.printStats=TRUE,
                                                                  strategyName="Golden Cross (BEL20)", nrCores=detectCores())
