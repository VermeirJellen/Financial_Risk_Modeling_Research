############################################################################
### EGARCH-Clayton-Gumbel model: Equal weight VaR targeting backtest      ##
############################################################################
par(mfrow=c(1,1))
readline(prompt="Press enter to perform equal allocation backtest for SPDR funds..")
SPDR <- SnPIndices[,-1]
returns.SPDR <- na.omit((SPDR / lag(SPDR, k= 1) - 1) * 100)
strategyResults.SPDR.equalWeights <- VaRTargetingMultivariateBacktest(returns=returns.SPDR, refit.every=10, 
                                                                      strategyName="SPDR Equal",
                                                                      plotOption=TRUE, printStats=TRUE, nrCores=detectCores())

readline(prompt="Press enter to perform equal allocation backtest for BEL20 stocks..")
BEL20Stocks <- BEL20[,-15]
returns.BEL20Stocks <- na.omit((BEL20Stocks / lag(BEL20Stocks, k= 1) - 1) * 100)
strategyResults.BEL20.equalWeights <- VaRTargetingMultivariateBacktest(returns=returns.BEL20Stocks, refit.every=10, 
                                                                       strategyName="BEL20 Equal", 
                                                                       plotOption=TRUE, printStats=TRUE, nrCores=detectCores())