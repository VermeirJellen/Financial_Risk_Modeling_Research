############################################################################
## Perform univariate out of sample EGARCH VaR Targeting backtest         ##
############################################################################
assets <- SnPIndices$SPY
SnP.returns <- na.omit((assets / lag(assets, k= 1) - 1) * 100)
readline(prompt="Perform VaRTargeting backtest for SPY..")
strategyResults.SnP <- VaRTargetingUnivariateBacktest(returns=SnP.returns, calibration.interval=375, 
                                                      refit.every=1, plotOption=TRUE, printStats=TRUE, 
                                                      strategyName="SPY ETF", nrCores=detectCores())

assets <- BEL20$BEL
BEL20.returns <- na.omit((assets / lag(assets, k= 1) - 1) * 100)
readline(prompt="Perform VaRTargeting backtest for BEL..")
strategyResults.BEL20 <- VaRTargetingUnivariateBacktest(returns=BEL20.returns, calibration.interval=375, 
                                                        refit.every=1, plotOption=TRUE, printStats=TRUE,
                                                        strategyName="BEL20 ETF", nrCores=detectCores())