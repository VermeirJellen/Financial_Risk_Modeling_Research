############################################################################
## Perform univariate out of sample EGARCH risk forecasting backtest      ##
############################################################################
par(mfrow=c(1,1))
# Need to use refit.every = 1 for better results
readline(prompt="Press enter to perform eGarch backtest for SPY return series (This takes a while)..")
noOutput1 <- GarchBacktest(asset=SPY, plotOption=TRUE, printStats=TRUE, 
                            refit.every=10, calibration.interval=500, nrCores=detectCores())

par(mfrow=c(1,1))
# Need to use refit.every = 1 for better results
readline(prompt="Press enter to perform eGarch backtest for ACKB return series (This takes a while)..")
noOutput2 <- GarchBacktest(asset=ACKB,plotOption=TRUE, printStats=TRUE, 
                           refit.every=10, calibration.interval=500, nrCores=detectCores())