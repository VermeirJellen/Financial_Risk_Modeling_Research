############################################################################
### Perform out of sample GHD backtest: Risk measurement forecasting   #####
############################################################################
asset <- SPY
readline(prompt="GHD backtest for SPY: Press enter to start..")
SPY.GHD <- GHDBacktest(asset=asset, printResults=TRUE, nrCores=detectCores())

asset <- AGS
readline(prompt="GHD backtest for AGS: Press enter to start..")
AGS.GHD <- GHDBacktest(asset=asset, printResults=TRUE, nrCores=detectCores())

