############################################################################
##### Out of sample backtest:                               ################
##### Perform EMACrossover strategy on one individual asset ################
############################################################################
EMACrossoverDirectionUnivariateBacktest <- function(asset, shortEMA=50, longEMA=200)
{
  # Short EMA
  shortest = EMA(asset, n=shortEMA)
  # long EMA
  longest = EMA(asset, n=longEMA)
  
  longsEntry <- shortest > longest
  longsExit <- shortest < longest
  
  shortsEntry <- shortest < longest
  shortsExit <- shortest > longest
  
  # Initialize long signal array
  longSignals <- xts(matrix(data=rep(NA, nrow(asset)),
                            nrow=nrow(asset), ncol=1),
                     order.by=index(asset))
  # Set 1 where long entry
  longSignals[longsEntry] <- 1
  # Set 0 where long exit
  longSignals[longsExit] <- 0
  # Carry long signals forward
  longSignals <- na.locf(longSignals, fromLast=FALSE)
  
  # Initialize short signal array
  shortSignals <- xts(matrix(data=rep(NA, nrow(asset)),
                             nrow=nrow(asset), ncol=1),
                      order.by=index(asset))
  # Set -1 where short entry
  shortSignals[shortsEntry] <- -1
  # Set 0 where short exit
  shortSignals[shortsExit] <- 0
  # Carry short signals forward
  shortSignals <- na.locf(shortSignals, fromLast=FALSE)
  
  # add long and short signals for overal direction
  direction <- longSignals+shortSignals
  # lag forward to allign forecasted direction with actual next day realized returns
  direction <- lag(direction, 1)
  
  return(direction)
}

############################################################################
##### Out of sample backtest:                                        #######
##### Perform EMACrossover strategy on a portfolio of assets         #######
##### Apply EGARCH-Clayton-Gumbel VaR targeting (depends on input)   ####### 
############################################################################
EMACrossoverStrategyBacktest <- function(assets=NULL, shortEMA=50, longEMA=200, risk.lookback=200,
                                         use.conservative.rebalancing=FALSE, 
                                         rebalancing.confidence=0.5, rebalancing.lookback=120,
                                         useScaling=TRUE, plotOption=TRUE, 
                                         scaling.riskLevel=-2, scaling.printStats=FALSE,
                                         calibration.interval=375, refit.every=50,
                                         strategyName="EMACross", leverage.maximum = 4, 
                                         garch.model="eGARCH", garch.order=c(1,1), arma.order=c(0,0), 
                                         include.mean=FALSE, distribution.model="sstd",
                                         copula.simulations=100000, level=0.01, nrCores=detectCores())
{
  returns <- na.omit((assets / lag(assets, k= 1) - 1) * 100)
  nrMarginals <- ncol(returns)
  
  # Get directional forecasts for the individual assets 
  # Note: Lagged one day forward, alligned with realized returns
  strategy.directions <- lapply(1:nrMarginals, function(x){ 
    EMACrossoverDirectionUnivariateBacktest(assets[,x],
                                            shortEMA=shortEMA,
                                            longEMA=longEMA) })
  # Unlist and convert to xts
  strategy.directions <- xts(matrix(unlist(strategy.directions),
                                    ncol=nrMarginals),
                                    order.by=index(assets))
  # Remove longEMA NA entries
  strategy.directions <- strategy.directions[complete.cases(strategy.directions)]
  
  # Calculate asset volatilities (standard deviations)
  risk <- rollapply(returns, FUN=sd, width=risk.lookback, by.column=TRUE)
  
  # We use volatility weighting
  strategy.weights <- 1/risk
  strategy.weights <- xts(strategy.weights, 
                          order.by=tail(index(returns), nrow(strategy.weights)))
  # Lag forward to allign forecasted weights with actual next day realized returns
  strategy.weights <- lag(strategy.weights, 1)
  # Remove NA induced by lagging
  strategy.weights <- strategy.weights[complete.cases(strategy.weights)]
  
  strategy.tstamps <- intersect(index(strategy.directions), index(strategy.weights))
  # Multiply absolute weights with the direction
  strategy.weights.directional <- strategy.weights[strategy.tstamps]*strategy.directions[strategy.tstamps]
  # normalize weights: sum of absolute values of weights is 1
  strategy.weights.directional <- strategy.weights.directional/rowSums(abs(strategy.weights.directional))
  
  strategyResults <- VaRTargetingMultivariateBacktest(returns=returns, weights=strategy.weights.directional, 
                                                      riskLevel=scaling.riskLevel,
                                                      calibration.interval=calibration.interval, refit.every=refit.every,
                                                      leverage.maximum=leverage.maximum, garch.model=garch.model, 
                                                      garch.order=garch.order, arma.order=arma.order, 
                                                      include.mean=include.mean, distribution.model=distribution.model,
                                                      copula.simulations=copula.simulations, level=level, nrCores=nrCores)
  if(plotOption)
    PlotStrategyResults(strategyResults, strategyName=strategyName, printStats=scaling.printStats) 
  
  return(strategyResults)
}