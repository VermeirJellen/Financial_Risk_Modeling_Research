############################################################################
### Perform out of sample univariate VaR targeting backtest:           #####
### Rescale exposure in one asset such that a fixed expected           #####
### next-day portfolio VaR is targeted                                 #####
############################################################################
VaRTargetingUnivariateBacktest <- function(returns=NULL, asset=NULL, riskLevel=-2, level=0.01, calibration.interval=375, 
                                           model.spec="eGARCH", distribution.model="sstd", model.garchOrder = c(1,1), 
                                           arma.order = c(0,0), include.mean=FALSE,
                                           refit.every=10, test.conf.level=0.95, strategyName="Strategy",
                                           plotOption=FALSE, printStats=FALSE, leverage.maximum=4, nrCores=detectCores())
{
  if(is.null(returns))
    returns <- na.omit((asset/lag(asset, k= 1) - 1) * 100)
  returns.original <- returns
  
  # Perform VaR / ES eGarch backtest
  riskForecasts <- GarchBacktest(returns=returns.original,level=level, calibration.interval=calibration.interval, 
                                 model.spec=model.spec, distribution.model=distribution.model, 
                                 model.garchOrder = model.garchOrder, 
                                 arma.order = arma.order, include.mean=include.mean, 
                                 refit.every=refit.every, test.conf.level=test.conf.level,
                                 plotOption=FALSE, printStats=FALSE, nrCores=nrCores)
  
  VaR.original <- riskForecasts[,1]
  ES.original <- riskForecasts[,2]
  VaR.original[which(is.na(VaR.original))] <- riskLevel
  ES.original[which(is.na(ES.original))] <- riskLevel
  
  scalingFactor <- xts(riskLevel/coredata(VaR.original), order.by=index(VaR.original))
  scalingFactor[which(is.na(scalingFactor))] <- 1
  scalingFactor[which(is.infinite(scalingFactor))] <- 1
  scalingFactor[which(scalingFactor > leverage.maximum)] <- leverage.maximum
  
  returns.scaled <- returns.original[index(scalingFactor)]*scalingFactor
  VaR.scaled <- xts(VaR.original, order.by=time(VaR.original))*scalingFactor
  ES.scaled <- xts(ES.original, order.by=time(ES.original))*scalingFactor
  
  strategyResults <- list(returns.original, VaR.original, ES.original, scalingFactor,
                          returns.scaled, VaR.scaled, ES.scaled)
  
  if(plotOption)
    PlotStrategyResults(strategyResults, strategyName=strategyName, printStats=printStats)
  
  return(strategyResults)
}