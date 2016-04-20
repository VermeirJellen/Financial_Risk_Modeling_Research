############################################################################
### Peform out of sample portfolio VaR targeting backtest:             #####
### Rescale portfolio weights such that a fixed expected               #####
### next-day portfolio VaR is targeted                                 #####
############################################################################
VaRTargetingMultivariateBacktest <- function(returns, weights=NULL, riskLevel=-2,
                                             calibration.interval=375, refit.every=10,
                                             leverage.maximum=4, garch.model="eGARCH", garch.order=c(1,1), 
                                             arma.order=c(0,0), include.mean=FALSE, distribution.model="sstd",
                                             copula.simulations=100000, level=0.01, nrCores=detectCores(),
                                             strategyName="Strategy", plotOption=FALSE, printStats=FALSE)
{
  # Set default weights to equal weights
  if(is.null(weights))
  {
    weights <- xts(matrix(1/ncol(returns), nrow=nrow(returns),
                          ncol=ncol(returns)), order.by=index(returns))
  }
  
  returns.original <- xts(rowSums(returns[index(weights)]*weights), order.by=index(weights))
  
  # Get the VaR and ES for the original weight structure
  VaR.ES <- GarchCopulaBacktest(returns=returns, weights=weights, 
                                calibration.interval=calibration.interval, refit.every=refit.every,
                                garch.model=garch.model, garch.order=garch.order, arma.order=arma.order,
                                include.mean=include.mean, distribution.model=distribution.model,
                                copula.simulations=copula.simulations, level=level, nrCores=nrCores)
  
  
  # We might only have partial out of sample weight information
  VaR.ES <- VaR.ES[index(weights)]
  
  # Extract VaR
  VaR.original <- VaR.ES[,1]
  # Capture potential numerical problems
  VaR.original[which(is.na(VaR.original))] <- riskLevel
  
  # Extract ES
  ES.original <- VaR.ES[,2]
  # Capture potential numerical problems
  ES.original[which(is.na(ES.original))] <- riskLevel
  
  # Calculate scalingfactor
  scalingFactor <- riskLevel/VaR.original
  # Capture potential numerical problems
  scalingFactor[which(is.na(scalingFactor))] <- 1
  # Only allow a maximum of 4x leverage
  scalingFactor[which(scalingFactor > leverage.maximum)] <- leverage.maximum
  
  # Calculate updated weights for VaR targeting
  weights.scaled <- weights[index(scalingFactor)]*drop(scalingFactor)
  # Calculate updated weights
  returns.scaled <- xts(rowSums(returns[time(weights.scaled)]*weights.scaled),
                        order.by=index(weights.scaled))
  VaR.scaled <- VaR.original*scalingFactor
  ES.scaled <- ES.original*scalingFactor
  
  strategyResults <- list(returns.original, VaR.original, ES.original, scalingFactor,
                          returns.scaled, VaR.scaled, ES.scaled, weights, weights.scaled)
  
  if(plotOption)
    PlotStrategyResults(strategyResults, strategyName=strategyName, printStats=printStats)
  
  return(strategyResults)
}