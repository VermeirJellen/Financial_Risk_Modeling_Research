############################################################################
### Rescale portfolio weights such that a fixed expected               #####
### next-day portfolio VaR is targeted                                 #####
############################################################################
VaRTargetingMultivariate <- function(returns=NULL, assets=NULL, weights=NULL, riskLevel=-2,
                                     leverage.maximum=4, garch.model="eGARCH", garch.order=c(1,1), 
                                     arma.order=c(0,0), include.mean=FALSE, distribution.model="sstd",
                                     copula.simulations=100000, level=0.01)
{
  if(is.null(returns))
    returns <- na.omit((assets/lag(assets, k= 1) - 1) * 100)
  
  # Set default weights to equal risk
  if(is.null(weights))
    weights <- rep(1/ncol(returns), ncol(returns))
  
  # Forecast the VaR and ES, using 
  riskForecasts <- GarchCopulaRiskForecasts(returns=returns, weights=weights, garch.model=garch.model, garch.order=garch.order,
                                            arma.order=arma.order, include.mean=include.mean, distribution.model=distribution.model,
                                            copula.simulations=copula.simulations, level=level)
  
  # Extract VaR
  VaR <- riskForecasts[1]
  
  # Calculate the scaling factor
  scalingFactor <- riskLevel/VaR
  # Take maximum leverage into account
  scalingFactor[which(scalingFactor > leverage.maximum)] <- leverage.maximum
  
  # Data cleaning (contingency measure: this should have no effect..)
  scalingFactor[which(is.na(scalingFactor))] <- 1
  scalingFactor[which(is.infinite(scalingFactor))] <- 1
  
  # Calculate VaR adjusted weights
  weights.scaled <- scalingFactor*weights
  return(weights.scaled)
}