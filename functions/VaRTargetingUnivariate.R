############################################################################
### Rescale exposure in one asset such that a fixed expected           #####
### next-day portfolio VaR is targeted                                 #####
############################################################################
VaRTargetingUnivariate <- function(returns=NULL, asset=NULL, riskLevel=-2, level=0.01, calibration.interval = 375, 
                                   model.spec="eGARCH", distribution.model="sstd", model.garchOrder = c(1,1), 
                                   arma.order = c(0,0), include.mean=FALSE, leverage.maximum=4)
{
  if(is.null(returns))
    returns <- na.omit((asset/lag(asset, k= 1) - 1) * 100)
  
  # Calculate the VaR and ES forecasts for the return series
  riskForecasts <- GarchRiskForecasts(returns=returns, level=level, 
                                      garch.model=model.spec, distribution.model=distribution.model, 
                                      garch.order=model.garchOrder, 
                                      arma.order=arma.order, include.mean=include.mean)
  
  # Extract VaR
  VaR <- riskForecasts[1]
  
  # Calculate the scaling factor
  scalingFactor <- riskLevel/VaR
  # Take maximum leverage into account
  scalingFactor[which(scalingFactor > leverage.maximum)] <- leverage.maximum
  
  # Data cleaning (contingency measure: this should have no effect..)
  scalingFactor[which(is.na(scalingFactor))] <- 1
  scalingFactor[which(is.infinite(scalingFactor))] <- 1
  
  return(scalingFactor)
}