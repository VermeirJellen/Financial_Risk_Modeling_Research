############################################################################
##### Generate one EGARCH out of sample VaR / ES forecast             ######
############################################################################
GarchRiskForecasts <- function(returns, level=0.01, garch.model="eGARCH", 
                               distribution.model="sstd", garch.order = c(1,1),
                               arma.order = c(0,0), include.mean=FALSE)
{
  garch.model = GarchCalibration(returns, garch.model=garch.model,
                                 distribution.model=distribution.model, garch.order=garch.order,
                                 arma.order=arma.order, include.mean=include.mean)
  
  # Get the garch specification and fix the calibrated parameters
  garch.spec <- getspec(garch.model)
  # Fix the coefficients
  setfixed(garch.spec)<- as.list(coef(garch.model))
  
  # Perform one day ahead forecast of conditional mean and volatility
  forecast.garch <- ugarchforecast(garch.spec, n.ahead=1, n.roll=0, data=returns, out.sample=0)
  
  # Extract forecasted conditional volatility
  forecast.garch.sigma = sigma(forecast.garch)
  # Extract forecasted conditional mean
  forecast.garch.mu = fitted(forecast.garch)
  
  # Fetch skew parameter of the conditional distributions
  skew <- coef(garch.model)["skew"]
  # skew is only applicable for skewed distributions. Default 1.
  skew[which(is.na(skew))] <- 1
  # Fetch the shape parameter of the condtiional distributions
  shape <- coef(garch.model)["shape"]
  # Fetch the lambda parameter of the conditional distribution
  lambda <- coef(garch.model)["ghlambda"]
  # lambda is only applicable for ghyp/nig. Default -0.5.
  lambda[which(is.na(lambda))] <- -0.5
  
  VaR = forecast.garch.mu + forecast.garch.sigma*qdist(distribution.model, level, mu=0, sigma=1, 
                                                      skew=skew, shape=shape, lambda=lambda)
  
  # This function is used to determine probability quantiles for standardized residuals
  qdist.function = function(x, skew, shape, lambda){
                            qdist(distribution.model, p=x, mu=0, sigma=1, 
                            skew=skew, shape=shape, lambda=lambda)}
  # Caculate expected shortfall
  ES = forecast.garch.mu + forecast.garch.sigma * 
                              integrate(qdist.function, 0, level, skew=skew, 
                                        shape=shape, lambda=lambda)$value/level
  
  # Return VaR and ES in vector
  return(c(VaR,ES))
}