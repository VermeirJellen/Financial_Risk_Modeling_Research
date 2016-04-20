############################################################################
### Fit a Garch Model to a univariate return timeseries                 ####
############################################################################
GarchCalibration <- function(returns, garch.model="eGARCH", garch.order=c(1,1),
                     arma.order=c(0,0), include.mean=FALSE, distribution.model="sstd")
{
  garch.fit <- tryCatch({
    spec = ugarchspec(variance.model = list(model=garch.model, garchOrder=garch.order), 
                      mean.model = list(armaOrder=arma.order, include.mean=include.mean),
                      distribution.model=distribution.model)
    ugarchfit(spec, returns, solver='hybrid')
  }, warning=function(w) # In case of failed convergence use conditional ghd distribution
  {
    warning("Garchcalibration failed. Using default model..")
    spec = ugarchspec(variance.model = list(model=garch.model, garchOrder=garch.order), 
                      mean.model = list(armaOrder=arma.order, include.mean=include.mean),
                      distribution.model="ged")
    ugarchfit(spec, returns, solver='hybrid')
  })
  
  return(garch.fit)
}

############################################################################
### Fit garch model and plot a QQ plot                                  ####
############################################################################
GarchPlotQQ <- function(asset, garch.model="eGARCH", garch.order=c(1,1),
                        arma.order=c(0,0), include.mean=FALSE,distribution.model="sstd")
{
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  calibrated.model <- GarchCalibration(returns, garch.model=garch.model, garch.order=garch.order,
                                       arma.order=arma.order, include.mean=include.mean, 
                                       distribution.model=distribution.model)
  # Get standardized residuals
  garch.residuals <- residuals(calibrated.model,standardize=TRUE)
  # get forecast for sigma
  
  # One step ahead forecast
  garch.forecast <- ugarchforecast(calibrated.model, n.ahead=1)@forecast
  # Extract forecasted mu
  garch.mu <- garch.forecast$seriesFor
  # Extract forecasted sigma
  garch.sigma <- garch.forecast$sigmaFor
  
  
  # Extract conditional distribution
  conditional.distribution <- getspec(calibrated.model)@model$modeldesc$distribution
  # Extract shape of the distribution
  skew <- coef(calibrated.model)["skew"]
  # Skew is only applicable for skewed distributions
  skew[which(is.na(skew))] <- 1
  # Extract skew of the distribution
  shape <- coef(calibrated.model)["shape"]
  # Extract lambda of the distribution
  lambda <- coef(calibrated.model)["ghlambda"]
  # lambda is only applicable for Ghyp / NIG, default is -0.5
  if(is.na(lambda))
    lambda=-0.5
  
  p <- ppoints(100)
  empirical.quantiles <- quantile(garch.residuals, p=p)
  distribution.quantiles <- qdist(distribution=conditional.distribution ,p=p, shape=shape, skew=skew, lambda=lambda)
  plot(distribution.quantiles, empirical.quantiles, xlab="Theoretical Quantiles",
       ylab="Empirical Quantiles", main=paste(garch.model," QQ plot for ",names(asset), " (",distribution.model,")",sep=""))
  qqline(empirical.quantiles, distribution= function(x) {
    qdist(distribution=conditional.distribution, x, shape=shape, skew=skew, lambda=lambda)}, col="blue", lty=2)
  
  return(calibrated.model)
}