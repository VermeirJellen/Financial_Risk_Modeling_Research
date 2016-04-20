############################################################################
### Generate one EGARCH-Clayton-Gumbel out of sample VaR / ES forecast  ####
############################################################################
GarchCopulaRiskForecasts <- function(returns, weights=NULL, garch.model="eGARCH", garch.order=c(1,1),
                                     arma.order=c(0,0), include.mean=FALSE, distribution.model="sstd",
                                     copula.simulations=100000, level=0.01)
{
  nrMarginals <- ncol(returns)
  # If weights is null, use equal weights default
  if(is.null(weights))
    weights <- rep(1/nrMarginals,nrMarginals)
  
  # Calibrate models for the marginals
  calibrated.garch.models <- lapply(1:nrMarginals, function(x){ 
          GarchCalibration(returns[,x], garch.model=garch.model, 
                     garch.order=garch.order, arma.order=arma.order, include.mean=include.mean,
                     distribution.model=distribution.model)})
  
  # Calibrate multivariate copula to the STANDARDIZED RESIDUALS of the marginals
  copula.mixed.parameters <- GarchCopulaCalibration(calibrated.garch.models)
  # Extract clayton param
  parameter.clayton = copula.mixed.parameters[1]
  # Extract Gumbel param
  parameter.gumbel = copula.mixed.parameters[2]
  # Extract mixing probab
  mixing.prob = copula.mixed.parameters[3]
  
  # initialize the Clayton copula
  copula.clayton <- claytonCopula(parameter.clayton, dim=nrMarginals)
  # initialize the Gumbel copula
  copula.gumbel <- gumbelCopula(parameter.gumbel, dim=nrMarginals)
  
  # Use uniform probabilities to determine the copula sampling distribution
  uniform.samples <- runif(copula.simulations)
  # Amount of samples for clayton
  clayton.simulations <- sum(uniform.samples <= mixing.prob)
  # Amount of samples for Gumbel
  gumbel.simulations <- sum(uniform.samples > mixing.prob)
  
  # Simulate multivariate samples from the clayton copula
  copula.clayton.rcop <- rCopula(clayton.simulations, copula.clayton)
  # Simulate multivariate samples from the gumbel copula
  copula.gumbel.rcop <- rCopula(gumbel.simulations, copula.gumbel)
  # Merge the simulations to acquire the mixed model samples
  copula.mixed.rcop <- rbind(copula.clayton.rcop,copula.gumbel.rcop)
  
  # Get the marginal conditional distributions from the Garch models
  distribution.conditional <- sapply(1:nrMarginals, function(x){
    getspec(calibrated.garch.models[[x]])@model$modeldesc$distribution})
  # Fetch skew parameter of the egarch conditional distributions
  skew <- sapply(1:nrMarginals, function(x) coef(calibrated.garch.models[[x]])["skew"])
  # skew is only applicable for skewed distributions
  skew[which(is.na(skew))] <- 1
  # Fetch the shape parameter of the egarch conditional distributions
  shape <- sapply(1:nrMarginals, function(x) coef(calibrated.garch.models[[x]])["shape"])
  # Fetch the lambda parameters of the egarch conditional distributions
  lambda <- sapply(1:nrMarginals, function(x) coef(calibrated.garch.models[[x]])["ghlambda"])
  # lambda is only applicable for ghyp / NIG. Default is -0.5
  lambda [which(is.na(lambda))] <- -0.5
  
  # Calculate the marginal quantile values that correspond to the multivariate simulations
  qMarginals <- sapply(1:nrMarginals,function(x){ qdist(distribution=distribution.conditional[x],
                                                        p=copula.mixed.rcop[,x], 
                                                        mu=0, sigma=1, skew=skew[x], 
                                                        shape=shape[x], lambda=lambda[x])})
  
  # Get the garch specification and fix the calibrated parameters
  garch.spec <- lapply(1:nrMarginals, function(x){ 
    s <- getspec(calibrated.garch.models[[x]])
    setfixed(s)<- as.list(coef(calibrated.garch.models[[x]]))
    s })
  
  # Perform mu and sigma forecasts
  forecasts <- lapply(1:nrMarginals, function(x){
    ugarchforecast(garch.spec[[x]], n.ahead=1, n.roll=0, data=returns, out.sample=0)})
  
  # Extract conditional volatility forecasts
  forecasts.garch.sigma = sapply(1:nrMarginals, function(x){
          as.numeric(rugarch:::sigma(forecasts[[x]]))})
  
  # Extract conditional mean forecasts
  forecasts.garch.mu = sapply(1:nrMarginals, function(x){
          as.numeric(rugarch:::fitted(forecasts[[x]]))})
  
  
  # Calculate amount of simulation outliers that fall "below the level"
  simulation.outliers <- floor(level*copula.simulations)
  
  # Multiply simulated standardized marginal errors with the forecasted sigma 
  returns.marginals <- forecasts.garch.mu + t(t(coredata(qMarginals))*as.vector(forecasts.garch.sigma))
  # Multiply with weight matrix
  returns.weighted <- returns.marginals %*% weights
  # Fetch outlier values
  returns.outliers <- head(sort(returns.weighted), simulation.outliers)
  
  # VaR is the largest outliers (corresponding to the level)
  VaR <- tail(returns.outliers,1)
  # ES is calculated as median value of the outliers that are smaller than VaR
  ES <- mean(returns.outliers)
  
  # Return the VaR and ES
  return(c(VaR,ES))
}