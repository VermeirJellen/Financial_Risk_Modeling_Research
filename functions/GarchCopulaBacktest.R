############################################################################
### EGARCH-Clayton-Gumbel out of sample VaR / ES forecasting backtest   ####
### (Asset returns and weight structure are given as input)             ####
############################################################################
GarchCopulaBacktest <- function(returns=NULL, assets=NULL, weights=NULL, calibration.interval=375, refit.every=10,
                                garch.model="eGARCH", garch.order=c(1,1), arma.order=c(0,0),
                                include.mean=FALSE, distribution.model="sstd",
                                copula.simulations=100000, level=0.01, nrCores=detectCores())
{
  if(is.null(returns))
    returns <- na.omit((assets / lag(assets, k= 1) - 1) * 100)
  
  nrMarginals <- ncol(returns)
  
  # If weights is null, use equal weights
  if(is.null(weights))
  {
    weights <- xts(matrix(rep(1/nrMarginals,nrMarginals),
                          ncol=nrMarginals, nrow=nrow(returns)), order.by=index(returns))
  }
  
  # If weights array contains less data than returns array, 
  # use equal weights allocation for missing portion
  time.diff <- setdiff(index(returns),index(weights))
  if(length(time.diff) > 0)
  {
    weights.remainder <- xts(matrix(rep(1/nrMarginals, nrMarginals),
                                    ncol=nrMarginals,nrow=length(time.diff)), order.by=time.diff)
    weights <- rbind(weights.remainder, weights)
  }
  
  nrIterations <- floor((nrow(returns)-calibration.interval)/refit.every)
  
  cl <- makeCluster(nrCores); registerDoParallel(cl)
  clusterEvalQ(cl,eval(parse("config/config.R")))
  garchSettings <- c("garch.model", "garch.order", 
                     "arma.order", "include.mean", "distribution.model")
  clusterExport(cl,c("returns",
                     "weights",
                     "calibration.interval",
                     "refit.every",
                     "copula.simulations", 
                     "nrMarginals",
                     garchSettings), envir=environment())
  
  # Calibration of Garch-Copula model occurs at first timestamp of the iteration
  # Sigma and mu parameters for the marginals of the subsequent timestamps will be forecasted with
  # the calibrated models of the first timestamp (Taking the incoming new datapoints into account)
  backtest.VaR.ES <- foreach(it=1:nrIterations) %dopar%
  {
    
    #res <- tryCatch({
      print(paste("Iteration:",it))
      # Beginning of calibration period
      is.start = (it-1)*refit.every + 1
      # End of the calibration period
      is.end = (it-1)*refit.every + calibration.interval
      # End of the out of sample period (depends on frequency of recalibration)
      os.end = (it-1)*refit.every + calibration.interval + refit.every
      
      # Complete interval for the current iteration
      interval.complete = seq(is.start,os.end)
      # Calibration interval for the current iteration
      interval.is = seq(is.start,is.end)
      # Out of sample interval for which VaR / ES (and garch Mu/Sigma) will be predicted
      interval.os = seq(is.end+1,os.end)
      
      # Calibrate models for the marginals
      calibrated.garch.models <- lapply(1:nrMarginals, function(x){ 
                                        GarchCalibration(returns=returns[interval.is,x], garch.model=garch.model, 
                                              garch.order=garch.order, arma.order=arma.order,
                                              include.mean=include.mean, distribution.model=distribution.model)})
      
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
      if(clayton.simulations > 0 & gumbel.simulations > 0)
        copula.mixed.rcop <- rbind(copula.clayton.rcop, copula.gumbel.rcop)
      else if(clayton.simulations > 0)
        copula.mixed.rcop <- copula.clayton.rcop
      else
        copula.mixed.rcop <- copula.gumbel.rcop
      
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
      # Note: forecast values are alligned/lagged to the actual date for which the forecast applies
      # Use refit.every-1 rolls to forecast the complete out of sample interval of size refit.every 
      forecasts <- lapply(1:nrMarginals, function(x){
                            ugarchforecast(garch.spec[[x]], 
                                n.ahead = 1, n.roll = refit.every-1, data=returns[interval.complete,x], 
                                out.sample=refit.every)})
      
      # Extract conditional volatility forecasts
      forecasts.sigma = lapply(1:nrMarginals, function(x){
                xts(as.vector(rugarch:::sigma(forecasts[[x]])),index(returns[interval.os,x]))})
      # Unlist the sigma forecasts and convert to xts
      forecasts.sigma = xts(matrix(unlist(forecasts.sigma),ncol=nrMarginals),
                            order.by=index(returns[interval.os,]))
      
      # Extract conditional mean forecasts
      forecasts.mu = lapply(1:nrMarginals, function(x){
                xts(as.vector(rugarch:::fitted(forecasts[[x]])),index(returns[interval.os,x]))})
      # Unlist the mean forecasts and convert to xts
      forecasts.mu = xts(matrix(unlist(forecasts.mu),ncol=nrMarginals),
                            order.by=index(returns[interval.os,]))
      
      # Get weights for the out of sample timestamps
      iteration.weights <- window(weights,start=index(returns[is.end+1]),
                                  end=index(returns[os.end]))
      # Calculate amount of simulation outliers that fall "below the VaR level"
      simulation.outliers <- floor(level*copula.simulations)
      
      # Calculate VaR and ES for the out of sample timestamps
      results.VaR.ES <- lapply(1:refit.every, function(x)
      {
        # Use the forecasted parameters to determine the marginal returns of the simulations
        returns.marginals <- as.vector(forecasts.mu[x,]) + t(t(coredata(qMarginals)) * as.vector(forecasts.sigma[x,]))
        # Multiply with weight matrix
        returns.weighted <- returns.marginals %*% as.vector(iteration.weights[x,])
        # Fetch outlier values
        returns.outliers <- head(sort(returns.weighted), simulation.outliers)
        
        # VaR is the largest outliers (corresponding to the level)
        VaR <- tail(returns.outliers,1)
        # ES is calculated as median value of the outliers
        ES <- mean(returns.outliers)
        
        if(length(VaR)==0){ VaR=-Inf; ES=-Inf}
        c(VaR,ES)
      })
    
      os.timestamps <- index(returns[interval.os,])
      # Extract VaR, convert to xts
      VaR <- xts(sapply(results.VaR.ES,"[[",1), order.by=os.timestamps)
      # Extract ES, convert to XTS
      ES <- xts(sapply(results.VaR.ES,"[[",2), order.by=os.timestamps)
      
      cbind(VaR,ES)
    #}, error=function(e){ # Catch potential numerical instabilities / crashes
    #    # End of the calibration period
    #    is.end = (it-1)*refit.every + calibration.interval
        # End of the out of sample period (depends on frequency of recalibration)
    #    os.end = (it-1)*refit.every + calibration.interval + refit.every
        # Out of sample interval
    #    interval.os = seq(is.end+1,os.end)
        
    #    returns.os <- returns[interval.os,]
        # Return all NA for VaR/ES when problem
    #    xts(matrix(NA, ncol=2, nrow=nrow(returns.os)), order.by=index(returns[interval.os,]))
    #})
  }
  stopCluster(cl)
  
  # Merge results
  backtest.VaR.ES = do.call("rbind", backtest.VaR.ES)
  
  return(backtest.VaR.ES)
}