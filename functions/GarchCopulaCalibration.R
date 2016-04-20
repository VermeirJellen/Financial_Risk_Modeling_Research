############################################################################
##### Perform mixed Clayton-Gumbel calibration on the conditional     ######
##### probabilities of the Garch input models' standardized residuals ######
############################################################################
GarchCopulaCalibration <- function(garchFits)
{
  nrMarginals <- length(garchFits)
  
  # Get the standardized residuals
  standardResiduals <- lapply(1:nrMarginals, function(x) residuals(garchFits[[x]], standardize=TRUE))
  
  # Get conditional distribution for Garch models
  conditional.distribution <- sapply(1:nrMarginals, function(x) getspec(garchFits[[x]])@model$modeldesc$distribution)
  # get Garch skew parameter for conditional distributions
  skew <- sapply(1:nrMarginals, function(x) coef(garchFits[[x]])["skew"])
  # skew only applies for skewed distributions
  skew[which(is.na(skew))] <- 1
  # Get Garch shape parameters for conditional distributions
  shape <- sapply(1:nrMarginals, function(x) coef(garchFits[[x]])["shape"])
  # Get lambda parameter for conditional distributions
  lambda <- sapply(1:nrMarginals, function(x) coef(garchFits[[x]])["ghlambda"])
  # lambda only applies for ghyp / NIG, default = -0.5
  lambda[which(is.na(lambda))] <- -0.5
  
  # Calculate univariate probabilities for the Garch standardized residuals
  U <- sapply(1:nrMarginals, function(x){ pdist(distribution=conditional.distribution[x],
                                               standardResiduals[[x]], mu=0, sigma=1, 
                                               skew=skew[x], shape=shape[x], lambda=lambda[x])})
  
 # Initialize C layton copula
  claytonCopula <- claytonCopula(2, dim=nrMarginals)
  # Initialize Gumbel copula
  gumbelCopula <- gumbelCopula(2, dim=nrMarginals)
  
  # Fit Clayton copula to the standardized marginal residuals (useful for initial param estimate)
  clayton.fit <- tryCatch(
    fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
              method="mpl", copula=claytonCopula, estimate.variance=FALSE),
    error = function(e){ # Use itau approximation when mpl fails to converge
      fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
                method="itau", copula=claytonCopula, estimate.variance=FALSE)
    }
  )
  
  # Fit Gumbel copula to the standardized marginal residuals (useful for initial param estimate)
  gumbel.fit <- tryCatch(
    fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
              method="mpl", copula=gumbelCopula, estimate.variance=FALSE),
    error = function(e){ # Use itau approximation when mpl fails to converge
      fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
                method="itau", copula=gumbelCopula, estimate.variance=FALSE)
    }
  )
  
  # Log likelihood function for mixed copula
  copula.mixed.likelihood <- function(params, marginals)
  {
    # Initialize the claytoncopula
    copula.clayton <- claytonCopula(params[1], dim=nrMarginals)
    # Initialize the gumbel copula
    copula.gumbel <- gumbelCopula(params[2], dim=nrMarginals)
    # Parameter for mixing
    mixing.prob <- params[3]
    
    # Maximize log likelihood
    logLikelihood <- sum(log(mixing.prob*dCopula(copula=copula.clayton, u=marginals) + 
                               (1-mixing.prob)*dCopula(copula=copula.gumbel, u=marginals))) 
  }
  
  # Log likelihood function for mixed copula (more efficient, but problem at boundaries)
  # copula.mixed.likelihood <- function(params, marginals, 
  #                                     copula.clayton, copula.gumbel)
  #{
  #  slot(copula.clayton, "parameters") <- params[1]
  #  slot(copula.gumbel, "parameters") <- params[2]
  #  mixing.param <- params[3]
  #  logLikelihood <- sum(log(mixing.param*dCopula(copula=copula.clayton, u=marginals) + 
  #                             (1-mixing.param)*dCopula(copula=copula.gumbel, u=marginals))) 
  #  logLikelihood
  #}
  
  # Optimize the log likelihood of the mixed model 
  copula.mixed.parameters <- tryCatch({
    maximum.likelihood <- optim(c(clayton.fit@estimate, gumbel.fit@estimate, 0.5), 
                                fn=copula.mixed.likelihood, marginals=U, 
                                # copula.clayton=claytonCopula, copula.gumbel=gumbelCopula,
                                lower=c(claytonCopula@param.lowbnd, gumbelCopula@param.lowbnd, 0),
                                upper=c(claytonCopula@param.upbnd, gumbelCopula@param.upbnd, 1),
                                method="L-BFGS-B", hessian=TRUE,
                                control=list(fnscale=-1))
    if(maximum.likelihood$convergence != 0)
      warning("Mixed copula likelihood calibration did not converge")
    maximum.likelihood$par
    },
    error = function(x)
    {
      warning("Mixed copula likelihood calibration failed: Using best guess parameters")
      c(clayton.fit@estimate, gumbel.fit@estimate, 0.5)
    })
  
  return(copula.mixed.parameters)
}


############################################################################
##### Generate samples from the EGARCH-Clayton-Copula distribution or ######
##### its underlying standardized error marginals (input settings)    ######
############################################################################
GarchCopulaSampling <- function(copula.mixed.parameters, nrSamples=10000, 
                                simulate.marginals=FALSE, marginals=NULL)
{
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
  uniform.samples <- runif(nrSamples)
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
  
  if(simulate.marginals)
  {
    calibrated.garch.models <- marginals
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
    return(qMarginals)
  }
  else
    return(copula.mixed.rcop)
}