############################################################################
##### Perform mixed Clayton-Gumbel calibration on the input return series ##
############################################################################
CopulaCalibration <- function(returns=NULL, assets=NULL)
{
  if(is.null(returns))
    returns <- na.omit((asset/lag(asset, k= 1) - 1) * 100)
  nrMarginals <- ncol(returns)
  
  # Calculate univariate probabilities
  U <- sapply(1:nrMarginals, function(x){ pobs(returns[,x])})
  
  # Initialize Clayton copula
  claytonCopula <- claytonCopula(2, dim=nrMarginals)
  # Initialize Gumbel copula
  gumbelCopula <- gumbelCopula(2, dim=nrMarginals)
  
  # Fit Clayton copula to the marginal residuals (useful for initial param estimate)
  clayton.fit <- tryCatch(
    fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
              method="mpl", copula=claytonCopula, estimate.variance=FALSE),
    error = function(e){ # Use itau approximation when mpl fails to converge
      fitCopula(data=U, optim.method="Nelder-Mead", optim.control = list(Hessian=FALSE),
                method="itau", copula=claytonCopula, estimate.variance=FALSE)
    }
  )
  
  # Fit Gumbel copula to the marginals (useful for initial param estimate)
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
  
  return(c(copula.mixed.parameters, nrMarginals))
}

############################################################################
##### Generate samples from the mixed Clayton-Copula distribution or  ######
##### its underlying return marginals (depending on input settings)   ######
############################################################################
CopulaSampling <- function(copula.mixed.parameters, nrSamples, 
                            simulate.marginals=FALSE, marginals=NULL)
{
  # Extract clayton param
  parameter.clayton = copula.mixed.parameters[1]
  # Extract Gumbel param
  parameter.gumbel = copula.mixed.parameters[2]
  # Extract mixing probab
  mixing.prob = copula.mixed.parameters[3]
  # Extract copula dimension
  nrMarginals <- copula.mixed.parameters[4]

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
    copula.sim <- rbind(copula.clayton.rcop, copula.gumbel.rcop)
  else if(clayton.simulations > 0)
    copula.sim <- copula.clayton.rcop
  else
    copula.sim <- copula.gumbel.rcop
  
  # Simulate the marginals
  if(simulate.marginals)
  {
    marginals.sim <- sapply(1:nrMarginals, function(x) 
                                { quantile(marginals[,x], copula.sim[,x]) })
    return(marginals.sim)
  }
  else
    return(copula.sim)
}