############################################################################
### Calibrate GHD models to returns data of the input asset             ####
############################################################################
fitGHD <- function(asset=NULL, returns=NULL, printStats=FALSE)
{
  print(paste("Calibrating GHD models to",names(asset),"returns data.."))
  if(is.null(returns))
    returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  
  ef <- density(returns)
  ghdfit <- fit.ghypuv(returns, symmetric = FALSE,
                       control = list(maxit = 1000), silent=TRUE)
  hypfit <- fit.hypuv(returns, symmetric = FALSE,
                      control = list(maxit = 1000), silent=TRUE)
  nigfit <- fit.NIGuv(returns, symmetric = FALSE,
                      control = list(maxit = 1000), silent=TRUE)
  
  if(printStats)
  {
    ## Compare maximum likelihood, AIC, convergence..
    AIC <- stepAIC.ghyp(returns, dist = c("ghyp", "hyp","NIG"),
                        symmetric = FALSE,
                        control = list(maxit = 1000), silent=TRUE)
    print(AIC$fit.table)
  
    readline(prompt="Press enter to display LRT: ghd versus nig..")
    LRghdnig <- lik.ratio.test(ghdfit, nigfit)
    print(LRghdnig)
  
    readline(prompt="Press enter to display LRT: ghd versus hyp..")
    LRghdhyp <- lik.ratio.test(ghdfit, hypfit)
    print(LRghdnig)
  }
  
  return(list(ghdfit,hypfit,nigfit))
}

############################################################################
### Plot GHD densities versus empirical data                            ####
############################################################################
plotDensityGHD <- function(asset, ghdFits=NULL)
{
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  ef <- density(returns)
  
  if(is.null(ghdFits))
  {
    ghdfit <- fit.ghypuv(returns, symmetric = FALSE,
                         control = list(maxit = 1000),silent=TRUE)
    hypfit <- fit.hypuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
    nigfit <- fit.NIGuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
  }
  else
  {
    ghdfit <- ghdFits[[1]]
    hypfit <- ghdFits[[2]]
    nigfit <- ghdFits[[3]]
  }
  
  # Estimate densities: 
  # Use n coordinates where the density is estimated (by kernel density)
  ghddens <- ghyp:::dghyp(ef$x, ghdfit)
  hypdens <- ghyp:::dghyp(ef$x, hypfit)
  nigdens <- ghyp:::dghyp(ef$x, nigfit)
  nordens <- dnorm(ef$x, mean = mean(returns), sd = sd(returns))
  
  assetName <- names(asset)
  col.def <- c("black", "red", "blue", "green", "orange")
  plot(ef, xlab = "Returns", ylab = expression(f(x)), ylim = c(0, 0.55),
       main = paste("Return density - Normal versus GHD (",assetName,")",sep=""))
  lines(ef$x, ghddens, col = "red")
  lines(ef$x, hypdens, col = "blue")
  lines(ef$x, nigdens, col = "green")
  lines(ef$x, nordens, col = "orange")
  legend("topleft", col = col.def, lty = 1,
         legend = c("empirical", "GHD", "HYP", "NIG", "NORM"))
}

############################################################################
### Generate GHD QQ Plots                                               ####
############################################################################
plotQQGHD <- function(asset, ghdFits=NULL)
{
  assetName <- names(asset)
  if(is.null(ghdFits))
  {
    returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
    ghdfit <- fit.ghypuv(returns, symmetric = FALSE,
                         control = list(maxit = 1000),silent=TRUE)
    hypfit <- fit.hypuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
    nigfit <- fit.NIGuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
  }
  else
  {
    ghdfit <- ghdFits[[1]]
    hypfit <- ghdFits[[2]]
    nigfit <- ghdFits[[3]]
  }
  
  ## QQ-Plots
  col.def <- c("black", "red", "blue", "green", "orange")
  qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE, gaussian = FALSE, 
         main = paste("QQ plot - GHD versus sample returns (",assetName,")",sep=""), cex = 0.8)
  qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
         gaussian = FALSE, line = FALSE, cex = 0.8)
  qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
         gaussian = FALSE, line = FALSE, cex = 0.8)
  legend("topleft", legend = c("GHD", "HYP", "NIG"),
         col = col.def[-c(1,5)], pch = 1:3)
}

############################################################################
### Plot VaR for GHD and normal destribution                            ####
############################################################################
plotVaRGHD <- function(asset,ghdFits=NULL,confidence=95)
{
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  assetName <- names(asset)
  
  if(is.null(ghdFits))
  {
    ghdfit <- fit.ghypuv(returns, symmetric = FALSE,
                         control = list(maxit = 1000),silent=TRUE)
    hypfit <- fit.hypuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
    nigfit <- fit.NIGuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
  }
  else
  {
    ghdfit <- ghdFits[[1]]
    hypfit <- ghdFits[[2]]
    nigfit <- ghdFits[[3]]
  }
  
  # Sequence for which quantiles must be determined
  probabilities <- seq(0.001, 1-confidence/100, 0.001)
  
  ## VaR calculation
  ghd.VaR <- abs(qghyp(probabilities, ghdfit))
  hyp.VaR <- abs(qghyp(probabilities, hypfit))
  nig.VaR <- abs(qghyp(probabilities, nigfit))
  nor.VaR <- abs(qnorm(probabilities, mean = mean(returns), sd = sd(returns)))
  emp.VaR <- abs(quantile(x = returns, probs = probabilities)) 
  
  col.def <- c("black", "red", "blue", "green", "orange")
  plot(emp.VaR, type = "l", ylab = "VaR", axes = FALSE,
       ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)),
       xlab = "Probability",
       main = paste("VaR - Normal versus GHD (",assetName,")",sep=""))
  box()
  axis(1, at = seq(along = probabilities), labels = names(emp.VaR), tick = FALSE)
  axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,nig.VaR, nor.VaR)))
  lines(seq(along = probabilities), ghd.VaR, col = "red")
  lines(seq(along = probabilities), hyp.VaR, col = "blue")
  lines(seq(along = probabilities), nig.VaR, col = "green")
  lines(seq(along = probabilities), nor.VaR, col = "orange")
  col.def <- c("black", "red", "blue", "green", "orange")
  legend("topright",
         legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
         col = col.def, lty = 1)
}


############################################################################
### Plot expected shortfall for GHD and normal models                   ####
############################################################################
plotESGHD <- function(asset, ghdFits=NULL, confidence=95)
{
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  assetName <- names(asset)
  
  if(is.null(ghdFits))
  {
    ghdfit <- fit.ghypuv(returns, symmetric = FALSE,
                         control = list(maxit = 1000),silent=TRUE)
    hypfit <- fit.hypuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
    nigfit <- fit.NIGuv(returns, symmetric = FALSE,
                        control = list(maxit = 1000),silent=TRUE)
  }
  else
  {
    ghdfit <- ghdFits[[1]]
    hypfit <- ghdFits[[2]]
    nigfit <- ghdFits[[3]]
  }
  
  # Sequence for which quantiles must be determined
  probabilities <- seq(0.001, 1-confidence/100, 0.001)
  
  ## Expected Shortfall
  ghd.ES <- abs(ESghyp(probabilities, ghdfit))
  hyp.ES <- abs(ESghyp(probabilities, hypfit))
  nig.ES <- abs(ESghyp(probabilities, nigfit))
  nor.ES <- abs(mean(returns) - sd(returns) *
                  dnorm(qnorm(1 - probabilities)) / probabilities)
  
  nrObservationsTail <- ceiling(probabilities * length(returns))
  emp.VaR <- abs(quantile(x = returns, probs = probabilities)) 
  emp.ES <- sapply(nrObservationsTail, 
                   function(x) abs(mean(sort(as.vector(returns))[1:x])))
  
  plot(emp.ES, type = "l",ylab = "ES", axes = FALSE,
       ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES)),
       xlab = "Probability",
       main = paste("ES - Normal versus GHD (",assetName,")",sep=""))
  box()
  axis(1, at = 1:length(probabilities), labels = names(emp.VaR), tick = FALSE)
  axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))
  lines(1:length(probabilities), ghd.ES, col = "red")
  lines(1:length(probabilities), hyp.ES, col = "blue")
  lines(1:length(probabilities), nig.ES, col = "green")
  lines(1:length(probabilities), nor.ES, col = "orange")
  col.def <- c("black", "red", "blue", "green", "orange")
  legend("topright",
         legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
         col = col.def, lty = 1)
}