############################################################################
### Perform GHD out of sample VaR / ES forecasting backtest             ####
############################################################################
GHDBacktest <- function(asset, calibration.interval = 252, 
                        alpha=0.01, nrCores=detectCores(), 
                        test.confLevel=0.95, printResults=FALSE)
{
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  assetName <- names(asset)
  
  backtest.to <- calibration.interval:nrow(returns)
  backtest.from <- 1:length(backtest.to)
  
  # Set up the cluster for parallel computation
  cl <- makeCluster(nrCores); registerDoParallel(cl)
  clusterEvalQ(cl,eval(parse("config/config.R")))
  invisible(
    clusterExport(cl,c("returns",
                       "calibration.interval",
                       "alpha"),
                  envir=environment())
  )
  # Calculate VaR/ES results
  backtest.results <- foreach(i=1:length(backtest.to),.combine=rbind) %dopar%
  {
    # Return list of NA's if numerical instability (crash)
    res <- tryCatch({
      x <- returns[backtest.from[i]:backtest.to[i]]
      
      ghdfit <- fit.ghypuv(x, symmetric = FALSE, 
                           control = list(maxit = 1000), silent=TRUE)
      ghd.VaR <- abs(ghyp:::qghyp(alpha, ghdfit))
      ghd.ES <- abs(ghyp:::ESghyp(alpha, ghdfit))
      
      nor.VaR <- abs(qnorm(alpha, mean(x), sd(x)))
      nor.ES <- abs(mean(x) - sd(x) * dnorm(qnorm(1 - alpha)) / alpha)
      
      c(ghd.VaR,nor.VaR,ghd.ES,nor.ES)
    }, error = function(e){c(NA,NA,NA,NA)})
  }
  stopCluster(cl)
  
  backtest.returns <- tail(returns,length(backtest.to))
  
  # Extract VaR from results
  backtest.VaR <- na.omit(lag(xts(backtest.results[,c(1,2)],
                              order.by=index(backtest.returns)),1))
  # Extract ES from results
  backtest.ES <- na.omit(lag(xts(backtest.results[,c(3,4)],
                             order.by=index(backtest.returns)),1))
  backtest.returns <- backtest.returns[index(backtest.ES)]
  # Merge results in 1 XTS object
  backtest.allResults <- cbind(backtest.returns, -backtest.ES, -backtest.VaR)
  
  
  par(mfrow=c(2,1))
  colnames(backtest.allResults) <- c("Returns","ghd.ES","nor.ES","ghd.VaR", "nor.VaR")
  
  ## Plot VaR results
  plot.max <- 0
  plot.min <- min(backtest.allResults[,"Returns"])
  plot(backtest.allResults[,"Returns"], type = "p", xlab = "Time", 
       main=paste("VaR ", (1-alpha)*100 ,"% - GHD versus Normal (",assetName,")",sep=""),
       ylab = "Percentage Returns", pch = 19, cex = 0.5,ylim = c(plot.min,plot.max))
  abline(h = 0, col = "grey")
  lines(backtest.allResults[, "ghd.VaR"], col = "blue", lwd = 2)
  lines(backtest.allResults[, "nor.VaR"], col = "red", lwd = 2)
  legend("bottomleft", legend = c("Returns", "VaR GHD", "VaR Normal"),
         col = c("black", "blue", "red"),lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")
  
  # Plot ES results
  plot(backtest.allResults[, "Returns"], type = "p", xlab = "Time", 
       main=paste("ES " , (1-alpha)*100,"% - GHD versus Normal (",assetName,")",sep=""),
       ylab = "Percentage Returns", pch = 19, cex = 0.5,ylim = c(plot.min,plot.max))
  abline(h = 0, col = "grey")
  lines(backtest.allResults[, "ghd.ES"], col = "blue", lwd = 2)
  lines(backtest.allResults[, "nor.ES"], col = "red", lwd = 2)
  legend("bottomleft", legend = c("Returns", "ES GHD", "ES Normal"),
         col = c("black", "blue", "red"),lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")
  
  ghd.VaRTest = VaRTest(alpha = alpha, VaR = backtest.allResults[, "ghd.VaR"], 
                        actual = backtest.allResults[, "Returns"])
  ghd.ESTest = ESTest(alpha = alpha, actual = backtest.allResults[, 'Returns'], 
                      ES = backtest.allResults[, "ghd.ES"],VaR = backtest.allResults[, "ghd.VaR"], 
                      conf.level = test.confLevel)
  
  nor.VaRTest = VaRTest(alpha = alpha, VaR = backtest.allResults[, "nor.VaR"], 
                        actual = backtest.allResults[, "Returns"])
  nor.ESTest = ESTest(alpha = alpha, actual = backtest.allResults[, 'Returns'], 
                      ES = backtest.allResults[, "nor.ES"], VaR = backtest.allResults[, "nor.VaR"], 
                      conf.level = test.confLevel)
  
  if(printResults)
  {
    readline(prompt=paste("Press enter to display Normal VaR results for", names(asset)))
    print(nor.VaRTest)
    readline(prompt=paste("Press enter to display Normal ES results for", names(asset)))
    print(nor.ESTest)
    readline(prompt=paste("Press enter to display GHD VaR results for", names(asset)))
    print(ghd.VaRTest)
    readline(prompt=paste("Press enter to display GHD ES results for", names(asset)))
    print(ghd.ESTest)
  }
  
  return(list(backtest.allResults,
              ghd.VaRTest,
              ghd.ESTest,
              nor.VaRTest,
              nor.ESTest))
}