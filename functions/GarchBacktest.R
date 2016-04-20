############################################################################
##### Perform EGARCH out of sample VaR / ES forecasting  backtest     ######
############################################################################
GarchBacktest <- function(returns=NULL, asset=NULL, level=0.01, calibration.interval = 375, 
                          model.spec="eGARCH", distribution.model="sstd", model.garchOrder = c(1,1), 
                          arma.order=c(1,1), include.mean=TRUE,
                          refit.every=10, test.conf.level=0.95,
                          plotOption=FALSE, printStats=FALSE, nrCores=detectCores())
{
  if(is.null(returns))
    returns <- na.omit((asset/lag(asset, k= 1) - 1) * 100)
  
  # amount of out of sample datapoints
  samples.oos <- nrow(returns)-calibration.interval
  # Get out of sample timestamps
  timestamps.oos <- tail(index(returns), samples.oos)
  
  # Initialize model specification object
  spec = ugarchspec(variance.model = list(model=model.spec, garchOrder=model.garchOrder), 
                    mean.model=list(armaOrder=arma.order, include.mean=include.mean),
                    distribution.model=distribution.model)
  
  cluster = makePSOCKcluster(nrCores)
  # Perform rolling forecasts for the conditional mean and volatility
  roll = ugarchroll(spec, returns, forecast.length=samples.oos, 
                    refit.every=refit.every, refit.window='moving', window.size=calibration.interval, 
                    calculate.VaR=FALSE, keep.coef=TRUE, cluster=cluster)
  stopCluster(cluster)
  # Extract density function parameters
  garch.density <- xts(as.data.frame(roll, which='density'), timestamps.oos)
  
  
  # Helper function that is used to determine ES
  # Determine quantiles for standardized residuals
  qdist.function = function(x, skew, shape, lambda){
                         qdist(distribution.model, p=x, mu=0, sigma=1, 
                         skew=skew, shape=shape, lambda=lambda)}
  
  
  cl <- makeCluster(nrCores); registerDoParallel(cl)
  clusterEvalQ(cl, eval(parse("config/config.R")))
  clusterExport(cl,c("garch.density",
                     "distribution.model",
                     "level", "qdist.function"), envir=environment())
  # Calculate VaR and ES
  backtest.VaR.ES <- foreach(index=1:nrow(garch.density), .combine="rbind") %dopar%
  {
    VaR <- garch.density[index,'Mu'] + garch.density[index, 'Sigma']*
                                       qdist(distribution.model, level, 0, 1,
                                             skew=garch.density[index, 'Skew'], 
                                             shape=garch.density[index, 'Shape'],
                                             lambda=garch.density[index, 'Shape(GIG)'])
    ES <- garch.density[index, 'Mu'] + 
          garch.density[index, 'Sigma']*integrate(qdist.function, 0, level, skew=garch.density[index, 'Skew'], 
                                                  shape=garch.density[index, 'Shape'], 
                                                  lambda=garch.density[index, 'Shape(GIG)'])$value/level
    cbind(VaR, ES)
  }
  stopCluster(cl)
  
  # extract VaR
  VaR <- backtest.VaR.ES[,1]
  # Extract ES
  ES <- backtest.VaR.ES[,2]
  
  if(plotOption)
  {
    oldPar <- par(col="blue")
    plot(garch.density$Realized,xlab="Time",ylab="returns",type="p",
         main=paste(names(returns), 
                    " - Realized returns VS forecasted VaR/ES (",(1-level)*100,"%)",sep=""))
    par(col="green"); lines(VaR)
    par(col="red"); lines(ES)
    par(oldPar)
    legend("topleft",legend = c("Returns", 
                                paste("VaR (",(1-level)*100,"%)",sep=""),
                                paste("ES (",(1-level)*100,"%)",sep="")),
                                lty = 1,col=c("blue","green","red"))
  }
  if(printStats)
  {
    readline(prompt=paste("Press enter to execute VaRTest for", names(returns),".."))
    VaR.test <- VaRTest(alpha=level, VaR=VaR, actual=garch.density[,'Realized'])
    print(VaR.test)
    
    readline(prompt=paste("Press enter to execute ESTest for",names(returns),".."))
    ES.test <- ESTest(alpha=level, actual=garch.density[,'Realized'], 
                      ES=ES, VaR=VaR, conf.level=test.conf.level)
    print(ES.test)
  }
  return(cbind(VaR,ES))
}