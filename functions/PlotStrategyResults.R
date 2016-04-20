############################################################################
# Plot (multivariate) strategy results before and after VaR targeting     ##
# This also includes graphical output of the risk measurement forecasts   ##
############################################################################
# Function expects a backtest strategyResults list as input:
# strategyResults[[1]] = strategyReturns without VaR targeting
# strategyResults[[2]] = VaR series without VaR targeting
# strategyResults[[3]] = ES series without VaR targeting
# strategyResults[[4]] = scaling factor series (unused)
# strategyResults[[5]] = strategyReturns with VaR targeting
# strategyResults[[6]] = VaR series with VaR targeting
# strategyResults[[7]] = ES series with VaR targeting

PlotStrategyResults <- function(strategyResults, strategyName="Strategy", 
                                level=0.01, printStats=FALSE, test.conf.level=0.95)
{
  par(mfrow=c(2,2))
  
  returns.original <- strategyResults[[1]]
  VaR.original <- strategyResults[[2]]
  ES.original <- strategyResults[[3]]
  returns.original <- returns.original[index(VaR.original)]
  
  cumprod.original <- cumulativeReturns(returns.original)
  sharpe.original <- customSharpe(returns.original)
  maxDD.original <- maxDrawdown(returns.original/100)
  APR.original <- customAPR(returns.original)
  
  # 2. With VaR scaling
  returns.original.scaled <- strategyResults[[5]]
  VaR.scaled <- strategyResults[[6]]
  ES.scaled <- strategyResults[[7]]
  cumprod.scaled <- cumulativeReturns(returns.original.scaled)
  sharpe.scaled <- customSharpe(returns.original.scaled)
  maxDD.scaled <- maxDrawdown(returns.original.scaled/100)
  APR.scaled <- customAPR(returns.original.scaled)
  
  ylim <- c(min(cumprod.original),max(cumprod.original))
  plot(time(cumprod.original),coredata(cumprod.original), main=paste(strategyName, "- No VaR targeting"),
       xlab="Time",ylab="Cumulative Returns",type='l',col="black",ylim=ylim)
  legend("topleft",legend= c(paste("Sharpe:",round(sharpe.original,digits=2)),
                             paste("MaxDD: ", round(maxDD.original*100,digits=2),"%",sep=""),
                             paste("APR: ", round(APR.original,digits=2),"%",sep="")))
  
  plot(time(returns.original),coredata(returns.original),type='p',col="black",
       main=paste(strategyName, "- No VaR targeting"),
       xlab="Time",ylab="returns")
  lines(VaR.original,col="green")
  lines(ES.original,col="red")
  legend("topleft",legend = c("Returns", 
                              paste("VaR (",(1-level)*100,"%)",sep=""),
                              paste("ES (",(1-level)*100,"%)",sep="")),
         col = c("black","green","red"), lty = 1)
  
  ylim <- c(min(cumprod.scaled),max(cumprod.scaled))
  plot(time(cumprod.scaled), coredata(cumprod.scaled),col="black",
       main=paste(strategyName, "- With VaR targeting"),type='l',
       xlab="Time",ylab="Cumulative Returns", ylim=ylim)
  legend("topleft",legend= c(paste("Sharpe:",round(sharpe.scaled,digits=2)),
                             paste("MaxDD: ", round(maxDD.scaled*100,digits=2),"%",sep=""),
                             paste("APR: ", round(APR.scaled,digits=2),"%",sep="")))
  
  plot(time(returns.original.scaled), coredata(returns.original.scaled),type='p',
       main=paste(strategyName, "- With VaR targeting"),col="black",
       xlab="Time",ylab="returns")
  lines(VaR.scaled,col="green")
  lines(ES.scaled,col="red")
  legend("topleft",legend = c("Returns", 
                              paste("VaR (",(1-level)*100,"%)",sep=""),
                              paste("ES (",(1-level)*100,"%)",sep="")),
                              col = c("black","green","red"), lty = c(1,1,1))
  
  if(printStats)
  {
    readline(prompt=paste("Press enter to execute VaRTest for", strategyName,".."))
    test.VaR <- VaRTest(alpha=level, VaR=VaR.original, actual=returns.original)
    print(test.VaR)
  
    readline(prompt=paste("Press enter to execute ESTest for", strategyName,".."))
    test.ES <- ESTest(alpha=level, actual=returns.original, 
                    ES=ES.original, VaR=VaR.original, conf.level=test.conf.level)
    print(test.ES)
  }
}

############################################################################
# Plot and compare trading performance results of 2 return series objects ##
############################################################################
plotReturns <- function(returns, strategyName="Strategy",
                        returns.compare=NULL, strategyName.compare=NULL)
{
  par(mfrow=c(1,1))
  if(!is.null(returns.compare))
    par(mfrow=c(2,1))
  
  cumprod <- cumulativeReturns(returns)
  sharpe <- customSharpe(returns)
  maxDD <- maxDrawdown(returns/100)
  APR <- customAPR(returns)
  
  ylim <- c(min(cumprod), max(cumprod))
  plot(time(cumprod), coredata(cumprod), main=paste(strategyName),
       xlab="Time",ylab="Cumulative Returns",type='l',col="black",ylim=ylim)
  legend("topleft",legend= c(paste("Sharpe:",round(sharpe, digits=2)),
                             paste("MaxDD: ", round(maxDD*100, digits=2),"%",sep=""),
                             paste("APR: ", round(APR, digits=2),"%",sep="")))
  
  if(!is.null(returns.compare))
  {
    cumprod <- cumulativeReturns(returns.compare)
    sharpe <- customSharpe(returns.compare)
    maxDD <- maxDrawdown(returns.compare/100)
    APR <- customAPR(returns.compare)
    
    ylim <- c(min(cumprod), max(cumprod))
    plot(time(cumprod), coredata(cumprod), main=paste(strategyName.compare),
         xlab="Time",ylab="Cumulative Returns",type='l',col="black",ylim=ylim)
    legend("topleft",legend= c(paste("Sharpe:",round(sharpe, digits=2)),
                               paste("MaxDD: ", round(maxDD*100, digits=2),"%",sep=""),
                               paste("APR: ", round(APR, digits=2),"%",sep="")))
  }
}