############################################################
############## Calculate Logreturns #######################
############################################################
getLogReturnsAsTimeSeries <- function(x)
{
  logReturns <- log(x/lag(x,1))
  logReturns <- timeSeries::as.timeSeries(logReturns[which(complete.cases(logReturns))])
  
  return(logReturns)
}

getLogReturns <- function(x)
{
  return(log(x/lag(x,1)))
}

############################################################
############## Univariate Stylized Facts ###################
############################################################
# Plot return properties: timeseries, boxplot, ACF and PACF
stylizedFactsI <- function(x)
{
  logReturns <- getLogReturnsAsTimeSeries(x)
  
  par(mfrow = c(2,2))
  fBasics::seriesPlot(logReturns, title = FALSE, 
             main = paste("Daily Returns of",names(x)),col = "blue")
  fBasics::boxPlot(logReturns, title = FALSE, 
          main = "Box plot of Returns",col = "blue", cex = 0.5, pch = 19)
  acf(logReturns, main = "ACF of Returns", 
      lag.max = 20, ylab = "",xlab = "", col = "blue", ci.col = "red")
  pacf(logReturns, main = "PACF of Returns", lag.max = 20, 
       ylab = "",xlab = "", col = "blue", ci.col = "red")
}

# Plot ACF / PACF of ABSOLUTE returns, QQ plot of returns, Volatility clustering
stylizedFactsII <- function(x)
{
  logReturns <- getLogReturnsAsTimeSeries(x)
  
  absReturns <- abs(logReturns)
  nrTailValues <- ceiling(0.025*length(logReturns))
  tailCutoff <- tail(sort(series(logReturns)),nrTailValues)[1]
  tailIdx <- which(absReturns > tailCutoff, arr.ind=TRUE)
  
  largestReturns <- timeSeries(rep(0,length(logReturns)),
                               charvec=time(logReturns))
  largestReturns[tailIdx,] <- absReturns[tailIdx]
  
  acf(absReturns, main = "ACF of Absolute Returns", lag.max = 20,
      ylab = "", xlab = "", col = "blue", ci.col = "red")
  pacf(absReturns, main = "PACF of Absolute Returns", lag.max = 20,
       ylab = "", xlab = "", col = "blue", ci.col = "red")
  qqnormPlot(logReturns, main = "QQ-Plot of Returns", title = FALSE,
             col = "blue", cex = 0.5, pch = 19)
  plot(largestReturns, type = "h", main = "Volatility Clustering (largest returns)",
       ylab = "", xlab = "", col = "blue")
}

############################################################
############## Multivariate Stylized Facts #################
############################################################
plotMultivariateTimeSeries <- function(series,plotReturns=FALSE)
{
  etfTickers <- names(series)
  etfString <- paste(names(etfs),collapse=" - ")
  
  if(plotReturns)
  {
    returns <- diff(log(series))*100
    returns <- as.zoo(returns[which(complete.cases(returns))])
    plot(returns,main=paste("Return Series (",etfString,")",sep=""), xlab="Time")
  }
  else
    plot(as.zoo(series),main=paste("Price Series (",etfString,")",sep=""),xlab="Time") 
}

# Plot ccf diagrams for returns and absolute returns
# Function Expects XTS object with three distinct timeseries as input
plotMultivariateCrossCorrelations <- function(series)
{
  if(ncol(series) != 3)
    stop(paste("Input: Expecting XTS object that contains",
               "price information for three distinct assets"))
  
  returns <- diff(log(series))*100
  returns <- as.zoo(returns[which(complete.cases(series))])
  sNames <- names(series)
  
  ## Cross correlations. One on one comparison.
  layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))
  ccf(returns[,1], returns[, 2], 
      ylab = "", xlab = "",lag.max = 20,na.action = na.pass,
      main = paste("Returns",sNames[1], "vs",sNames[2]))
  ccf(abs(returns)[, 1], abs(returns)[, 2], 
      ylab = "",xlab = "", lag.max = 20, na.action = na.pass,
      main = paste("Absolute returns",sNames[1], "vs",sNames[2]))
  
  ccf(returns[,1], returns[, 3], 
      ylab = "", xlab = "",lag.max = 20,na.action = na.pass,
      main = paste("Returns",sNames[1], "vs", sNames[3]))
  ccf(abs(returns)[, 1], abs(returns)[, 3], 
      ylab = "",xlab = "", lag.max = 20, na.action = na.pass,
      main = paste("Absolute returns", sNames[1], "vs", sNames[3]))
  
  ccf(returns[,2], returns[, 3], 
      ylab = "", xlab = "",lag.max = 20,na.action = na.pass,
      main = paste("returns", sNames[2], "vs",sNames[3]))
  ccf(abs(returns)[, 2], abs(returns)[, 3], 
      ylab = "",xlab = "", lag.max = 20, na.action = na.pass,
      main = paste("Absolute returns",sNames[2], "vs",sNames[3]))
}

############################################################################
# Plot contamporaneous correlations between timeseries                    ##
# (Function Expects XTS object with three distinct timeseries as input)   ##
############################################################################
plotMultivariateContemporaneousCorrelations <- function(series)
{
  if(ncol(series) != 3)
    stop(paste("Input: Expecting XTS object that contains",
               "price information for three distinct assets"))
  
  # Calculate cross correlations
  crossCor <- function(x){
    dim <- ncol(x)
    rcor <- cor(x)[lower.tri(diag(dim), diag = FALSE)]
    return(rcor)
  }
  
  returns <- diff(log(series))*100
  returns <- as.zoo(returns[which(complete.cases(series))])
  sNames <- names(series)
  
  
  cCrossCor <- rollapply(returns, width = 250, crossCor,
                         align = "right", by.column = FALSE)
  colnames(cCrossCor) <- c(paste(sNames[1],"and",sNames[2]), 
                           paste(sNames[1],"and",sNames[3]),
                           paste(sNames[2],"and",sNames[3]))
  plot(cCrossCor, main = "Contemporaneous Correlations", xlab = "Time") 
}