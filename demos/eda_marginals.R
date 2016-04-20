############################################################################
### Exploratory data analysis for the SPDR ETF and BEL20 stock portfolio  ##
############################################################################
readline(prompt="Press enter to plot index timeseries..")
par(mfrow=c(2,1))
plot(time(SPY),coredata(SPY), main="SPY",type="l",xlab="Time",ylab="Price")
plot(time(BEL),coredata(BEL), main="BEL",type="l",xlab="Time",ylab="Price")


readline(prompt="Press enter to plot SPDR ETF timeseries..")
SPDR <- SnPIndices[,-1]
nrAssets <- ncol(SPDR)
SPDR.returns <- na.omit((SPDR / lag(SPDR, k= 1) - 1) * 100)
par(mfrow=c(rep(ceiling(sqrt(nrAssets)),2)))
res <- sapply(1:nrAssets, function(x) plot(time(SPDR[,x]),
                                           coredata(SPDR[,x]), main=names(SPDR)[x],type="l",
                                           xlab="Time",ylab="Price"))

readline(prompt="Press enter to plot SPDR ETF returns boxplot..")
par(mfrow=c(1,1))
boxplot(coredata(SPDR.returns), main="SPDR ETFs - Boxplot of returns", ylab="Returns (%)") 
print(rbind(apply(coredata(SPDR.returns),2,summary), 
                  skewness(SPDR.returns), kurtosis(SPDR.returns)))


readline(prompt="Press enter to plot BEL20 stock timeseries..")
BEL20Stocks <- BEL20[,-15]
nrAssets <- ncol(BEL20Stocks)
BEL20Stocks.returns <- na.omit((BEL20Stocks / lag(BEL20Stocks, k= 1) - 1) * 100)
par(mfrow=c(rep(ceiling(sqrt(nrAssets)),2)))
sapply(1:nrAssets, function(x) plot(time(BEL20Stocks[,x]),
                                    coredata(BEL20Stocks[,x]), main=names(BEL20Stocks)[x],type="l",
                                    xlab="Time",ylab="Price"))

readline(prompt="Press enter to plot BEL20 stocks returns boxplot..")
par(mfrow=c(1,1))
boxplot(coredata(BEL20Stocks.returns), main="BEL20 stocks - Boxplot of returns", ylab="Returns (%)") 
print(rbind(apply(coredata(BEL20Stocks.returns),2,summary), 
            skewness(BEL20Stocks.returns), kurtosis(BEL20Stocks.returns)))