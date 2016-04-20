############################################################
############## Univariate Stylized Features ################
############################################################
promptStr <- "Press enter to generate next chart.."

# Illustrate return properties: timeseries, boxplot, ACF and PACF
readline(prompt="Executing script: Press enter to continue")
stylizedFactsI(SPY)

readline(prompt=promptStr)
stylizedFactsI(AGS)

SPYret <- getLogReturnsAsTimeSeries(SPY)
summary(SPYret);
# skewnesss(SPYret)
kurtosis(SPYret)

AGSret <- getLogReturnsAsTimeSeries(AGS)
summary(AGSret) 
# skewnesss(AGSret)
kurtosis(AGSret)

# Illustrate ACF / PACF of ABSOLUTE returns, 
# QQ plot of returns, Volatility clustering
readline(prompt=promptStr)
stylizedFactsII(SPY)

readline(prompt="Press enter to generate next chart..")
stylizedFactsII(AGS)

##############################################################
############# MULTIVARIATE STYLYZED FEATURES #################
##############################################################
# SPDR sector ETF (remove SPY)
etfs <- SnPIndices[,-1]
# Select three random ETFS
etfs <- etfs[,c(1,2,3)]

readline(prompt=promptStr)
plotMultivariateTimeSeries(etfs,plotReturns=FALSE)

readline(prompt=promptStr)
plotMultivariateTimeSeries(etfs,plotReturns=TRUE)

readline(prompt=promptStr)
# plot ccf. input timeseries must contain 3 assets
plotMultivariateCrossCorrelations(etfs)

readline(prompt=promptStr)
# plot cross correlations over time. Input must contain 3 assets
plotMultivariateContemporaneousCorrelations(etfs)