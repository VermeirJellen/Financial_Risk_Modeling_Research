############################################################################
### Load necessary packages and functions into memory                  #####
############################################################################
# if(!require(installr)) {install.packages("installr"); require(installr)}
# updateR()
packages <- c("RMySQL", "fBasics", "evir", "ghyp", "timeSeries", 
              "zoo", "xts", "quantmod", "fExtremes", "QRM", "fGarch", 
              "QRM", "copula", "quadprog", "rrcov", "PerformanceAnalytics", 
              "rugarch", "FRAPO", "doParallel", "fPortfolio", "ismev", 
              "MASS", "matrixcalc", "lpSolve")

packages <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

Sys.setenv(TZ='UTC')

source("facade/DBFacade.R")

source("functions/FetchData.R")
source("functions/StylizedFeatures.R")
source("functions/GHDFit.R")
source("functions/GHDBacktest.R")
source("functions/EVTFit.R")

source("functions/GarchCalibration.R")
source("functions/GarchRiskForecasts.R")
source("functions/GarchBacktest.R")

source("functions/CopulaCalibration.R")
source("functions/GarchCopulaCalibration.R")
source("functions/GarchCopulaRiskForecasts.R")
source("functions/GarchCopulaBacktest.R")

source("functions/PerformanceMeasurement.R")
source("functions/PlotStrategyResults.R")
source("functions/VaRTargetingUnivariate.R")
source("functions/VaRTargetingUnivariateBacktest.R")
source("functions/VaRTargetingMultivariate.R")
source("functions/VaRTargetingMultivariateBacktest.R")

source("functions/EMACrossoverStrategyBacktest.R")