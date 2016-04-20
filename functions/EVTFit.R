############################################################################
##### Calibrate GEV model to univariate return timeSeries   ################
############################################################################
GEVFit <- function(asset, gev.blockSize=50)
{
  assetName <- names(asset)
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  losses <- -1*returns
  
  # Fit GEV model using the given blocksize
  GEV <- gev(losses, block = gev.blockSize)
  
  par(mfrow=c(3,1))
  # Assumption i.i.d blocks violated
  plot(GEV$data, type = "h", col = "blue",
       ylab = "Block Maxima (losses)",
       main = paste("Maximal losses for ",assetName," (blocksize = ",gev.blockSize,")",sep=""),
       xlab = "blocknumber")
  
  GEV2 <- gev.fit(GEV$data)
  
  # profile log likelihood (period = m*gev.blockSize)
  m=504/gev.blockSize
  gev.prof(GEV2, m = m, xlow = 3, xup = max(losses), conf = 0.95)
  gev.profxi(GEV2, xlow = 0.0, xup = 0.7, conf = 0.95)
  
  maxLoss <- max(GEV$data)
  maxYears <- 1 / (1 - evir::pgev(maxLoss, mu = GEV2$mle[1],
                                  sigma = GEV2$mle[2],
                                  xi = GEV2$mle[3])) / (252/gev.blockSize)
  cat(paste("\nThe middle graph shows the return level profile log likelihood for ",
              gev.blockSize*m," days (",names(asset),")",sep=""))
  cat(paste("\nAccording to the model, the maximum loss of ", round(maxLoss,2),
              "% Should only occur every ",round(maxYears,2)," years",sep=""))
}

############################################################################
##### Calibrate GPD model to univariate return timeSeries   ################
############################################################################
GPDFit <- function(asset, gpd.quantile=0.99)
{
  assetName <- names(asset)
  returns <- na.omit((asset / lag(asset, k= 1) - 1) * 100)
  losses <- -1*returns
  
  par(mfrow = c(1,1))
  mrlPlot(losses)
  
  mu <- quantile(losses,0.99)
  print(paste("Use ",gpd.quantile*100,"% loss quantile for gpdFit (mu=",round(mu,2),")",sep=""))
  BAFit <- gpdFit(as.vector(losses), u = mu)
  par(mfrow = c(2,2))
  plot(BAFit)
  
  print("Risk measures..")
  print(gpdRiskMeasures(BAFit, prob = c(0.95, 0.99, 0.995)))
}