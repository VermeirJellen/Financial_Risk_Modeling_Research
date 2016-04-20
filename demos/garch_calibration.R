############################################################################
## Calibrate eGARCH model to asset return series: Investigate properties  ##
############################################################################
par(mfrow=c(2,2))
assets <- SnPIndices[,c("XLE","XLU","XLK","XLB")]
readline(promp="Press enter to start egarch calibration for 4 SPDR etf return series..")
results <- lapply(1:ncol(assets), function(x) GarchPlotQQ(assets[,x]))

assets <- BEL20[,c("ACKB","AGS","ABI","DELB")]
readline(promp="Press enter to start egarch calibration for BEL20 stock return series..")
results <- sapply(1:ncol(assets), function(x) GarchPlotQQ(assets[,x]))