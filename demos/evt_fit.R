############################################################################
### Extreme Value Theory: Tail distribution modeling of asset returns  #####
############################################################################
readline(paste("Press enter to calibrate the GEV model to SPY returns data.."))
GEVFit(SPY)
readline(paste("Press enter to calibrate the GEV model to AGS returns data.."))
GEVFit(AGS)
readline(paste("Press enter to calibrate the GEV model to ACKB returns data.."))
GEVFit(ACKB)

readline(paste("Press enter to calibrate the GPD model to SPY returns data.."))
GPDFit(SPY)
readline(paste("Press enter to calibrate the GPD model to AGS returns data.."))
GPDFit(AGS)
readline(paste("Press enter to calibrate the GPD model to ACKB returns data.."))
GPDFit(ACKB)