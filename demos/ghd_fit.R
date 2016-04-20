############################################################################
### Demo: Generalized Hyperbolic Distribution Calibration              #####
############################################################################
######## Init some strings that are used for printing output
strCalibrate <- "Press enter to calibrate GHD model to"
strDensity <- "Press enter to plot GHD density estimates for"
strQQ <- "Press enter to generate GHD QQ plot for"
strVaR <- "Press enter to generate GHD VaR plot for"
strES <- "Press enter to generate GHD ES plot for"

par(mfrow=c(1,1))

############################################################################
### Perform GHD calibration on SPY return data                         #####
############################################################################
asset <- SPY
readline(prompt=paste(strCalibrate, names(asset),"returns.."))
# Calibrate ghd, hyp and nig models to asset return data
modelFits <- fitGHD(asset)

readline(prompt=paste(strDensity, names(asset),"returns.."))
# Plot GHD densities versus empirical data
plotDensityGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strQQ, names(asset),"returns.."))
# Generate QQ plots
plotQQGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strVaR, names(asset),"returns.."))
# Plot VaR for GHD and normal destributions
plotVaRGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strES, names(asset),"returns.."))
# Plot ES for GHD and normal destributions
plotESGHD(asset, ghdFits=modelFits)


############################################################################
### Perform GHD calibration on AGS return data                         #####
############################################################################
asset <- AGS
readline(prompt=paste(strCalibrate, names(asset),"returns.."))
modelFits <- fitGHD(asset)

readline(prompt=paste(strDensity, names(asset),"returns.."))
plotDensityGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strQQ, names(asset),"returns.."))
plotQQGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strVaR, names(asset),"returns.."))
plotVaRGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strES, names(asset),"returns.."))
plotESGHD(asset, ghdFits=modelFits)


############################################################################
### Perform GHD calibration on ACKB return data                        #####
############################################################################
asset <- ACKB
readline(prompt=paste(strCalibrate, names(asset),"returns.."))
modelFits <- fitGHD(asset)

readline(prompt=paste(strDensity, names(asset),"returns.."))
plotDensityGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strQQ, names(asset),"returns.."))
plotQQGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strVaR, names(asset),"returns.."))
plotVaRGHD(asset, ghdFits=modelFits)

readline(prompt=paste(strES, names(asset),"returns.."))
plotESGHD(asset, ghdFits=modelFits)