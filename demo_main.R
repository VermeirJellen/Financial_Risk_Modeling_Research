# rm(list=ls())
# setwd("demo_main_directory")
#################################################################
############## Load demo packages, functions and data ###########
#################################################################
# Load packages and import functions: MUST be initially executed.
source("config/config.R")

# Loading time series data from external DB: MUST be initially executed.
source("config/load_data.R")

#################################################################
######## PART 1 - Stylized Facts of Asset Returns ###############
#################################################################
# DEMO 1
# Illustration of univariate and multivariate stylistic asset return properties
source("demos/stylized_facts.R")

#################################################################
######## PART 2 - Univariate Financial Risk Modeling ############
#################################################################
# DEMO 2
# Generalized Hyperbolic Distribution Calibration
source("demos/ghd_fit.R")

# DEMO 3
# GHD backtest: Risk measurement forecasting and performance evaluation
source("demos/ghd_backtest.R")

# DEMO 4
# Extreme Value Theory: Tail distribution modeling of asset returns
source("demos/evt_fit.R")

# DEMO 5
# Calibrate eGARCH models to univariate return series and investigate properties
source("demos/garch_calibration.R")

# DEMO 6
# Perform out of sample EGARCH risk forecasting backtest
source("demos/garch_backtest.R")

# DEMO 7
# Perform out of sample EGARCH VaR Targeting backtest
source("demos/var_targeting_univariate.R")

#################################################################
######## PART 3 - Multivariate Financial Risk Modelling #########
#################################################################
# DEMO 8
# Exploratory data analysis for the SPDR ETF and BEL20 stock portfolios
source("demos/eda_marginals.R")

# DEMO 9
# EGARCH-Clayton-Gumbel model: Equal weight VaR targeting backtest
source("demos/strategy_equal_allocation.R")

#################################################################
######## PART 4 - Practical Application: Strategy Management ####
#################################################################

# DEMO 10
# EGARCH-Clayton-Gumbel model: VaR targeting backtest for trading strategy
source("demos/strategy_golden_cross.R")