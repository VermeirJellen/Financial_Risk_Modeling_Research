# Financial Risk Modeling - Research Project

###Global Research Context
This project is part of a larger academic research project. The global project aims to give a broad overview on the state of the art regarding both financial risk management and robust portfolio optimization. The methods and techniques under consideration mainly consist of a recombination of well known statistical
concepts and robust convex optimization procedures that are applied to finance-related problems. However, the added value of the project aims to be of a more practical nature. Our main objective is to derive useful tools from the theoretical concepts and illustrate and apply the techniques in a realistic setting in such a way that industry professionals can directly benefit from them. The research project was initially divided into three main parts:

1. Financial Risk Management
2. Robust Portfolio Optimization
3. Tactical Asset Allocation

This subproject contains the functionality and demos that are relevant only to the **first part** of the research project. Here, we focus our attention on the risk modelling aspects of the portfolio management process.

###Financial Risk Modeling - Project Summary
The theoretical background on the statistical models and the explanations that accompany the scripts and demos can be found in `./paper/Financial_Risk_Modeling.pdf`.

In the paper and demos (explained below), we start by offering some motivational examples on why one should stay clear of the normality assumption while assessing portfolio risk. Stylized facts of univariate and multivariate financial market data are illustrated and discussed. We follow up with a few definitions and interpretations of financial risk and focus our attention to the Value at Risk (VaR) and Expected Shortfall (ES) measures. In the remainder of the paper we present alternatives to the normal distribution for modelling and forecasting the latter risk measures:

- We give an exposition on the Generalized Hyperbolic Distribution (GHD).
- We introduce methods and concepts from extreme value theory (EVT) as a means of capturing severe financial losses. 
- Conditional risk measurements are presented in the form of GARCH models.
- Copulae are discussed with the purpose of modelling multivariate dependencies between assets. 

From a more practical point of view we perform out of sample backtests and run statistical tests to evaluate the risk forecasting performance of the models. Here, we argue that a mixed (E)GARCH-Clayton/Gumbel copula model is especially suitable for asset return modeling and risk forecasting at the portfolio level. We demonstrate how the latter tool can be employed to provide superior risk adjusted returns in a portfolio context by reliably targetting a certain expected VaR (or ES) level during the portfolio rebalancing process.


# Demos
#### Running the Demo's
The entrance point of the main demo can be found in `./demo_main.R`. First, `./config/config.R` (line 7) must be sourced to load the relevant packages and functions into memory (also view `./functions` folder). Next, `./config/load_data.R` (line 10) must be sourced to load the timeseries data: This script downloads BEL20 stock and SPDR ETF data from an external database server located on the Amazon RDS servers. For more information on the data setup, view <https://github.com/VermeirJellen/Securities_Master_Database>

The individual demo's can be launched separately from eachother by running the relevant lines from the main script. The output graphs can also be viewed in the `./images` folder. 

**Note:** some of the backtest demo's might consume an excessive amount of memory on low-RAM machines. If this issue occurs then the problem can be mitigated by modifying the `nrCores` setting to a smaller number inside the demo script function calls (by default, the max number of cores / threads is allocated to the backtest functions).

##Part 1 - Stylized Facts of Asset Returns
In the first part of the demo suite we investigate the stylistic features and properties of univariate and multivariate asset return series.
#### Demo 1 - Asset Return Properties
View chapter 2 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/stylized_facts.R`. Additionally, also view `./functions/StylizedFeatures.R` for technical implementation details. The first part of the demo illustrates the univariate stylistic return features of SPY and AGS.  The stylized facts for univariate time series (assets) can be summarized as follows:

- Time series data of asset returns are not independent and not identically distributed.
- The volatility of the asset return process is not constant with respect to time.
Extreme events are observed closely together (volatility clustering).
- The absolute (or squared) returns are highly autocorrelated.
- The distribution of returns is leptokurtic and left skewed. Large hegative returns are
more likely to occur than large positive returns.

In the second part of the demo, multivariate features are investigated by analyzing the XLE, XLU and XLK sector ETF asset price and return timeseries. The stylized facts for multivariate asset return series can be summarized as follows:

- The absolute (or squared) returns show high cross-correlations. This finding is
similar to the univariate case.
-  The absolute value of cross-correlations between return series are less pronounced. The contemporaneous correlations are in general the strongest.
-  Contemporaneous correlations are not constant over time.
-  Extreme observations in one return series are often accompanied by extremes in the
other return series.

## Part 2 - Univariate Financial Risk Modeling
In the first part of the demo suite we investigate the topic of univariate financial risk modeling.

#### DEMO 2 - Calibrating the Generalized Hyperbolic Distribution (GHD)

View chapter 3 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/ghd_fit.R`. Additionally, also view `./functions/GHDFit.R` for technical implementation details. In the first part of this demo we calibrate the Generalized Hyperbolic Distribution (GHD) and its special cases, the Hyperbolic Distribution (HYP) and the normal inverse Gaussion Distribution (NIG) to univariate return timeseries of SPY and AGS. We investigate the properties and suitability of the fitted models and make a comparison with the normal distribution:

- The normal distribution is unable to capture the excess kurtosis in the return data.
- In contrast, the GHD distributions manage to track the empirical return distribution function rather well.

In the second part of the demo we investigate the risk assessment properties by analyzing the behavior of the VaR and ES according to each of the models:

- We notice that the normal distribution falls short of capturing extreme risk events while the riskiness of holding a position in the asset is overestimated for the higher confidence regions. 
- The GHD and NIG fit fairly well for the whole data range while HYP seems to underestimate risk in the lower confidence regions.

#### DEMO 3 - Backtesting the Generalized Hypberbolic Distribution (GHD)
View chapter 3 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/ghd_backtest.R`. Additionally, also view `./functions/GHDBacktest.R` for technical implementation details. In this demo, we use the GHD distribution to conduct a one day out of sample forecast of the 99% Value at Risk (VaR) and Expected Shortfall (ES) for the daily returns of SPY and AGS in a moving window of 252 days. A comparison is made with the normal distribution. Next, we perform conditional and unconditional VaR tests on the out of sample backtest results:

- The null hypothesis of a correct amount of unconditional exceedances is rejected for the Gaussian distribution model.
- For the GHD distributions the the null hypothesis can not be rejected.
- Results for the unconditional exceedances are inconclusive.

we also evaluate the ES measures using the expected shortfall test of McNeil and Frey:

- The null hypothesis of i.i.d excess conditional shortfall with zero mean is rejected for the normal distribution 
- For the GHD distribution, the null can not be rejected for the SPY timeseries, but it is rejected for AGS return series. As shown in the previous demo, the return series of AGS are more volatile and contain heavier tails.

In general, it can be stated that the GHD distribution is sufficiently flexible to capture asset return properties. However, **caution is required when using this distribution for dynamic risk modelling purposes**:

- Even though the null hypothesis of the statistical VaR tests were never rejected, we encountered an overall excessive amount of expected VaR violations in our case studies.
- The distribution and properties of the return outliers in the tail might not be adequately captures by the GHD model.
- The GHD model assumes that financial market returns are identically and independently distributed. In reality this assumption is clearly violated due to the volatility clustering property of asset returns.


#### DEMO 4 - Extreme Value Theory
View chapter 4 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/evt_fit.R`. Additionally, also view `./functions/EVTFit.R` for technical implementation details. In this demo we attempt to model the tail distribution of asset returns by employing Extreme Value Theory statistics. In the first part of the demo we calibrate the SPY return series to the Generalized Extreme Value (GEV) distribution by using a 50 day blocksize. The demo illustrates that multiple problems manifest themselves when applying the block maxima approach to financial time series data:

- The unknown distribution parameters are estimated with great uncertainty as a result of insufficient data history.
- The volatility clustering features of asset returns are again not adequately captured by the model:
	- Not all extreme observations are exploited during the calibration process.
	- Data points during tranquil periods are selected as extremes when they are in fact not extreme datapoints.

The second part of the demo attemps to model the asset return tail distribution with the peaks-over-threshold approach: The goal here is to circumvent some of the previous problems by considering all observations above a certain threshold as extreme observations. We calibrate the asset returns to the Generalized Pareto Distribution (GPD) and conclude that the model captures the tail distrbitution of asset returns in a satisfactory manner. Worst case risk metrics -such as VaR and ES- can hence be derived from this tail distribution. 

**Word of caution:** Calibration of asset return data to the GPD suffers from the same shortcommings previously mentioned during our GHD exposition. Asset returns are assumed to be i.i.d and dynamic autocorrelation / volatility clustering properties are not taken into account. Strong caution is advised when making out of sample risk forecasts. Recent market conditions should be manually taken into consideration to assess the reliability of the forecasts.


#### DEMO 5 - (E)GARCH Calibration
View chapter 5 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/garch_calibration.R`. Additionally, also view `./functions/GarchCalibration.R` and `./functions/GarchRiskForecasts.R` for technical implementation details. In this demo and the next we demonstrate that an EGARCH model is well suited to capture the stylistic features of univariate asset returns:

- Autocorrelated Conditional Heteroscedastic (ARCH) models allow distributions with excess kurtosis and/or skewness to be interspersed inside the model in the form of error terms.
- The ARCH model does not assume that the data is i.i.d: The variance parameter is conditional and depends on errors from previous time periods.
- Generalized ARCH (GARCH) models allow the variance to be conditional on previous volatility values. Hence, the model is better able to account for volatility clustering behavior. 
- Exponential GARCH (EGARCH) models allow the modelling of assymetric volatility behavior by employing a logarithmic variance equation. These models are well suited to capture the assymetric volatility behavior of asset returns.

#### DEMO 6 - (E)GARCH Backtest
View chapter 5 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/garch_backtest.R`. Additionally, also view `./functions/GarchBacktest.R` for technical implementation details. In this demo we evaluate the risk forecasting power of the EGARCH model by conducting an out of sample VaR and ES forecasting backtest for SPY and ACKB:

- The risk forecasts are reliable: The null hypothesis of correct amount of conditional VaR exceedances can not be rejected. 
- The model rapidly adapts to changing market conditions and volatility spikes.

**Conclusion:**  We demonstrated that EGARCH models manage to succesfully incorporate the volatility clustering properties of asset returns while also taking the other stylistic properties of asset returns into account. EGARCH models take past market conditions into consideration and they circumvent the problems that we encountered during previous evaluations of the GHD and GPD models. Hence, EGARCH models are well suited to perform out of sample risk measurement forecasts.

#### DEMO 7 - (E)GARCH univariate VaR targeting
View chapter 5 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/var_targeting_univariate.R`. Additionally, also view `./functions/VaRTargetingUnivariateBacktest.R` for technical implementation details. In this demo we use the EGARCH tool to perform daily rebalancing of our position in an underlying asset such that an expected next-day VaR of 2% is targeted. We do this for the SPY and BEL20 indices.

## Part 3 - Multivariate Financial Risk Modeling
In the third part of the demo suite we investigate the topic of multivariate financial risk modeling.

#### DEMO 8 - Exploratory data analysis for multi-asset portfolios
View chapter 6 of `./paper/Financial_Risk_Management.pdf` and `./demos/eda_marginals.R`.In this demo we perform some initial exploratory data analysis on the underlying price and return series of two portfolios:

1. The SPDR ETF index funds 
2. 14 of the BEL20 stocks 

#### DEMO 9 - Mixed EGARCH-Clayton-Gumbel expected VaR targeting
View chapter 6 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/strategy_equal_allocation.R`. Additionally, also view `./functions/GarchCopulaCalibration.R`, `./functions/GarchCopulaRiskForecasts.R`, `./functions/GarchCopulaBacktest.R` and `./functions/VaRTargetingMultivariateBacktest.R` for technical implementation details. In this demo, we assume an equal weight asset allocation between the underlying assets in the SPDR ETF and BEL20 stock portfolios. We calibrate the EGARCH-Clayton-Gumbel model to the SPDR ETF and BEL20 portfolios and utilize the model as a risk forecasting tool at the portfolio level. More concretely, in our backtest we use the model to perform out of sample expected next-day VaR and ES forecasts and subsequently use the forecasted risk measurements to rescale the asset weights such that a next day VaR level of 2% is targeted.

- The risk forecasts are reliable: The null hypothesis of correct amount of VaR exceedances can not be rejected. 
- The VaR targeting approach downscales positions during more volatile periods and has te reverse effect during more tranquil periods.
- The VaR targeting approach has the potential to significantly improve overall performance results.

## Part 4 - Practical Application Towards Strategy Management
In the final part of the demo suite we apply the previously developed tools towards global strategy management and portfolio rebalancing.

#### DEMO 10 - Trading Strategy VaR targeting
View chapter 7 of `./paper/Financial_Risk_Modeling.pdf` and `./demos/strategy_golden_cross.R`. Additionally, also view `./functions/EMACrossoverStrategyBacktest.R` for technical implementation details. In this demo we perform an out of sample backtest of a modified "Golden Cross" trading strategy. The asset weights are rebalanced daily according to the strategy rules and the target weights are subsequently rescaled in such a way that a 2% expected next-day VaR level is targeted according to the EGARCH-Clayton-Gumbel model. Conclusions are the same as before:

- The risk measurement forecasts are reliable.
- VaR targeting mitigates downside risk during volatile periods and increases upside potential during trangquil periods.
- VaR targeting significantly improves overall trading performance / sharpe ratio's.

# Licensing and credits
Copyright 2016 Jellen Vermeir. <jellenvermeir@gmail.com>

Financial Risk Management - Research Project is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. Financial Risk Management - Research Project is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with Financial Risk Management - Research Project. If not, see <http://www.gnu.org/licenses/>.

Credit is given to Bernhard Pfaff and his book [Financial Risk Modelling and Portfolio Optimization with R](http://eu.wiley.com/WileyCDA/WileyTitle/productCd-0470978708.html "Wiley"). This book provides the theoretical framework and a comprehensive literature overview on the statistical concepts that formed the basis of this research project.





