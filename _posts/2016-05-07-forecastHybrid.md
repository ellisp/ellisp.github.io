---
layout: post
title: Announcing new forecastHybrid package
date: 2016-05-07
tag: 
   - Timeseries
   - Forecasting
   - R
description: The new forecastHybrid package for R by David Shaub and myself provides convenient access to ensemble time series forecasting methods, in any combination of up to five of the modelling approaches from Hyndman's forecast package.  These are auto-selected autoregressive integrated moving average; exponential smoothing state space (both ets and tbats); feed forward neural network with a single hidden layer and lagged inputs; and forecasts of loess-based seasonal decomposition.

image: /img/0039-unemployment.svg
socialimage: http://ellisp.github.io/img/0039-unemployment.png
category: R
---

## Background and motivation
In an [earlier post](/blog/2016/01/30/hybrid-forecasts.html) I explored ways that might improve on standard methods for prediction intervals from univariate time series forecasting.  One of the tools I used was a convenience function to combine forecasts from Rob Hyndman's `ets` and `auto.arima` functions.  David Shaub (with a small contribution from myself) has now built and published an R package `forecastHybrid` that expands on this idea to create ensembles from other forecasting methods from Hyndman's `forecast` package.  

The motivation is to make it easy to improve forecasts, both their point estimates and their prediction intervals.  It has been well known for many years that taking the average of rival forecast methods improves the performance of forecasts.  This new R package aims to make it as easy for people to do this as to fit the individual models in the first place.

## Installation
The stable version of the `forecastHybrid` package is on CRAN, and is installed the usual way:
{% highlight R lang lineanchors %} 
install.packages("forecastHybrid")
library(forecastHybrid)   
{% endhighlight %}    
It requires at least version 7.1 of Rob Hyndman's `forecast` package, which is a recent upgrade.

## Usage

### Basic
Usage is a two step process:

* fitting the required time series models
* forecasting them and taking a weighted average

{% highlight R lang lineanchors %} 
mod1 <- hybridModel(AirPassengers)
fc1 <- forecast(mod1)
fc1

         Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
Jan 1961       449.0761 420.9284 474.4528 409.9071 486.2105
Feb 1961       430.5014 402.5180 463.4376 392.8622 477.7219
Mar 1961       476.6044 427.8241 541.2952 416.7423 560.7848
Apr 1961       489.6462 445.6985 533.1953 425.6737 554.8180
May 1961       499.3256 443.0128 546.2434 420.7317 570.5889
Jun 1961       565.3663 498.8558 624.6220 471.1949 654.7751
Jul 1961       642.3351 550.5136 710.4620 517.2336 747.2133
<truncated>
{% endhighlight %}    

Prediction intervals are based on the conservative (and accurate, at least for the auto.arima / ets combination and the M3 competition data) method I set out in my [earlier post](/blog/2016/01/30/hybrid-forecasts.html) on hybrid methods.  That is, at each time period of the forecast, the points of the prediction intervals of all the component models in the ensemble that are highest in absolute magnitude are used for the boundaries of the ensemble prediction interval.  This method seems to give truer prediction intervals than any individual model's prediction intervals, which are usually too narrow (ie give a false sense of precision) because they don't take model uncertainty into account.  This is an active area of investigation; [we're not sure we're going to keep them calculated this way](https://github.com/ellisp/forecastHybrid/issues/19).

The object created by the above procedure is of class `forecast` and the base graphics plotting method from Hyndman's `forecast` package applies:

{% highlight R lang lineanchors %} 
par(mar = c(5, 4, 8, 2))
plot(fc1, ylab = "International airline passengers")
{% endhighlight %}   

![basic](/img/0039-airpassengers1.svg)

### Custom combinations and weights
Controlling which of the five types of available models are used in the ensemble is done via the `models` argument of `hybridModel`.  `models` is a character string of any combination of `a`, `e`, `n`, `s`, and `t` for `auto.arima`, `ets`, `nnetar`, `stlm` and `tbats` respectively.  In the code below I combine just `auto.arima` and `ets` (exponential state space smoothing) models, two component models that make up a high-performing (on average) forecasting method.  

I also use this example to show how, instead of weighting the models equally, we can specify greater weight to be given to the model that fits the historical data better.  This procedure isn't recommended - it seems better just to give the models a priori weights, usually equal, rather than let the data dictate them.  Why this is the case is out of my scope just here.  Here's the example:

{% highlight R lang lineanchors %} 
mod2 <- hybridModel(AirPassengers, models = "ae",
                    weights = "insample.errors")
fc2 <- forecast(mod2)
par(mar = c(5, 4, 8, 2))
plot(fc2, ylab = "International airline passengers")
{% endhighlight %}   

![ae](/img/0039-airpassengers2.svg)

### External regressors
While most of the candidate models for an ensemble are univariate methods, `auto.arima` and `nnetar` models can incorporate an `xreg` argument.  If you have actual values for the forecast period of external regressors, it's often useful to use them in the forecasting process.  The forecastHybrid approach lets you do this with the component models that support `xreg`, while ignoring it and fitting univariate time series models with other component models (eg `ets`).

To pass `xreg` or other parameters through to the model-fitting functions, the user passes up to five lists of parameters (`a.arg`, `e.arg`, `n.arg`, `s.arg` and `t.arg`).  Here's an example showing how to pass through xreg parameters to `auto.arima` for automated ARIMA modelling and `nnetar` for the feed-forward neural network model.  This example tries to forecast 12 months of unemployment in Wisconsin, given known values of unemployment in surrounding states and for the USA as a whole (this isn't a particularly realistic example in itself, but is of a type of forecast that does occur in reality, for example when one economic time series is only available after a much longer delay than other more timely measures).

The data management and forecast question below has been pinched from [an example on the BIBA blog by joaquin](http://biba.etsii.upm.es/web/tiki-view_blog_post.php?postId=26)

{% highlight R lang lineanchors %} 
# adapted from http://biba.etsii.upm.es/web/tiki-view_blog_post.php?postId=26 
# donwload individual datasets from QUANDL
wi<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/WIUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
us<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/UNRATE.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
il<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/ILUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
mi<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/MIUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
mn<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/MNUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
io<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/IAUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))

#merging the data into one dataframe
unemp <- merge(wi, io, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io')
unemp <- merge(unemp, mi, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi')
unemp <- merge(unemp, mn, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn')
unemp <- merge(unemp, us, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn', 'us')
unemp <- merge(unemp, il, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn', 'us', 'il')

# break into historical and forecast periods
n <- nrow(unemp)
unemp_hist <- unemp[1:(n - 12), ]
unemp_fc <- unemp[(n - 11):448,]
unemp_hist_ts <- ts(unemp_hist$wi, start = c(1976, 1), frequency = 12)

# fit hybrid model, passing arguments to auto.arima with a.args and to
# nnetar with n.args
mod4 <- hybridModel(unemp_hist_ts, models = "aen", 
                    a.args = list(xreg = unemp_hist[ , 3:7]),
                    n.args = list(xreg = unemp_hist[ , 3:7]))

# fit the forecast - note that you have to supply the future xreg values,
# but you only need to do it once (both nnetar and auto.arima use the same 
# ones):                    
par(mar = c(5, 4, 6, 2))
plot(fc4, ylab = "Unemployment in Wisonsin", 
     sub = "Forecasts based on unemployment in\nUSA, Iowa, Michigan, Minnesota, and Illinois")
{% endhighlight %}   

![unemp](/img/0039-unemployment.svg)

There's a lot more functionality in this package, mostly just inherited from Hyndman's `forecast` package.  So [check it out on CRAN](https://cran.rstudio.com/web/packages/forecastHybrid/).

## Future work
Future work on the package, if / when we get around to it, is likely to include:

* Allowing the weights between the models to be set based on cross-validation performance of the component models
* Allowing weights between the models to change over different forecast horizons (eg some models are known to be generally better at predicting long term than short term, so could be given extra weight as the forecast horizon increase)
* `ggplot2` graphics integration
* More models
* Improved parallelization (it already works with parallel processing, but this could be improved between models)
* Automating model selection
* Various under the hood things

I also hope to do some more work *with* the package, eg more systematic tests of the performance of these hybrid model forecasts both as point forecasters and prediction intervals.

Bugs, issues and enhancement requests can be [filed at GitHub](https://github.com/ellisp/forecastHybrid/issues).

Nearly all the credit for this package goes to David Shaub; although it's hosted on my GitHub account, my contribution has been small.  So thanks David for a great convenient set of functionality for forecasters using R.

{% highlight R lang lineanchors %} 
  @Manual{,
    title = {forecastHybrid: Convenient Functions for Ensemble Time Series Forecasts},
    author = {David Shaub},
    year = {2016},
    note = {R package version 0.1.5},
    url = {https://CRAN.R-project.org/package=forecastHybrid},
  }
{% endhighlight %}   
