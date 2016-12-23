---
layout: post
title: forecastHybrid 0.3.0 on CRAN
date: 2016-12-24
tag: 
   - R
   - Forecasting
   - Timeseries
description: forecastHybrid 0.3.0 for ensemble time series forecasting is now on CRAN. Two new features are prediction intervals for the nnetar (neural network) component of the combination; and theta method models.
image: /img/0074-six-forecasts.svg
socialimage: http://ellisp.github.io/img/0074-six-forecasts.png
category: R
---

## Make it easy to make ensemble time series forecast
`forecastHybrid` is an R package to make it easier to use the average predictions of 'ensembles' (or 'combinations') of time series models from Rob Hyndman's `forecast` package.  It looks after the averaging, and also calculates prediction intervals by a [conservative method](/presentations/ellis-prediction-intervals-for-ensemble-forecasts.pptx) that aims to redress the general over-optimism in forecasting prediction intervals.

New in version 0.3.0 is:

* Following developments in the `forecast` package, prediction intervals are now created for `nnetar` objects in the ensemble. This should address one aspect of incorrect prediction intervals (e.g. [issue #37](https://github.com/ellisp/forecastHybrid/issues/37)).
* theta models can be added (by including "`f`" in the `models =` argument for `hybridModel()`) and are indeed part of the default - so unless set otherwise, `hybridModel()` will now fit six models.
* `accuracy.cvts()` is now exported.  It returns ra ange of summary measures of the cross-validated forecast accuracy for objects created by `cvts`.   Note that development on `cvts` for `forecastHybrid` has been independent of Hyndman's work on a [similarly purposed `tscv` function in `forecast`](http://robjhyndman.com/hyndsight/tscv/) - be careful not to get the two confused.
* `plot.hybridModel()` now supports `ggplot2` graphics when the argument `ggplot = TRUE` is passed.
* Time series must be at least four observations long.
* Fixed an error where `e.args` was passed to `tbats` instead of `t.args`.

In some respects, version 0.2.0 was a more important upgrade because it introduced `cvts` and the use of a weighted average forecast with weights for the component models chosen by cross-validation.  I didn't get around to writing a blog post when that happened.

## Installation
{% highlight R %}
# Install from CRAN the usual way:
install.packages("forecastHybrid")
library(forecastHybrid)
library(ggplot2)
library(scales)
library(tidyverse)

citation("forecastHybrid")

# To cite package ‘forecastHybrid’ in publications use:
#    
#    David Shaub and Peter Ellis (2016). forecastHybrid: Convenient Functions for
# Ensemble Time Series Forecasts. R package version 0.3.0.
# https://CRAN.R-project.org/package=forecastHybrid
# 
# A BibTeX entry for LaTeX users is
# 
# @Manual{,
#    title = {forecastHybrid: Convenient Functions for Ensemble Time Series Forecasts},
#    author = {David Shaub and Peter Ellis},
#    year = {2016},
#    note = {R package version 0.3.0},
#    url = {https://CRAN.R-project.org/package=forecastHybrid},
# }

{% endhighlight %}

## Basic use

The `hybridModel` function fits between two and six time series models to a time series, with or without `xreg` external regressor explanatory variables.  The default, if the `models=` argument is left blank, is to fit all six available models.  The modelling process also determines a set of weights to be used in subsequent forecasts.  The two options are:

* equal weights ie a simple average
* cross validation weights ie a weighted average, with models that performed better in cross-validation given more weight.

Generally, we expect cross-validation weights to perform better, but they are quite computationally intensive and we haven't got round to implementing parallel processing yet.

Here's a demo with the median duration of unemployment in weeks from the `economics` dataset in the `ggplot2` package.  This snippet demonstrates both ways of using weights and the default base plot method.  This plot method is the one provided by `forecast` for all objects of class `forecast`, including those created by forecastHybrid.

{% highlight R %}
# median duration of unemployment, in weeks
uempmed <- ts(economics$uempmed, start = c(1967, 7), frequency = 12)

BoxCox.lambda(uempmed) # very close to zero, so use logarithm tansform

m1 <- hybridModel(uempmed, lambda = 0, weights = "equal")

m2 <- hybridModel(uempmed, lambda = 0, weights = "cv.errors")

f1 <- forecast(m1, 24)
f2 <- forecast(m2, 24)

par(mfrow = c(2, 1), font.main = 1, bty = "l", mar = c(4,3,6,2) + 0.1, cex.main = 0.6)
plot(f1)
plot(f2)
{% endhighlight %}

![base](/img/0074-base-forecast.svg)

The cross-validation errors should be better because they give less weight to poor performing forecasts.  The forecasts from `nnetar` for example are often not good, and in a set of experiments for a [recent talk at the Australian Statistical Conference](/presentations/ellis-prediction-intervals-for-ensemble-forecasts.pptx) I showed that they are sufficiently poor that they often worsen ensemble forecasts when added to the mix.  Setting weights by cross-validation should mitigate this.

All of the component models can be accessed individually if desired:

{% highlight R %}
par(mfrow = c(3, 2), bty = "l", cex = 0.8)
plot(f2$thetam)
plot(f2$ets)
plot(f2$stlm)
plot(f2$auto.arima)
plot(f2$nnetar)
plot(f2$tbats)
{% endhighlight %}

![six](/img/0074-six-forecasts.svg)

The `ggplot2::autoplot()` method provided by the `forecast` package also works:

{% highlight R %}
autoplot(f2) +
   ggtitle("Forecast median length of unemployment",
           subtitle = "Six component models, weights chosen by cross-validation") +
   labs(y = "Weeks", x = "",
        caption = "Source: 'economics' in ggplot2 R package,\nultimately from http://research.stlouisfed.org/fred2")
{% endhighlight %}

![gg](/img/0074-gg-forecast.svg)

## Theta models
We implemented the Theta forecast method based on `thetaf()` in Hyndman's `forecast` (although there are alternatives).  We had to split the process into a modelling function `thetam()`, and an accompanying `forecast.thetam` method.  The results are identical to `forecast::thetaf()`, as demonstrated below:

{% highlight R %}
f3 <- thetaf(uempmed, h = 24)
f4 <- forecast(thetam(uempmed), h = 24)

data.frame(thetaf = f3$mean, thetam = f3$mean) %>%
   mutate_each("as.numeric") %>%
   ggplot(aes(x = thetaf, y = thetam)) +
   geom_abline(intercept = 0, slope = 1) +
   geom_point() +
   labs(x = "thetaf()", y = "forecast(thetam())",
        caption = "Forecasts are of median length of unemployment in the USA") +
   ggtitle("Separating thetaf into a modelling function with a forecast method",
           subtitle = "The two methods give identical results")
{% endhighlight %}

![theta](/img/0074-theta.svg)

## Conservative prediction intervals?


The `forecastHybrid` method of setting prediction intervals is conservative.  It takes the widest range of values covered by any of the component models.  In a [blog post on different ways of setting forecast combinations](http://robjhyndman.com/hyndsight/forecast-combinations/), Rob Hyndman points out that, using the monthly `co2` dataset of Mauna Loa Atmospheric CO2 Concentration distributed with R, the prediction intervals for the `forecastHybrid` method with six component models look too conservative ie sufficiently wide that they look very likely to contain the correct values much more than aimed for.  In this particular case I agree - the method should be used with a bit of caution.  

In practice, I often use only the `auto.arima` and `ets` forecasts in combination rather than all six possibilities.  Even then, the method is sometimes too conservative with data that of monthly or higher frequency, particularly at the 80% level.  Here's the conclusions from [my recent presentation on this topic]((/presentations/ellis-prediction-intervals-for-ensemble-forecasts.pptx)) (apologies for the screenshot from PowerPoint):

<img src='/img/0074-table.png' width='500'>

So for annual and quarterly real world data, the method is not too conservative at all; and if you want your prediction intervals to contain the true value 95% of the time, the method checks out ok even for monthly data.

## Feedback and suggestions

Our preferred approach for you to provide suggestions and bug reports is to file an [issue on GitHub](https://github.com/ellisp/forecastHybrid/issues).  Other comments and feedback could be provided as a comment on this blog post or on [Twitter](https://twitter.com/ellis2013nz).
