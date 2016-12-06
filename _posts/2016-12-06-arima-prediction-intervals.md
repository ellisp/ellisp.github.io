---
layout: post
title: Sources of error in time series forecasts
date: 2016-12-06
tag: 
   - R
   - Timeseries
description: A quick demonstration of the impact of only having random estimates of the parameters and meta-parameters in ARIMA time series modelling
image: /img/0072-mean.svg
socialimage: http://ellisp.github.io/img/0070-mean.png
category: R
---

## A broad family of fast and effective forecast methods
[Exponential smoothing state space](https://en.wikipedia.org/wiki/Exponential_smoothing) methods constitute a broad family of approaches to univariate time series forecasting that have been around for many decades and only in the twenty-first century placed into a systematic framework.  The definitive book on the subject is Hyndman, Koehler, Ord and Snyder's [Forecasting with Exponential Smoothing: The State Space Approach](http://www.exponentialsmoothing.net/).  Well known models such as [simple or single exponential smoothing](https://www.otexts.org/fpp/7/1), Holt's linear trend method (ingeniously implemented here [in Excel](http://www.real-statistics.com/time-series-analysis/basic-time-series-forecasting/holt-linear-trend/)  -  these methods are popular in the business world where better tools are often not used) and [the Holt-Winters seasonal method](https://www.otexts.org/fpp/7/5) can be shown to be special cases of a single family.

A simple must-read in this space is the [taxonomy of exponential smoothing methods](https://www.otexts.org/fpp/7/6) in Hyndman and Athanasopolous' *Forecasting Principles and Practice*.  Their taxonomy is based on characterising each model against three dimensions: error, trend and seasonality (hence the function that implements these models is `ets` in the `forecast` package).  Each of those three can be characteristised as "additive", "multiplicative", or "none".  From the ets helpfile:

> `model`: Usually a three-character string identifying method using the framework terminology of Hyndman et al. (2002) and Hyndman et al. (2008). The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend type ("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected. So, for example, "ANN" is simple exponential smoothing with additive errors, "MAM" is multiplicative Holt-Winters' method with multiplicative errors, and so on.

By taking a fully general approach, `ets` is able to make the most of all of the members of its family and automatically choose the most effective method for a given dataset.

## Three relatives

I wanted to get my head around the performance with seasonal data of two close relatives of `ets`: `stlm` and `thetaf`, Hyndman's implementation of the successful [Theta decomposition method of forecasting](https://www.google.co.nz/url?sa=t&rct=j&q=&esrc=s&source=web&cd=5&cad=rja&uact=8&ved=0ahUKEwi2i-3NxsnQAhWDF5QKHdw7A-YQFgg5MAQ&url=https%3A%2F%2Fwww.researchgate.net%2Fpublication%2F223049702_The_theta_model_A_decomposition_approach_to_forecasting&usg=AFQjCNGN1PCpPDu5KSMWzHvNbhDFYLQofw&sig2=cdBAHctXx8kd2eVsDPXgLQ).  

After the Theta method made a name for itself in the forecasting competition world as a complex new algorithm, Hyndman and Billah [successfully showed](http://www.robjhyndman.com/papers/Theta.pdf) that it could be seen as yet another special case of an exponential smoothing model with drift.  Both the `thetaf` and `stlm` methods, in Hyndman's `forecast` package, work by first seasonally adjusting a series, then applying a type of exponential smoothing model to it.  The original Theta method [did not detect seasonality](http://stats.stackexchange.com/questions/67036/does-thetaf-in-the-forecast-package-in-r-detect-seasonality), but Hyndman's more recent implementations of it do.

Because Hyndman's `forecast` package is open source, we can inspect the [source code](https://github.com/robjhyndman/forecast/tree/master/R) to be sure the functions are doing what we think they are.  Here are the three functions I'm looking at:

- `ets` The most general and flexible version.  Models error, trend and seasonal elements together.
- `stlm` Seasonal adjustment via `stl` (Cleveland-style loess); then a non-seasonal `ets` model (ie just error and trend terms); then re-seasonalise.
- `thetaf` Seasonal adjustment via `decomp` (classical multiplicative seasonal decomposition); `ets` with just an additive model (no trend or seasonality); estimate trend/drift by an adjusted form of linear regression of the seasonally adjusted variable against time.

One way of looking at this is that `ets` uses exponential smoothing for all three of error, trend and seasonal; `stlm` for just error and trend; and `thetaf` for just error.  `stlm` and `thetaf` bring in other methods for dealing with the seasonal and trend elements.

Here are those three methods in action against one of the example monthly data sets from the [2010 Tourism Forecasting Competition](http://ellisp.github.io/blog/2016/10/19/Tcomp).

![eg1](/img/0070-eg1.svg)

We see from the plot titles in this example that the `ets` approach detects additive error and seasonal structures; whereas the `stlm` approach first removes the seasonality, so what is left is simply additive error.  None of the methods is good at picking up the subtle upwards trend and they all slightly underestimate the forward values of the series, albeit with good prediction interval coverage.

Here's a second example, this time a quarterly series:

![eg2](/img/0070-eg2.svg)

In this case, multiplicative error and seasonal components are detected in the original data.

## A systematic comparison

To get a more systematic view on which of these variants of methods is most effective with seasonal data of the sort I mostly deal with, I ran an experiment.  I used all three methods against each of the quarterly and monthly series in the 2010 tourism forecasting competition and the 1982 "M1" forecasting competition from Makridakis et al.  This constitutes well over a thousand time series in total.  I calculated the mean absolute scaled error (MASE) of the forecasts against the actual future data.  Here's the distribution of those values of MASE:

![density](/img/0070-density.svg)

Easier to take in than all those individual values of MASE is a summary statistic.  Here's the trimmed mean:
![trmean](/img/0070-trmean.svg)

The Theta method stands out as not as consistently accurate as either `ets` or `stlm` and there is little to tell the two apart.  But here is the mean:

![mean](/img/0070-mean.svg)

All three methods obviously generated a few poor values of MASE that drag their untrimmed mean scores up.  Those pesky, hard to predict real datasets... Mostly but not always `stlm` appears a bit better than `ets`, which might suggest that it is a good idea to seasonally adjust the data before fitting an exponential smoothing model to it - but there isn't much in it.  `thetaf` does seem to be systematically outperformed by its more general and flexible cousins with this seasonal data.  We can't tell from this whether it is the simple seasonal adjustment method used in `thetaf` that is responsible or the Theta method itself; my suspicion is with the seasonal adjustment.  Classical decomposition is a less flexible method than the rolling window smoothing approach used by `stl`, which is able to pick up changes in seasonality over time.

Here's the code that produces all the above examples and runs the tests against the competition data:

{% highlight R %}
library(ggplot2)
library(forecast)
library(Tcomp)
library(doParallel)
library(dplyr)
library(tidyr)
library(Mcomp)


#======example datasets=============

# Example 1
x <- tourism[[150]]$x
h <- tourism[[150]]$h
par(mfrow = c(4, 1), bty = "l", font.main = 1)
plot(forecast(ets(x), h = h)); grid()
plot(forecast(stlm(x), h = h)); grid()
plot(thetaf(x, h = h)); grid()
plot(tourism[[150]], main = "Actual results - `tourism` data series 150"); grid()

# Example 2
x <- tourism[[600]]$x
h <- tourism[[600]]$h
par(mfrow = c(4, 1), bty = "l", font.main = 1)
plot(forecast(ets(x), h = h)); grid()
plot(forecast(stlm(x), h = h)); grid()
plot(thetaf(x, h = h)); grid()
plot(tourism[[600]], main = "Actual results - `tourism` data series 600"); grid()

#============set up cluster for parallel computing===========
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(Tcomp)
   library(Mcomp)
   library(forecast)
})

#==========the actual analytical function==============
competition <- function(collection, maxfors = length(collection)){
   if(class(collection) != "Mcomp"){
      stop("This function only works on objects of class Mcomp, eg from the Mcomp or Tcomp packages.")
   }
   nseries <- length(collection)
   mases <- foreach(i = 1:maxfors, .combine = "rbind") %dopar% {
      thedata <- collection[[i]]  
      fc1 <- forecast(ets(thedata$x), h = thedata$h)
      fc2 <- forecast(stlm(thedata$x), h = thedata$h)
      fc3 <- thetaf(thedata$x, h = thedata$h)
      mase <- c(accuracy(fc1, thedata$xx)[2, "MASE"],
                accuracy(fc2, thedata$xx)[2, "MASE"],
                accuracy(fc3, thedata$xx)[2, "MASE"])
      return(mase)
   }
   colnames(mases) <- c("ets", "stlm", "thetaf")
   return(mases)
}

## Test on a small set of data, useful during dev
small_collection <- list(tourism[[100]], tourism[[200]], tourism[[300]], tourism[[400]], tourism[[500]], tourism[[600]])
class(small_collection) <- "Mcomp"
test1 <- competition(small_collection)
round(test1, 1)

#===========application=================
# takes a few minutes to run
t4 <- competition(subset(tourism, "quarterly"))
t12 <- competition(subset(tourism, "monthly"))
m4 <- competition(subset(M1, "quarterly"))
m12 <- competition(subset(M1, "monthly"))

# shut down cluster
stopCluster(cluster)

#===========results=============

results <- as.data.frame(rbind(t4, t12, m4, m12) ) %>%
   mutate(dataset = rep(c("Tourism", "M1"), 
                        times = c(nrow(t4) + nrow(t12), nrow(m4) + nrow(m12)))) %>%
   mutate(period = rep(c("Quarterly", "Monthly", "Quarterly", "Monthly"),
                       times = c(nrow(t4), nrow(t12), nrow(m4), nrow(m12)))) %>%
   gather(method, MASE, -period, -dataset)

results %>%
   ggplot(aes(x = MASE, colour = method, fill = method)) +
   geom_density(alpha = 0.1) + 
   geom_rug() +
   scale_colour_brewer("", palette = "Set1") +
   scale_fill_brewer("", palette = "Set1") +
   facet_grid(period ~ dataset) +
   scale_x_sqrt()

results %>%
   group_by(period, dataset, method) %>%
   summarise(MASE = mean(MASE, tr = 0.1)) %>%
   ggplot(aes(x = period, y = MASE, label = method, colour = method)) +
   facet_wrap(~dataset) +
   geom_text() +
   scale_colour_brewer(palette = "Set1") +
   theme(legend.position = "none") +
   labs(x = "", y = "Trimmed mean of Mean Absolute Scaled Error", colour = "") +
   ggtitle("Comparison of three related forecasting methods",
           subtitle = "'Tourism' and 'M1' competition datasets")

results %>%
   group_by(period, dataset, method) %>%
   summarise(MASE = mean(MASE, tr = 0)) %>%
   ggplot(aes(x = period, y = MASE, label = method, colour = method)) +
   facet_wrap(~dataset) +
   geom_text() +
   scale_colour_brewer(palette = "Set1") +
   theme(legend.position = "none") +
   labs(x = "", y = "Mean of Mean Absolute Scaled Error", colour = "") +
   ggtitle("Comparison of three related forecasting methods",
           subtitle = "'Tourism' and 'M1' competition datasets")
{% endhighlight %}
