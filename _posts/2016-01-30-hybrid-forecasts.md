---
layout: post
title: Better prediction intervals for time series forecasts
date: 2016-01-30
tag: 
   - Timeseries
   - Forecasting
   - R
description: Hybrid forecasts - averages of single-model forecasts - are commonly used to produce point estimates that are better than any of the contributing forecast models.  I show how prediction intervals can be constructed for a hybrid forecast that have more accurate coverage than most commonly used prediction intervals (ie 80% of actual observations do indeed turn out to be within the 80% confidence interval), tested on the 3,003 M3 forecasting competition datasets.
image: /img/0028-USAccDeaths.svg
socialimage: http://ellisp.github.io/img/0028-USAccDeaths.png
category: R
---
## Forecast Combination
I've referred several times to [this blog post by Rob Hyndman](http://robjhyndman.com/hyndsight/show-me-the-evidence/) in which he shows that a simple averaging of the `ets()` and `auto.arima()` functions in his `forecast` R package not only out-performs `ets()` and `auto.arima()` individually (in the long run, not every time), they outperform nearly every method that was entered in the [M3 competition](https://forecasters.org/resources/time-series-data/m3-competition/) in the year 2000.  This is a good example of a well known part of the forecasting craft, that a "Forecast Combination" of models will produce results closer to reality than its individual component models do.

In an [earlier post](/blog/2015/12/21/m3-and-x13.html), I showed that adding X13-SEATS to the combination further improves the forecast, reaching a point where the Mean Absolute Scaled Error (compared to the actual observations once made available) for the ets-auto.arima-SEATS combination is lower even than the competition's top ranking Theta method.

## Prediction intervals
A question for the forecaster is what prediction interval to use in a forecast combination.  A prediction interval is a similar but not identical concept to a confidence interval.  A prediction interval is an estimate of a value (or rather, the range of likely values) that isn't yet known but is going to be observed at some point in the future.  Whereas a confidence interval is an estimate of the likely range of values for a fundamentally unobserveable parameter.  A prediction interval needs to take into account uncertainty in the model, uncertain estimates of the parameters in a model (ie the confidence intervals for those parameters), and also the individual randomness associated with the particular point or points being predicted.

Prediction intervals for forecasts are well known to be [usually too narrow](http://robjhyndman.com/hyndsight/narrow-pi/).  For example, one study found prediction intervals calculated to include the true results 95% of the time only get it right between 71% and 87% of the time (thanks to Hyndman again for making that result easily available on his blog).  There are a number of contributing reasons but the main one is that the uncertainty in the model building and selection process is not adequately taken into account.  Most methods of developing prediction intervals are in effect estimating a range of values conditional on the model being correct in the first place.  As our models are only simplifications of reality we fail more often than we would if the model were exactly right.

On a cursory glance, I couldn't find much discussion of how to produce a prediction interval from a forecast combination.  Hyndman avoids referring the issue in the first post linked to above by making claims only about point estimates "If you only want point forecasts, that (average of `ets` and `auto.arima`) is the best approach available in the `forecast` package."  [One paper I found](http://repository.upenn.edu/cgi/viewcontent.cgi?article=1005&context=marketing_papers) makes the common sense suggestion of taking averages of the prediction intervals from the component models.  But if the original prediction intervals are too narrow, is averaging them going to help?

Forecasting makes me nervous, and in my day job when the reality is noticeably different from the forecast points it causes problems.  I'd like there to be much more focus on the range of prediction intervals in communicating forecasts, and I'd like that range to be accurate or if anything slightly conservative (eg I'd be happier for 83% of observations to come inside my 80% prediction interval than for 77%).

## Introducing hybridf()
I like combining `auto.arima()` and `ets()` for a quick, effective hybrid forecast that is probably as good as can be hoped for with a univariate series.  In fact, it's the sort of thing that could easily be done thousands of times in a day so to make it more convenient I created a function `hybridf()` that does this for me in R and produces an object of class `forecast`.  This means that I can fit a forecast with one line of code and that other functionality Hyndman developed for that class, like the standard forecast plot, can be used on the resulting object.  Here it is in action with the monthly time series of accidental deaths in the USA from 1973 to 1978, used in Brockwell and Davis' 1991 <i>Time Series: Theory and Methods</i> and one of R's in-built datasets.
{% highlight R lineanchors %}
library(devtools)
install_github("robjhyndman/forecast") # development version needed sorry
library(forecast)

source("https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R")

fc <- hybridf(USAccDeaths)
par(mfrow = c(3, 1), bty = "l")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)
{% endhighlight %}
![USAccDeaths](/img/0028-USAccDeaths.svg)

The dark grey areas are 80% prediction intervals and the light grey the 95% prediction interval.  The top panel shows the hybrid forecast.  The dark blue line is just the average of the point forecasts of the other two methods, and the prediction interval takes the conservative view of showing the widest range of values of combining the two.  So the hybrid prediction interval will be wider than the prediction intervals for either of the contributing models.

## Testing against the M3 competition series
In these days of easy access to computers and to data, we don't have to just theorise about the success rates of different prediction intervals, we can test methods against actual data.  I used the 3,003 [M3 competition datasets](https://forecasters.org/resources/time-series-data/m3-competition/) to compare the 80% and 95% prediction intervals generated by `ets()`, `auto.arima()`, and my `hybridf()`.  After fitting the models on the given historical data and producing forecasts of the desired length, I counted how many of the actual results were in the prediction intervals.  Here's the results:

|variable       | Success|
|:--------------|-------:|
|ets_p80        |    0.75|
|ets_p95        |    0.90|
|auto.arima_p80 |    0.74|
|auto.arima_p95 |    0.88|
|hybrid_p80     |    0.83|
|hybrid_p95     |    0.94|


My hybrid method has prediction intervals that succeed at close to the advertised rates, whereas both `ets()` and `auto.arima()` are less successful.  For example, the hybrid 80% prediction interval contains the actual results 83% of the time, and the 95% prediction interval has the actual result 94% of the time; whereas for auto.arima the success rates are 74% and 88% respectively.

Here's how I tested that on the M3 data.  I build a little function `pi_accuracy()` to help, which makes use of the fact that objects of class forecast return a matrix called "lower" and another called "upper", with a column for each prediction interval level.  As this is only a temporary function for this blog, I leave it so it only works with the default values of forecast objects producing 80% and 95% intervals:
{% highlight R lineanchors %}
#------------------setup------------------------
library(showtext)
library(ggplot2)
library(scales)
library(forecast)
library(Mcomp)
library(tidyr)
library(dplyr)

source("https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R")

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

pi_accuracy <- function(fc, yobs){
   # checks the success of prediction intervals of an object of class 
   # forecast with actual values
   if(length(yobs) != length(fc$mean)){
      stop("yobs needs to be the same length as the forecast period.")
   }
   n <- length(yobs)
   yobsm <- cbind(yobs, yobs)
   In <- (yobsm > fc$lower & yobsm < fc$upper) 
   colnames(In) <- c("Series 1", "Series 2")
   Success <- colMeans(In)
   return(list(In = In, Success = Success, n = n))
 }
{% endhighlight %}

Actually fitting all the forecasts is relatively straightforward.  It took about an hour on my laptop.  As the `hybridf()` function returns an object that provides the underlying `ets()` and `auto.arima()` objects, they don't need to be refitted and there's some modest efficiency.
{% highlight R lineanchors %}
#============forecasting with default values===============
num_series <- length(M3) # ie 3003
results <- matrix(0, nrow = num_series, ncol = 7)

for(i in 1:num_series){
   cat(i, " ")        # let me know how it's going as it loops through...
   series <- M3[[i]]
   x <- series$x      # ie the data to be fitted
   xx <- series$xx    # ie the true, actual values of the forecast period
   h <- length(xx)    # ie the length of the forecast period
   
   fc3 <- hybridf(x, h = h)
   results[i, 5:6] <- pi_accuracy(fc3, xx)$Success
   
   fc1 <- fc3$fc_ets
   results[i, 1:2] <- pi_accuracy(fc1, xx)$Success
   
   fc2 <- fc3$fc_aa
   results[i, 3:4] <- pi_accuracy(fc2, xx)$Success
   
   results[i, 7] <- h
}

results <- as.data.frame(results)

names(results) <- c("ets_p80", "ets_p95", "auto.arima_p80", "auto.arima_p95",
                    "hybrid_p80", "hybrid_p95", "h")

# The results are saved as percentages that were in the intervals,
# and the forecast lengths are different, so we need to weight by
# forecast length (h) to get the actual total percentage of observations
# that were within prediction interval.  This code produces the table
# reproduced in the blog post above:                    
results %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2))

results %>%
   gather(variable, value, -h) %>%
   mutate(Level = ifelse(grepl("p80", variable), "80%", "95%"),
          Level = factor(Level, levels = c("95%", "80%")),
          variable = gsub("_p[0-9].", "", variable)) %>%
   ggplot(aes(x = h, y = value, colour = Level)) +
   facet_grid(Level~variable) +
   scale_y_continuous("Percentage of actual results within forecast prediction interval\n",
                      label = percent, breaks = c(0, .25, .5, .75, .8, .95, 1)) +
   labs(x = "Forecast period", colour = "Desired level") +
   ggtitle("Prediction interval success for three forecasting methods on 3003 M3 timeseries") +
   geom_jitter(alpha = 0.2, width = 1.3, height = 0.1, shape = 1) +
   geom_smooth(se = FALSE, method = "lm") +
   theme(panel.grid.minor = element_blank())
{% endhighlight %}

![image](/img/0028-indiv-error.svg)

An interesting pattern emerges when we look at the success rates of individual forecasts, as in the image above.  A small collection of unfortunates have 0% of the actual data within the prediction interval - things went wrong and stayed wrong.  Generally, the longer the forecast period, the higher the accuracy rate of the prediction intervals.  Prediction intervals get wider as they forecast further periods out; and the randomness that is explicitly included in the intervals this way starts to dominate over the sunk cost inaccuracy of having a wrong model in the first place.  For longer forecast periods, the standard prediction intervals tend towards performing as advertised, whereas for shorter forecast periods they are over-optimistic.


## Bootstrapping
The forecast methods for both `ets()` and `auto.arima()` have the option to estimate prediction intervals by simulation and bootstrapping residuals rather than analytically, and those methods are inherited by my `hybridf()`.  I checked the value of these prediction intervals too.  The results are very similar to the non-bootstrap results; if anything, the prediction intervals based on bootstrap and simulation are slightly less accurate, but the difference is nothing to write home about.

|variable       | Success|
|:--------------|-------:|
|ets_p80        |    0.72|
|ets_p95        |    0.88|
|auto.arima_p80 |    0.70|
|auto.arima_p95 |    0.86|
|hybrid_p80     |    0.80|
|hybrid_p95     |    0.92|
   


   
{% highlight R lineanchors %}
#=====with bootstrapping instead of formulae for the prediction intervals=============

num_series <- length(M3)
resultsb <- matrix(0, nrow = num_series, ncol = 7)

for(i in 1:num_series){
   cat(i, " ")
   series <- M3[[i]]
   x <- series$x
   xx <- series$xx
   h <- length(xx)
   
   fc3 <- hybridf(x, h = h, simulate = TRUE, bootstrap.ets = TRUE, bootstrap.aa = TRUE)
   resultsb[i, 5:6] <- pi_accuracy(fc3, xx)$Success
   
   fc1 <- fc3$fc_ets
   resultsb[i, 1:2] <- pi_accuracy(fc1, xx)$Success
   
   fc2 <- fc3$fc_aa
   resultsb[i, 3:4] <- pi_accuracy(fc2, xx)$Success
   
   resultsb[i, 7] <- h
}

resultsb <- as.data.frame(resultsb)

names(resultsb) <- c("ets_p80", "ets_p95", "auto.arima_p80", "auto.arima_p95",
                    "hybrid_p80", "hybrid_p95", "h")

resultsb %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2))
{% endhighlight %}

## Conclusions

* The `hybridf()` function at [https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R](https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R) provides a convenient and easy way of performing a forecast combination of `ets()` and `auto.arima()`, which gives high performing point estimates and an easily-managed object of class forecast.
* Tested against the M3 competition data, the prediction intervals from `hybridf()`, formed by combining the prediction intervals of `ets()` and `auto.arima()` in a conservative manner ("take the widest range covered by superimposing the two source intervals") performs true to the desired level ie the 80% prediction interval contains the true value just over 80% of the time, and the 95% prediction interval contains the true value just under 95% of the time.
