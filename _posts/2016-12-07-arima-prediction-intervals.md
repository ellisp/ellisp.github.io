---
layout: post
title: Why time series forecasts prediction intervals aren't as good as we'd hope
date: 2016-12-07
tag: 
   - R
   - Timeseries
   - Forecasting
description: A quick demonstration of the impact of inevitably random estimates of the parameters and meta-parameters in ARIMA time series modelling
image: /img/0072-results.svg
socialimage: http://ellisp.github.io/img/0072-results.png
category: R
---

## Five different sources of error

When it comes to time series forecasts from a statistical model we have five sources of error:

1. Random individual errors
2. Random estimates of parameters (eg the coefficients for each autoregressive term)
3. Uncertain meta-parameters (eg number of autoregressive terms)
4. Unsure if the model was right for the historical data
5. Even given #4, unsure if the model will continue to be right

A *confidence interval* is an estimate of the statistical uncertainty of the estimated parameters in the model.  It usually estimates the uncertainty source #2 above, not interested in #1 and conditional on the uncertainty of sources #3, #4 and #5 all being taken out of the picture.  A *prediction interval* should ideally take all five sources into account (see Rob Hyndman for more on [the distinction between prediction and confidence intervals](http://robjhyndman.com/hyndsight/intervals/)).

Unfortunately, the standard ways of providing time series prediction intervals [typically only take  source #1 into account](http://robjhyndman.com/hyndsight/narrow-pi/) - random individual errors.  This differs from standard prediction intervals from more straightforward regression and generalized linear models, which at least usually factor in uncertainty of the estimates of parameters.  

The problem is that **for all but the most trivial time series forecasting method there is no simple way of estimating the uncertainty that comes from having estimated the parameters from the data, and much less so the values of meta-parameters** like the amount of differencing needed, how many autoregressive terms, how many moving average terms, etc (those example meta-parameters come from the Box-Jenkins ARIMA approach, but other forecasting methods have their own meta-parameters to estimate too).

## Demonstration of the cost of estimating meta-parameters

Here's a simple simulation to show the cost of estimating the meta-parameters, even when sources of error #4 and #5 can be discounted.  I generated 10,000 time series, each 100 observations long, from an ARIMA(1, 1, 1) process and split them into a 90 observation training set and 10 observation test set.  This creates data that looks like this (just four examples shown):

![examples](/img/0072-examples.svg)

For each one of those datasets, I fit an ARIMA model to the 90 observation test set two ways:

- using `forecast::Arima` and advising the algorithm that the model to use is (1,1,1)
- using `forecast::auto.arima` and forcing the algorithm to estimate from the data the correct level of differencing, number of autoregression terms, and number of moving average terms.

Then I compared the resulting forecast prediction intervals (at 80% and 95% confidence levels) of the last 10 observations to see what percentage of forecasts actually had the true values in their range.  Here's the result:

![result](/img/0072-results.svg)

As we can see, the `Arima` model which knew in advance the correct meta-parameter specification didn't do too badly.  Its prediction intervals contain the correct values only just below the promised 80% and 95% of times.  But the `auto.arima` method, which had to estimate the meta-parameters itself, did noticeably worse.  Not only that, as the forecast horizon increases, its prediction intervals got increasingly over-optimistic.  Having the wrong meta-parameters for your model becomes more of a problem the further you go out (I suspect this is particularly the case for the amount of differencing that needs to be done - an unlucky choice here would have big implications for estimating the trend).

An important thing to remember is that out in the wild, results will generally be worse than this.  In this occasion, both methods had the luxury of fitting the same family of models that had generated the data, and knowing that the final ten observations were generated the same way.  No outliers, black swans, or changes in data generating process to worry about.  One way of partially dealing with this problem with careful use of the method implemented in the [`forecastHybrid`](https://cran.r-project.org/package=forecastHybrid) R package by David Shaub and myself was the subject of [my talk yesterday at the Australian Statistical Conference](/presentations/ellis-prediction-intervals-for-ensemble-forecasts.pptx).  

## Code
Here's the code that does the simulation above:

{% highlight R %}
library(forecast)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(grid)
library(colorspace)


#=============prediction interval functions===================
#' takes a forecast object, and the actual results, and returns a data frame
#' binary indicator of success as well as the horizon for each row and an optional
#' "type" column.
accuracy_pi <- function(fc, actual, type = NULL, labs = c("80%", "95%")){
   if(nrow(fc$upper) != length(actual)){
      stop("`fc` must be a forecast object with `upper` and `lower` objects that are matrices with number of rows equal to length of `actual`")
   }
   actual <- as.vector(actual)
   h <- length(actual)
   # take advantage of actual being a vector that will recycle, going down each
   # column of the two matrices of logical conditions we superimpose here:
   tmp <- as.data.frame(fc$lower < actual & fc$upper > actual)
   names(tmp) <- labs
   tmp <- cbind(tmp, 
                "h" = 1:h,
                "type" = rep(type, h))
   return(tmp)
}

#===================simulations=======================
n <- 100 # size of full series
h <- 10  # how much of the series to forecast forward
R <- 10000 # how many datasets to try this with
the_model <- list(ar = 0.5, ma = 0.5)

cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(forecast)
   
})

results <- foreach(1:R, .combine = rbind) %dopar% {
   sim_data <- ts(cumsum(arima.sim(model = the_model, n = n)))
   
   x <- sim_data[1:(n - h)]
   actual <- sim_data[(n - h + 1):n]
   mod1 <- auto.arima(x)
   fc1 <- forecast(mod1, h = h)
   
   mod2 <- Arima(x, order = c(1, 1, 1))
   fc2 <- forecast(mod2, h = h)
   rbind(
      accuracy_pi(fc1, actual = actual, type = "auto"),
      accuracy_pi(fc2, actual = actual, type = "pre-knowledge")
   )
}


stopCluster(cluster)

#==========present results==================
pal <- rainbow_hcl(2)

targetlines <- data.frame(
   limit = c("80%", "95%"),
   yintercept = c(0.8, 0.95)
)

results %>%
   gather(limit, success, -h, -type) %>%
   group_by(h, type, limit) %>%
   summarise(success = mean(success)) %>%
   ggplot(aes(x = h, y = success, colour = type)) +
   facet_wrap(~limit) +
   geom_line() +
   scale_y_continuous(label = percent) +
   ggtitle("Prediction interval coverage of two modelling methods",
           subtitle = "Comparing auto.arima with pre-knowledge of the true data generating process") +
   labs(x = "Number of periods forecast forward",
        caption = "Data generated with an ARIMA(1,1,1) process, which is much more regular than real life data.",
        colour = "Modelling approach:") +
   scale_colour_manual(values = pal) +
   scale_x_continuous(breaks = 0:5 * 2) +
   geom_hline(data = targetlines, aes(yintercept = yintercept), colour = "violet", linetype = 2, size = 1.1)

grid.text(0.3, y= 0.65,
          label = str_wrap("When the true model family and the true meta-parameters p, d, q are known, coverage is close to the 80% and 95% promised.", 33),
          gp = gpar(family = "myfont", col = pal[2], cex = 0.8))
grid.text(0.8, y= 0.45,
          label = str_wrap("When the meta parameters p, d and q are estimated from the data, coverage is materially less.", 33),
          gp = gpar(family = "myfont", col = pal[1], cex = 0.8))

grid.text(0.9, y = 0.779, label = "Gap from estimating\nmeta-parameters",
          gp = gpar(family = "myfont", col = "grey50", cex = 0.7))

grid.text(0.85, y = 0.840, label = "Gap from estimating parameters",
          gp = gpar(family = "myfont", col = "grey50", cex = 0.7))
 
#=================demo data============
# four example datasets
set.seed(123) # for reproducibility
par(mfrow = c(2, 2), bty = "l", font.main = 1)
for(i in 1:4){
   
   sim_data <- ts(cumsum(arima.sim(model = the_model, n = n)))
   
   x <- sim_data[1:(n - h)]
   actual <- sim_data[(n - h + 1):n]
   
   mod2 <- Arima(x, order = c(1, 1, 1))
   fc2 <- forecast(mod2, h = h)   
   plot(fc2, main = paste0("Simulated ARIMA(1,1,1) data: ", i))
   lines((n - h + 1):n, actual, col = "red")
}
{% endhighlight %}
