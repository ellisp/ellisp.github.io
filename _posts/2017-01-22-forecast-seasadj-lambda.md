---
layout: post
title: Does seasonally adjusting first help forecasting?
date: 2017-01-22
tag: 
   - Forecasting
   - Timeseries
   - Transformations
   - R
description: I test some forecasting models on nearly 3,000 seasonal timeseries to see if it's better to seasonally adjust first or to incorporate the seasonality into the model used for forecasting.  Turns out it is marginally better to seasonally adjust beforehand when using an ARIMA model and it doesn't matter with exponential smoothing state space models.  Automated use of Box-Cox transformations also makes forecasts with these test series slightly worse.  The average effects were very small, and dwarfed by different performance on different domains and frequency of data.
image: /img/0079-lmer-results.svg
socialimage: http://ellisp.github.io/img/0079-lmer-results.png
category: R
---

## The experiment
A colleague at work was working with a time series where one got quite different results depending on whether one seasonally adjusted it first, or treated the seasonality as part of a SARIMA (seasonal auto-regressive integrated moving average)  model.  I have some theories about why this might have happened which I won't go into in this post.  Instead, I'm going to report back on an experiment with the purpose of determining, empirically, if either of these strategies dominates the other or at least is better on average.

To do this I took all the quarterly and monthly time series from the [M3](https://forecasters.org/resources/time-series-data/m3-competition/) and the [Tourism](http://robjhyndman.com/papers/the-tourism-forecasting-competition/) forecasting competition data collections, available in the [Mcomp](https://CRAN.R-project.org/package=Mcomp) and [Tcomp](https://CRAN.R-project.org/package=Mcomp) R packages.  This is 2,977 data series in total.  I developed forecasts with eight different methods for the training set of each of these time series - the combinations of:

- (S)ARIMA and exponential smoothing state space (using `auto.arima` and `ets` from Hyndman's `forecast` package);
- with and without a Box-Cox transformation;
- seasonally adjusting before modelling and incorporating seasonality into the model.

I then compared the predictions of the models against the official test sets and calculated the mean absolute scaled error (MASE) with the `accuracy` function from `forecast`.  The forecasting horizons were 8 periods for the quarterly data, 18 periods for the M3 monthly data, and 24 periods for the tourism monthly data.

For the seasonal adjustment prior to modelling I made use of the [`stlm` function](https://www.rdocumentation.org/packages/forecast/versions/7.3/topics/forecast.stl) in the `forecast` package for which "forecasts of STL objects are obtained by applying a non-seasonal forecasting method to the seasonally adjusted data and re-seasonalizing using the last year of the seasonal component."

I was interested in the impact of a Box-Cox transformation on forecasting accuracy in its own right, but also particularly in its interaction with seasonal adjustment.  I had a sort of working hypothesis that modelling untransformed data that isn't second-order stationary is made harder by seasonality.  My intuition there was that if (as is common with untransformed data) the variance of a time series increases as its mean increases, the seasonality may magnify this effect, giving even more inappropriate weight than otherwise to parts of the time series with a higher mean value.  This would suggest that, with ARIMA models that rely on second-order stationarity, it would be helpful to do at least one of a Box-Cox transformation or seasonal adjustment before fitting the model.  *spoiler - the results aren't completely consistent with this; seasonal adjustment helps, but Box-Cox transformations don't seem to*.

For the models that included an automated Box-Cox transformation, the value of lambda for the transformation was chosen by Guerrero's method implemented in `forecast::BoxCox.lambda`, but constrained to be between 0 and 1 (or 0.5 and 1.0 if the data included values of zero, which tended to result in very erratic forecasts when values of lambda were below 0.5). 

## Results

Here's an example of how the results look for just one of the collections of data; quarterly time series from the M3 data collection.  Each point on the plot below is a single data series.  The horizontal axes show the error from the forecasts that incorporate seasonality into the models; the vertical axes show the same for when the series were seasonally adjusted before the modelling.  The diagonal line shows equality - where a point would be if the two methods gave equally accurate forecasts:

![mq](/img/0079-m-results-quarterly-2.svg)

Several things stand out:

- The results are quite different.  For any individual data series, you might get much worse/better results with one of the methods rather than the other.  The differences can be quite substantial.
- On average there's no really obvious trend.  The cloud of points is actually a little to the right and below the diagonal line, showing that errors are slightly lower when seasonal adjustment is done first, but not to the degree it's obvious to the eye.
- The decision to Box-Cox transform or not first also doesn't have any large, obvious overall average impact.  The two trend lines drawn through the cloud of points are practically identical.  This doesn't mean the decision doesn't matter for an individual series' forecast, just that on average neither strategy is clearly better.

Here are the overall results for all data collections in a graphic designed to show overall effects:

![box](/img/0079-all-boxplot.svg)

There's not much to tell here!  For the ARIMA models (but not so much ETS), the blue boxes are a tiny bit to the left of the orange boxes, showing that on average the strategy of prior seasonally adjusting the series is a little better than incorporating seasonality into the forecasting model.

To quantify these results, I estimated a mixed-model with 

- Mean Absolute Scaled Error as the response term; 
- modelling type, transformation type, and seasonal strategy as fixed explanatory effects of interest; 
- data origin (M3 or Tourism) and frequency (quarterly or monthly) as fixed explanatory nuisance effects; and 
- randomness at the level of data series, and of data series - strategy combination.  

Here's the summary of the fixed effects for the simplest model: 

<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>mase</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">modelETS</td><td>-0.013<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">transformNone</td><td>-0.032<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">seasadjSeasonally adjusted first</td><td>-0.027<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.005)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">frequencyquarterly</td><td>0.238<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.031)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">collectionTourism</td><td>0.563<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.035)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.940<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>23,816</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-17,232.660</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>34,481.320</td></tr>
<tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>34,545.950</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

Positive coefficients mean that factor is associated with a *higher* mean absolute scaled error ie worse predictions.  Negative values, like those associated with the `ets` models, no Box-Cox transformation, and seasonal adjustment prior to modelling, mean better predictions.  Tourism data has a higher average scaled error, as does quarterly data (compared to monthly).

This table of coefficient estimates was produced with `stargazer`, which is awesome but unfortunately doesn't include a summary of the random effects from a mixed effects model.  There are [workarounds for this](http://svmiller.com/blog/2015/02/quasi-automating-the-inclusion-of-random-effects-in-rs-stargazer-package/) but I couldn't be bothered. In this case, the size of those random effects is just a nuisance anyway.  For the record, the standard deviation at the level of "model - series" combination was 0.81 and of the lowest level individual residuals was 0.40.

What we see here is that there are small but noticeable impacts:

- `ets` performs slightly better (ie lower mean absolute scaled error, on average) than `auto.arima`
- Fitting the model on untransformed data performs slightly better than using an automated Box Cox transformation
- Seasonally adjusting first performs slightly better than incorporating the seasonality into the model

### Subtlety - `ets` versus `auto.arima`

The conclusions above are fine for a general takehome, but it's worth noting that a better performing model to explain the experimental forecasting performance results (on the basis of AIC) includes a set of interaction effects between model type and my experimental factors of seasonal adjustment and transformation strategies.  I performed a single comparison of a full model with the expected interactions versus the simple model.  The interactions I looked at were between the model type (`ets` versus `auto.arima`) and the other explanatory variables; and between Box-Cox transformation and seasonality strategy.  This superior model is quite a bit harder to interpret:

<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>mase</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">modelETS</td><td>-0.052<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">transformNone</td><td>-0.061<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">seasadjSeasonally adjusted first</td><td>-0.065<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">frequencyquarterly</td><td>0.212<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.032)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">collectionTourism</td><td>0.604<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.035)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">transformNone:seasadjSeasonally adjusted first</td><td>0.034<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">modelETS:transformNone</td><td>0.038<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">modelETS:seasadjSeasonally adjusted first</td><td>0.059<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">modelETS:frequencyquarterly</td><td>0.052<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">modelETS:collectionTourism</td><td>-0.084<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.012)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">modelETS:transformNone:seasadjSeasonally adjusted first</td><td>-0.031</td></tr>
<tr><td style="text-align:left"></td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.965<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>23,816</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-17,208.300</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>34,444.600</td></tr>
<tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>34,557.690</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

It's basically impossible to think through the various interactions in the above single combined model, so purely for the purpose of presenting results I refit the model separately to the `ets` results and the `auto.arima` results.

#### ets
<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>mase</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">transformNone</td><td>-0.022<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">seasadjSeasonally adjusted first</td><td>-0.006</td></tr>
<tr><td style="text-align:left"></td><td>(0.009)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">frequencyquarterly</td><td>0.264<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.033)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">collectionTourism</td><td>0.521<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.036)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">transformNone:seasadjSeasonally adjusted first</td><td>0.003</td></tr>
<tr><td style="text-align:left"></td><td>(0.012)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.913<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>11,908</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-8,653.195</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>17,322.390</td></tr>
<tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>17,381.470</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

#### auto.arima
<table style="text-align:center"><tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="1" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td>mase</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">transformNone</td><td>-0.061<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">seasadjSeasonally adjusted first</td><td>-0.065<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.010)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">frequencyquarterly</td><td>0.212<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.032)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">collectionTourism</td><td>0.604<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.035)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">transformNone:seasadjSeasonally adjusted first</td><td>0.034<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.015)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>0.965<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td></tr>
<tr><td style="text-align:left"></td><td></td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>11,908</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-10,361.570</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>20,739.150</td></tr>
<tr><td style="text-align:left">Bayesian Inf. Crit.</td><td>20,798.220</td></tr>
<tr><td colspan="2" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>

And here's a graphic representation that helps (a bit):

<img src ='/img/0079-lmer-results.svg' align="middle">

All my attempts to get a way to easily understand those implications couldn't improve on a simple contingency table of the average MASE values (these are trimmed means of the MASE return for all 2,977 series), using the tables of coefficients above to guide inference:

<table>
 <thead>
  <tr>
   <th style="text-align:left;">| transform    |</th>
   <th style="text-align:left;">| seasadj             |</th>
   <th style="text-align:right;">|   ARIMA    |</th>
   <th style="text-align:right;">|   ETS      |</th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> BoxCox </td>
   <td style="text-align:left;"> Seasonal in model </td>
   <td style="text-align:right;"> 1.020 </td>
   <td style="text-align:right;"> 0.987 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BoxCox </td>
   <td style="text-align:left;"> Seasonally adjusted first </td>
   <td style="text-align:right;"> 0.987 </td>
   <td style="text-align:right;"> 0.993 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None </td>
   <td style="text-align:left;"> Seasonal in model </td>
   <td style="text-align:right;"> 0.993 </td>
   <td style="text-align:right;"> 0.976 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> None </td>
   <td style="text-align:left;"> Seasonally adjusted first </td>
   <td style="text-align:right;"> 0.976 </td>
   <td style="text-align:right;"> 0.992 </td>
  </tr>
</tbody>
</table>

## Conclusion

- For an ARIMA model, the best approach on average is to seasonally adjust first, and not use an automated Box-Cox transformation.  
- For an ETS model, no evidence either way on whether to seasonally adjust first or to incorporate into the model; but again, it is better to not automatically use a Box-Cox transformation.

That it's best not to automatically use a Box-Cox transformation slightly surprised me, particularly for SARIMA models which assume second order stationarity.  Something to think about.

Limitations or caveats include:

- The accuracy checks were all done on a single point, 8 (for quarterly data) and 18 or 24 (for monthly) periods after the end of the training series.  The results may not generalize, particularly for forecast horizons that aren't a relatively simple multiple of the series' frequency.
- `stlm` is not the state of the art in seasonal adjustment.  A more thorough experiment would use X13-SEATS-ARIMA to do the seasonal adjustment.  My reason for *not* doing this was convenience - `stlm` in combination with `forecast` makes it easy to backtransform the forecasts to the original seasonal version that I did the accuracy check on.
- There may be a better way to automate the choice of Box-Cox transformation.

## Code

All the code for the above (and a bit more) in one chunk today:

{% highlight R %}
library(tidyverse)
library(Mcomp)
library(Tcomp)
library(foreach)
library(doParallel)
library(scales)
library(broom)
library(stringr)
library(stargazer)
library(lme4)
library(forcats)

#=========================analysis functions=======================

# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(forecast)
})


#' fit 8 models (all combinations of ets/arima, seasonally adjusted / not, BoxCox transformed / not)
#' to all data series in data_collection.  data_collection needs to be an object of class Mcomp
#' @value data frame with columns for mase (mean absolute scaled error), series number, and
#' three columns with information on the characteristics of the fit model
eval_forecasts <- function(data_collection){
   clusterExport(cluster, "data_collection")
   
   results <- foreach(i = 1:length(data_collection), .combine = rbind) %dopar% {
      the_data <- data_collection[[i]]
   
      x <- the_data$x
      xx <- the_data$xx
      h <- the_data$h
      
      l <- BoxCox.lambda(x)
      l <- max(min(l, 1), 0)
      
      # some time series with 0 in them fail catastrophically with stlm if lambda is close
      # to zero, so we force them to be at most a square rootish transformation
      if(min(x) <= 0){ l <- max(l, 0.5)}
      
      fc <- list()
      ac <- numeric()
      
      # no seasonally adjustment, Box Cox transform
      fc[[1]] <- forecast(auto.arima(x, lambda = l), h = h)
      fc[[2]] <- forecast(ets(x, lambda = l), h = h)
      
      # no seasonally adjustment, no Box Cox transform
      fc[[3]] <- forecast(auto.arima(x), h = h)
      fc[[4]] <- forecast(ets(x), h = h)
      
      # seasonally adjust first, Box Cox transform
      fc[[5]] <- forecast(stlm(x, method = "arima", lambda = l), h = h)
      fc[[6]] <- forecast(stlm(x, method = "ets", lambda = l), h = h)
      
      # seasonally adjust first, no transform
      fc[[7]] <- forecast(stlm(x, method = "arima"), h = h)
      fc[[8]] <- forecast(stlm(x, method = "ets"), h = h)
      
      ac <- sapply(fc, function(mod){
         accuracy(mod, xx)["Test set", "MASE"]
      })
      
      data.frame(mase = ac,
                 model = rep(c("ARIMA", "ETS"), 4),
                 transform = rep(c("BoxCox", "BoxCox", "None", "None"), 2),
                 seasadj = rep(c("Seasonal in model", "Seasonally adjusted first"), each = 4),
                 series = i)
   }
   return(results)
}

#=============plotting and summary functions====================
# Functions for summarising the results of eval_forecasts()
p2 <- function(results){
   results %>%
      spread(seasadj, mase) %>% 
      ggplot(aes(y = `Seasonally adjusted first`, x = `Seasonal in model`, colour = transform)) +
      geom_point(alpha = 0.2) +
      geom_abline(slope = 1, intercept = 0) +
      geom_smooth(se = FALSE) +
      facet_wrap(~model) +
      scale_x_log10() +
      scale_y_log10() +
      coord_equal() +
      labs(x = "Mean absolute scaled error from fitting a model to the original data",
           y = "Error from fitting a model to data\nthat was first seasonally adjusted",
           colour = "")
}

t1 <- function(results){
   results %>%
      group_by(seasadj, transform, model) %>%
      summarise(mase = round(mean(mase, tr = 0.1), 2)) %>%
      spread(seasadj, mase) 
}

#=============apply to data=============

m_results_monthly <- eval_forecasts(subset(M3, "MONTHLY"))
m_results_quarterly <- eval_forecasts(subset(M3, "QUARTERLY"))
t_results_monthly <- eval_forecasts(subset(tourism, "MONTHLY"))
t_results_quarterly <- eval_forecasts(subset(tourism, "QUARTERLY"))

#-----------------plots of individual results--------------

p2(m_results_monthly);
t1(m_results_monthly)

p2(m_results_quarterly)
t1(m_results_quarterly)

p2(m_results_quarterly ) +
   ggtitle("Marginally better results on average from seasonally adjusting\na series prior to modelling and forecasting",
           "... and little obvious change from choosing to Box-Cox transform or not") +
   labs(caption = "Quarterly data from the M3 forecasting competition")

p2(t_results_monthly)
t1(t_results_monthly)

p2(t_results_quarterly)
t1(t_results_quarterly)


#--------------------combined results-------------------
all_results <- rbind(
   m_results_monthly,
   m_results_quarterly,
   t_results_monthly,
   t_results_quarterly
) %>%
   mutate(
      collection = rep(c("M3", "Tourism"), 
                       c(nrow(m_results_monthly) + nrow(m_results_quarterly),
                         nrow(t_results_monthly) + nrow(t_results_quarterly))),
      frequency = rep(c("monthly", "quarterly", "monthly", "quarterly"), 
                      c(nrow(m_results_monthly) , nrow(m_results_quarterly),
                        nrow(t_results_monthly) , nrow(t_results_quarterly))),
      dataset_series = paste(collection, frequency, series),
      collection_frequency = paste(collection, frequency)
   ) %>%
   as_tibble()

ggplot(all_results, aes(x = transform, colour = seasadj, y = mase)) +
   facet_grid(model ~ collection_frequency) +
   geom_boxplot()  +
   coord_flip() +
   scale_y_log10("Mean absolute scaled error") +
   labs(x = "", colour = "", 
        caption = "2,977 quarterly and monthly datasets from the M3 and Tourism forecasting competitions") +
   ggtitle("Comparison of variants in forecasting methods",
           "Box-Cox transformation or not; seasonally adjust the data before model fitting or not.")

#==========modelling of results======================
model <- lmer(mase ~ model + transform + seasadj + frequency + collection + (1|dataset_series), data = all_results)
model_inters <- lmer(mase ~ model * (transform * seasadj + frequency + collection) + (1|dataset_series), data = all_results)

AIC(model_inters, model)

stargazer(model, type = "html")
stargazer(model_inters, type = "html")



# Interpreting all those interactions is basically impossible as-is, so just for
# illustrating results I fit models separately to ETS and ARIMA
model_ets <- lmer(mase ~ transform * seasadj + frequency + collection + (1|dataset_series), 
                  data = subset(all_results, model == "ETS"))
model_arima <- lmer(mase ~ transform * seasadj + frequency + collection + (1|dataset_series), 
                  data = subset(all_results, model == "ARIMA"))


stargazer(model_ets, type = "html")
stargazer(model_arima, type = "html")

effects_ets <- cbind(tidy(confint(model_ets))[-(1:3), ],
                 tidy(model_ets)[2:6, ],
                 model = "ETS")

effects_arima <- cbind(tidy(confint(model_arima))[-(1:3), ],
                     tidy(model_arima)[2:6, ],
                     model = "ARIMA")

effects <- rbind(effects_ets, effects_arima)

names(effects)[1:3] <- c("variable", "lower", "upper")

effects %>%
   ggplot(aes(x = variable, y = estimate)) +
   facet_wrap(~model) +
   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4)  +
   geom_point() +
   coord_flip() +
   geom_hline(yintercept = 0, colour = "red") +
   labs(y = "Impact on forecasting, measured by mean absolute scaled error", x = "",
        caption = "Tested on all 2,977 quarterly and monthly datasets from the M3 and Tourism forecasting competitions") +
   ggtitle("Small but noticeable impact of different forecasting methods",
"ARIMA works best when seasonally adjusted beforehand rather than seasonality included in the model.
For ETS, no significant evidence either seasonal adjustment strategy is better.
For both models, it seems better not to automatically use a Box-Cox transformation.")

all_results %>%
   group_by(model, transform, seasadj) %>%
   summarise(mase = round(mean(mase, tr = 0.1), 3)) %>%
   spread(model, mase) %>%
   knitr::kable("html")
{% endhighlight %}