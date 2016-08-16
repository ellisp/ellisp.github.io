---
layout: post
title: Elastic net regularization of a model of burned calories
date: 2016-08-13
tag: 
   - R
   - ModellingStrategy
description: Elastic net regularization of estimates is a good way of dealing with collinearity and feature selection; this is illustrated with a simple dataset of 30 daily observations from a fitbit tracker.
image: /img/0050-pairs.svg
socialimage: http://ellisp.github.io/img/0050-pairs.png
category: R
---

## Deal with feature selection and collinearity

Recently I've been making more use of [elastic net regularization](https://en.wikipedia.org/wiki/Elastic_net_regularization) as a way of fitting linear models to data when I have more candidate explanatory variables than I know what to do with and some of them are collinear ie their information doubles up on what is in other variables.  

Elastic net regularization is a more general form of the ridge regression and lasso methods.  Ridge regression was primarily a method to deal with the instability of estimates of the coefficients of linear models when they are collinear; the lasso a method of feature selection that forces coefficients for some/many explanatory variables to be zero and provides good estimates of the coefficients of the remaining features.  Both are preferable to stepwise selection which provides estimates of effect sizes biased upwards in absolute magnitude.  I deliberately use the term "was" for ridge regression because I don't see any reason for using either of these methods any more; they are superseded by the more general elastic net.  A hyperparameter `alpha` means the method is equivalent to lasso when `alpha == 1` and to ridge regression when `alpha == 0`.  We can use cross-validation assessing the fit of the best models with different values of `alpha` to work out the best value of `alpha`.

The method works as an extension of ordinary least squares (OLS), the most common and basic (ie Stats 101) method of estimating the coefficients in a linear model.  Instead of minimising the sum of the squares of the differences between predicted and actual values of the response variables (the OLS method), the elastic net regularization estimation process minimises that sum of those squares *and* a penalty based on the size of the estimated coefficients (on a standardised scale) in the model.  I like to think of the impact of this as to create an increased burden of proof on the data for including a variable's effect in the model; the estimation process drags coefficients towards zero in a spirit of conservatism.  The `alpha` hyperparameter controls the *balance* between the different penalty methods of ridge and lasso.  A second hyperparameter, `lambda` sets the total *weight* of the penalty, and can also be set at the most effective level by trying different values with cross validation.

While ridge regression has been around for decades and the [lasso since the 1990s](http://www.jstor.org/stable/2346178), the generalisation to elastic net regularations was only [properly elaborated in 2005](https://web.stanford.edu/~hastie/Papers/B67.2%20(2005)%20301-320%20Zou%20&%20Hastie.pdf) and software to implement it is fairly new.  The [`glmnet` package in R](https://cran.r-project.org/web/packages/glmnet/index.html) gives very fast estimation and good cross-validation methods, and scales up well to large and wide datasets.

## A modest sized dataset with collinearity

To build more familiarity with this tool I grabbed a small dataset of 30 observations from my fitbit tracker.  I originally downloaded the data from the web browser interface in accordance with the [fitbit instructions](https://help.fitbit.com/articles/en_US/Help_article/1133).  For reproducibility I've made it [available to others](https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/fitbit_export_20160806.csv) on the web. 

My aim with this data is to reverse engineer the way fitbit estimates calories burned in a day.  I know from [their website](https://help.fitbit.com/articles/en_US/Help_article/1381) that this is done based on an estimate of my basal metabolic rate and the activities my tracker records (plus any activities manually logged by me, which is none in my case).  

Basal metabolic rate is dependent on my gender, age, height and weight, none of which changed in this thirty day period, so I would expect that component of the calories burnt to be constant over time.  As well as the calories burned per day, the information the fitbit provides on my activities is available in these variables:

* steps
* distance (estimated from steps)
* floors (ie of stairs and stair-like equivalents - some of this period I walked up hills in Western Australia's Stirling Ranges)
* sedentary minutes
* lightly active minutes
* fairly active minutes
* very active minutes

Here's how those seven candidate explanatory variables relate to eachother:

![pairs](/img/0050-pairs.svg)

From this chart we see a few things of interest:

* `Distance` can be calculated almost exactly directly from `Steps` 
* The number of sedentary minutes in a day is inversely related to the other minutes measured - lightly active, fairly active, and very active

I wondered if the four minutes measures would be perfectly collinear - there's a set number of minutes each day after all, so if I know how long I've been lightly active, fairly active and very active couldn't I precisely say that the remaining minutes were sedentary?  A bit of exploration shows that the four "minutes" measures tend to add up to 15 or 16 hours a day, suggesting that sedentary minutes actually means "sedentary but awake minutes".  So they are not completely collinear because the amount I sleep varies from day to day.

For those who want to follow along in R, here's how I did that chart with the correlation coefficients in the lower triangle of panels.  This adapts an example from the helpfile for the `pairs()` function in base graphics.
{% highlight R %}
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(RColorBrewer)

fitbit <- read.csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/fitbit_export_20160806.csv", 
                   skip = 1, stringsAsFactors = FALSE) %>%
   mutate(
      Calories.Burned = as.numeric(gsub(",", "", Calories.Burned)),
      Steps = as.numeric(gsub(",", "", Steps)),
      Activity.Calories = as.numeric(gsub(",", "", Activity.Calories)),
      Date = as.Date(Date, format = "%d/%m/%Y")
   )

fitbit2 <- fitbit
fitbit2$Activity.Calories <- NULL
fitbit2$Date <- NULL
fitbit2$Steps <- fitbit2$Steps / 1000 # so coefficients will be calories per thousand steps, easier to describe

par(family = "myfont")
# let's look at all the candidate variables
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  # function for calculating correlation coefficients and using
  # as panels in pairs() function.  Taken from ?pairs
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(fitbit2[ , -1], lower.panel = panel.cor, main = "Pairwise relationships of Fitbit's measured activities")

{% endhighlight %}

The close relationship between steps and distance (correlation coefficient of 1.00, which means that knowing one you can almost exactly predict the other) isn't surprising to me because fitbit's website makes clear it estimates distance based on steps and stride length.  It applies one stride length to convert from steps to distance for walking, and one for running (which it can detect, presumably, from the pace and violence of motion), but I know that I did virtually no running in this period so it's only converting from steps to walking.  Here's the distribution of fitbit's estimated distance divided by steps:

![stride](/img/0050-stridelenth.svg)

{% highlight R %}
ggplot(fitbit2, aes(x = Distance / Steps)) + 
   geom_rug() + 
   geom_density() +
   ggtitle("Stride length reverse-engineered from Fitbit data",
             subtitle = "Not all strides identical, due to rounding or other jitter")
{% endhighlight %}

## 69 calories per 1,000 steps on average

To get a sense of the data I was interested in a super-simple model with a single explanatory variable.  "What's the relationship between the number of steps walked and calories burned, ignoring height gains and intensity of heart rate", I asked myself?  Judging from various sites like [this](http://www.livestrong.com/article/320124-how-many-calories-does-the-average-person-use-per-step/) and [this](http://www.livestrong.com/article/238020-how-to-convert-pedometer-steps-to-calories/) the answer might be about 40-50 calories per thousand steps for walking.  Fitbit would use a more sophisticated model than this and take into account the fact that some of my walking was on tougher terrain and/or up stairs or slopes which would reflect in increased heart rate, so it will be interesting to see its estimates of calories per thousand steps compared to the simple website estimates.

The way to do this is just an ordinary least squares estimation of a model with steps as the sole explanatory variable and calories burned as the response variable.  This returns the estimate of my base daily rate of 1,926 calories per day, and 69 additional calories burned per thousand steps.  This figure of 69 is more than the "moderate walking" rates on various websites, presumably because of the terrain and slope issues mentioned above.

{% highlight R %}
mod0 <- lm(Calories.Burned ~ Steps, data = fitbit2)
round(coef(mod0))
## (Intercept)       Steps 
##       1926          69 
{% endhighlight %}

That figure of 1,926 per day as my "no steps" base calory burn is also somewhat more than the 1,629 that [this website](http://www.bodybuilding.com/fun/bmr_calculator.htm) says should be my basal metabolic rate based on the Harris-Benedict equation.  I'm not sure what's going on here - either fitbit uses a different calculation, or perhaps it's judging various activities from me from my heart rate that don't feature in steps, or something else.  Fitbit doesn't know my real calory burn rates, so it can't be something mysterious to do with my metabolism as it has no way of knowing, it just estimates based on steps, heart rate, weight, height, etc.  Anyway, the simple model above isn't too bad an estimate of how many calories fitbit *says* I use; for the record, here's a comparison of the predicted calories compared to residuals from that model:

![residuals](/img/0050-1var-resid.svg)

And just to put one issue to rest, here's the partial autocorrelation function of the residuals, showing that there's no particular pattern over time not captured in the model.  This is not a particularly interesting chart but I'm including it because it should be standard practice to check for such patterns (which would be shown by one of the vertical bars crossing the horizontal blue lines) when working with data that was captured over a time period.

![pacf](/img/0050-1var-resid-pacf.svg)

{% highlight R %}
# check residuals v. fit
plot(mod0, which = 1, bty = "l"); grid()

# check autocorrelation:
pacf(resid(mod0), main = "Partial autocorrelation of residuals from single variable regression")
{% endhighlight %}

## Choose alpha by cross-validation over a grid of candidate values

Now I'm ready to move to my full model, predicting daily calories based on all the seven explanatory variables available.

The first step is choosing an appropriate alpha - the balance between the extremes of ridge regression and lasso estimation.  I choose repeated cross-validation to do this.  Basically this means fitting the model to slightly different re-samples many times at different values of alpha, using the fit model to predict the "out of bag" points from the original sample that weren't in the re-sample.  The results show only a weak impact of the choice of alpha:

![alphas](/img/0050-cv-alpha.svg)

Here's the code that did this.
{% highlight R %}
# create X matrix and Y vector for use with glmnet, which doesn't take Rmodel formulae
# First column of fitbit2 is the response variable "Calories burned" so we exclude from X:
X <- as.matrix(fitbit2[ , -1]) # standardisation happens as part of glmnet
Y <- fitbit2$Calories.Burned

set.seed(123)
alphas <- seq(from = 0, to  = 1, length.out = 10)
res <- matrix(0, nrow = length(alphas), ncol = 6) # five columns for results - five repeats of each CV run
for(i in 1:length(alphas)){
   for(j in 2:6){
      cvmod <- cv.glmnet(X, Y, alpha = alphas[i])
      res[i, c(1, j)] <- c(alphas[i], sqrt(min(cvmod$cvm)))
   }
}
res <- data.frame(res)
res$average_rmse <- apply(res[ , 2:6], 1, mean)
res <- res[order(res$average_rmse), ]
names(res)[1] <- "alpha"

res %>%
   select(-average_rmse) %>%
   gather(trial, rmse, -alpha) %>%
   ggplot(aes(x = alpha, y = rmse)) +
   geom_point() +
   geom_smooth(se = FALSE) +
   labs(y = "Root mean square error") +
   ggtitle("Cross validation best RMSE for differing values of alpha")

# best alpha varies according to the random seed set earlier but with seed 123 it is 0.667
bestalpha <- res[1, 1]
{% endhighlight %}

## Elastic net gives more sensible results in presence of collinearity

Now that I have a preferred value of alpha I can use the elastic net to get estimated values of the eight coefficients (seven explanatory variables plus an intercept), and compare them to the ordinary least squares equivalents.  Remember that there's a second hyper-parameter, `lambda` to estimate too - again I choose this from cross-validation with the chosen value of `alpha`, and following the implied suggesting on the `glmnet` package I try two versions of `lambda` - the level of shrinkage that gives the smallest errors in cross validation, and the higher level of shrinkage that gets acceptable close (within one standard deviation in the cross validation process) to that smallest error.  This practice is predicated on the idea that a model with a higher degree of shrinkage may be cheaper (in data management or interpretation) than a lower, so it is worth taking a small hit of accuracy for it.

Here are the results from the three estimation methods:
{% highlight R %}
                       original   shrunk very.shrunk
(Intercept)            1941.189 1971.199    2157.172
Steps                   -66.827    9.176      15.703
Distance                116.077   15.045      21.835
Floors                    0.027    0.000       0.000
Minutes.Sedentary        -0.246   -0.236      -0.187
Minutes.Lightly.Active    2.247    1.985       0.888
Minutes.Fairly.Active     4.402    4.384       3.842
Minutes.Very.Active       3.895    3.295       1.019
{% endhighlight %}

As experienced modellers would have expected, the "original" (ie ordinary least squares) method gives non-sensible estimates of the coefficients of the two highly collinear variabls, `Steps` and `Distance`.  it clearly makes no sense to say that my calories consumed go *down* by 67 for each additional 1,000 steps but *up* by 116 for each kilometre.  The elastic net method also struggles with these two, but returns values that are better for interpretation.

Here's the code for obtaining those coefficient estimates:
{% highlight R %}
# Cross-validated version to determine lambda at best value of alpha, for use later
cvmod <- cv.glmnet(X, Y, alpha = bestalpha)

# the model itself
mod1 <- glmnet(X, Y, alpha = bestalpha)

# the OLS model
mod2 <- lm(Calories.Burned ~ ., data = fitbit2)

coefs <- data.frame(original = coef(mod2), 
                  shrunk =  as.vector(coef(mod1, s = cvmod$lambda.min)),
                  very.shrunk = as.vector(coef(mod1, s = cvmod$lambda.1se)))

round(coefs, 3)
{% endhighlight %}

## Even with `lambda=0`, elastic net gives different result to OLS due to numerical calculation issues

I thought I would check what happens if lambda is set to zero so there is no penalty for the total size of coefficients.  In theory, the result should be the same as the ordinary least squares estimate, but in the presence of severe collinearity like we have here (between `Steps` and `Distance`) this turns out not to be the case.  

{% highlight R %}
> mod3 <- glmnet(X, Y, lambda = 0)
> round(data.frame("elastic, lambda = 0" = as.vector(coef(mod3)), "lm" = coef(mod2), check.names = FALSE), 3)
                       elastic, lambda = 0       lm
(Intercept)                       1937.924 1941.189
Steps                               15.455  -66.827
Distance                             0.653  116.077
Floors                               0.011    0.027
Minutes.Sedentary                   -0.241   -0.246
Minutes.Lightly.Active               2.236    2.247
Minutes.Fairly.Active                4.415    4.402
Minutes.Very.Active                  3.894    3.895
{% endhighlight %}

This comes about from numerical issues to do with the estimation method under the hood in `glmnet` and I won't try to understand or explain exactly why.  If we remove the severe collinearity (which, in a real life situation, I would have done as the very first step when I realised `Distance` contains no real information additional to `Steps`) the estimated coefficients are almost identical, as they should be:

{% highlight R %}
> # what about a simpler case with less numerical instability - 
> # drop "distance" (which is the second column)
> mod4 <- glmnet(X[ , -2], Y, lambda = 0)
> mod5 <- lm(Y ~ X[ , -2])
> round(data.frame("elastic, lambda = 0" = as.vector(coef(mod4)), "lm" = coef(mod5), check.names = FALSE), 3)
                       elastic, lambda = 0       lm
(Intercept)                       1938.103 1938.129
Steps                               15.885   15.798
Floors                               0.011    0.010
Minutes.Sedentary                   -0.241   -0.241
Minutes.Lightly.Active               2.236    2.239
Minutes.Fairly.Active                4.415    4.413
Minutes.Very.Active                  3.897    3.906
{% endhighlight %}

## In this particular dataset, predictive power is similar in OLS and after elastic net regularisation

To compare the predictive strength of these different models I used the simple bootstrap, where the modelling approach is applied to bootstrap resamples of the data and the estimated model then used to predict the full original dataset.  The elastic net regularized model slightly out-performs the ordinary least squares model with all variables; both significantly out-perform the super-simple one variable model with only `Steps` in the explanatory side of the formula.  Here are the root mean square errors of the three different estimation methods:
{% highlight R %}
  elastic lm_allvar   lm_1var 
     95.7      99.5     165.0 
{% endhighlight %}

Code for the validation process:
{% highlight R %}
#----------------validation--------
# function to feed to boot that does elastic modelling
mod1f <- function(data, i){
   X <- as.matrix(data[i , -1])
   Y <- data[i , 1]
   cvmod <- cv.glmnet(X, Y, alpha = 1, nfolds = 30)
   mod1 <- glmnet(X, Y, alpha = 1)
   RMSE(predict(mod1, newx = as.matrix(data[ , -1]), s = cvmod$lambda.min), data[ , 1])
}

elastic_boot <- boot(fitbit2, statistic = mod1f, R = 99)


# function to feed to boot that does OLS modelling
mod2f <- function(data, i){
   mod2 <- lm(Calories.Burned ~ ., data = data[i, ])
   RMSE(predict(mod2, newdata = data), data[ , 1])
}

lm_boot <- boot(fitbit2, statistic = mod2f, R = 99)

# for boot with OLS modelling, only one explanatory variable
mod0f <- function(data, i){
   mod0 <- lm(Calories.Burned ~ Steps, data = data[i, ])
   RMSE(predict(mod0, newdata = data), data[ , 1])
}
lm0_boot <- boot(fitbit2, statistic = mod0f, R = 99)

round(c("elastic" = mean(elastic_boot$t), 
        "lm_allvar" = mean(lm_boot$t),
        "lm_1var" = mean(lm0_boot$t)), 1)
{% endhighlight %}

## "Floors" not needed for a good prediction of calories

One substantive issue of interest to me that I haven't mentioned was the relationship of "floors climbed" to total calories.  This is of interest because it's the sort of thing that it's easy to set a personal target for ("steps" is similar).  However, none of the estimation methods showed significant evidence of `Floors` as an explanatory variable.  Basically, the "minutes active" variables (at various levels of intensity) contains all the information needed by fitbit to make its estimates of calories.  Walking up stairs contributes to more active minutes as shown by heart rate but other than through that is not directly informative itself.

This can be illustrated by what has become one of the standard plots for this sort of estimation method - a graphic illustration of how the size of coefficients for different variables increase as the penalty against coefficients is relaxed:

![variables](/img/0050-elastic-coefs.svg)

We see from this that even the fullest model leaves a zero coefficient for variable #3, `Floors` - the horizontal green line.  `Minutes.Very.Active` is the second last variable to be allowed to increase from zero - it contains relatively little information after all the other "minutes" variables are included.

Code for that graphic:
{% highlight R %}
# refit the model with scaled variables just for the graphic.
# working with scaled variables just for visual distinguishing
# shouldn't make a difference as under the hood glmnet does it anyway
# in the models fit earlier.
mod1s <- glmnet(scale(X), Y, alpha = bestalpha)

# set a palette
thepal <- brewer.pal(7, "Set1")

# set the ordering for ease of reading on graphic
ordering <- c(7,5,6,2,1,3,4)

par(mar = c(5.1, 4.1, 6.5, 1), bg = "grey90")
plot(mod1s, xvar = "dev", label = TRUE, col = thepal, lwd = 2, main = 
        "Increasing contribution of different explanatory variables\nas penalty for including them is relaxed")

legend("topleft", legend = colnames(X)[ordering], text.col = thepal[ordering], 
       lwd = 2, bty = "n", col = thepal[ordering])
{% endhighlight %}
