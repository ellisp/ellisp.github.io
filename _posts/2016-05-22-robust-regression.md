---
layout: post
title: Visual contrast of two robust regression methods
date: 2016-05-22
tag: 
   - Animations
   - NewZealand
   - NZIS2011
   - RobustMethods
   - R
description: I use animations to show some of the properties of least trimmed squares compared to a Huber M estimator as alternative robust regression estimation methods for a simple linear models.

image: /img/0041-rtm-lqs.gif
socialimage: http://ellisp.github.io/img/0041-rtm-lqs.gif
category: R
---

## Robust regression

For training purposes, I was looking for a way to illustrate some of the different properties of two different [robust estimation methods](https://en.wikipedia.org/wiki/Robust_regression) for linear regression models.  The two methods I'm looking at are:

* [least trimmed squares](https://en.wikipedia.org/wiki/Least_trimmed_squares), implemented as the default option in `lqs()` 
* a [Huber M-estimator](https://en.wikipedia.org/wiki/M-estimator), implemented as the default option in `rlm()`

Both functions are in Venables and Ripley's `MASS` R package which comes with the standard distribution of R.

These methods are alternatives to ordinary least squares that can provide estimates with superior qualities when the classical assumptions of linear regression aren't met.  M-estimators are effective in dealing with non-Normal randomness and with non-constant variance, while still dealing excellent results if the assumptions happen to be valid after all.  Least trimmed squares is very resistant to outliers, at a cost to efficiency.

## New Zealand Income Survey 2011

To explore this I use the simulated unit record file from the New Zealand Income Survey 2011 that I've used in a lot of blog posts in the past year.  My [first post on this dataset](http://ellisp.github.io/blog/2015/08/15/importing-nzis-surf/) sets out how to import this data and tidy it up into a database.  The data shows weekly individual income from all sources for New Zealanders aged over 15, as well as hours worked and a range of demographic information I won't be using today.  For looking at robust regression estimation methods, I'm going to pretend that I only ever have a sample of thirty observations from this dataset.

To illustrate the data, here's an animation showing the full dataset (in grey) and repeated different samples of thirty data points, as well as lines representing linear models fit to those small samples with ordinary least squares (`lm`) and the two robust regression methods I'm investigating today.
![animation](/img/0041-rtm-lqs.gif)

Some characteristics of this data that make it a useful illustration for robust regression include:

* It's reasonable to postulate the underlying relationship between hours worked and income as linear for much of the population.  So a linear model on the original scale is likely to be appropriate.
* However, the population seems likely to be made up of a number of diverse groups, making assumptions of homogeneity implausible. For example, a large number of people have incomes, some of them quite substantial, despite working zero hours.  General economic and social knowledge suggests that divisions into wage-earners, profit-receivers, and social security receivers probably constitute three quite different populations, while still being a significant simplification of the actual heterogeneity.
* On the original scale the data are expected to show [heteroscedasticity](https://en.wikipedia.org/wiki/Heteroscedasticity) (in this case, higher variance for higher expected income), making classical inference invalid
* There are a number of outliers on both the x and y axes; and a cluster of individuals with negative income that appear to form yet another group in the mixture.

## Comparing robust methods

To reflect one of the key features of the data which is the unusual distribution of income when working hours are zero, the models fit in this post are all of the structure:

`income ~ hours + (hours == 0)`

The graphics show just the relationship between income and hours for when a non-zero amount of hours is worked.  In effect, (just for the graphics), this is like fitting the model after removing all the individuals that performed zero hours of work in the surveyed week.

The animation shown above was the germ idea for this post and already illustrates the key points.  In particular, it shows how the randomness associated with a small sample of thirty points leads to a different result for any particular sample; and once you have that sample, different estimation methods can give you materially differing results.  In some frames of the animation, we see a sample of thirty where slopes from the different estimation methods point in different directions, suggesting that if that particular small sample was the only one available, one might conclude that working more hours leads to decreased income.

The animation is nice for reminding us of the randomness associated with small samples, but actually isn't a particularly effective method of looking at how the different robust estimation methods are doing.  Here's a better way of doing that; a plot of the actual regression lines fit to 10,000 different samples of thirty.  Each line is semi-transparent, to give a better visual impression of where the bulk of fit lines end up:

![comparison](/img/0041-compare-lqs-rlm.png)

This makes it graphically clear that the `lqs` method (least trimmed squares) gives a noticeably wider range of results than does the Huber M-estimator.  This is an expected result; least trimmed squares is highly resistant to outliers, but this comes at the cost of [sufficiency](https://en.wikipedia.org/wiki/Sufficient_statistic) (effectively, a significant part of the data has very little impact on the slope, so the information contained in the data is not utilised to the full).  

Here's another way of looking at that; the density plots of the slopes of the three estimation methods (two robust methods plus ordinary least squares fitted with lm).  I am using slope because it's the most likely parameter of interest - it represents the marginal increase in income from an extra hour of work.

![slopes](/img/0041-slope-densities.svg)

The density plot shows that the estimates from `lqs` tend to be lower than for either `rlm` or `lm`.  This is to be expected, in way related to the reason that trimmed mean or median income is usually less than mean income.  An estimation method that knocks out of the data the most extreme points (as done by least trimmed squares) will give differing results for samples drawn from skewed distributions.

Here are the mean, median and standard deviation of the slopes estimate by the three methods for the 10,000 samples of thirty:

{% highlight R lang lineanchors %} 
method  mean median    sd
    lm  21.3   21.9  18.9
   rlm  20.5   20.7  10.9
   lqs  17.7   18.2  23.7
{% endhighlight %}    

We see that `lm` and `rlm` on average give similar results, but `rlm` is much more stable, with a noticeably lower variance in results (shown by the standard deviation).  `lqs` on the other hand is actually quite unstable, for this small sample size.

## Recommendation
This post was made possible by Venables and Ripley's `MASS` package and by the accompanying book [Modern Applied Statistics with S](http://www.springer.com/us/book/9780387954578).  Their own discussion of the issue implies (to my reading) the preferred use of the MM-estimator proposed by Yohai, Stahel & Zamar in 1991, which retains the outlier-resistance of the S-estimator methods related to trimmed least squares, and the greater efficiency of M-estimators.  MM-estimators are available via `rlm()` with the `method = 'MM'` argument.

{% highlight R lang lineanchors %} 
# test


{% endhighlight %}    
