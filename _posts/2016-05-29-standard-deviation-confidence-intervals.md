---
layout: post
title: Actual coverage of confidence intervals for standard deviation
date: 2016-05-29
tag: 
   - NZIS2011
   - RobustMethods
   - R
description: The actual coverage of 95% confidence intervals from the bootstrap when estimating population standard deviation can be very poor for complex mixed distributions, such as real world weekly income from a modest sample size (<10,000).

image: /img/0042-sd-ci-coverage.svg
socialimage: http://ellisp.github.io/img/0042-sd-ci-coverage.png
category: R
---

## Overview
In this post I explore the phenomenon shown in the first chart below; lower than hoped-for coverage of 95% confidence intervals calculated with the bootstrap when estimating a population standard deviation from a modest sample size.  In fact, the true coverage doesn't get up to around 95% until your sample size is about 10,000, which is much larger than rules of thumb (30-50) prevalent for minimum sample size of estimating relatively simple statistics from a single variable.

![coverage](/img/0042-sd-ci-coverage.svg)

The test is admittedly a tough one, albeit real world.  The population data are the microdata from the New Zealand Income Survey 2011, which I've written about in numerous posts.  The distribution of weekly income in this data set is complex; it could perhaps be described as a mixture of individuals with positive income that is approximately log normally distributed; individuals with negative income of some difficult-to-describe distribution; a large spike of individuals with zero income; plus a sprinkling of outliers.  In fact, it looks like this:

![fulldata](/img/0042-full-data.svg)

The actual standard deviation of all 29,471 observations of income is $806.

The inference problem is how to estimate that value if you have only a smaller sample, eg 50?  Because if you have only 50 observations, you think the data looks like this:

![sampledata](/img/0042-sample-data.svg)

In the case of this particular sample of 50, the standard deviation is $527.

## Unbiased estimate of standard deviation
The first challenge - although turns out to be not that important - is to get the best estimate of standard deviation.  While an unbiased estimator of variance is well known, standard deviation (the square root of variance) is complex.  Creating an [unbiased estimator of standard deviation](https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation) turns out to depend on the vagaries of the shape of the original population.

A reasonably generally well-performing estimator for non-Normal variables is apparently (image from the Wikipedia article linked to above):

![equation](/img/0042-unbiased-sd.png)

## Method
Using the unbiased estimate of standard deviation above, I set out to test the performance of bootstrap confidence intervals in covering the true value of population standard deviation for sample sizes of 50, 100, 200, 400, ..., 12,800.  I took 200 samples of the data for each of these sample sizes; estimated the standard deviation from the sample; and use the bootstrap to estimate confidence intervals for population standard deviation from each sample, with 999 bootstrap replicates.  I tested both the [basic and the percentile bootstrap methods](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)#Methods_for_bootstrap_confidence_intervals).

## Results



## Simpler statistics

{% highlight R lang lineanchors %} 

{% endhighlight %}    
