---
layout: post
title: Extrapolation is tough for trees!
date: 2016-12-10
tag: 
   - R
   - MachineLearning
description: A quick demonstration of the impact of inevitably random estimates of the parameters and meta-parameters in ARIMA time series modelling
image: /img/0071-four-methods.svg
socialimage: http://ellisp.github.io/img/0071-four-methods.png
category: R
---

## Testing out-of-sample extrapolation

This post is an offshoot of some simple experiments I made to help clarify my thinking about some machine learning methods.  In this experiment I fit four kinds of model to a super-simple artificial dataset with two columns, x and y; and then try to predict new values of y based on values of x that are outside the original range of y.  Here's the end result:

![four-methods](/img/0071-four-methods.svg)

The four methods I've used are:

- linear regression estimated with ordinary least squares
- single layer artificial neural network with the `nnet` R package
- extreme gradient boosting with the `xgboost` R package
- random forests with the `ranger` R package (faster and more efficient than the older `randomForest` package, not that it matters with this toy dataset)

All these methods are now a standard part of the toolkit for predictive modelling.  Linear regression, $ is the oldest and arguably the most fundamental statistical model of this sort around.  The other three can be characterised as black box methods in that they don't return a parameterised model that can be expressed as a simple Neural networks

{% highlight R %}



{% endhighlight %}




{% highlight R %}



{% endhighlight %}
