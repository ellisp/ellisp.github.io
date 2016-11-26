---
layout: post
title: ets and friends - comparing similar forecast methods
date: 2016-11-27
tag: 
   - R
   - Timeseries
description: XXXXXXXXXXXXXX
image: /img/0070-comparison.svg
socialimage: http://ellisp.github.io/img/0070-comparison.png
category: R
---

XXXXXXXXXX



- `ets()` The most general and flexible version.  Models error, trend and seasonal elements together
- `stlm()` Seasaonal adjustment via `stl()` (Cleveland-style loess), then a non-seasonal `ets()` model (ie just error and trend terms); then re-seasonalise
- `thetaf()` Seasonal adjustment via `decomp()` (classical multiplicative seasonal decomposition); `ets()` with just an additive model (no trend or seasonality); estimate trend by an adjusted form simple linear regression of the seasonally adjusted variable against time


{% highlight R %}














{% endhighlight %}
