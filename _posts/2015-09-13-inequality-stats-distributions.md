---
layout: post
title: sampling distribution of inequality statistics
date: 2015-09-13
tag: 
   - Inequality
   - Distributions
   - R
   - NewZealand
   - NZIS2011
description: Sampling distributions of Gini coefficients, p90/p10, p80/p10, top 10%, top 1% and top 0.1%
image: /img/XXX.png
socialimage: http://ellisp.github.io/img/XXX.png
category: R
---

## Inequality measures

Part of my motivation for [importing the New Zealand Income Survey](/blog/2015/08/15/importing-nzis-surf/) simulated unit record file provided by Statistics New Zealand was to explore the characteristics of various measures of inequality.  In particular, I'm interested in what happens to the distributions and sampling errors as sample size changes of the following summary statistics:

* Gini coefficient (ie the area between a Lorenz curve and the line of perfect equality)
* P90/P10 (ie income at the 90% richest percentile divided by that at the 10% percentile)
* P80/P20 (as above, but the 80 and 20 percentiles
* Top 1% income as a percentage of all income
* Top 0.1% income as a percentage of all income

A Lorenz curve
![lorenz-plot](/img/0008-lorenz.svg)



{% highlight R lineanchors %}

{% endhighlight %}


{% highlight R lineanchors %}

{% endhighlight %}