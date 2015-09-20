---
layout: post
title: How can we judge two blackbox timeseries generators?
date: 2015-09-20
tag: 
   - Timeseries
   - Robust methods
   - R
description: A brute force method for comparing whether two data generating black boxes that produce time series are the same process or not.  Basically, you need to use each black box to generate many instances, calculate the average result of the first black box at each point of time, and see if the second black box is noticeably more different from that average than the first.  As the estimates of differences are not normally distributed I use a bootstrapped robust regression to generate a p value to help decide whether to reject the null hypothesis of the two black boxes being identical
image: /img/0011-hundred-reps.svg
socialimage: http://ellisp.github.io/img/0011-hundred-reps.png
category: R
---
## Comparing two timeseries-generating blackboxes
In my [last post](/blog/2015/09/19/timeseries-same-acf/) I talked about how [this question on Cross-Validated](http://stats.stackexchange.com/questions/172226/proving-similarities-of-two-time-series/172353#172353) got me interested.  

{% highlight R lineanchors %}

{% endhighlight %}
