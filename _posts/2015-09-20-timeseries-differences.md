---
layout: post
title: How can we compare two blackbox timeseries generators?
date: 2015-09-20
tag: 
   - Timeseries
   - Robust methods
   - R
description: A brute force method for comparing whether two data generating black boxes that produce time series are the same process or not. Basically, you need to use each black box to generate many instances, calculate the average result of the first black box at each point of time, and see if the second black box is noticeably more different from that average than the first.
image: /img/0011-hundred-reps.svg
socialimage: http://ellisp.github.io/img/0011-hundred-reps.png
category: R
---
## Comparing two timeseries-generating blackboxes
In my [last post](/blog/2015/09/19/timeseries-same-acf/) I talked about how [this question on Cross-Validated](http://stats.stackexchange.com/questions/172226/proving-similarities-of-two-time-series/172353#172353) got me interested.  Basically the challenge is to compare two data generating models to see if they are essentially the same. Since then I’ve noticed that this problem comes up in a number of other contexts too; for example, this New Zealand Treasury Working paper by Kam Leong Szeto on [Estimating New Zealand’s Output Gap Using a Small Macro Model](http://www.treasury.govt.nz/publications/research-policy/wp/2013/13-18/twp13-18.pdf) at one point runs lots of sets of two simulations of the New Zealand economy and compares the distribution of the standard deviations of various interesting estimated parameters, and of their first order autocorrelation coefficients (ie the correlation of the series with the lagged version of itself):

![szeto](/img/0011-szeto-screenshot.png)

This latter process of comparing autocorrelation coefficients is similar to what user333700 suggested on Cross-Validated, and is what I showed in my last post is not a bullet-proof means of comparing time series; most importantly, linear combinations of a time series will have the same autocorrelation function. So one of Szeto’s models might be generating values 10 percent larger than the other, but still having the same autocorrelation coefficients in his Figure 2 above. However, as he also compares the standard deviations of the simulated values of interest, he fixes some of that problem (not all of it - just adding a constant, for example, would leave him with identical autocorrelation coefficients and standard deviations, but very different results).

## My example black boxes

To explore this for a more general solution, I first need something to try it on. I made two functions that generate data that is close enough to wonder if they are the same, but which a good enough method can distinguish between. I define for myself two similar but different time series, one an ARIMA(2,1,1) process and the other an ARIMA(1,1,2). Here’s one instance each coming out from those black boxes. Notice that they look very different!

![one-each](/img/0011-one-instance-each.svg)

{% highlight R lineanchors %}
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext) # for fonts

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

bb1 <- function(n = 1000){
   # ARIMA(2, 1, 1) with drift
   cumsum(arima.sim(model = list(ar = c(0.5, -0.2), ma = 0.3), n = n) + 0.10)
}

bb2 <- function(n = 1000){
   # ARIMA(1, 1, 2) with drift
   cumsum(arima.sim(model = list(ar = c(0.5), ma = c(0.3, -0.1)), n = n) + 0.04)
}

# one example each from both of my black boxes
set.seed(134)
par(mfrow = c(2, 1), family = "myfont", mar = c(4, 5, 1, 1))
plot.ts(bb1(), bty = "l", main = "One instance from blackbox 1")
plot.ts(bb2(), bty = "l", main = "One instance from blackbox 2")
{% endhighlight %}

So those two series look obviously different, right, no need for any fancy stats to say they’re different… except that if you make lots of examples of them, you get a different result each time. Here’s how they look when we do 100 instances of each one, 500 observations long each:

![hundred-each](/img/0011-hundred-reps.svg)

{% highlight R lineanchors %}
# create skeleton of dataframe to hold data:
the_data <- data_frame(blackbox1 = numeric(), blackbox2 = numeric(), trial = numeric(), time = numeric())

# populate with simulated versions:
reps <- 100
n <- 500
set.seed(123) # for reproducibility
for(i in 1:reps){
   tmp <- data_frame(blackbox1 = bb1(n), blackbox2 = bb2(n), trial = i, time = 1:n)
   the_data <- rbind(the_data, tmp)
}

# gather into long tidy format
the_data_m <- the_data %>%
   gather(source, value, -trial, -time)

# show the data from the two black boxes:
the_data_m %>%
   ggplot(aes(x = time, y = value, colour = as.character(trial))) +
      facet_wrap(~source) +
      geom_line(alpha = 0.3) +
      geom_smooth(se = FALSE, method = "loess") +
      theme(legend.position = "none")
{% endhighlight %}

## Comparison method

My method of comparison is based on assuming blackbox1 is the known good generator and we want to see if blackbox2 is plausibly the same. I do these steps:

* calculate the average value at each point in time of the various instances of blackbox1, and the standard deviation of blackbox1’s values at that point
* calculate how the absolute value of how different each instance of both series is from that average value, standardised by the standard deviation
* correct that difference value for blackbox1 by multiplying it by the (number of instances) / (number of instances - 1), to control for the fact that the centre was defined by blackbox1 data and hence blackbox1 has a head start in trying to be close to the defined centre. In effect, I give it a degrees-of-freedom correction
* analyse the distribution of the differences to see if the instances of blackbox2 seem, on average, to be further away from the centres than are the instances of blackbox1.
* Here’s how it looks. In this case, blackbox2’s differences from the known-good centre is generally higher (ie displaced to the right in this density graph) than is blackbox1’s:

![density](/img/0011-density-differences.svg)

{% highlight R lineanchors %}
difference <- the_data_m %>%
   left_join(
      the_data %>%
         group_by(time) %>%
         summarise(centre_1 = mean(blackbox1),
                   sd_1 = sd(blackbox1)),
      by = "time"
      ) %>%
   # bias correction because the centres are defined from blackbox1's data,
   # hence on average they will always be a little closer to them
   mutate(correction = ifelse(source == "blackbox1", max(trial) / (max(trial) - 1), 1)) %>%
   group_by(trial, source)%>%
   summarise(
      meandiff = mean(abs(value - centre_1) / sd_1 * correction))

   ggplot(difference, aes(x=meandiff, colour = source)) +
      geom_density() +
      ggtitle("Mean absolute standardise difference\nat each time point from the average of\nblackbox1 at that point")
{% endhighlight %}




{% highlight R lineanchors %}

{% endhighlight %}

{% highlight R lineanchors %}

{% endhighlight %}