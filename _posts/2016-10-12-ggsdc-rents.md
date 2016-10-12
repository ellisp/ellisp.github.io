---
layout: post
title: Update of `ggseas` for seasonal decomposition on the fly
date: 2016-10-12
tag: 
   - R
   - NewZealand
   - TimeSeries
description: ggseas 0.5.1 is available on CRAN.  
image: /img/0060-seas-rents.svg
socialimage: http://ellisp.github.io/img/0060-seas-rents.png
category: R
---

## What's new

A new version (0.5.1) of my `ggseas` R package is now [available on CRAN](https://CRAN.R-project.org/package=ggseas).  `ggseas` is a small package that provides several tools to make it easier to do seasonal adjustment or decomposition of time series on the fly in a `ggplot2` pipeline.  New in this version:

* A `facet.titles` argument to the decomposition graphic function `ggsdc`
* Addition of a vignette and other small documentation improvements
* `ggsdc` now supports tibbles
* Fixed some minor bugs, such as how the ordering of factor levels is preserved

## Installation
Installation is the usual way:
{% highlight R %}
install.packages("ggseas")
{% endhighlight %}

## Example usage

One of the two main areas of functionality is the ability to easily do seasonal decomposition into seasonal, trend and random components in an exploratory workflow.  Here's an example, applied to the [mean rent by territorial authority data](http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data) published by New Zealand Ministry of Business, Innovation and Employment (disclaimer - I work there, but not on rental data).

The aim is to produce this graphic, comparing mean rents in two selected cities:

![seas](/img/0060-seas-rents.svg)

Auckland is New Zealand's largest city, on the north island.  Dunedin is on the south island and has a high proportion of students which, even before we look at the data, we might expect to impact on the rental market.  In this graphic we see that the seasonal effect in Dunedin is in fact stronger than in Auckland; actually, there is virtually no seasonality in Auckland rents at all.  In Dunedin, rents for leases that start in the most expensive time are around $60 or more a week more than in the cheapest.  The decomposition in this graphic is done with X13-SEATS-ARIMA, which `ggseas::ggsdc` calls via the excellent `seasonal` R package.

Here's the code

{% highlight R %}
library(tidyr)
library(dplyr)
# We use the dev version of ggplot2 for subtitle and captions, 
# so uncomment the following line if you don't already have it
# devtools::install_github("hadley/ggplot2")
library(ggseas)

www <- "http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data/documents-images/by-ta/ta-mean-rents.csv"
rents_orig <- read.csv(www, check.names = FALSE)

rents <- rents_orig %>%
   gather(TA, MeanRent, -Month) %>%
   mutate(Month = as.Date(Month))

rents %>%
   filter(TA %in% c("Auckland", "Dunedin")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = TA), 
         frequency = 12, method = "seas", start = c(1993, 1),
         facet.titles = c("Original series", "Underlying trend",
                          "Regular seasonal impacts", "Residual randomness")) +
   geom_line() +
   labs(colour = "", x = "Month lodged") +
   scale_y_continuous("Decomposition of mean rent", label = dollar) +
   ggtitle("Rent in Dunedin has a strong seasonal pattern",
           subtitle = "Seasonal decomposition with X13-SEATS-ARIMA via ggsdc") +
   labs(caption = "Source: Ministry of Business, Innovation and Employment")
{% endhighlight %}

As another view of the data, I use the same approach to show classical multiplicative decomposition.  This isn't the best way to treat seasonal data, particularly economic data where the seasonality can be expected to change over time, but it is the simplest and can form a useful reality check on the more sophisticated algorithms used in X13-SEATS-ARIMA.

![decomp](/img/0060-decomp-rents.svg)

This approach forces the seasonality to be the same over the whole life of the series.  Note that the "seasonality" facet is now showing multipliers, not additions.  

{% highlight R %}
rents %>%
   filter(TA %in% c("Auckland", "Dunedin")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = TA), 
         frequency = 12, method = "decompose", type = "multiplicative") +
   geom_line()
{% endhighlight %}

# Which months are cheaper?
That last graphic makes it clearer that Dunedin's rents vary in a regular pattern, but it's difficult to see exaclty read the axis in enough detail to see which months are low and high.  To understand this we need a monthplot:

![monthplot1](/img/0060-monthplot.svg)

...or a monthplot of the detrended series:

![monthplot2](/img/0060-monthplot-detrended.svg)

These graphics show that the months that have the lowest rents lodged are actually in the middle of the year - June and July.

The last two graphics were made with the benefit of `stl` and `monthplot` from the standard `stats` package, not related to `ggseas` at all:

{% highlight R %}
dun_ts <- rents %>%
   filter(TA %in% c("Dunedin")) %>%
   mutate(LogRent = log(MeanRent)) %>%
   select(LogRent) %>%
   ts(start = c(1993, 1), frequency = 12) 

# plot of months, including trend
monthplot(dun_ts)
grid()

# de-trended version
dun_stl <- stl(dun_ts[ ,1], s.window = 7)
monthplot(with(as.data.frame(dun_stl$time.series), seasonal + remainder))
grid()

{% endhighlight %}

## More

There's more to `ggseas` than these decomposition plots.  It also offers indexing, rolling average, and seasonal adjustment "stats" that integrate into the standard `ggplot` graphic production line.  Check out the [new vignette](https://cran.r-project.org/web/packages/ggseas/vignettes/ggsdc.html).  Any bugs or feature requests please [post as issues on GitHub](https://github.com/ellisp/ggseas/issues).

