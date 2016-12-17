---
layout: post
title: Air quality in India
date: 2016-12-17
tag: 
   - R
   - OpenData
   - Timeseries
description: Air pollution in India is unambiguously seasonal, and also has a noticeable Diwali impact
image: /img/0073-decomp.svg
socialimage: http://ellisp.github.io/img/0073-decomp.png
category: R
---

## Seasonal air pollution in India
The motivation for this blog post was a paper presented at a conference I was recently at that analysed five years of daily pollution data in an India city with a non-seasonal auto-regressive integrated moving average (ARIMA) model.  In discussion after the presentation, there were differing views on whether such data should be modelled with a seasonal approach, and I wanted to look at the data to see for myself (disclaimer - I was one of the pro-seasonal camp).

To cut to the chase, ultimately I was looking at this sort of result:

![decomp](/img/0073-decomp.png)

## Open Air Quality Data
But first thing was to get some data.  I didn't have access to the original data but was aware of other sources, with shorter time spans, but hopefully still enough for my purposes.  My first call was the [openaq Air Quality Data](https://openaq.org/#/locations?page=3&countries=AU&_k=iz4lwf) site and in particular the excellent `ropenaq` R package which speaks to the openaq API and makes it easy to access their data.  This looked very promising, but the data available are for far too short a period for me.  Here is the six months or so of data which is all that is available for Ahmedabad.

![openaq](/img/0073-openaq.svg)

For the record, here's the R code that downloaded that data (nearly 23,000 observations - many observations per day) and drew the graph, and also set up the session for everything else done in the rest of this post

{% highlight R %}
# Might need to uncomment out and run these next two lines if not already installed:
# devtools::install_github("Ather-Energy/ggTimeSeries")
# devtools::install_github("masalmon/usaqmindia")

# load up functionality
library(ropenaq) # sourced from OpenAQ, see https://github.com/ropensci/ropenaq
library(ggplot2)
library(scales)
library(tidyverse)
library(usaqmindia) # sourced from US Air Quality Monitoring, see https://github.com/masalmon/usaqmindia
library(forecastHybrid)
library(seasonal)   # for Diwali dates
library(ggseas)
library(ggmap)
library(lubridate)
library(png)
library(RColorBrewer)

data(holiday) # will be using this for "diwali"

#============openaq experiment================
# based on the example on the package page on GitHub
how_many <- attr(aq_measurements(city = "Ahmedabad"), "meta")
n <- how_many$found # unfortunately only goes back to mid 2015
results_ls <- list()
# Limit is 1000 observations at once, so we bring them all in 1000 at a time:
for(i in 1:ceiling(n / 1000)){
   results_ls[[i]] <- aq_measurements(country = "IN", city = "Ahmedabad", 
                                      date_from = "2010-01-01", 
                                      limit = 1000, page = i)   
   cat(i)
}

# convert into a data frame:
results_df <- do.call("rbind", results_ls) %>%
   arrange(dateLocal)

# draw exploratory graph
results_df %>%
   ggplot(aes(x = dateLocal, y = value)) + 
   facet_wrap(~parameter, ncol = 1, scales = "free_y") +
   geom_line() +
   ggtitle("Air pollutants in Ahmedabad, 2016") +
   scale_y_continuous("Value", label = comma) +
   labs(x = "", caption = "Source: OpenAQ")
{% endhighlight %}

## US Embassy and Consulate India Air Quality Monitoring

Luckily, the [GitHub page for the `ropenaq` R package](https://github.com/ropensci/ropenaq) referenced a second R package, [`usaqmindia`](https://github.com/masalmon/usaqmindia).  This package has tidied and bundled up various CSV and PDF files [published by the US State Department](https://in.usembassy.gov/embassy-consulates/new-delhi/air-quality-data/) on particulate matter concentrations in five Indian cities.  It may not be completely current, but it looks pretty good.  If your Indian geography is rusty you may appreciate this map of the five cities in question:

![map](/img/0073-map.svg)

{% highlight R %}
#=========map of india========
cities <- data_frame(
   city = c("Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai"),
   lat = c(13.067439, 28.644800, 17.387140, 22.572645, 19.0759837),
   long = c(80.2784700, 77.216721, 78.491684	, 88.363892,	72.8776559)
)

india <- get_map("India", zoom = 5, maptype = "satellite")

ggmap(india) +
   geom_text(data = cities,  aes(x = long, y = lat, label = city), colour = "white") +
   theme_nothing()
{% endhighlight %}



![raw](/img/0073-six-cities-orig.svg)



![daily](/img/0073-six-cities-daily.svg)



{% highlight R %}

{% endhighlight %}

![monthly](/img/0073-six-cities-monthly.svg)

![monthplots](/img/0073-monthplot.svg)




{% highlight R %}

{% endhighlight %}


## Delhi time series analysis

![acf](/img/0073-acf.svg)

![decomp](/img/0073-decomp.svg)

![decomp-index](/img/0073-decomp-index.svg)

{% highlight R %}

{% endhighlight %}


![forecast](/img/0073-forecast.svg)




