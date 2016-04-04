---
layout: post
title: Seasonal decomposition in the ggplot2 universe with ggseas
date: 2016-03-28
tag: 
   - Timeseries
   - R
description: I've updated the {ggseas} R package on CRAN to version 0.4.0 and it now includes a ggplot2-based seasonal decomposition, rolling averages on the fly, and options to scale the data to an index.
image: /img/0034-ggsdc.svg
socialimage: http://ellisp.github.io/img/0034-ggsdc.png
category: R
---
The ggseas package for R, which provides convenient treatment of seasonal time series in the ggplot2 universe, was first released by me in February 2016 and since then has been enhanced several ways.  The latest version, 0.4.0, is now on CRAN.  

![ggsdc](/img/0034-ggsdc.svg)

The improvements since I last blogged about ggseas include:

* added the convenience function tsdf() to convert a time series or multiple time series object easily into a data frame
* added stats for rolling functions (most likely to be rolling sums or averages of some sort)
* stats no longer need to be told the frequency and start date if the variable mapped to the x axis is numeric, but can deduce it from the data (thanks Christophe Sax for the idea and starting the code)
* series can be converted to an index (eg starting point = 100)
* added ggplot seasonal decomposition into trend, seasonal and irregular components, including for multiple series at once (thanks Paul Hendricks for the enhancement request)

I think it's pretty stable now unless anyone identifies some bugs - I don't have any planned work on this for the immediate future.

[![Travis-CI Build Status](https://travis-ci.org/ellisp/ggseas.svg?branch=master)](https://travis-ci.org/ellisp/ggseas)
[![CRAN version](http://www.r-pkg.org/badges/version/ggseas)](http://www.r-pkg.org/pkg/ggseas)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/ggseas)](http://www.r-pkg.org/pkg/ggseas)

Installation can be done from CRAN:
{% highlight R lineanchors %}
install.packages("ggseas")
{% endhighlight %}


Let's go through the changes one at a time.

## Convert time series to data frame

tsdf() is a new, very simple function that takes a ts or mts (univariate or multiple time series) object and converts it to a data frame that is then convenient for use with packages built around data frames, such as ggplot2 and dplyr.  It's super simple to use:

{% highlight R lineanchors %}
ap_df <- tsdf(AirPassengers)
{% endhighlight %}

## Rolling averages and sums
Rolling averages (usually rolling mean) and sums are commonly used, particularly for audiences that aren't used to the greater sophistication of seasonal adjustment.  It's an inferior form of smoothing and comes from the days when seasonal adjustment and scatter plot smoothers weren't readily available, but it's still sometimes useful to be able to do this easily in a ggplot graphic.  The stat_rollapplyr function does this, using the rollapplyr function from the zoo package under the hood.  A 'width' argument is mandatory, and FUN specifying which function to apply is optional (defaults to mean).  There's also an optional 'align' function which defaults to the 'right'; this means that the graphic shows the rolling average (or whatever other function) for the width number of observations up to the latest observation (alternatives are 'center' or 'left').

{% highlight R lineanchors %}
ggplot(ldeaths_df, aes(x = YearMon, y = deaths)) +
   geom_point() +
   facet_wrap(~sex) +
   stat_rollapplyr(width = 12, FUN = median) +
   ggtitle("Lung deaths in the UK\n") +
   labs(x = "", y = "Deaths (including moving median")
{% endhighlight %}

![rolling](/img/0034-rolling.svg)

## Deduce frequency from data
If the variable that is mapped to the x axis is numeric (as will be the case if it was created with tsdf(), but not if it is of class Date) the functions in ggseas can now deduce the starting point of the time period and its frequency from the data.  This makes it easier to just chuck in stat_seas() into your ggplot pipeline:
{% highlight R lineanchors %}
ggplot(ldeaths_df, aes(x = YearMon, y = deaths, colour = sex)) +
   geom_point() +
   facet_wrap(~sex) +
   stat_seas() +
   labs(x = "") +
   scale_y_continuous("Number of deaths", label = comma) +
   ggtitle("Seasonally adjusted lung deaths in the UK\n") +
   theme(legend.position = "none")
{% endhighlight %}

![simple-seas](/img/0034-deduce-frequency.svg)

## Series can be converted to an index
If you're interested in trends including growth and other patterns over time rather than absolute levels, it can be useful to convert time series to an index.  All the ggplot2-related functions in ggseas now offer arguments index.ref (to set the reference period - commonly but not always the first point, or an average of the first w points) and index.basis (what value to give the index in the reference period, usually 100, 1 or 1000).

{% highlight R lineanchors %}
ggplot(ldeaths_df, aes(x = YearMon, y = deaths, colour = sex)) +
   stat_seas(index.ref = 1:12) +
   ggtitle("Indexed lung deaths in the UK\n") +
   labs(x = "", y = "Seasonally adjusted deaths\n(first 12 months average = 100\n") 
{% endhighlight %}

![index](/img/0034-index.svg)

## Seasonal decomposition
The biggest addition is the ability to easily do graphical decomposition of an original time series into trend, seasonal and irregular components, comparable to plot(stl()) or plot(decompose()) in base graphics but with the added access to X13-SEATS-ARIMA, and the ability to use the ggplot ethos to map a variable to colour and hence decompose several variables at once.

This is done with the ggsdc() function, which produces an object of class ggplot with four facets.  The user needs to specify the geom (normally geom_line).  The image at the top of this post was produced with the code below.  It expands on an example in the helpfile and uses Balance of Payments data from Statistics New Zealand, which has been included in the ggseas package for illustrative purposes.

{% highlight R lineanchors %}
serv <- subset(nzbop, Account == "Current account" & 
                  Category %in% c("Services; Exports total", "Services; Imports total"))
                  
ggsdc(serv, aes(x = TimePeriod, y = Value, colour = Category),
      method = "seas", start = c(1971, 2), frequency = 4) +
   geom_line() +
   labs(x = "   \n  ", colour = "") +
   scale_y_continuous("Value in millions of New Zealand dollars\n", label = comma) +
   ggtitle("New Zealand service exports and imports") +
   theme(legend.position = c(0.17, 0.92))

   grid.text("Source: Statistics New Zealand, Balance of Payments", 0.7, 0.03,
          gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7))   
{% endhighlight %}

## Conclusion
ggseas aims to make it easier and quicker to incorporate seasonal adjustment, including with the professional standard X13-SEATS-ARIMA algorithms, into exploratory work flows.  It does this by letting you incorporate seasonally adjusted variables into graphics with multiple series (dimensions mapped to facets or to colour), and by letting you simultaneously decompose and plot on the same graphic at once several related series.  Adding indexing, rolling averages or sums, and quick conversion from ts to data frame to the toolbox is part of the same idea, making it easier to do exploratory data analysis with many time series at once.

All the functions mentioned above have helpfiles that are more comprehensive than the examples can show.

Please log any issues - enhancement and bug requests - at [https://github.com/ellisp/ggseas/issues](https://github.com/ellisp/ggseas/issues).