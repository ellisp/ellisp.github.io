---
layout: post
title: Inequality measures in the World Development Indicators
date: 2015-12-05
tag: 
   - OpenData
   - Inequality
   - DataFromTheWeb
   - Shiny
   - R
description: Introduces a new web-app to help navigate and explore the World Bank's World Development Indicators; and a one-off comparison of the inequality measures available in that collection.
image: /img/0022-shiny-screenshot1.png
socialimage: http://ellisp.github.io/img/0022-shiny-screenshot1.png
category: R
---
## World Development Indicators
After my [last post](http://ellisp.github.io/blog/2015/11/26/violent-deaths/) on deaths from assault, I got a few comments both on and off-line asking for links to inequality data, and for more exploration of developing country issues.  I'd always intended at some point to do something with the World Bank's [World Development Indicators](http://databank.worldbank.org/data/reports.aspx?source=world-development-indicators) so now seems a good time to at least get started.

> "World Development Indicators (WDI) is the primary World Bank collection of development indicators, compiled from officially recognized international sources. It presents the most current and accurate global development data available, and includes national, regional and global estimates"

I'll be analysing the data using Vincent Arel-Bundock's [{WDI} R package](https://cran.r-project.org/web/packages/WDI/index.html) which makes it super easy to grab data via the World Bank's Application Programming Interface, including fast searching a cached version of the indicator names and of course downloading the actual data into an R session.  With that search ability and the Bank's own page with its categorisations of indicators it wasn't hard to find data that I needed.  

However I quickly found myself doing some repetitive tasks - downloading a dataset and turning them into a standard faceted time series plot.  Following the DRY (Don't Repeat Yourself) fundamental principle of programming I decided this phase of exploration was more suited to a graphic user interface so I spun up this [web-app](https://ellisp.shinyapps.io/WorldDevelopmentIndicators) using Shiny.

The app has my standard plot I was making each time I looked at a new indicator:
[![screen1](/img/0022-shiny-screenshot1.png)](https://ellisp.shinyapps.io/WorldDevelopmentIndicators)

and a responsive, fast, searchable table of the available indicators:
![screen1](/img/0022-shiny-screenshot2.png)

The source code is available in the <source> branch of my blog repository if anyone wants to hunt it down.

As well as producing my standard plot quickly for me, putting all the 3,700 indicators into a JavaScript DataTable gave me even more convenient searching and navigating.  I was also able to do a one-off download all 7,145 available indicators, which is the number of rows returned in this snippet of code:

{% highlight R lineanchors %}
library(WDI)
search_results <- WDIsearch("")
nrow(search_results)
{% endhighlight %}

This was convenient to have a local copy of all that data (only about 70MB when saved in R's super efficient .rda format), but the main motivation for doing this was identifying which series actually have data.  Only the 3,700 series with at least one row of data on 3 December 2015 are listed in my Shiny app.  The app does download the live data, not using my cached copy; this obviously makes the performance quite slow (about 10 - 20 seconds of apparent unresponsiveness when downloading a dataset) but saves me having a very large shiny app to pay for, and ensures the data are current.

