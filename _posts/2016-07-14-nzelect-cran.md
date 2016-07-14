---
layout: post
title: nzelect 0.2.0 on CRAN
date: 2016-07-14
tag: 
   - R
   - VotingBehaviour
   - NewZealand
description: The nzelect R package is now available on CRAN; so far it has aggregate results by voting place for the New Zealand 2014 general election.
image: /img/0049-map.svg
socialimage: http://ellisp.github.io/img/0049-map.png
category: R
---

## Introduction
The `nzelect` R package which I first introduced in [a blog post in April](/blog/2016/04/03/nzelect1) is now [available on CRAN](https://cran.r-project.org/web/packages/nzelect/).  The version number is 0.2.0.  

The difference from version 0.1.0 is sizeable - all the 2013 census data has been removed and is now in a companion package, `nzcensus`.  This is for ease of development and maintenance, and to allow organisations that aren't interested in the election results still to use the census data.  It also keeps the package size within CRAN guidelines.  I'll write about `nzcensus` in a separate post; its coverage has somewhat expanded from when I last blogged about combining census and election data.  `nzcensus` is [only available from GitHub](http://github.com/ellisp/nzelect) due to its size.

The changes to `nzelect` and the creation of `nzcensus` are not backwards-compatible; the code in posts I wrote in April will no longer run out of the box.  However, the changes needed are small and in the comments under those posts I've provided links to working versions.

I was very pleased when last week the New Zealand Herald used my `nzelect` package and example analysis in my blog as the basis for a more comprehensive piece of analysis and [interactive web application on "How New Zealand Votes"](http://insights.nzherald.co.nz/article/how-new-zealand-votes).

The [New Zealand Electoral Commission](http://www.electionresults.govt.nz/) had no involvement in preparing this package and bear no responsibility for any errors. In the event of any uncertainty about election results, refer to the definitive source materials on their website.

## Usage

The functionality of the voting results part of the `nzelect` package hasn't changed since the GitHub version in April.  All the results are verified against the official election results.  Results are available by voting place and are easy to aggregate up by electorate of enrolment and by meshblock, area unit, territorial authority and regional council by voting location.  The package has a [vignette](https://cran.r-project.org/web/packages/nzelect/vignettes/README.html) with some basic analysis ideas.

Here's one idea that's not in the vignette - mapping the precise casting location of party votes in a city:

![map](/img/0049-map.svg)

Here's the code for producing that map of Christchurch.  Thanks to David Kahle and Hadley Wickham for the `ggmap` package which makes it easy (amongst other things) to provide backgrounds for maps.  These particular map tiles are by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.

{% highlight R %}
install.packages("nzelect")

library(nzelect)
library(ggplot2)
library(scales)
library(dplyr)
library(ggmap)
library(ggthemes) # for theme_map

# download background map of Christchurch
background2 <- get_map("Christchurch", maptype = "toner-background")

# Extract just the votes physically case in Christchurch
the_data <- GE2014 %>%
   filter(VotingType == "Party") %>%
   group_by(VotingPlace) %>%
   summarise(VotesNational = sum(Votes[Party == "National Party"]) / sum(Votes),
             VotesLabour = sum(Votes[Party == "Labour Party"]) / sum(Votes),
             VotesGreens = sum(Votes[Party == "Green Party"]) / sum(Votes),
             TotalVotes = sum(Votes)) %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   filter(TA2014_NAM == "Christchurch City")

# draw a map
ggmap(background2) +
   geom_point(data = the_data, aes(x = WGS84Longitude, y = WGS84Latitude, colour = VotesNational, size = TotalVotes)) +
   geom_point(data = the_data, aes(x = WGS84Longitude, y = WGS84Latitude, size = TotalVotes), colour = "grey35", shape = 1) +
   theme_map() +
   theme(legend.position = "right") +
   coord_map(xlim = range(the_data$WGS84Longitude), ylim = range(the_data$WGS84Latitude)) +
   scale_colour_gradientn("Party vote\nNational", 
                          colours = c("darkred", "pink", "lightgoldenrod", "lightblue", "darkblue"), label = percent) +
   ggtitle("Party vote by voting place in Christchurch", subtitle = "New Zealand General Election 2014") +
   scale_size_area("Total votes", label = comma) +
   labs(caption = ("Analysis by http://ellis.github.io\nBackground tiles by Stamen Design under Creative Commons CC BY 3.0"))
{% endhighlight %}

## Next steps

Next steps for `nzelect` include:

* add individual level data from the New Zealand Election Study
* add results from 2011 and earlier elections
* more example analysis of combinations of `nzelect` and `nzcensus`.

Please file bugs or enhancement requests as [issues on GitHub](https://github.com/ellisp/nzelect/issues).

