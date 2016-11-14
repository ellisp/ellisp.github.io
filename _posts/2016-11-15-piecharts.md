---
layout: post
title: Extreme pie chart polishing
date: 2016-11-15
tag: 
   - R
   - NewZealand
description: I polish up a dramatic pie chart from stuff.co.nz on earthquake energy released in New Zealand over the last few years.
image: /img/0067-pie.svg
socialimage: http://ellisp.github.io/img/0067-pie.png
category: R
---

The usual response from statisticians and data professionals to pie charts ranges from lofty disdain to outright snobbery.  But sometimes I think they're the right tool for communication with a particular audience.  Like others I was struck by [this image from New Zealand news site stuff.co.nz](http://www.stuff.co.nz/national/86458731/Cheviot-earthquake-Tracing-the-source-of-the-7-5-magnitude-quake-and-its-aftermath) showing that nearly half the earthquake energy of the past six years came in one day (last Sunday night, and the shaking continues by the way).  Pie charts work well when the main impression of relative proportions to the whole is obvious, and fine comparisons aren't needed.

Here's my own version of the graphic.  I polished this up during a break while working at home due to the office being shut for earthquake-related reasons:

![pie](/img/0067-pie.svg)

Some of the particular aspects of pie chart polishing to note here:

* Colour filling in the wedges follows a natural sequence over time
* Colour of text is on a scale in the reverse direction to the colour of the fill, to improve readability
* Wedges advance in time clockwise rather than anti-clockwise, more intuitive for most readers
* Order of legend chosen to follow the natural sequence of the eye following the wedges around
* Pale polar co-ordinates gridlines left in place, but other unnecessary aixs numbers, ticks and titles removed
* Interpretive title, and caption showing the source

Here's the code using R and `ggplot2`.  Of course, a pie chart is just a bar chart with polar co-ordinates:

{% highlight R %}
library(ggplot2)
library(scales)
library(dplyr)

# http://www.stuff.co.nz/national/86458731/Cheviot-earthquake-Tracing-the-source-of-the-7-5-magnitude-quake-and-its-aftermath?utm_source=dlvr.it&utm_medium=twitter

eqs <- data.frame(
   energy = c(48.99, 13.34, 2.67, 6.25, 4.93, 2.71, 4.73, 16.38),
   period = c("Nov 14 2016", "Earlier 2016", paste("All", 2015:2010)),
   stringsAsFactors = FALSE)

eqs <- eqs %>%
   mutate(period = factor(period, levels = period)) %>%
   arrange(desc(period)) %>%
   mutate(cumenergy = cumsum(energy),
          centres = cumenergy - energy / 2)
             

leg <- guide_legend(reverse = TRUE)

ggplot(eqs, aes(x = 1, weight = energy, fill = period)) +
   geom_bar(width = 1, colour = "grey50") +
   geom_text(x = 1.3, aes(y = centres, label = paste0(energy, "%"), colour = period)) +
   coord_polar(theta = "y") +
   scale_fill_brewer(palette = "Oranges", direction = -1, guide = leg) +
   scale_color_brewer(palette = "Blues", direction = 1, guide = leg)    +
   theme_minimal(base_family = "myfont") +
   theme(axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank(),
         plot.caption = element_text(hjust = 0.5)) +
   labs(fill = "", colour = "", 
        caption = "Source: http://www.stuff.co.nz/national/86458731/\nCheviot-earthquake-Tracing-the-source-of-the-7-5-magnitude-quake-and-its-aftermath\nSupplied by John Holdaway") +
   ggtitle("Half the earthquake energy released since 2010 came in a single day",
           subtitle = "Energy released in all New Zealand earthquakes, 2010-2016")
{% endhighlight %}
