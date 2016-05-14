---
layout: post
title: Minimalist Tufte-inspired axis text with Scottish-New Zealand historical material
date: 2016-05-14
tag: 
   - History
   - R
description: The breaks= argument in scales in the ggplot2 universe makes it simple to do minimalist Tufte-inspired scales where the text and tick marks show the actual data values rather than just regular intervals.

image: /img/0040-females.svg
socialimage: http://ellisp.github.io/img/0040-females.png
category: R
---
## Minimalist axes text and tick marks

One of the ideas in Edward Tufte's [*The Visual Display of Quantitative Information*](http://www.amazon.com/Visual-Display-Quantitative-Information/dp/0961392142) was to minimise non-data-ink by dropping the regular text labelling values on axis guides, and instead using the axis guides to mark the values of the actual data.  For those of you with the book in front of you, I'm thinking of the graphic *Current Receipts of Government as a Percentage of Gross Domestic Product, 1970 and 1979*, on page 158 of the 2013 edition.


Looking for example data on which to apply this, I picked up a table from Rebecca Lenihan's [*From Alba to Aotearoa*](http://www.otago.ac.nz/press/books/otago115810.html) which I'm reading at the moment.  It's a comprehensive look at what can be known about Scottish immigrants to New Zealand from 1840 to 1920 and includes a fair amount of tabular information that is ideal for presentation with this sort of graphic.  Here's where I got to with visualising one of her tables (Table 3.6 on page 105, for those following along), showing the disproportionately maleness of Scottish immigrants in the nineteenth century even compared to other ethnic groups.
![plot](/img/0040-females.svg)

The key message from this graphic is that for both Scottish and Irish migrants to New Zealand in the nineteenth century, females were under-represented; but the ratio of females to males was noticeably lower for those of Scottish origin (all the provinces to the left of and above the pale blue diagonal line).  

Lenihan's original table includes a range of additional information that can't be easily shown in this graphic, so the graphic is very much a complement rather than replacement of the detailed numeric information.  But for the two dimensions that it does show, it's nice example of integration of detailed numeric data into spatial representation - a combination of numbers and graphic visualisation.

## Features of this approach

Now I've noticed the trick of over-riding the boring, traditional regular spacing of axis text and tick marks, I think it might become a regular part of the repertoire for sparse two-dimensional data.  The key technical features of this as a graphic include:

* The province labels are carefully in pale grey, and the points representing the actual data are the final layer and in black so they are in the foreground of the graphic
* I use pale blue, with different shades of transparency, to put some of the non-data guides (like the line representing equality between the two ethnicities' female-male ratios) into the background.
* Even with this sparse summarised data, some of the actual values are too close for the axis text to include every value.  In those cases (eg 81.3% and 81.5% female-male ratios for Canterbury and Otago people of Scottish descent) I've included a single average value as the axis text and tick mark, but used "rug" marks to indicate the two different actual values
* the vertical and horizontal coordinate systems are forced to be equal, so a centimetre of distance in either direction equates to the same amount of change in female-male ratio
* Because it's all a homage to Tufte, I've used serif fonts rather than my usually preferred non-serifs...

## Using the breaks= argument in scale_XXX_continuous()

Here's the code in R that does all that.  Big thanks to the authors of the `ggplot2`, `scales`, `ggrepel` and `ggthemes` R packages, and a note that the latest dev version (from GitHub) of `ggplot2` is needed for the subtitle and caption to work.
{% highlight R lang lineanchors %} 
library(ggplot2)
library(scales)
library(ggrepel)
library(ggthemes)

# Ratio of females to males in the Scotland- and Ireland- born population in 1881
fem_ratio <- data.frame(
   province = c("Auckland", "Taranaki", "Hawke's Bay", "Wellington", "Marlborough", "Nelson", "West Coast", "Canterbury", "Otago"), 
   scots = c(68.14, 55.24, 58.74, 71.07, 51.56, 52.66, 38.95, 67.80, 75.16) / 100,
   irish = c(84.96, 43.40, 65.87, 87.73, 69.98, 50.20, 67.92, 81.28, 81.52) / 100
)

# create a set of axis tick/text breakpoints that combine those which are too close
# together for showing on screen:
sanitise_ticks <- function(x, digits = 4){
   ticks <- sort(unique(round(x, digits = digits)))
   for(i in 2:length(ticks)){
      if(ticks[i] - ticks[i-1] < 0.004){
         ticks[i-1] = mean(ticks[(i-1):i])
         ticks[i] <- 0
      }
   }
   ticks <- ticks[ticks != 0] 
}

yticks <- sanitise_ticks(fem_ratio$irish)
xticks <- sanitise_ticks(fem_ratio$scots)


# Define a frequently used colour and draw plot:
linecol <- "steelblue"

ggplot(fem_ratio, aes(x = scots, y = irish, label = province)) +
   theme_tufte() +
   geom_abline(intercept = 0, slope = 1, colour = linecol, alpha = 0.5) +
   geom_point() +
   geom_rug(colour = "steelblue") +
   geom_text_repel(colour = "grey60", segment.color = NA, family = "serif") +
   scale_x_continuous(breaks = xticks, label = percent) +
   scale_y_continuous(breaks = yticks, label = percent) +
   theme(axis.text = element_text(colour = "steelblue"),
         axis.ticks = element_line(colour = "steelblue")) +
   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)) +
   labs(x = "\nScottish born\n", y = "Irish born\n", 
        caption = "Source: Census, analysed in Lenihan 'From Alba to Aotearoa'") +
   ggtitle("Females as a percentage of males in New Zealand 1881",
           subtitle = "Selected places of birth shown") +
   annotate("text", x = 0.68, y = 0.60, colour = linecol, family = "serif",
            label = "Line shows same ratio\nfor Scots- and Irish- born",
            alpha = 0.5) +
   coord_equal()
{% endhighlight %}    
