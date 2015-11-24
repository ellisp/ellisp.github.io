---
layout: post
title: Violent deaths over time in 40 countries
date: 2015-11-22
tag: 
   - OpenData
   - Crime
   - R
description: Simple but powerful graphics showing relative country standings with regard to violent deaths, and trends over time.  Most countries are seeing a decline in violent deaths from peaks in the 1970s, '80s or '90s.  Data downloaded from OECD.Stat via their API and SDMX.
image: /img/0020-assault-average.svg
socialimage: http://ellisp.github.io/img/0020-assault-average.png
category: R
---
## Violent deaths
A friend was looking at comparative data on violent deaths in a range of countries and I couldn't resist the chance to turn out some graphics.  The data come from the [OECD](http://stats.oecd.org/), who compiles them from contributions by member and associated country governments.  I ended up with some graphics that I think tell a powerful story.

###Compare across countries and sex
Looking at snapshot cross-sectional average data since 1990 (for which there is more comparable data across countries, with a number of countries such as Germany and Czech Republic only coming into existence at or around that period) we see two things that met Tukey's "intra-ocular impact test" ie they hit you between the eyes:

* Nearly everywhere, males are much more likely to killed in a violent assault than are females
* The Americas and the former Soviet Socialist Republics (Russia, Estonia, Latvia, Lithuania) dominate the list of most violent places to live (or rather, to die).  Other than countries of this type, only South Africa makes the top 11 countries.  In case you're wondering if this is selection bias (the OECD and its non-member associates who provide it data comprise the wealthier countries of the world), [World Health Organisation reports](link) suggest that this is not just a result of missing out the rest of Africa.

In the chart below, the label for each country is centred at the overall population rate of violent deaths and the red and blue vertical strokes mark the female and male rates respectively.  Beware that the scale is logarithmic - this was necessary to stop everywhere except Colombia being squashed into the left of the chart.
![snapshot](/img/0020-assault-average.svg)

Looking into the imbalance between the sexes, we see that the countries with consistently high male to female ratios of rates of violent death are Colombia, Brazil, Mexico, Chile, Costa Rica and South Africa, in all of which males are at least 5 times more likely to be killed violently than are females.  There looks to be a possible correlation between being predominantly Catholic and a high ratio of male to female violent death rates (Italy and Ireland also fairly high up on the list), but Catholicism as a variable is so confounded with the phenomenon of violence in the Americas south of Canada that establishing a link at a country level is fraught with statistical dangers.

![gender-plot](/img/0020-gender-ratios.svg)

###Compare across time
Looking at trends across time we see an encouraging sign.  Most countries' violent death rates peaked in the 1980s or 90s and have been declining, in some cases dramatically, in recent years.

In the chart below the vertical axis for each country has been set to make most use of the plot area and draw attention to trends rather than absolute levels, so you can't compare the absolute levels of violence across countries.  That's what the first chart was for, so the two charts complement eachother.  The facets in the trend chart below *have* been ordered from lowest to highest rates (1990 onwards), so there is still some visual indicator of absolute size of the problem.

The recent declines are particularly strong in the  Eastern Europe and USSR region - Russia, Estonia, Latvia, Lithuania, Poland, Slovak Republic, Slovenia, Czech Republic, and Germany have all seen dramatic drops in rates of violent death after rapid growth in the early 1990s associated with the magnitude of the transition in economic and political systems that took place at that time.  There's also the possibility of changing official statistical practice, but that's pure speculation on my part.
![over-time](/img/0020-deaths-trends.svg)

## Arranging the data
Here's how I import the data into R.  
{% highlight R lineanchors %}
library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(rsdmx)
library(ISOcodes)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_grey(base_family = "myfont"))



#----------Import and mung data---------------
# load deaths by assault from OECD.Stat
if(!exists("viol_sdmx")){
   myUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/CICDHOCD.TXCMFETF+TXCMHOTH+TXCMILTX.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LVA+LTU+RUS+ZAF/all?startTime=1960&endTime=2014"
   dataset <- readSDMX(myUrl) # takes about 30 seconds on a slow hotel internet connection
   viol_sdmx <- as.data.frame(dataset) # takes about 10 seconds on an i5
}
{% endhighlight %}

{% highlight R lineanchors %}
# load Country codes from ISOcodes R package
data("ISO_3166_1")

# mung:
viol <- viol_sdmx %>%
   # match country codes to country names:
   left_join(ISO_3166_1[ , c("Alpha_3", "Name")], by = c("COU" = "Alpha_3")) %>%
   # more friendly names:
   rename(Country = Name,
          Year = obsTime,
          Value = obsValue) %>%
   # limit to columns we want:
   select(UNIT, Country, Year, Value) %>%
   # make graphics-friendly versions of Unit and Year variables:
   mutate(Unit = ifelse(UNIT == "TXCMILTX", 
                        "Deaths per 100 000 population", "Deaths per 100 000 males"),
          Unit = ifelse(UNIT == "TXCMFETF", "Deaths per 100 000 females", Unit),
          Unit = factor(Unit, levels = unique(Unit)[c(2, 3, 1)]),
          Year = as.numeric(Year)) %>%
   # not enough data for Turkey to be useful so we knock it out:
   filter(Country != "Turkey")
{% endhighlight %}

Having created the original object "viol" I make a few summary and total objects to help with structuring my graphics.  In particular, I need a data frame that provides average rates since 1990 for the first chart, and a data frame of totals so I can arrange the plots in order of increasing rates of violent death.

{% highlight R lineanchors %}
viol_sum <- viol %>%
   filter(Year > 1990) %>%
   group_by(Country, Unit) %>%
   summarise(Value = mean(Value)) %>%
   ungroup()

totals <- viol_sum %>%
   group_by(Country) %>%
   summarise(Value = mean(Value)) %>%
   arrange(Value)

viol_spread <- viol_sum %>%
   mutate(Unit = as.character(Unit),
          Unit = ifelse(grepl("female", Unit), "Female", Unit),
          Unit = ifelse(grepl(" males", Unit), "Male", Unit),
          Unit = ifelse(grepl("population", Unit), "Population", Unit)) %>%
   spread(Unit, Value) %>%
   mutate(Country = factor(Country, levels = totals$Country))
{% endhighlight %}


## The actual plots
Now we're ready to draw some plots.  Here's the code that makes the first two:

{% highlight R lineanchors %}
viol_sum %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   mutate(Label = ifelse(grepl("population", Unit), as.character(Country), "|")) %>%
   ggplot(aes(x = Value, y = Country)) +
   geom_segment(data = viol_spread, aes(y = Country, yend = Country, x = Male, xend = Female),
                colour = "white", size = 3) +
   geom_text(size = 4, aes(label = Label, colour = Unit), alpha = 0.8,
             gp = gpar(fontfamily = "myfont")) +
   labs(y = "") +
   scale_x_log10(("Deaths per 100,000 (logarithmic scale)")) +
   theme(legend.position = "bottom") +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   labs(colour = "") +
   ggtitle("Mean annual deaths from violent assault 1990 to 2013") 
   
viol %>%
   filter(!grepl("population", Unit)) %>%
   select(UNIT, Value, Year, Country) %>%
   spread(UNIT, Value) %>% 
   mutate(ratio = TXCMHOTH / TXCMFETF)  %>%
   group_by(Country) %>%
   summarise(ratio = mean(ratio, tr = 0.2)) %>%
   arrange(ratio) %>%
   # knock out Luxembourg and Iceland, too many NAs:
   filter(!is.na(ratio)) %>%
   mutate(Country = factor(Country, levels = Country)) %>%
   ggplot(aes(x = ratio, y = Country)) +
   geom_point() +
   labs(x = "Trimmed mean annual ratio of male to female rates\nof violent death over whole period", y = "")
{% endhighlight %}

And here's the plot that draws the smoothed trend lines:
{% highlight R lineanchors %}
viol %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   ggplot(aes(x = Year, y = Value, colour = Unit)) +
   facet_wrap(~Country, scales = "free_y", ncol = 5) +
   geom_smooth(se = FALSE, method = "loess") +
   geom_point(alpha = 0.8, size = 1) +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   theme(legend.position = "bottom") +
   labs(y = "Deaths per 100,000 per year - note changing vertical scale", 
        title = "Deaths from violent assault", x = "")
{% endhighlight %}
