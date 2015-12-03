---
layout: post
title: Deaths from assault over time in 40 relatively rich countries
date: 2015-11-26
tag: 
   - OpenData
   - Crime
   - DataFromTheWeb
   - R
description: Simple but effective graphics showing relative country standings with regard to deaths from assault, and trends over time.  Most countries in this group are seeing a decline in deaths from assault, from peaks in the 1970s, '80s or '90s.  Data are downloaded from OECD.Stat via their API and SDMX and this post shows how to do this, and also demos screen scraping with {rvest} - not that it needs any demo-ing, it's so user-friendly.
image: /img/0020-assault-average.svg
socialimage: http://ellisp.github.io/img/0020-assault-average.png
category: R
---
## Deaths from assault
A friend was looking at comparative data on violent deaths in a range of countries and I couldn't resist the chance to turn out some graphics.  The data come from the [OECD](http://stats.oecd.org/), who compiles them from contributions by member and associated country governments - effectively, wealthier countries - and makes them available via a web Application Programming Interface in the SDMX format.  I ended up with some graphics that I think tell a powerful story.

The data in this post are age-standardised deaths per 100,000 for whole population, male population, and female population; the OECD variables coded as TXCMILTX, TXCMHOTH and TXCMFETF.  I'm not an expert in this area - if they mean something else (I'm a little unsure on the age-standardisation) don't just curse me, let me know.

###Compare across countries and sex
Looking at snapshot cross-sectional average data since 1990 (for which there is more comparable data across countries, with a number of countries such as Germany and Czech Republic only coming into existence at or around that period) we see two things that met the "intra-ocular impact test" ie they hit you between the eyes:

* For nearly all the listed countries, males are much more likely to killed in a assault than are females
* The Americas and the former Soviet Socialist Republics (Russia, Estonia, Latvia, Lithuania) dominate the list of most violent places to live (or rather, to die).  Other than countries of these two types, only South Africa makes the top 11 countries.  The safer countries include those in eastern Europe (other than former USSR, but including countries that were in its political orbit), Asia, Australasia and western Europe.

Remember that this only shows relatively weathly countries - for a more complete list (but less complete data) check out this [Wikipedia list of countries that can be ranked by intentional homicide rates](https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate) (not quite the same as our data, which includes all deaths from assault, intentional or otherwise) or this [report from the UN Office on Drugs and Crime](https://www.unodc.org/documents/data-and-analysis/Crime-statistics/International_Statistics_on_Crime_and_Justice.pdf).

In the chart below, the label for each country is centred at the overall population rate of deaths from assault and the red and blue vertical strokes mark the female and male rates respectively.  Beware that the scale is logarithmic - this was necessary to stop everywhere except Colombia being squashed into the left of the chart.
![snapshot](/img/0020-assault-average.svg)

###Compare across time
Looking at trends across time we see an encouraging sign.  Most countries' violent death rates peaked in the 1980s or 90s and have been declining, in some cases dramatically, in recent years.

In the chart below the vertical axis for each country has been set to make most use of the plot area and draw attention to trends rather than absolute levels, so you can't compare the absolute levels of violence across countries.  That's what the first chart was for, so the two charts complement eachother.  The facets in the trend chart below have been *ordered* from lowest to highest rates (1990+ averages), so there is still some visual indicator of absolute size of the problem.

The recent declines are particularly strong in the  Eastern Europe and USSR region - Russia, Estonia, Latvia, Lithuania, Poland, Slovak Republic, Slovenia, Czech Republic, and Germany have all seen dramatic drops in rates of death from assault after rapid growth in the early 1990s, associated with the magnitude of the transition in economic and political systems that took place at that time.  There's also the possibility of changing official statistical practice with the changing political system, but there's no particular reason I know of (I'm not an expert in former and post Soviet statistics systems, either...) to think of that, and the general curve looks plausible given an outsider's view of the recent history there.
![over-time](/img/0020-deaths-trends.svg)

Tragically, Norway's [spike in 2011](https://en.wikipedia.org/wiki/2011_Norway_attacks) is really noticeable to a close viewer, even in a chart like this with 40 countries covering 50 years.

### Secondary point - Catholic countries and gender (more men getting killed) violence patterns?
Looking into the imbalance between the sexes, we see that the countries with consistently high male to female ratios of rates of violent death are Colombia, Brazil, Mexico, Chile, Costa Rica and South Africa, in all of which males are at least 5 times more likely to be killed violently than are females.  

![gender-plot](/img/0020-gender-ratios.svg)

At first glance there looks to be a possible correlation between being predominantly Catholic and a high ratio of male to female violent death rates (as well as the Latin American countries leading the pool, Italy and Ireland are also fairly high up on the list).  In an early draft of this post I had a casual mention of this and just a warning against slack inference, but I decided it was irresponsible to leave it at that.  To be more professional, I followed up my hunch with data on the proportion of a country that was Catholic in 2005 courtesy of [data published by David M. Cheney](http://www.catholic-hierarchy.org/country/sc1.html), a Catholic enthusiast (not sanctioned by any Catholic church authority but good enough for our purposes).

As often proves to be the case, the answer is ["I think you'll find it's a bit more complicated than that"](http://www.goodreads.com/book/show/23132200-i-think-you-ll-find-it-s-a-bit-more-complicated-than-that) and in fact there isn't an easy way to draw a conclusion about whether females are protected, or males picked on, particularly in Catholic countries, just from this aggregated data.

The challenge is that we've made up our hypothesis - relationship between Catholicism and males being disproportionately killed in assaults - after peeking at the data.  This generally renders post-peek model building difficult if not impossible, at least as a basis of hypothesis testing. 

The nub of the issue is that the apparent relationship with Catholicism might be an artefact of the high male murder by assault rates in five specific countries - Colmbia, Brazil, Mexico, etc - where the high Catholicism is highly correlated with other factors, specifically "Latin Americanism".  Any decent model should take this into account - it's just wrong to treat these five countries as independent observations (as is implicitly done in fitting a simple regression).  The image below illustrates the conundrum:

![catholics-gender](/img/0020-gender-catholic.svg)

* The top plot ignores the Latin Americanism of those five countries and fits a simple linear regression, which finds (illusory) statistically significant evidence of a link between Catholicism and a higher ratio of male to female death-from-assault rates.  As mentioned above, this approach is unsound because it ignores the grouping of the five prominent countries that drag up the slope of the line.
* The second plot controls for this by adding a dummy variable for "Americas".  The result is still statistically significant evidence of a Catholic effect, but interacting with the continent effect ie only in the Americas does it seem to matter being an increasingly Catholic country when wondering if males are more likely to be killed in an assault than females.
* The third plot (which I think is my preferred) adds a dummy variable for "Latin American" and finds no statistically significant relationship between Catholicism and male-female death ratios after that effect is accounted for.

There's no simple way to solve the correlation of our two explnatory variables - Americanism and Catholicism - and picking which of the second or third model above is best, and the essentially arbitrary nature of our continent-classification variable.  There's no possibility of creating a test-set of data, for example.  Even if we brought in what data are available on poorer countries and used them as a test set, the confounding relationship between Latin America and Catholicism would pertain to that dataset too.  So until more data or ideas come out (and I'm not claiming to have looked for them, either way) I prefer to stick to the third plot, and say only that male/female assault death ratios are much higher in Latin American countries, we don't know why from just this dataset, and after we've taken that into account there's no real further evidence in this particular dataset of an additional Catholic effect.

## Arranging the data
Here's how I import the data into R.  If you know exactly what you're looking for, you can import data directly from OECD.Stat with the [{rsdmx}](https://cran.r-project.org/web/packages/rsdmx/index.html) package by sending an appropriately structured URL (line 18 below) to their website.  However, to work out that URL you'll probably need to navigate [their site](http://stats.oecd.org/) by hand and choose your particular 'customising', then choose the "export" and "SDMX" options, then copy the URL it generates for you.  It's useful for reproducibility purposes (for example, it meant I didn't need to save a copy of the data anywhere to make the code below reproducible for others).
{% highlight R lineanchors %}
library(ggplot2)
library(scales)
library(grid)
library(gridExtra) # added 27/11/2015, for grid.arrange() to work!
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(rsdmx)    # for importing OECD data
library(ISOcodes) # for merging country codes with names
library(rvest)    # for screen scraping data about Catholicism

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_grey(base_family = "myfont"))

#----------Import and mung the death from assault data---------------
# load deaths by assault from OECD.Stat
if(!exists("viol_sdmx")){
   myUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/CICDHOCD.TXCMFETF+TXCMHOTH+TXCMILTX.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LVA+LTU+RUS+ZAF/all?startTime=1960&endTime=2014"
   dataset <- readSDMX(myUrl) # takes about 30 seconds on a slow hotel internet connection
   viol_sdmx <- as.data.frame(dataset) # takes about 10 seconds on an i5
}
{% endhighlight %}

The country codes provided by the OECD are ISO 3166 three digit country codes and we want to turn them into something more friendly for presentation purposes.  We also want to rename the levels of our units ("TXCMILTX" is less friendly than "Deaths per 100 000 population"), order some factor levels for future graphics purposes, and knock out Turkey which only has a few year's data.

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

Having created the original object "viol" I make a few summary and total objects to help with structuring my graphics.  In particular, I need a data frame that provides average rates since 1990 for the first chart, and a data frame of totals so I can arrange the plots in order of increasing rates of death from assault.

{% highlight R lineanchors %}
# create country x Unit summaries   
viol_sum <- viol %>%
   filter(Year > 1990) %>%
   group_by(Country, Unit) %>%
   summarise(Value = mean(Value)) %>%
   ungroup()

# create country totals (for ordering in charts)
totals <- viol_sum %>%
   group_by(Country) %>%
   summarise(Value = mean(Value)) %>%
   arrange(Value)

# create wider version, with one column per variable:
viol_spread <- viol_sum %>%
   mutate(Unit = as.character(Unit),
          Unit = ifelse(grepl("female", Unit), "Female", Unit),
          Unit = ifelse(grepl(" males", Unit), "Male", Unit),
          Unit = ifelse(grepl("population", Unit), "Population", Unit)) %>%
   spread(Unit, Value) %>%
   mutate(Country = factor(Country, levels = totals$Country))
{% endhighlight %}


## The actual plots
Now we're ready to draw some plots.  Here's the code that makes the main two plots:

{% highlight R lineanchors %}
viol_sum %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   mutate(Label = ifelse(grepl("population", Unit), as.character(Country), "|")) %>%
   ggplot(aes(x = Value, y = Country)) +
   geom_segment(data = viol_spread, aes(y = Country, yend = Country, x = Male, xend = Female),
                colour = "white", size = 3) +
   geom_text(size = 4, aes(label = Label, colour = Unit), alpha = 0.8,
             family = "myfont") +
   labs(y = "") +
   scale_x_log10(("Deaths per 100,000 (logarithmic scale)")) +
   theme(legend.position = "bottom") +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   labs(colour = "") +
   ggtitle("Mean annual deaths from assault 1990 to 2013") 

viol %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   ggplot(aes(x = Year, y = Value, colour = Unit)) +
   facet_wrap(~Country, scales = "free_y", ncol = 5) +
   geom_smooth(se = FALSE, method = "loess") +
   geom_point(alpha = 0.8, size = 1) +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   theme(legend.position = "bottom") +
   labs(y = "Deaths per 100,000 per year - note changing vertical scale", 
        title = "Deaths from assault", x = "")
{% endhighlight %}

To investigate the Catholicism and gender ratio issues in the second set of plots, I had to pull down some data from the web using [Hadley Wickham's amazing {rvest} package](https://cran.r-project.org/web/packages/rvest/index.html).  It's inspired by Python's wonderful [Beautiful Soup](http://www.crummy.com/software/BeautifulSoup/) but I have to say is *astonishingly* user friendly.

Here's my complete set of code for the gender dot plot, downloading the Catholicism data, and the resulting scatter plot.  The web-scraping part occupies a total of two lines of code (21 and 22 in the code below) - amazing:
{% highlight R lineanchors %}
# create a gender summary:
viol_gender <- viol %>%
   filter(!grepl("population", Unit)) %>%
   select(UNIT, Value, Year, Country) %>%
   spread(UNIT, Value) %>% 
   mutate(ratio = TXCMHOTH / TXCMFETF)  %>%
   group_by(Country) %>%
   summarise(ratio = mean(ratio, tr = 0.2)) %>%
   arrange(ratio) %>%
   # knock out Luxembourg and Iceland, too many NAs:
   filter(!is.na(ratio)) %>%
   mutate(Country = factor(Country, levels = Country))

# plot it:   
viol_gender %>%
   ggplot(aes(x = ratio, y = Country)) +
   geom_point() +
   labs(x = "Trimmed mean annual ratio of male to female rates\nof deaths from assault over whole period", y = "")

# download catholic proportion data
cath_page <- read_html("http://www.catholic-hierarchy.org/country/sc1.html")
cath_data <- html_table(cath_page)[[1]] 
names(cath_data) <- gsub(" ", "_", names(cath_data))

# create vectors of country names for use in creating dummy variables:
lat_american <- c("Colombia", "Brazil", "Mexico", "Chile", "Costa Rica")
american <- c(lat_american, c("Canada", "United States"))

# mung:
cath_data <- cath_data %>%
   mutate(Country = gsub("M.+xico", "Mexico", Country),
          Country = ifelse(Country == "USA", "United States", Country),
          Country = ifelse(Country == "Great Britain", "United Kingdom", Country),
          Country = ifelse(Country == "Korea (South)" , "Korea, Republic of", Country)) %>%
   select(Country, Percent_Catholic) %>%
   mutate(Percent_Catholic = as.numeric(gsub("%", "", Percent_Catholic)),
          Continent1 = ifelse(Country %in% american, "Americas", "Other"),
          Continent2 = ifelse(Country %in% lat_american, "Latin American", "Other"))

# join the Catholic data to the assault data:          
viol_gender_cath <- viol_gender %>%
   left_join(cath_data, by = "Country")

# set up base plot:   
cath1 <-    viol_gender_cath %>%
   ggplot(aes(x = Percent_Catholic, y = ratio, label = Country))  +
   geom_smooth(method = "lm") +
   geom_text(family = "myfont", size = 3, alpha = 0.8) +
   labs(x = "Percentage of country reported to be Catholic", 
        y = "Ratio of female to male\nassault death rates") +
      xlim(-5, 110)

# and the other two types of plots:      
cath2 <- cath1 + aes(colour = Continent1)
cath3 <- cath1 + aes(colour = Continent2)

# draw plots:
grid.arrange(cath1, cath2, cath3)

# for reference, look explicitly at the underlying statistical models:
mod1 <- lm(ratio ~ Percent_Catholic, data = viol_gender_cath)
mod2 <- lm(ratio ~ Percent_Catholic * Continent1, data = viol_gender_cath)
mod3 <- lm(ratio ~ Percent_Catholic * Continent2, data = viol_gender_cath)

summary(mod1)
summary(mod2)
summary(mod3)
{% endhighlight %}
