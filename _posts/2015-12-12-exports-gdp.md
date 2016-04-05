---
layout: post
title: Importance of exports and economic growth, cross-country time series
date: 2015-12-12
tag: 
   - OpenData
   - EconomicGrowth
   - DataFromTheWeb
   - Timeseries
   - R
description: Fitting a mixed-effects model with auto-correlated residuals to longitudinal country-level data, showing how changing importance of exports is positively related to economic growth.
image: /img/0023-p6.svg
socialimage: http://ellisp.github.io/img/0023-p3.png
category: R
---
## Exports and economic growth
I was looking to show a more substantive piece of analysis using the World Development Indicators data, and at the same time show how to get started on fitting a mixed effects model with grouped time series data.  The relationship between exports' importance in an economy and economic growth forms a good start as it is of considerable theoretical and practical policy interest and has fairly reliable time series data for many countries.  At the most basic level, there is a well known positive relationship between these two variables:
![scatterplot](/img/0023-p3.svg)

The relationship is made particularly strong by the small, rich countries in the top right corner.  Larger countries, with their own large domestic markets, can flourish economically while being less exports-dependent than smaller countries - the USA being the example par excellence.  If the regression is weighted by population, the relationship is much weaker than shown in the above diagram.

However, today, I'm looking at a different aspect of the relationship - changes over time.  Partly this is because I'm genuinely interested, but mostly because I needed an example demonstrating fitting a mixed effects model to longitudinal data with a time series component.  

The data come from the World Bank's [World Development Indicators](http://databank.worldbank.org/data/reports.aspx?source=world-development-indicators) (WDI), which I explored recently in [my last post](http://ellisp.github.io/blog/2015/12/05/wdi-inequality/).  I'm comparing "Exports of goods and services (% of GDP)" with "GDP per capita (constant 2000 US$)".  The WDI have at least some data on these variables for 186 countries, but different starting years for each (earliest being 1962).  The data look like this, in a connected scatterplot showing the relationship between the two variables for 12 randomly chosen countries:

![csp1](/img/0023-p1.svg)

... and this, in a more straightforward time series line plot:
![ts1](/img/0023-p2.svg)

Close watchers will see that there are some country groupings in the dataset (eg "Pacific island small states") that we don't want, in addition to the individual countries we do.  So our first job is to get rid of these.  Here's the R code that pulls in the data, draws the plots so far, and removes those country groupings.

{% highlight R lineanchors %}
library(WDI)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(nlme)

# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


#-------------------data imports, first explore, and clean-----------------
if(!exists("exports")){
   # Exports of goods and services (% of GDP)
   exports <- WDI(indicator = "NE.EXP.GNFS.ZS", end = 2015, start = 1950)
   # GDP per capita (constant 2000 US$)
   gdp <- WDI(indicator = "NY.GDP.PCAP.KD", end = 2015, start = 1950)
}

both <- merge(exports, gdp) %>%
   rename(exports = NE.EXP.GNFS.ZS,
          gdp = NY.GDP.PCAP.KD) %>%
   # removing any year-country combos missing eith gdp or exports:
   filter(!(is.na(exports) | is.na(gdp))) %>%
   arrange(country, year)

# let's look at 12 countries at a time
all_countries <- unique(both$country)
sampled <- both    %>%
   filter(country %in% sample(all_countries, 12))
   
# connected scatter plot:
p1 <- sampled %>%
   ggplot(aes(y = gdp, x = exports, colour = year)) +
   facet_wrap(~country, scales = "free") +
   geom_path() +
   labs(x = "Exports as a percentage of GDP") +
   scale_y_continuous("GDP per capita, constant 2000 US dollars", label = dollar) +
   ggtitle("Exports and GDP over time, selected countries")


# univariate time series plots
p2 <- sampled %>%
   gather(variable, value, -(iso2c:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")


# how to get rid of the country groups?
unique(both[ , c("iso2c", "country")]) %>% arrange(iso2c)
# they have a number as first or second digit, or X or Z as first digit (but ZW ZA ZM legit)

both2 <- both %>%
   filter(!grepl("X.", iso2c) & !grepl("[0-9]", iso2c)) %>%
   filter(!grepl("Z.", iso2c) | iso2c %in% c("ZW", "ZA", "ZM")) %>%
   filter(!iso2c %in% c("OE", "EU"))

#------------------------simple cross section used for first plot-------------------
country_sum <- both2 %>%
   group_by(country) %>%
   filter(year == max(year))

p3 <- country_sum %>%
   ggplot(aes(x = exports / 100, y = gdp, label = country)) +
   geom_text(size = 3) +
   geom_smooth(method = "lm") +
   scale_y_log10(label = dollar) +
   scale_x_log10(label = percent) +
   labs(x = "Exports as a percentage of GDP",
        y = "GDP per capita, constant 2000 US dollars",
        title = "Cross section of exports and GDP, latest years when data are present")
{% endhighlight %}

The plan is to fit an appropriate time series model with GDP as a response variable and exports as a percentage of GDP as an explanatory variable, allowing country to be a group random effect, and the country-year individual randomness to be related year to year as a time series.  This means a [mixed effects model](https://en.wikipedia.org/wiki/Mixed_model). There's a bit of transforming I need to do:

* To avoid spurious correlation I'll need to transform the data until each time series is at least approximately stationary.  I'll do this by taking differences of the logarithm of the series, which has the same impact as looking at year on year growth rates.
* I'll also need some kind of control for the nuisance factor that each country has a different starting year for its data.
* I want to control for the possibility that the absolute value of exports-orientation at the *start* of recorded data has a persistent impact, so I need to have that absolute level (or the logarithm of it, which has nicer properties) as a country-level variable.
* I'm interested in whether a change in exports has a persistent impact, so I'll need a lagged value of my transformed exports variable.  Otherwise we might just be picking up the inherent impact on GDP of a change in exports or their prices, with domestic contribution to GDP kept constant.  If there's a persistent effect on future years from a change exports-orientation in one year, it's much more likely to reflect something significant economically (still not necessarily causal though).
* Even after taking first differences of the logarithms of my two main variables (GDP, and Exports as a percentage of GDP), I'm still a little worried that in particular countries there will be non-stationary time series that result in spurious associations.  For example, if the growth rates of both variables are steadily growing or declining.  To reduce this risk I'm going to include year in my model, so at least any linear trend of that sort is removed before looking at whether the two variables move together.

Here's the distribution of the first recorded value of the logarithm of exports as a percentage of gdp:
![first](/img/0023-p4.svg)

Here's the connected scatter plot of the differenced logarithms (effectively, growth rates):
![csp2](/img/0023-p6.svg)

Here's the traditional time series plots:
![ts2](/img/0023-p5.svg)

And here's the code that does the transformations and plots:
{% highlight R lineanchors %}
# first we want to know the value of exports at the beginning of each series
first_values <- both2 %>%
   group_by(country) %>%
   filter(year == min(year)) %>%
   ungroup() %>%
   mutate(exports_starter = log(exports),
          first_year = year) %>%
   select(iso2c, exports_starter, first_year)

p4 <- ggplot(both3, aes(x = exports_starter)) + geom_density()

# all those individual time series have problem of non-stationarity and of course autocorrelation
# so when we merge with the starting values of exports we also calculate first
# differences of logarithms
both3 <- both2 %>%
   left_join(first_values, by = "iso2c") %>%
   arrange(country, year) %>%
   group_by(country) %>%
   mutate(exports_g = c(NA, diff(log(exports))),
          gdp_g = c(NA, diff(log(gdp))),
          exports_lag = lag(exports_g)) %>%
   ungroup() %>%
   filter(!is.na(gdp_g)) %>%
   filter(!is.na(exports_lag))

all_countries2 <- unique(both3$country)

set.seed(123)
sampled <- both3 %>%
   filter(country %in% sample(all_countries2, 12)) 

p5 <- sampled %>%
   select(country, year, exports_g, gdp_g) %>%
   gather(variable, value, -(country:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")

p6 <- sampled %>%
   select(country, year, exports_g, gdp_g) %>%
   ggplot(aes(x = exports_g, y = gdp_g, colour = year)) +
   geom_smooth(colour = "red", method = "lm") +
   geom_path() +
   geom_point() +
   facet_wrap(~ country, scales = "free") +
   labs(x = "change in logarithm of exports share of GDP",
        y = "change in logarithm of GDP per capita, constant 2000 USD",
        title = "Changing importance of exports and GDP growth, selected countries")
{% endhighlight %}
So we're ready for some modelling.  After all the data transformations, this part is relatively easy.  I use a mixed effects model that lets the key effects vary in each country, around an average level of the effect.

I tried a few simpler models than below and checked the auto-correlation functions to be sure the time series part was needed (it is; `ACF()` returns an autocorrelation at lag 1 of around 0.23, and the decay of autocorrelation in subsequent lags characteristic of a fairly simple AR process).  So here we go:

{% highlight R lineanchors %}
model4 <- lme(fixed = gdp_g ~ ordered(first_year) + exports_g + exports_lag + exports_starter + year,
              random = ~ exports_g + exports_lag + year | country, 
              correlation = corAR1(form = ~ year | country),
              data = both3)

{% endhighlight %}

Just to visualise what this model is doing, imagine the connected scatter plot of the differenced logarithms of each variable, as shown earlier.  We're drawing a diagonal line of best fit on each facet, with the  slope and  intercept of each line allowed to different for each country.  The average slope is the average impact of changes in exports as a percent of GDP on GDP growth.  Now add to this the complication of the other variables - the first year data starts (basically just a nuisance variable we want to control for); each countrys' absolute level of exports as a percentage of GDP when its data started; the lagged value of exports (which is actually the number of most interest); and year, for which we're trying to control for any linear trend.

The `correlation = corAR1(...)` part is important, as it means that when we come to conduct inference (t statistics, p values and confidence intervals) the model takes into account that each observation in a particular country is related to the previous year's - they aren't as valuable as independent and identically distributed data from a simple random sample.  Failing to include this factor would mean that inference was biased in the direction of concluding results are significant that are in fact due to chance.

And here are the results.  Bear in mind that:

* the response variable is change in the logarithm of GDP; 
* `exports_g` is change in the logarithm of exports as a percentage of GDP; 
* `exports_lag` is the lagged value of `exports_g`;
* `exports_starter` is the logarithm of the starting value of exports as a percentage of GDP;
* `year` is year, and we are testing for a linear trend in growth of GDP

{% highlight R lineanchors %}
> round(tail(summary(model4)$tTable, 4), 3)
                 Value Std.Error   DF t-value p-value
exports_g        0.031     0.010 7006   3.191   0.001
exports_lag      0.021     0.006 7006   3.579   0.000
exports_starter -0.002     0.002  150  -1.233   0.220
year             0.000     0.000 7006  -1.328   0.184
{% endhighlight %}

Not wanting to give too precise an interpretation of this without a bit more theory and thinking, it suggests for countries on average:

* evidence of impact (on GDP growth) of a change in exports as a percentage of GDP, and the impact is in the same direction; 
* the impact persists beyond the immediate year into subsequent years;
* no evidence of a systematic change in growth rates of GDP over time simply related to time and nothing else in the model;
* no evidence that the starting absolute value of exports as a percentage of GDP impacts on subsequent GDP growth rates

As `exports_g`, `exports_lag` and `year` were all allowed to be random effects ie take different values by country, their differing values for each country are of interest.  The figures above reflect an overall effect of these variables; the actual value in any country is a (approximately) normally distributed random variable.  Here's how their' densities look:

![ref](/img/0023-p7.svg)

Each observation (the little rug marks on the horizontal axis) is the size of the effect for a particular country.  So we can see that while overall value of exports_lag - the persistent impact of a change in exports as a percentage of GDP on GDP growth - was 0.021 as per the table above, any particular country has a value that is lower or hight than that, with a reasonable number of countries seeing a negative impact. 

The value for the `exports_g` parameter plus the `exports_lag` parameter gives a crude sense of the overall impact, for a particular country, of changing importance of exports on GDP growth.  As can be seen from the top right panel of the above set of plots, this combined value is generally but not always positive.

All up, while we might conclude that for countries "on average" there is a positive relationship between growth in the importance of exports and GDP growth, for any particular country the relationship will vary and may in fact be negative.  Further investigation would need to place this in a more theoretical context, and control for other variables that might be confounding the results; but the above is probably enough for an exploratory blog post.

Here's how to extract those country-level effects from our mixed effects model object:

{% highlight R lineanchors %}
cf <- coef(model4)

# let's look at just the last four coefficients ie exclude the nuisance of first_year
cf_df <- cf[, (ncol(cf) - 3):ncol(cf)] %>%
   as.data.frame() %>%
   mutate(country = rownames(cf),
          combined = exports_lag + exports_g) %>%
   arrange(combined)


p7 <- cf_df %>%
   gather(variable, value, -country, -exports_starter) %>%
   mutate(variable = gsub("combined", "exports_g + exports_lag", variable, fixed = TRUE)) %>%
   ggplot(aes(x = value)) +
   facet_wrap(~variable, scales = "free") +
   geom_density() +
   geom_rug()
{% endhighlight %}