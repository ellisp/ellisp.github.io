---
layout: post
title: Sampling distribution of Gini coefficient
date: 2015-09-12
tag: 
   - Inequality
   - Distributions
   - R
   - NewZealand
   - NZIS2011
description: I explore the sampling distributions of estimates of Gini coefficients from a sample, using the New Zealand Income Survey 2011.  At the actual sample size of nearly 30,000, sampling error is negligible, and as low as a sample size of 1,000 a 95% confidence interval is (0.48, 0.55), precise enough for most purposes and certainly good enough given the non-sampling vagaries of the underlying data.  I discuss why individual and weekly income data - which is all I have to hand - returns a higher measure of inequality than does annual household income, the more usual and internationally-comparable (and completely valid) measure.
image: /img/0008-lorenz.svg
socialimage: http://ellisp.github.io/img/0008-lorenz.png
category: R
---

## Inequality measures

Part of my motivation for [importing the New Zealand Income Survey](/blog/2015/08/15/importing-nzis-surf.html)(NZIS) simulated unit record file provided by Statistics New Zealand was to explore the characteristics of various measures of inequality.  In particular, I'm interested in what happens to the [sampling distributions](https://en.wikipedia.org/wiki/Sampling_distribution) as sample size changes of the following summary statistics:

* Gini coefficient (ie the area between a Lorenz curve and the line of perfect equality)
* P90/P10 (ie income at the 90% richest percentile divided by that at the 10% percentile)
* P80/P20 (as above, but the 80 and 20 percentiles
* Top 1% income as a percentage of all income
* Top 0.1% income as a percentage of all income

A Lorenz curve is an attempt to show the full shape of the income distribution at once.  It maps the cumulative proportion of the population against the cumulative proportion of income (or whatever variable of interest) that they own.  In the example below, you can read that the poorest 50% of New Zealand individuals, for example, earn around 13% of total income in a given week

![lorenz-plot](/img/0008-lorenz.svg)

This plot was produced with the following R code, which requires the data to have been imported to a database called PlayPen in accordance with the [previous post](/blog/2015/08/15/importing-nzis-surf.html).
{% highlight R lineanchors %}
library(RODBC)
library(ineq)     # for Lc and Gini
library(dplyr)
library(ggplot2)
library(scales)
library(showtext) # for fonts
library(stringr)  # for str_wrap

font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- odbcConnect("PlayPen_Prod")
sqlQuery(PlayPen, "use nzis11")

inc <- sqlQuery(PlayPen, "select income from vw_mainheader")$income

lorenz <- Lc(inc)
lorenz_df <- data.frame(prop_pop = lorenz$p, income = lorenz$L) %>%
   mutate(prop_equality = prop_pop)

p1 <- ggplot(lorenz_df, aes(x = prop_pop, y = income)) +
   geom_ribbon(aes(ymax = prop_equality, ymin = income), fill = "yellow") +
   geom_line() +
   geom_abline(slope = 1, xintercept = 0) +
   scale_x_continuous("\nCumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income\n", label = percent) +
   theme_minimal(base_family = "myfont") +
   coord_equal() +
   annotate("text", 0.53, 0.32, label = "Inequality\ngap", family = "myfont") +
   annotate("text", 0.5, 0.6, label = "Complete equality line", angle = 45, family = "myfont") + 
   ggtitle (
      str_wrap("Cumulative distribution of New Zealand individual weekly income from all sources", 46))

print(p1)

grid.text("Source: Statistics New Zealand\nNational Income Survey 2011 SURF", 0.8, 0.23, 
       gp = gpar(fontfamily = "myfont", fontsize = 8))
{% endhighlight %}

### Choice of measures
Income inequality is notoriously difficult to measure, and the P90/P10 and P80/P20 measures are attempts to deal with the patchy information available particuarly at the top level (the super rich don't slum it filling in surveys, and are often expected to be particularly disinclined to reveal how rich they are voluntarily; and there are strong incentives to minimise income in a tax context).  However, the P90/P10 and similar measures have been criticised (for example, by Thomas Picketty in  his excellent book [Capital in the 21st Century](http://www.amazon.com/Capital-Twenty-First-Century-Thomas-Piketty/dp/1491534656)) on several grounds, the most important of which seem to me to be:

* They ignore what is going on in the top 10% or top 20%, which might be precisely the point of most interest
* They are unstable, because by dividing by the income (or wealth) measure of the lowest 10% or 20% you are dividing by a very low number, and small changes in measurement (for example, how one counts household furniture, if looking at wealth inequality) make big changes to the measure even though inequality to most minds is unchanged.

These arguments seem to me cogent, although I see reasons for keeping those measures too.  Picketty prefers to talk about the income or wealth of the bottom 50%, the next 40%, then the top 10% and 1%.  He dislikes the Gini coefficient because it is difficult to explain (on which I agree with him) and for not really showing the true inequality (on which I disagree).  I disagree on the latter point because I think that the Gini coefficient is actually the logical extension of his approach of looking at the cumulative income or wealth of the bottom 50%, next 40% etc; all the Gini coefficient does is take it to the extreme, and it gives a genuinely good measure of total inequality (assuming the source data are ok), including the impact of the top 10%, 1%, 0.1%, etc.

## Gini coefficient
The curve above yields a Gini coefficient of 0.51, which is high compared to the Gini coefficients that are commonly reported.  For example [Statistics New Zealand via the OECD report a Gini coefficient of 0.33](http://www.stats.govt.nz/browse_for_stats/snapshots-of-nz/nz-social-indicators/Home/Standard%20of%20living/income-inequality.aspx) for household income.  There are three reasons (at least) for the discrepancy, which make the NZIS a poor choice for inequality measures that are comparable to the most commonly used (although still ok for my purposes):

* The NZIS is an individual measure, whereas most reported measures are done at the household level.  In previous posts I've discussed the large numbers of zero income individuals in the NZIS.  Many of these will be part of households with positive total income, contributing to a lower level of inequality in a household-based measure.  There's arguments for both methods, I think - it's not hard to imagine the [feminist argument for paying more attention to intra-household income differences](https://scholar.google.co.nz/scholar?q=feminist+critique+of+measuring+income+at+the+household+level&hl=en&as_sdt=0&as_vis=1&oi=scholart&sa=X&ved=0CBkQgQMwAGoVChMIjrKF6t3wxwIVR8emCh17uwXw).
* Conventionally these sorts of measures are done on an annual basis.  Again, this will lead to a more equal distribution than a snapshot of a single week.  There's nothing magic about annual income (why not ten years? per lifetime? or a month?), but it is a strong and understandable convention.
* Most income inequality measures are based on income after taxes and transfers (or if both before and after are used, more emphasis is usually given to 'after taxes and transfers').  My interpretation of the "gross weekly income from all sources" described in the [technical notes for the NZIS SURF](http://www.stats.govt.nz/tools_and_services/microdata-access/nzis-2011-cart-surf.aspx) is that it represents income after transfers, but prior to tax.

Let's be clear - I'm not saying my Gini coefficient is better than Statistics New Zealand's.  If anyone claims I'm criticising the official measure you're misrepresenting me and please don't!  In this post, I'm exploring the technical characteristics of estimates of Gini coefficients, and I happen to have weekly, individual gross income rather than annual, household, after taxes-and-transfers income to do it with.

### Weekly versus annual measures?
Let's explore that weekly issue a bit more.  Consider two extremes:

* If everyone's weekly income is unchanging and the week that we've sampled is representative of every week for them, the annual inequality will be identical to the weekly ie 0.51
* If income in one week bears no relation to income in the other weeks, but each week an individual gets a random draw from the pool of possible incomes, the annual inequality will be substantively less - around 0.09

For those not familiar with the Gini coefficient, 1 means complete inequality (ie one person receives all the income) and 0 means complete inequality (everyone gets exactly the same income).  0.09 is far lower than any observed economic inequalities, and reflects the absurdity of everyone's weekly income being random - cleaners don't spend the occasional week as CEO of a large firm.  However, one certainly expects some degree of smoothing.

Taking all those factors into account, the weekly individual income Gini coefficient of 0.51 does not contradict the official household annual figure of 0.33 (which is a relief, of course).  Here's how I worked out that 0.09 number, with a little simulation based on resampling the observed data:
{% highlight R lineanchors %}
Gini(inc)                   # 0.51

# if completely constant each week
Gini(inc * 52)              # 0.51

# create incomes that simulate each week being a random pull from the pool
random_incomes <- data_frame(
   income = sample(inc, 52 * length(inc), replace = TRUE),
   person = rep(1:length(inc), 52)) %>%  
   group_by(person) %>%
   summarise(income = sum(income))

Gini(random_incomes$income) # 0.09

{% endhighlight %}

### "Must be positive non zero"?
The literature on Gini coefficients says it can only be fitted to positive, non zero data but I see no practical problem with fitting them to a dataset with zeroes and negative values, even one with a [large number of zeroes like the NZIS](/blog/2015/09/05/creating-a-scale-transformation.html).  Including negative numbers means the Lorenz curve briefly dips below zero, and it becomes possible to have a Gini coefficient greater than 1 (for example, if one person has a positive income and everyone else has negative), but this doesn't strike me as a reason for not using it.  The result is intuitively ok and there are no computational difficulties so long as at least one number is positive and the sum of the positive numbers is enough to outweigh the sum of the negative numbers.  If anyone can think of a reason for only applying Gini coefficients and Lorenz curves to strictly positive data let me know.

### Sampling distribution of a Gini coefficient
So the question I'm seeking to answer is, how do inequality statistics like the Gini coefficient and the others stand up with small samples of real data?  Putting aside non-sampling error (like people misleading the interviewers), what happens with a smaller survey?  I know from [various sources online](http://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm) that "The small sample variance properties of G are not known, and large sample approximations to the variance of G are poor".

I'm interested in both the shape of the distribution of estimates of these figures, and their standard errors ie how far out from the "true" value we'd expect the estimates to be.  The NZIS has a sample of around 29,000; let's shrink that down to 30 and see how much the estimated Gini coefficient changes with different randomly selected bunches of 30 people, compared to 1,000, and the original sample of around 30,000.  Note that the horizontal axes on the plots below are on differing scales - I did this to keep a visual sense of the shape of the distribution.

![n30](/img/0008-density-gini-n30.svg)
![n1000](/img/0008-density-gini-n1000.svg)
![n30000](/img/0008-density-gini-n30000.svg)

This estimator behaves better than I thought it would with real data that has all the weird and wonderful extremes we get with individual economic variables.  Certainly by the time the sample size gets up to 1,000 or so there are no outrageous values and the range of estimated values is reasonably narrow, although wide enough to beware of comparisons of small differences at that sample size.  This is in contrast to the sample size of 30, where clearly in one instance we got a sample with one high earner and many zeros or negatives, resulting in a Gini coefficient of more than 1!  Make a note not to estimate a population's inequality from such a small sample.  For the actual sample size of the New Zealand Income Survey of nearly 30,000, sampling error is negligible.  Now, if only we could say the same for the non-sampling error - so much harder to quantify, so much harder to control for.  But subject for reflections at a later date.

The above analysis was done by creating a function to conduct the simulation and draw the plot for a given sample size n:
{% highlight R lineanchors %}
sim_gini <- function(n, reps = 1000){
   results <- data.frame(
      trial = 1:reps,
      estimate = numeric(reps))
   
   set.seed(123) # for reproducibility
   for(i in 1:reps){
      results[i, "estimate"] <- 
         Gini(sample(inc, n, replace = TRUE))
   }
   
   print(results %>%
      ggplot(aes(x = estimate)) +
      geom_density() +
      geom_rug() +
      theme_minimal(base_family = "myfont") +
      ggtitle(paste("Distribution of estimated Gini coefficient, n =", n)))
   
   grid.text(paste0("Standard error: ", round(sd(results$estimate), 3)), 0.8, 0.6, 
             gp = gpar(fontfamily = "myfont", fontsize = 9))
   
   grid.text(paste0("95% Confidence interval:\n", 
                    paste(round(quantile(results$estimate, c(0.025, 0.975)), 3), collapse = ", ")), 0.8, 0.5, 
             gp = gpar(fontfamily = "myfont", fontsize = 9))
 }
 
sim_gini(30)

sim_gini(1000)

sim_gini(30000)
{% endhighlight %}

Further exploration of the other inequality measures and their distributions will wait for another post, as this one is long enough already.