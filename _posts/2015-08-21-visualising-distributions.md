---
layout: post
title: A better way of visualising income distributions with zeroes and negatives
date: 2015-08-21
tag: 
   - R
   - NewZealand
   - Transformations
   - NZIS2011
description: I demonstrate a better way of visualising income distributions when they include zero and negative values rather than just putting them on a logarithmic scale and dropping the inconvenient values.  I use a modified power transformation, which applies a transformation like square root to the absolute value of the original variable and then restores the sign.  I apply the method to the New Zealand Income Survey 2011 data.
image: /img/0004_better_density_plot.svg
category: R
---


I wasn't happy with [my visualisation of individual incomes from the New Zealand income survey](/blog/2015/08/15/importing-nzis-surf/).  Because it used a logarithmic scale to improve readability, in effect all zero and negative values are excluded from the data.  Whenever I throw out data, my tail goes bushy... there has to be a better way.  Those zero and negative values are an important part of the story, and it's too easy to forget them.  If you don't include them in your standard graphic for exploring the distribution of the data, the next thing is you're excluding them from your model in serious analysis.

It turns out this is a very common problem, and the routine solutions (at least according to this [SAS blog](http://blogs.sas.com/content/iml/2011/04/27/log-transformations-how-to-handle-negative-data-values.html), and this [Q&A on another blog](http://andrewgelman.com/2005/11/10/transforming_va/)) seem to be either adding an arbitrary constant before taking logarithms, excluding those data points and treating them as missing values, or discretizing the data altogether.  If my tail was bushy before, now I'm really nervous - those sound like really dangerous things to do (and yes, I freely admit having done all three myself plenty of times).

A bit of reflection shows that what's going on here is a mixture of distributions - one of the most dangerous things for statistics based on traditional assumptions:

* A large bunch of the population, which we sometimes mistake for the whole population, who earn positive incomes that are roughly log-normally distributed, with mean and variance depending on structural variables like education, location, etc.
* A small subset, probably of enterpreneurs, who are making losses.  The losses have their own distribution of interest.
* A medium sized subset who are not in work or owning businesses and have zero income

It's folly to think that these three groups in the data-generating process will result in a single smooth distribution, as is implied by adding a constant to the income and taking logarithms.  It's worse to chuck out the inconvenient second and third groups altogether.

My solution is to use a modified power transform - one that transforms the absolute value of the original data, then restores the sign (negative or positive) to the result.  I started by thinking of square roots, and I needed to handle the fact that the square root of a negative number is imaginary.  By applying the square root to the absolute value and then restoring the sign you get a transformation like this:

![plot](/img/0004_transformation.svg)

Produced by (in R):



{% highlight R lineanchors %}
power_trans <- function(x, k = 0.5){
   sign(x) * abs(x) ^ k
}

sim <- -5000:5000

plot(sim, power_trans(sim), type = "l", bty = "l",
     xlab = "Original value", ylab = "Transformed value")
grid()

{% endhighlight %}
I've given myself flexibility to use powers other than 0.5 (square root).  When I apply this approach to the New Zealand Income Survey 2011 simulated unit record file provided by Statistics New Zealand, I get a much more satisfactory representation of the full range of incomes there.  Not only are the two modes at $825 and $325 per week visible that I discussed in my last post, but we can compare them to the spike at $0 per week, and observe a little bump at the mode negative income level of -$200.

![plot](/img/0004_better_density_plot.svg)

I haven't heard of this approach being used before and would welcome comment on it.  As taking logarithms of income data is extremely common - pretty much the standard approach in fact - I would suggest that analysts are routinely losing sight of some important complexity in their data.

Here's the final code producing that plot.  It depends on the data being present in a database, as I described in the previous post.

{% highlight R lineanchors %}
library(RODBC)
library(ggplot2)
library(scales)
library(showtext)
library(dplyr)

# comnect to database
PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")

# load fonts
font.add.google("Poppins", "myfont")
showtext.auto()




# define the power to use in the transformation
k <- 0.2

# download the data from the database
inc <- sqlQuery(PlayPen, "select income from f_mainheader") %>%
   mutate(inc_trans = power_trans(income, k))

# define where to put the gridlines and labels on the x axis
breaks <- data.frame(labels = c(-5000, -1500, -200, -10, 0, 10, 345, 825, 5000) ) %>%
   mutate(points = power_trans(labels, k))

inc %>%
   ggplot(aes(x = inc_trans)) +
   geom_density() +
   scale_x_continuous(breaks = breaks$points, labels = breaks$labels) +
   theme_minimal(base_family = "myfont") +
   labs(x= "\nWeekly income (transformed scale, $)",
        y = "Density\n",
        title = "Distribution of individual income in New Zealand 2011
showing the full range including zero and negative")

{% endhighlight %}