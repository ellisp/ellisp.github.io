---
layout: post
title: Actual coverage of confidence intervals for standard deviation
date: 2016-05-29
tag: 
   - NZIS2011
   - RobustMethods
   - R
description: The success rate (proportion of times the true value is covered by the interval) of 95% confidence intervals from the bootstrap when estimating population standard deviation can be very poor for complex mixed distributions, such as real world weekly income from a modest sample size (<10,000).

image: /img/0042-sd-ci-coverage.svg
socialimage: http://ellisp.github.io/img/0042-sd-ci-coverage.png
category: R
---

## Overview
How big a sample size would you think you need to get a reliable 95% confidence interval (ie one that really does contain the true value 95% of the time) for a single univariate statistic like standard deviation? 30? 50?  Turns out the answer is more like 10,000 for some not-particularly-extreme real-world data.

In this post I explore the phenomenon shown in the first chart below; lower than hoped-for coverage of 95% confidence intervals calculated with the bootstrap when estimating a population standard deviation from a modest sample size.  

![coverage](/img/0042-sd-ci-coverage.svg)

The test is admittedly a tough one, albeit realistically real world.  The population data from which my samples are drawn are the [simulated unit record file microdata](http://www.stats.govt.nz/tools_and_services/microdata-access/nzis-2011-cart-surf.aspx) from Statistics New Zealand's New Zealand Income Survey 2011, which I've written about in [numerous posts](/blog/index_by_tag.html).  The distribution of weekly income in this data set is complex; it could perhaps be described as a mixture of: 

* individuals with positive income that is approximately log-normal distributed; 
* individuals with negative income of some difficult-to-describe distribution; 
* a large spike of individuals with zero income; plus a sprinkling of outliers.  

In fact, it looks like this:

![fulldata](/img/0042-full-data.svg)

The actual standard deviation of all 29,471 observations of income is $806.

The inference problem is how to estimate that value if you have only a smaller sample, eg 50?  Because if you have only 50 observations, you think the data looks like this:

![sampledata](/img/0042-sample-data.svg)

In the case of this particular sample of 50, the standard deviation is $527.

## Unbiased estimate of standard deviation
The first challenge - although it turns out to be a relatively small detail in terms of the challenge - is to get the best estimator for the situation when you want to estimate a population's standard deviation from a sample.  While an unbiased estimator of variance is well known, standard deviation (the square root of variance) is complex.  Creating an [unbiased estimator of standard deviation](https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation) turns out to depend on the vagaries of the shape of the original population, with the *fourth* moment (expected value of the variable to the power of four) playing a role.

A reasonably generally well-performing estimator for non-Normal variables is reportedly (image from the Wikipedia article linked to above):

![equation](/img/0042-unbiased-sd.png)

## Method
Using the unbiased estimate of standard deviation above, I set out to test the performance of bootstrap confidence intervals in covering the true value of population standard deviation for sample sizes of 50, 100, 200, 400, ..., 12800.  I took 200 samples of the data for each of these sample sizes; estimated the standard deviation from the sample; and use the bootstrap to estimate confidence intervals for population standard deviation from each sample, with 999 bootstrap replicates.  I tested both the [basic and the percentile methods for bootstrap confidence intervals](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)#Methods_for_bootstrap_confidence_intervals).

## Results - income
The unbiased estimator works ok, at least in terms of bias.  Here's the full distribution of the estimated standard deviation from all those different samples:

![sd-bias-full](/img/0042-sd-bias-full.svg)

For the smaller samples, it's obvious that most of the estimates are to the left of the blue line ie the estimated standard deviation is less than the true standard deviation from the full 29,471 observations.  However, this frequent underestimation is compensated for by the fact that when the standard deviation is *over*-estimated, it tends to be by quite a lot - with a strong rightwards skew in the distribution of this statistic for all the smaller sample sizes.  This all sounds bad of course, but it is the sort of thing that's needed if a statistic with a skewed sampling distribution is going to be on average unbiased.  We see how this plays out when we look at the actual average error of the estimator:

![sd-bias-summ](/img/0042-sd-bias-summary.svg)

There's a slight bias to under-estimation for smaller sample sizes, but by the time sample size is 200 or higher the estimator on average gets it pretty much right.

It's the coverage of the bootstrapped confidence intervals that is disappointing however.  Here's a repeat of that graphic I started the post with:
![coverage](/img/0042-sd-ci-coverage.svg)

Basically, when you have a really complex mixed and dirty variable like this one (New Zealand weekly income from survey data), not until you have a sample size 10,000 do your bootstrapped confidence intervals start to have the coverage that is planned for them.  

What's going on here?  Well, the bootstrap depends on you taking many re-samples with replacements from your original sample, in the hope that doing this mimics the behaviour of taking many samples from the whole population which isn't possible for reasons of expense or logistics.  For this to work, the original sample has to sufficiently resemble the whole population that the resamples from the sample behave similarly to samples from the population.  There's no straightforward definition of "sufficiently resemble" that I know of, but what we're seeing here is that until the sample size is really quite moderately large, for a statistic like standard deviation for a moderately complex population distribution, the sample does not "sufficiently resemble" the population for the bootstrap to work as advertised.

## Results - other variables

To get more of a feel for what was going on here, I ran the same computer program over more regular, simulated data:

* a standard normal distribution (mean zero, variance one)
* a standard uniform distribution (bounded by zero and one)
* a standard log-normal distribution (e to the power of standard normal)
* a mixture of all three

The results in terms of confidence interval coverage can be seen in this graphic:

![other-dists](/img/0042-other-dists.svg)

Points to make from this are:

* In all cases the "basic" and "percentile" methods perform similarly, with neither of the two consistently outperforming the other
* For the two symmetrical distributions - uniform and normal - the coverage at small sample sizes isn't bad.  Even with only 50 observations in the sample, more than 90% of the confidence intervals contain the true value, and the sample size doesn't need to be much larger than that before it gets up to the desired 95%.
* In fact, for the two symmetrical distributions the confidence intervals at larger sample sizes consistently over-perform, which suggests that narrower confidence intervals could be reported and still contain the true value 95% of the time.
* In contrast, for the non-symmetrical, right-skewed log-normal data and the mixture distribution, the success rate of 95% confidence intervals actually containing the true value is relatively low until the sample size is many thousands; although perform better than the real-world income data used earlier.

Mixed or "contaminated" distributions are known to cause problems for statistical inference (although I don't like the word "contaminated" as too value-laden - as though it is the data's fault it doesn't live up to twentieth century simplifying assumptions!).  The bootstrap was part of the late twentieth century revolution in applied statistical methods to help address those and other problems, but the observations in this post confirm that it doesn't magically make them go away.

> "Classic, routinely used statistical methods are based on the assumptions that observations follow a normal curve.  It was once thought that violating the normality assumption rarely had a detrimental impact on these methods, but theoretical and empirical advances have made it clear that general types of non-normality cause serious practical problems in a wide range of commonly occurring situations.  Indeed, even very slight departures from normality can be a source of concern." 

Wilcox, [*Modern Statistics for the Social and Behavioral Sciences*](http://www.amazon.com/Modern-Statistics-Social-Behavioral-Sciences/dp/1439834563)


## Simpler statistics

Finally, to round out the picture I tried the same approach on statistics that are simpler than standard deviation - mean and trimmed mean - on the same awkward weekly income data.

The confidence interval coverage is pretty satisfactory even with small sample sizes:
![ci-means](/img/0042-mean-ci-coverage.svg)

And as expected from theoretical results, there's no discernable bias:
![bias-means](/img/0042-mean-bias.svg)

## Code
Here's the computer programs, all in R.

First I load up the packages I'm using.  This is fairly straightforward.  The `moments` package is needed because I need to estimate excess kurtosis as part of the formula for an unbiased estimate of standard deviation
{% highlight R lang lineanchors %} 
#===================setup=======================
library(ggplot2) # using the dev version from GitHub so can have subtitles and captions
library(scales)
library(boot)
library(dplyr)
library(moments) # for kurtosis()
library(tidyr)
library(gridExtra) # for grid.arrange()

# themes for graphics
theme_set(theme_light(10) )
theme_small <- theme_light(8) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

{% endhighlight %}    

Next I create two functions.  One is unbiased standard deviation of a vector x, to be scrambled/resampled by means of the index i - the sort of function that is accepted by the `boot` function as a statistic:
{% highlight R lang lineanchors %} 
#==========Functions==================
sampsd <- function(x, i, xbar = mean(x)){
   # function suitable for boot which returns estimated standard deviation
   # from a sample x that has been scrambled by the index i
   d <- x[i]
   n <- length(d)
   
   # this next formula is the best general approximation to a general  
   # unbiased estimator of standard deviation from a non-normal distribution
   # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
   unbiased_sd <- sqrt(
      sum((d - xbar) ^ 2) / 
         (n - 1.5 - (kurtosis(d) - 3) / 4)
   )
   return(unbiased_sd)
}
{% endhighlight %}   

Second is the main routine that performs the actual testing of the confidence interval.  This is little involved so I won't try to explain it line by line.  Hopefully the in-function comments are adequate:
{% highlight R lang lineanchors %} 
test_boot_ci <- function(full_data, 
                         statistic, 
                         truevalue = statistic(full_data, i = 1:length(full_data)),
                         reps = 50, 
                         R = 499, 
                         title = match.call()){

   # Function to explore the coverage of basic and percentile bootstrap 
   # 95% confidence intervals.
   #' @full_data A full population of data from which samples of various sizes
   #           will be drawn without replacement, and each of those samples will
   #           be analysed with the bootstrap as though it were all that is
   #           available.
   #' @statistic a function suitable for passing to the statistic argument of boot
   #           ie first argument is data, second argument is an index for 
   #           scrambling the data
   #' @truevalue true value that the statistic is trying to esimate.
   #' @reps number of repeats of each sample size to analyse
   #' @R number of bootstrap resamples for each sample
   #' @title Title for plots
   
   
   # create a data frame to hold the results, with "reps" rows for each
   # sample size from 50, 100, 200, ..., 12800
   ns <- rep(2 ^ (0:8) * 50, each = reps)
   results <- data.frame(
      n = ns,
      point_est = numeric(length(ns)),
      ci_basic_correct = logical(length(ns)),
      ci_perc_correct = logical(length(ns))
   )
      
   
   # Create point estimates and two types of bootstrap confidence interval
   # for each repetition at each value of sample size:
   for(i in 1:nrow(results)){
   
      this_data <- full_data[sample(1:length(full_data), results[i, "n"])]   
      res <- boot(this_data, statistic = statistic, R = R)
      
      results[i, "point_est"] <- res$t0
      
      ciobj <- boot.ci(res, type = "basic")
      citest <- (truevalue > ciobj$basic[4] & truevalue < ciobj$basic[5])
      results[i, "ci_basic_correct"] <- citest
      
      ciobj <- boot.ci(res, type = "perc")
      citest <- (truevalue > ciobj$perc[4] & truevalue < ciobj$perc[5])
      results[i, "ci_perc_correct"] <- citest
      
   }
   
   # Prepare summary table and plots for output
   tab <- results %>%
      group_by(n) %>%
      summarise(coverage_basic = round(sum(ci_basic_correct) / length(ci_basic_correct), 3),
                coverage_perc =  round(sum(ci_perc_correct) / length(ci_perc_correct), 3),
                bias = round((mean(point_est) - truevalue) / truevalue, 3))
   
   p1 <- tab %>%
      dplyr::select(-bias) %>%
      gather(Method, value, -n) %>%
      mutate(Method = gsub("coverage_perc", "Percentile", Method),
             Method = gsub("coverage_basic", "Basic", Method),
             Method = factor(Method, levels = c("Percentile", "Basic"))) %>%
      ggplot(aes(x = n, y = value, color = Method)) +
      geom_hline(yintercept = 0.95, colour = "blue") +
      geom_point() +
      geom_line() +
      scale_y_continuous("Actual coverage of 95% confidence interval", 
                         label = percent) +
      scale_x_sqrt("Sample size", label = comma, breaks = unique(ns)) +
      labs(colour = "Bootstrap\nmethod") +
      ggtitle(title)
   
   p2 <- tab %>%
      ggplot(aes(x = n, y = bias)) +
      geom_hline(yintercept = 0, colour = "blue") +
      geom_point() +
      geom_line() +
      scale_y_continuous("Bias (as percentage of true value)", 
                         label = percent) +
      scale_x_sqrt("Sample size", label = comma, breaks = unique(ns)) +
      ggtitle(title)
   
   p3 <- results %>%
      mutate(n = paste("n =", n),
             n = factor(n, levels = paste("n =", unique(ns) ) ) ) %>%
      ggplot(aes(x =point_est)) +
      geom_density(fill = "grey75", alpha = 0.5) +
      geom_rug() +
      facet_wrap(~n, scales = "free_y") +
      geom_vline(xintercept = truevalue, colour = "blue") +
      ggtitle(title) +
      labs(x = "Point estimates (blue line shows true value)")
   
   return(list(tab = tab, p1 = p1, p2 = p2, p3 = p3))
   
}
{% endhighlight %}   

Next I load in the data, and apply my testing program to the income data and to the various simulated datasets needed for the blog.

Note that with the settings in the code below, nearly two million sets of data are analysed for each call to `test_boot_ci()`, so it takes several hours to run the whole thing.  The `reps_per_sample_size` and `reps_per_bootstrap` variables should be set to much lower values (eg 20 and 99) if you just want to give this a go, or to adapt it.
{% highlight R lang lineanchors %} 
if(!exists("nzis")){
   nzis <- read.csv("http://www.stats.govt.nz/~/media/Statistics/services/microdata-access/nzis11-cart-surf/nzis11-cart-surf.csv")   
}


reps_per_sample_size <- 200
reps_per_bootstrap <- 999

#------------standard deviation of income data------------
income_sd <- test_boot_ci(full_data = nzis$income, 
                            statistic = sampsd, 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "Estimating standard deviation from income data")

#---------standard deviation of simulated data--------------
normal_sd <- test_boot_ci(full_data = rnorm(300000), 
                          statistic = sampsd, 
                          reps = reps_per_sample_size, 
                          R = reps_per_bootstrap,
                          title = "Estimating standard deviation from simulated Normal data")

unif_sd <- test_boot_ci(full_data = runif(300000), 
                        statistic = sampsd, 
                        reps = reps_per_sample_size, 
                        R = reps_per_bootstrap,
                        title = "Estimating standard deviation from simulated uniform data")

lognormal_sd <- test_boot_ci(full_data = exp(rnorm(300000)), 
                             reps = reps_per_sample_size, 
                             statistic = sampsd, 
                             R = reps_per_bootstrap,
                             title = "Estimating standard deviation from simulated log-normal data")


mixture <- c(exp(rnorm(100000)), rnorm(100000), runif(100000))
mixture_sd <- test_boot_ci(full_data = mixture, 
                             reps = reps_per_sample_size, 
                             statistic = sampsd, 
                             R = reps_per_bootstrap,
                             title = "Estimating standard deviation from simulated mixture data")


#-------simpler statistics on income data-----------
income_mean <- test_boot_ci(full_data = nzis$income,
                            statistic = function(x, i){mean(x[i])}, 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "Estimating mean from income data")

income_trmean <- test_boot_ci(full_data = nzis$income,
                              statistic = function(x, i){mean(x[i], tr = 0.2)}, 
                              reps = reps_per_sample_size, 
                              R = reps_per_bootstrap,
                              title = "Estimating trimmed mean from income data")
                             
{% endhighlight %}   

Finally, I present the results in the graphics used in this post:
{% highlight R lang lineanchors %} 
#------------Density of income data---------------
ggplot(nzis, aes(x = income)) +
   geom_density(fill = "grey75", alpha = 0.5) +
   geom_rug() +
   scale_x_continuous("Weekly income", label = dollar) +
   ggtitle("New Zealand Income Survey 2011", subtitle = "Full data")

set.seed(123)
ggplot(nzis[sample(1:nrow(nzis), 50), ], aes(x = income)) +
   geom_density(fill = "grey75", alpha = 0.5) +
   geom_rug() +
   scale_x_continuous("Weekly income", label = dollar) +
   ggtitle("New Zealand Income Survey 2011", subtitle = "Subsample of 50")

#-----------standard deviation of income--------------
income_sd$p1  + 
   labs(caption = "Source: New Zealand Income Survey 2011, Statistics New Zealand"))

income_sd$p2

income_sd$p3

#----------------other distributions--------------
grid.arrange(
   normal_sd$p1 + theme_light(8),
   unif_sd$p1 + theme_light(8),
   lognormal_sd$p1 + theme_light(8),
   mixture_sd$p1 + theme_light(8)
)

#----------------mean and trimmed mean-----------------
grid.arrange(
   income_mean$p1 + 
      theme_light(8, base_family = "myfont") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
   income_trmean$p1 + 
      theme_light(8, base_family = "myfont") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)),
      ncol = 2
)


grid.arrange(
   income_mean$p2 + 
      theme_light(8, base_family = "myfont") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(c(-0.02, 0.02)),
   income_trmean$p2 + 
      theme_light(8, base_family = "myfont") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(c(-0.02, 0.02)),
   ncol = 2
)



{% endhighlight %} 