---
layout: post
title: Bootstrap and cross-validation for evaluating modelling strategies
date: 2016-06-05
tag: 
   - NZIS2011
   - RobustMethods
   - ModellingStrategy
   - R
description: I compare 'simple' bootstrap, 'enhanced' (optimism-correcting) bootstrap, and repeated k-fold cross-validation as methods for estimating fit of three example modelling strategies.
image: /img/0043-boot-results.svg
socialimage: http://ellisp.github.io/img/0043-boot-results.png
category: R
---

## Modelling strategies
I've been re-reading Frank Harrell's [Regression Modelling Strategies](http://www.springer.com/us/book/9781441929181), a must read for anyone who ever fits a regression model, although be prepared - depending on your background, you might get 30 pages in and suddenly become convinced you've been doing nearly everything wrong before, which can be disturbing.  

I wanted to evaluate three simple modelling strategies in dealing with data with many variables.  Using data with 54 variables on 1,785 area units from New Zealand's 2013 census, I'm looking to predict median income on the basis of the other 53 variables.  The features are all continuous and are variables like "mean number of bedrooms", "proportion of individuals with no religion" and "proportion of individuals who are smokers".   Restricting myself to traditional linear regression with a normally distributed response, my three alternative strategies were:

* use all 53 variables;
* eliminate the variables that can be predicted easily from the other variables (defined by having a [variance inflation factor](https://en.wikipedia.org/wiki/Variance_inflation_factor) greater than ten), one by one until the main collinearity problems are gone; or
* eliminate variables one at a time from the full model on the basis of comparing [Akaike's Information Criterion](https://en.wikipedia.org/wiki/Akaike_information_criterion) of models with and without each variable.

None of these is exactly what I would use for real, but they serve the purpose of setting up a competition of strategies that I can test with a variety of model validation techniques.

## Validating models
The main purpose of the exercise was actually to ensure I had my head around different ways of estimating the validity of a model, loosely definable as how well it would perform at predicting new data.  As there is no possibility of new areas in New Zealand from 2013 that need to have their income predicted, the "prediction" is a thought-exercise which we need to find a plausible way of simulating.  Confidence in hypothetical predictions gives us confidence in the insights the model gives into relationships between variables.  

There are many methods of validating models, although I think k-fold cross-validation has market dominance (not with Harrell though, who prefers varieties of the bootstrap).  The three validation methods I've used for this post are:

1. 'simple' bootstrap.  This involves creating resamples with replacement from the original data, of the same size; applying the modelling strategy to the resample; using the model to predict the values of the full set of original data and calculating a goodness of fit statistic (eg either R-squared or root mean squared error) comparing the predicted value to the actual value.  *Note - Following Efron, Harrell calls this the "simple bootstrap", but other authors and the useful `caret` package use "simple bootstrap" to mean the resample model is used to predict the out-of-bag values at each resample point, rather than the full original sample.*
2. 'enhanced' bootstrap.  This is a little more involved and is basically a method of estimating the 'optimism' of the goodness of fit statistic.  There's a nice step by step explanation by [thestatsgeek](http://thestatsgeek.com/2014/10/04/adjusting-for-optimismoverfitting-in-measures-of-predictive-ability-using-bootstrapping/) which I won't try to improve on.
3. repeated 10-fold cross-validation.  10-fold cross-validation involves dividing your data into ten parts, then taking turns to fit the model on 90% of the data and using that model to predict the remaining 10%.  The average of the 10 goodness of fit statistics becomes your estimate of the actual goodness of fit.  One of the problems with k-fold cross-validation is that it has a high variance ie doing it different times you get different results based on the luck of you k-way split; so repeated k-fold cross-validation addresses this by performing the whole process a number of times and taking the average.

As the sample sizes get bigger relative to the number of variables in the model the methods should converge.  The bootstrap methods can give over-optimistic estimates of model validity compared to cross-validation; there are various other methods available to address this issue although none seem to me to provide all-purpose solution.

It's critical that the re-sampling in the process envelopes the entire model-building strategy, not just the final fit.  In particular, if the strategy involves variable selection (as two of my candidate strategies do), you have to automate that selection process and run it on each different resample.  That's because one of the highest risk parts of the modelling process is that variable selection.  Running cross-validation or the bootstrap on a final model after you've eliminated a bunch of variables is missing the point, and will give materially misleading statistics (biased towards things being more "significant" than there really is evidence for).  Of course, that doesn't stop this being common misguided practice.

## Results

One nice feature of statistics since the revolution of the 1980s is that the bootstrap helps you conceptualise what might have happened but didn't.  Here's the root mean squared error from the 100 different bootstrap resamples when the three different modelling strategies (including variable selection) were applied:

![boot-results](/img/0043-boot-results.svg)

Notice anything?  Not only does it seem to be generally a bad idea to drop variables just because they are collinear with others, but occasionally it turns out to be a *really* bad idea - like in resamples #4, #6  and around thirty others.  Those thirty or so spikes are in resamples where random chance led to one of the more important variables being dumped before it had a chance to contribute to the model.

The thing that surprised me here was that the generally maligned step-wise selection strategy performed nearly as well as the full model, judged by the simple bootstrap.  That result comes through for the other two validation methods as well:

![boot-v-cv](/img/0043-boot-v-cv.svg)

In all three validation methods there's really nothing substantive to choose between the "full model" and "stepwise" strategies, based purely on results.  

## Reflections
The full model is much easier to fit, interpret, estimate confidence intervals and perform tests on than stepwise.  All the standard statistics for a final model chosen by stepwise methods are misleading and careful recalculations are needed based on elaborate bootstrapping.  So the full model wins hands-down as a general strategy in this case.

With this data, we have a bit of freedom from the generous sample size.  If approaching this for real I wouldn't eliminate any variables unless there were theoretical / subject matter reasons to do so.  I have made the mistake of eliminating the co-linear variables before from this dataset but will try not to do it again.  The rule of thumb is to have 20 observations for each parameter (this is one of the most asked and most dodged questions in statistics education; see Table 4.1 of *Regression Modelling Strategies* for this particular answer), which suggests we can have up to 80 parameters with a bit to spare.  This gives us 30 parameters to use for non-linear relationships and/or interactions, which is the direction I might go in a subsequent post.  Bearing that in mind, I'm not bothering to report here the actual substantive results (eg which factors are related to income and how); that can wait for a better model down the track.

## Data and computing

The census data are ultimately from Statistics New Zealand of course, but are tidied up and available in my [`nzelect`](https://github.com/ellisp/nzelect) R package, which is still very much under development and may change without notice.  It's only available from GitHub at the moment (installation code below).

I do the bootstrapping with the aid of the `boot` package, which is generally the recommended approach in R.  For repeated cross-validation of the two straightforward strategies (full model and stepwise variable selection) I use the `caret` package, in combination with `stepAIC` which is in the Venables and Ripley `MASS` package.  For the more complex strategy that involved dropping variables with high variance inflation factors I found it easiest to do the repeated cross-validation old-school with my own `for` loops.

This exercise was a bit complex and I won't be astonished if someone points out an error.  If you see a problem, or have any suggestions or questions, please leave a comment.

Here's the code:

{% highlight R lang lineanchors %} 
#===================setup=======================
library(ggplot2)
library(scales)
library(MASS)
library(boot)
library(caret)
library(dplyr)
library(tidyr)
library(directlabels)

set.seed(123)

# install nzelect package that has census data
devtools::install_github("ellisp/nzelect/pkg")
library(nzelect)

# drop the columns with areas' code and name
au <- AreaUnits2013 %>%
   select(-AU2014, -Area_Code_and_Description)

# give meaningful rownames, helpful for some diagnostic plots later
row.names(au) <- AreaUnits2013$Area_Code_and_Description

# remove some repetition from the variable names
names(au) <- gsub("2013", "", names(au))

# restrict to areas with no missing data.  If this was any more complicated (eg
# imputation),it would need to be part of the validation resampling too; but
# just dropping them all at the beginning doesn't need to be resampled; the only
# implication would be sample size which would be small impact and complicating.
au <- au[complete.cases(au), ]

#==================functions for two of the modelling strategies=====================
# The stepwise variable selection:
model_process_step <- function(the_data){
   model_full <- lm(MedianIncome ~ ., data = the_data)
   model_final <- stepAIC(model_full, direction = "both", trace = 0)
   return(model_final)
}

# The dropping of highly collinear variables, based on Variance Inflation Factor:
model_process_vif <- function(the_data){
   # remove the collinear variables based on vif
   x <- 20
   
   while(max(x) > 10){
      mod1 <- lm(MedianIncome ~ . , data = the_data)
      x <- sort(car::vif(mod1) , decreasing = TRUE)
      the_data <- the_data[ , names(the_data) != names(x)[1]]
      # message(paste("dropping", names(x)[1]))
   }
   
   model_vif <- lm(MedianIncome ~ ., data = the_data)
   return(model_vif)
}

# The third strategy, full model, is only a one-liner with standard functions
# so I don't need to define a function separately for it.

#==================Different validation methods=================

#------------------simple bootstrap comparison-------------------------
# create a function suitable for boot that will return the goodness of fit
# statistics testing models against the full original sample.
compare <- function(orig_data, i){
   # create the resampled data
   train_data <- orig_data[i, ]
   test_data <- orig_data # ie the full original sample
   
   # fit the three modelling processes
   model_step <- model_process_step(train_data)
   model_vif  <- model_process_vif(train_data)
   model_full <- lm(MedianIncome ~ ., data = train_data)
   
   # predict the values on the original, unresampled data
   predict_step <- predict(model_step, newdata = test_data)
   predict_vif  <- predict(model_vif, newdata = test_data)
   predict_full  <- predict(model_full, newdata = test_data)
   
   # return a vector of 6 summary results
   results <- c(
      step_R2 = R2(predict_step, test_data$MedianIncome),
      vif_R2  = R2(predict_vif, test_data$MedianIncome),
      full_R2  = R2(predict_full, test_data$MedianIncome),
      step_RMSE = RMSE(predict_step, test_data$MedianIncome),
      vif_RMSE  = RMSE(predict_vif, test_data$MedianIncome),
      full_RMSE  = RMSE(predict_full, test_data$MedianIncome)
   )
   return(results)
}

# perform bootstrap
Repeats <- 100
res <- boot(au, statistic = compare, R = Repeats)

# restructure results for a graphic showing root mean square error, and for
# later combination with the other results.  I chose just to focus on RMSE;
# the messages are similar if R squared is used.
RMSE_res <- as.data.frame(res$t[ , 4:6])
names(RMSE_res) <- c("AIC stepwise selection", "Remove collinear variables", "Use all variables")

RMSE_res %>%
   mutate(trial = 1:Repeats) %>%
   gather(variable, value, -trial) %>% 
   # re-order levels:
   mutate(variable = factor(variable, levels = c(
      "Remove collinear variables", "AIC stepwise selection", "Use all variables"
   ))) %>%
   ggplot(aes(x = trial, y = value, colour = variable)) +
   geom_line() +
   geom_point() +
   ggtitle("'Simple' bootstrap of model fit of three different regression strategies",
           subtitle = "Predicting areas' median income based on census variables") +
   labs(x = "Resample id (there no meaning in the order of resamples)\n",
        y = "Root Mean Square Error (higher is worse)\n",
        colour = "Strategy",
        caption = "Data from New Zealand Census 2013")

# store the three "simple bootstrap" RMSE results for later
simple <- apply(RMSE_res, 2, mean)

#-----------------------enhanced (optimism) bootstrap comparison-------------------
# for convenience, estimate the models on the original sample of data
orig_step <- model_process_step(au)
orig_vif <- model_process_vif(au)
orig_full <- lm(MedianIncome ~ ., data = au)

# create a function suitable for boot that will return the optimism estimates for
# statistics testing models against the full original sample.
compare_opt <- function(orig_data, i){
   # create the resampled data
   train_data <- orig_data[i, ]

   # fit the three modelling processes
   model_step <- model_process_step(train_data)
   model_vif  <- model_process_vif(train_data)
   model_full <- lm(MedianIncome ~ ., data = train_data)
   
   # predict the values on the original, unresampled data
   predict_step <- predict(model_step, newdata = orig_data)
   predict_vif  <- predict(model_vif, newdata = orig_data)
   predict_full  <- predict(model_full, newdata = orig_data)
   
   # return a vector of 6 summary optimism results
   results <- c(
      step_R2 = R2(fitted(model_step), train_data$MedianIncome) - R2(predict_step, orig_data$MedianIncome),
      vif_R2  = R2(fitted(model_vif), train_data$MedianIncome) - R2(predict_vif, orig_data$MedianIncome),
      full_R2  = R2(fitted(model_full), train_data$MedianIncome) - R2(predict_full, orig_data$MedianIncome),
      step_RMSE = RMSE(fitted(model_step), train_data$MedianIncome) - RMSE(predict_step, orig_data$MedianIncome),
      vif_RMSE  = RMSE(fitted(model_vif), train_data$MedianIncome) - RMSE(predict_vif, orig_data$MedianIncome),
      full_RMSE  = RMSE(fitted(model_full), train_data$MedianIncome) - RMSE(predict_full, orig_data$MedianIncome)
   )
   return(results)
}

# perform bootstrap
res_opt <- boot(au, statistic = compare_opt, R = Repeats)

# calculate and store the results for later
original <- c(
   RMSE(fitted(orig_step), au$MedianIncome), 
   RMSE(fitted(orig_vif), au$MedianIncome),
   RMSE(fitted(orig_full), au$MedianIncome)
)

optimism <- apply(res_opt$t[ , 4:6], 2, mean)
enhanced <- original - optimism


#------------------repeated cross-validation------------------
# The number of cross validation repeats is the number of bootstrap repeats / 10:
cv_repeat_num <- Repeats / 10

# use caret::train for the two standard models:
the_control <- trainControl(method = "repeatedcv", number = 10, repeats = cv_repeat_num)
cv_full <- train(MedianIncome ~ ., data = au, method = "lm", trControl = the_control)
cv_step <- train(MedianIncome ~ ., data = au, method = "lmStepAIC", trControl = the_control, trace = 0)

# do it by hand for the VIF model:
results <- numeric(10 * cv_repeat_num)
for(j in 0:(cv_repeat_num - 1)){
   cv_group <- sample(1:10, nrow(au), replace = TRUE)
   for(i in 1:10){
      train_data <- au[cv_group != i, ]
      test_data <- au[cv_group == i, ]
      results[j * 10 + i] <- RMSE(
         predict(model_process_vif(train_data), newdata = test_data),
         test_data$MedianIncome)
   }
}
cv_vif <- mean(results)

cv_vif_results <- data.frame(
   results = results,
   trial = rep(1:10, cv_repeat_num),
   cv_repeat = rep(1:cv_repeat_num, each = 10)
)


#===============reporting results===============
# combine the three cross-validation results together and combined with
# the bootstrap results from earlier
summary_results <- data.frame(rbind(
   simple, 
   enhanced,
   c(mean(cv_step$resample$RMSE), 
     cv_vif,
     mean(cv_full$resample$RMSE)
     )
   ), check.names = FALSE) %>%
   mutate(method = c("Simple bootstrap", "Enhanced bootstrap", 
                     paste(cv_repeat_num, "repeats 10-fold\ncross-validation"))) %>%
   gather(variable, value, -method)

# Draw a plot summarising the results
direct.label(
summary_results %>%
   mutate(variable = factor(variable, levels = c(
      "Use all variables", "AIC stepwise selection", "Remove collinear variables"
   ))) %>%
   ggplot(aes(y = method, x = value, colour = variable)) +
   geom_point(size = 3) +
   labs(x = "Estimated Root Mean Square Error (higher is worse)\n",
        colour = "Modelling\nstrategy",
        y = "Method of estimating model fit\n",
        caption = "Data from New Zealand Census 2013") +
   ggtitle("Three different validation methods of three different regression strategies",
           subtitle = "Predicting areas' median income based on census variables")
   
)
{% endhighlight %} 