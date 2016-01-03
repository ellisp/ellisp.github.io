---
layout: post
title: Filling in the gaps - highly granular estimates of income and population for New Zealand from survey data
date: 2016-01-05
tag: 
   - NewZealand
   - OpenData
   - Shiny
   - MachineLearning
   - Animations
   - NZIS2011
   - Distributions
   - R
description: Commuting patterns between districts and cities in New Zealand are used to illustrate static (for printing) and interactive (for the web) network charts with R and D3.
image: /img/0026-rf.gif
socialimage: http://ellisp.github.io/img/0026-rf.gif
category: R
---
<style>
               #scaled-frame { width: 900px; height: 880px; border: 0px; }
               #scaled-frame {
               zoom: 0.75;
               -moz-transform: scale(0.75);
               -moz-transform-origin: 0 0;
               -o-transform: scale(0.75);
               -o-transform-origin: 0 0;
               -webkit-transform: scale(0.75);
               -webkit-transform-origin: 0 0;
               overflow: hidden;
               }

               @media screen and (-webkit-min-device-pixel-ratio:0) {
               #scaled-frame  { zoom: 1;  }
               }
</style>

## Individual-level estimates from survey data

I was motivated by web apps like the British Office of National Statistics' [How well do you know your area?](http://www.ons.gov.uk/ons/dcp14298_372374.xml) and [How well does your job pay?](http://www.ons.gov.uk/ons/dcp14298_386103.xml) to see if I could turn the New Zealand Income Survey into an individual-oriented estimate of income given age group, qualification, occupation, ethnicity, region and hours worked.  My tentative go at this is embedded below, and there's also a [full screen version](https://ellisp.shinyapps.io/NZIS) available.
<div style="height: 680px">
<iframe id="scaled-frame" width="775" src="https://ellisp.shinyapps.io/NZIS" style = "overflow-y: hidden;"></iframe>
</div>
The job's a tricky one because the survey data available doesn't go to anywhere near that level of granularity.  It could be done with census data of course, but any such effort to publish would come up against confidentiality problems - there are just too few people in any particular combination of category to release real data there.  So some kind of modelling is required that can smooth over the actual data but still give a plausible and realistic estimate.

I also wanted to emphasise the *distribution* of income, not just a single measure like mean or median - something I think that we statisticians should do much more than we do, with all sorts of variables.  And in particular I wanted to find a good way of dealing with the significant number of people in many categories (particularly but not only "no occupation") who have zero income; and also the people who have negative income in any given week.

My data source is the New Zealand Income Survey 2011 simulated record file published by Statistics New Zealand.  An [earlier post](http://ellisp.github.io/blog/2015/08/15/importing-nzis-surf/) by me describes how I accessed this, normalised it and put it into a database.  I've also written several posts about dealing with the tricky distribution of individual incomes, listed [here](http://ellisp.github.io/blog/index_by_tag.html) under the "NZIS2011" heading.

This is a longer post than usual, with a digression into the use of Random Forests (tm) to predict continuous variables, an attempt at producing a much more polished plot of a regression tree than usually available, and some reflections on strengths and weakness of several different approaches to estimating distributions.

## Data import and shape
I begin by setting up the environment and importing the data I'd placed in the data base in that [earlier post](http://ellisp.github.io/blog/2015/08/15/importing-nzis-surf/).  There's a big chunk of R packages needed for all the things I'm doing  here.  I also re-create some helper functions for transforming skewed continuous variables that include zero and negative values, which I first created in [another post back in September 2015](http://ellisp.github.io/blog/2015/09/07/transforming-breaks-in-a-scale/).

{% highlight R lineanchors %}
#------------------setup------------------------
library(showtext) # for fonts
library(RODBC) # for database connections
library(ggplot2)
library(scales)
library(MASS) # for stepAIC.  Needs to be before dplyr to avoid select namespace clash
library(dplyr)
library(tidyr)
library(stringr)

library(rpart)
library(rpart.plot)     # for prp()
library(caret)          # for train()
library(randomForest)
library(partykit)       # for plot(as.party())
library(quantregForest) # for Random Forest prediction intervals

library(xgboost)
library(Matrix)
library(data.table)

library(survey) # for rake()

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")

#------------------transformation functions------------
# helper functions for transformations of skewed data that crosses zero.  See 
# http://ellisp.github.io/blog/2015/09/07/transforming-breaks-in-a-scale/
.mod_transform <- function(y, lambda){
   if(lambda != 0){
      yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
   } else {
      yt = sign(y) * (log(abs(y) + 1))
   }
   return(yt)
}


.mod_inverse <- function(yt, lambda){
   if(lambda != 0){
      y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
   } else {
      y <- (exp(abs(yt)) - 1) * sign(yt)
      
   }
   return(y)
}

# parameter for reshaping - equivalent to sqrt:
lambda <- 0.5
{% endhighlight %}

Importing the data is a straightforward SQL query, with some reshaping required because survey respondents were allowed to specify either one or two ethnicities.  This means I need an indicator column for each individual ethnicity if I'm going to include ethnicity in any meaningful way (for example, an "Asian" column with "Yes" or "No" for each survey respondent).  Wickham's {dplyr} and {tidyr} packages handle this sort of thing easily.

{% highlight R lineanchors %}
#---------------------------download and transform data--------------------------
# This query will include double counting of people with multiple ethnicities:
sql <-
"SELECT sex, agegrp, occupation, qualification, region, hours, income, 
         a.survey_id, ethnicity FROM
   f_mainheader a                                               JOIN
   d_sex b           on a.sex_id = b.sex_id                     JOIN
   d_agegrp c        on a.agegrp_id = c.agegrp_id               JOIN
   d_occupation e    on a.occupation_id = e.occupation_id       JOIN
   d_qualification f on a.qualification_id = f.qualification_id JOIN
   d_region g        on a.region_id = g.region_id               JOIN
   f_ethnicity h     on h.survey_id = a.survey_id               JOIN
   d_ethnicity i     on h.ethnicity_id = i.ethnicity_id"

orig <- sqlQuery(PlayPen, sql, stringsAsFactors = TRUE)

# ...so we spread into wider format with one column per ethnicity
nzis <- orig %>%
   mutate(ind = TRUE) %>%
   spread(ethnicity, ind, fill = FALSE) %>%
   select(-survey_id) %>%
   mutate(income = .mod_transform(income, lambda = lambda))

for(col in unique(orig$ethnicity)){
   nzis[ , col] <- factor(ifelse(nzis[ , col], "Yes", "No"))
}

names(nzis)[11:14] <- c("MELAA", "Other", "Pacific", "Residual")
{% endhighlight %}

After reshaping ethnicity and transforming the income data, I split my data into training and test sets, with 80 percent of the sample in the training set.  I actually end up with 3 versions of each subset of the data:

* one for use in the R modelling formula formulation eg Y ~ x1 + x2, data = my_data
* one for the other traditional way that R modelling functions accept data ie a rectangle X for the explanatory variables and a vector Y for the response, but still keeps a single column for multi-class factors like occupation
* a design matrix that has provided dummy variables for each level of each factor ie has created a column of 1s and 0s for each occupation apart from the reference class.
{% highlight R lineanchors %}
set.seed(123)
nzis$use <- ifelse(runif(nrow(nzis)) > 0.8, "Test", "Train")

# traditional R modelling formula versions:
trainData <- nzis %>% filter(use == "Train") %>% select(-use)
testData <- nzis %>% filter(use == "Test") %>% select(-use)

# traditional R modelling X and Y versions:
trainX <- trainData %>% select(-income) %>% data.frame()
trainY <- trainData$income
testX <- testData %>% select(-income) %>% data.frame()
testY <- testData$income

# sparse versions with a column for each factor-level combination:
trainX2 <- model.matrix(income ~ ., trainData)[ , -1]
testX2 <- model.matrix(income ~ ., testData)[ , -1]
{% endhighlight %}

## Modelling income
The first job is to get a model that can estimate income for any arbitrary combination of the explanatory variables hourse worked, occupation, qualification, age group, ethnicity x 7 and region.  I worked through five or six different ways of doing this before eventually settling on Random Forests which had the right combination of convenience and accuracy.

###Regression tree
My first crude baseline is a single regression tree.  I didn't seriously expect this to work particularly well, but treated it as an interim measure before moving to a random forest.  I use the train() function from the {caret} package to determine the best value for the complexity parameter (cp) - the minimum improvement in overall R-squared needed before a split is made.  The best single tree is shown below.

<img width = "750px" src="/img/0026-polished-tree.svg">

One nice feature of regression trees - so long as they aren't too large to see all at once - is usually their easy interpretability.  Unfortunately this goes a bit by the wayside because I'm using a transformed version of income, and the tree is returning the mean of that transformed version.  When I reverse the transform back into dollars I get a dollar number that is in effect the squared mean of the square root of the original income in a particular category; which happens to generally be close to the median, hence the somewhat obscure footnote in the bottom right corner of the plot above.  It's a reasonable measure of the centre in any particular group, but not one I'd relish explaining to a client.

Following the tree through, we see that 

* the overall centre of the data is $511 income per week
* for people who work less than 12 hours, it goes down to $220; and those who work 12 or more hours receive $942.
* of those who work few hours, if they are aged 15-19 their income is down to $29; whereas the average for other age groups is $270.
* of those people who work few hours and are aged 20 or more - clerical and administrative workers, community and personal service workers, labourers, no occupation, and 'residual categories' occupation go down the left branch to receive $215 on average, whereas all other occupations go to the right and receive $495.
* and so on.

It takes a bit of effort to look at this plot and work out what is going on (and the abbreviated occupation labels don't help sorry), but it's possible once you've got the hang of it.  Leftwards branches always receive less income than rightwards branches; the split is always done on only one variable at a time, and the leftwards split label is slightly higher on the page than the rightwards split label.

Trees are a nice tool for this sort of data because they can capture fairly complex interactions in a very flexible way (eg in the tree above, managers and professionals working 35 or more hours per week earn more if they're 35 or older but for people working between 12 and 35 hours per week the break happens at age 25).  Where they're weaker is in dealing with relationships between continuous variables.

The code that fitted and plotted this tree (using the wonderful and not-used-enough prp() function that allows considerable control and polish of rpart trees) is below.
{% highlight R lineanchors %}
set.seed(234)
rpartTune <- train(trainX, trainY,
                     method = "rpart",
                     tuneLength = 10,
                     trControl = trainControl(method = "cv"))


rpartTree <- rpart(income ~ ., data = trainData, 
                   control = rpart.control(cp = rpartTune$bestTune),
                   method = "anova")


node.fun1 <- function(x, labs, digits, varlen){
   paste0("$", round(.mod_inverse(x$frame$yval, lambda = lambda), 0))
}

# exploratory plot only - not for dissemination:
plot(as.party(rpartTree))

par(fg = "blue", family = "myfont")

prp(rpartTree, varlen = 5, faclen = 7, type = 4, extra = 1, 
    under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
    split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
    branch.col = "grey85", under.col = "lightblue",
    node.fun = node.fun1)

grid.text("New Zealanders' income in one week in 2011", 0.5, 0.89,
          gp = gpar(fontfamily = "myfont", fontface = "bold"))  

grid.text("Other factors considered: qualification, region, ethnicity.",
          0.8, 0.2, 
          gp = gpar(fontfamily = "myfont", cex = 0.8))

grid.text("$ numbers in blue are 'average' weekly income:\nsquared(mean(sign(sqrt(abs(x)))))\nwhich is a little less than the median.",
          0.8, 0.1, 
          gp = gpar(fontfamily = "myfont", cex = 0.8, col = "blue"))
{% endhighlight %}

The success rates of the various modelling methods in predicting income in the test data I put aside will be shown all in one part of this post, later.

### A home-made random spinney (not forest...)
Regression trees have high variance.  Basically, they are unstable, and vulnerable to influential small pockets of data changing them quite radically.  The solution to this problem is to generate an ensemble of different trees and take the average prediction.  Two most commonly used methods are:

* ["bagging"](https://en.wikipedia.org/wiki/Bootstrap_aggregating) or bootstrap aggregation, which involves resampling from the data and fitting trees to the resamples
* [Random Forests](https://en.wikipedia.org/wiki/Random_forest) (trademark of Breiman and Cutler), which resamples rows from the data and also restricts the number of variables to a different subset of variables for each split.

Gradient boosting can also be seen as a variant in this class of solutions but I think takes a sufficiently different approach for me to leave it to further down the post.

Random Forests (tm) are a subset of the broader group of ensemble tree techniques known as "random decision forests", and I set out to explore this visually (I'm a very visual person - if I can't make a picture or movie of something happening I can't understand it).  The animation below shows an ensemble of 50 differing trees, where each tree was fitted to a set of data sample with replacement from the original data, and each tree was also restricted to just three randomly chosen variables.  Note that this differs from a Random Forest, where the restriction differs for each split within a tree, rather than being a restriction for the tree as a whole.

<img width = "750px" src="/img/0026-rf.gif">

Here's how I generated my [spinney](http://www.thefreedictionary.com/spinney) of regression trees.  Some of this code depends on a particular folder structure.  The basic strategy is to 

* subset the data
* subset the variables
* use cross-validation to work out the best tuning for the complexity parameter 
* fit the best tree possible
* draw an image, with appropriate bits of commentary and labelling added to it, and save it for later
* repeat the above 50 times, and then knit all the images into an animated GIF using ImageMagick.

{% highlight R lineanchors %}
# resample both rows and columns, as in a random forest,
# and draw a picture for each fitted tree.  Knit these
# into an animation.

variables <- c("sex", "agegrp", "occupation", "qualification",
               "region", "hours", "Maori")
n <- nrow(trainData)

home_made_rf <- list()
reps <- 50

commentary <- str_wrap(c(
   "This animation illustrates the use of an ensemble of regression trees to improve estimates of income based on a range of predictor variables.",
   "Each tree is fitted on a resample with replacement from the original data; and only three variables are available to the tree.",
   "The result is that each tree will have a different but still unbiased forecast for a new data point when a prediction is made.  Taken together, the average prediction is still unbiased and has less variance than the prediction of any single tree.",
   "This method is similar but not identical to a Random Forest (tm).  In a Random Forest, the choice of variables is made at each split in a tree rather than for the tree as a whole."
   ), 50)


set.seed(123)
for(i in 1:reps){
   
   these_variables <- sample(variables, 3, replace = FALSE)
   
   this_data <- trainData[
      sample(1:n, n, replace = TRUE),
      c(these_variables, "income")
   ]
   
   
   
   this_rpartTune <- train(this_data[,1:3], this_data[,4],
                      method = "rpart",
                      tuneLength = 10,
                      trControl = trainControl(method = "cv"))
   
   
   
   home_made_rf[[i]] <- rpart(income ~ ., data = this_data, 
                      control = rpart.control(cp = this_rpartTune$bestTune),
                      method = "anova")
 
   png(paste0("_output/0026_random_forest/", 1000 + i, ".png"), 1200, 1000, res = 100)  
      par(fg = "blue", family = "myfont")
      prp(home_made_rf[[i]], varlen = 5, faclen = 7, type = 4, extra = 1, 
          under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
          split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
          branch.col = "grey85", under.col = "lightblue",
          node.fun = node.fun1, mar = c(3, 1, 5, 1))
      
      grid.text(paste0("Variables available to this tree: ", 
                      paste(these_variables, collapse = ", "))
                , 0.5, 0.90,
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "darkblue"))
      
      grid.text("One tree in a random spinney - three randomly chosen predictor variables for weekly income,
resampled observations from New Zealand Income Survey 2011", 0.5, 0.95,
                gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text(i, 0.05, 0.05, gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text("$ numbers in blue are 'average' weekly income:\nsquared(mean(sign(sqrt(abs(x)))))\nwhich is a little less than the median.",
                0.8, 0.1, 
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "blue"))
      
      comment_i <- floor(i / 12.5) + 1
      
      grid.text(commentary[comment_i], 
                0.3, 0.1,
                gp = gpar(fontfamily = "myfont", cex = 1.2, col = "orange"))
      
      dev.off()

}   

# knit into an actual animation
old_dir <- setwd("_output/0026_random_forest")
# combine images into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 400 *.png "rf.gif"')
# move the asset over to where needed for the blog
file.copy("rf.gif", "../../../img/0026-rf.gif", overwrite = TRUE)
setwd(old_dir)
{% endhighlight %}

### Random Forest
Next model to try is a genuine Random Forest (tm).  As explained above, a Random Forest is an ensemble of regression trees, where each tree is a resample with replacement (variations are possible) of the original data, and each split in the tree is only allowed to choose from a subset of the variables available.  To do this I used the {randomForests} R package, but I've decided since that this isn't a go-er for the future.  It's just not efficient enough for even moderately large data - cross-validation and tuning become a right pain.  Unless I find a definitive article already out there, I'll write a future post about how xgboost and h20 both provide much more efficient ways of fitting random forests.  In the meantime, this [benchmarking of random forest implementations by Szilard Pafka](http://www.r-bloggers.com/benchmarking-random-forest-implementations/) gives excellent food for thought.

I couldn't get the train() function from the {caret} package to work for tuning my randomForest meta-parameters with my limited hardware available, so I did a more explicit cross-validation tuning myself.  Actually, it's nice to be able to do this explicitly anyway.

Cross-validation is all about splitting the data into a number of different training and testing sets, to get around the problem of using a single hold-out test set for multiple purposes.  It's better to give each bit of the data a turn as the hold-out test set.  In the tuning exercise below, I divide the data into three so I can try different values of the "mtry" parameter in my randomForest fitting and see the average Root Mean Square Error for the three fits for each value of mtry.  "mtry" defines the number of variables the tree building algorithm has available to it at each split of the tree.  For forests with a continuous response variable like mine, the default value is the number of variables divided by three and I have 10 variables, so I try a range of options from 2 to 8 as the subset of variables for the tree to choose from at each split.  It turns out the conventional value of mtry = 3 is in fact the best:

![rf-tuning](/img/0026-rf-cv3.svg)

Here's the code for this home-made cross-validation of randomForest:

{% highlight R lineanchors %}
# Hold ntree constant and try different values of mtry
# values of m to try for mtry
m <- c(2, 3, 4, 5, 6, 8)

folds <- 3

cvData <- trainData %>%
   mutate(group = sample(1:folds, nrow(trainData), replace = TRUE))

results <- matrix(numeric(length(m) * folds), ncol = folds)

for(i in 1:length(m)){
   message(i)
   for(j in 1:folds){
      
      cv_train <- cvData %>% filter(group != j) %>% select(-group)
      cv_train_x <- cv_train %>% select(-income)
      cv_train_y <- cv_train$income
      cv_test <- cvData %>% filter(group == j) %>% select(-group)
      cv_test_x <- cv_test %>% select(-income)
      cv_test_y <- cv_test$income
      system.time(tmp <- randomForest(cv_train_x, cv_train_y, 
                                      ntree = 250, mtry = m[i])) # about 20 minutes per fit
      preds <- predict(tmp, newdata = cv_test_x)
      results[i, j] <- RMSE(preds, obs = cv_test_y)
      print(paste("mtry", m[i], j, round(results[i, j], 2), sep = " : "))
   }
}

results_df <- as.data.frame(results)
results_df$mtry <- m

print(
   results_df %>% 
   gather(trial, RMSE, -mtry) %>% 
   ggplot() +
   aes(x = mtry, y = RMSE) +
   geom_point() +
   geom_smooth(se = FALSE) +
   ggtitle("3-fold cross-validation for random forest;\ndiffering values of mtry")
)
{% endhighlight %}



{% highlight R lineanchors %}


{% endhighlight %}


{% highlight R lineanchors %}


{% endhighlight %}
