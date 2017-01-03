---
layout: post
title: Cross-validation of topic modelling
date: 2017-01-04
tag: 
   - R
   - Text
description: Cross-validation of the "perplexity" from a topic model, to help determine a good number of topics
image: /img/0077-cv.svg
socialimage: http://ellisp.github.io/img/0077-cv.png
category: R
---

## Determining the number of "topics" in a corpus of documents
In my [last post](/blog/2016/12/31/sparse-bags) I finished by topic modelling a set of political blogs from 2004.  I made a passing comment that it's a challenge to know how many topics to set; existing algorithms don't do this.  There's quite a discussion on this out there, but all the extant approaches amount to fitting your model with lots of different values of k (where k is the number of latent topics to identify) and seeing which is "best".  Naturally, this begs the question of what is meant by "best".

A literature has grown around this question, and the convenient [`ldatuning` R package](https://cran.r-project.org/package=ldatuning) by Nikita Murzintcev provides metrics on four of the best methods.  A [helpful vignette](https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html) shows this in action with the `AssociatedPress` dataset of 2,246 articles and 10,473 words distributed with several of the commonly used R text-mining packages.  It's super-simple to run; here's the all the code needed:

{% highlight R %}
library(ldatuning)
library(topicmodels)
data("AssociatedPress", package="topicmodels")
full_data <- AssociatedPress

system.time({
tunes <- FindTopicsNumber(
   full_data,
   topics = c(1:10 * 10, 120, 140, 160, 180, 0:3 * 50 + 200),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
   method = "Gibbs",
   control = list(seed = 77),
   mc.cores = 8L,
   verbose = TRUE
)
})

FindTopicsNumber_plot(tunes)
{% endhighlight %}

and here's the results.  I dropped the method of Deveaud et al because the full vignette shows pretty convincingly that it doesn't work well in this situation.

![ldatuning](0077-ldatuning-screenshot.png)

OK, pretty convincing, and a good literature behind it.  Time consuming, but rigorous.

## Cross-validation

While I was researching this though I came across [this question on Stack Overflow](http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity) as well as a few mentions in the literature of cross-validation of topic models to choose optimal number of topics.  As part of my familiarisation with the whole approach to topic modelling I decided to look further into this.  

The key idea of cross-validation is that you divide the data into different numbers of subsets - conventionally 5 or 10, let's say 5 from now on - and take turns at using one of the five as a validation set while the remaining four are used as a training set.  This way each data point gets one turn as part of the hold-out validation, and four as part of the training set.  It's useful for assessing the overall validity of the model on data that wasn't invovled in its training, and also for identifying good values of tuning hyper-parameters.  For today, I want to use it to find the best value of the number of topics hyperparameter "k".  I'm going to use the `perplexity` measure for the applicability of a topic model to new data.

Let's do this one step at a time, building up to the full cross-validation at different values of k:

- first, I'm going to do validation (not cross-validation), on a single hold-out set of one fifth of the data, at a single value of k
- then I'm going to use cross-validation at a single value of k
- finally I'm going to use cross-validation at many values of k

Cross-validation is very computing-intensive and embarrassingly parallel so I'm going to parallelize the second and third of the efforts above.

### Simple validation with a single number of topics

For the simple validation, I make a single `train_set` version of 75% of rows from the `AssociatedPress` document-term-matrix, randomly chosen; and a `valid_set` of the remaining 25%.  The model is fit with the `LDA` function, and then (final two lines of code in the chunk below) I estimate how perplexed the resulting model is with both the training data set and the validation set.

{% highlight R %}
library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)

data("AssociatedPress", package = "topicmodels")

burnin = 1000
iter = 1000
keep = 50

# define our "full data" - during development I pretend the full dataset is 
# just the first 500 AP articles, which is enough for a reasonable test while not taking
# forever to run.  When I ran the final model, I came back and removed the "1:500" from below
full_data  <- AssociatedPress[1:500 , ]
n <- nrow(full_data)

#-----------validation--------
k <- 5 # number of topics

splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]

fitted <- LDA(train_set, k = k, method = "Gibbs",
                          control = list(burnin = burnin, iter = iter, keep = keep) )
perplexity(fitted, newdata = train_set)
perplexity(fitted, newdata = valid_set)
{% endhighlight %}

Perplexity was 2728 on the training data, and a much higher 4280 on the validation set.  This is to be expected - the model has been fit to the training set and naturally finds the new validation data more perplexing - that's the whole reason for confronting a model with fresh data.  BTW, to check whether this was impacted on by the number of documents, I also tried the above with equally sized training and validation sets and got similar results.

### Cross validation with a single number of topics
{% highlight R %}

{% endhighlight %}

### Cross validation with many candidate numbers of topics


This took 75 minutes to run on my laptop, steadily keeping 7 of my logical cores at about 50%-80% capacity.

### A small note on `topicmodels` in linux.

In writing this post I used both Windows and Linux (Ubuntu) machines, and it was my first time using `topicmodels` on Linux.  I had some frustrations getting it installed; basically the same problem with missing GNU Scientific Library in [this blog post](http://tinyheero.github.io/2016/02/20/install-r-topicmodels.html) and [this Stack Overflow Q&A](http://stackoverflow.com/questions/24172188/how-can-i-install-topicmodels-package-in-r).  

The trick was I needed not just `gsl-bin` but also `libsg10-dev`.  Also the `mpfr` library which is "a GNU portable C library for arbitrary-precision binary floating-point computation with correct rounding, based on GNU Multi-Precision Library."

{% highlight bash %}
sudo apt-get install gsl-bin libgsl0-dev
sudo apt-get install libmpfr-dev libmpfr-doc libmpfr4 libmpfr4-dbg
{% endhighlight %}

