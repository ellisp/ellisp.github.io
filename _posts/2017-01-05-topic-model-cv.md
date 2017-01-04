---
layout: post
title: Cross-validation of topic modelling
date: 2017-01-05
tag: 
   - R
   - Text
   - Animations
   - ModellingStrategy
description: Cross-validation of the "perplexity" from a topic model, to help determine a good number of topics.
image: /img/0077-cv.svg
socialimage: http://ellisp.github.io/img/0077-cv.png
category: R
---
<center><img src = "/img/0077-AssociatedPress.gif" width = "400"></center>

## Determining the number of "topics" in a corpus of documents
In my [last post](/blog/2016/12/31/sparse-bags) I finished by topic modelling a set of political blogs from 2004.  I made a passing comment that it's a challenge to know how many topics to set; the R `topicmodels` package doesn't do this for you.  There's quite a discussion on this out there, but nearly all the extant approaches amount to fitting your model with lots of different values of k (where k is the number of latent topics to identify) and seeing which is "best".  Naturally, this begs the question of what is meant by "best".

A literature has grown around this question, and the convenient [`ldatuning` R package](https://cran.r-project.org/package=ldatuning) by Nikita Murzintcev provides metrics using four of the methods.  A [helpful vignette](https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html) shows this in action with the `AssociatedPress` dataset of 2,246 articles and 10,473 words.  This dataset was presented at the First Text Retrieval Conference (TREC-1) in 1992 and is distributed with several of the commonly used R text-mining packages.  The `ldatuning` approach is super-simple to run; here's the all the code needed to fit calculate three of the measures for a range of different topic numbers from 10 to 350.  To save computational effort, I omitted a fourth method that the vignette shows not to work well with this particular data.

{% highlight R %}
# Load up R packages including a few we only need later:
library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
data("AssociatedPress", package="topicmodels")
full_data <- AssociatedPress

system.time({
tunes <- FindTopicsNumber(
   full_data,
   topics = c(1:10 * 10, 120, 140, 160, 180, 0:3 * 50 + 200),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
   method = "Gibbs",
   control = list(seed = 77),
   mc.cores = 4L,
   verbose = TRUE
)
})

FindTopicsNumber_plot(tunes)
{% endhighlight %}

and here's the results.  

![ldatuning](/img/0077-ldatuning.svg)

OK, pretty convincing, and a good literature behind it to justify these methods.  The various methods agree that somewhere between 90 and 140 topics is optimal for this dataset.  Time consuming, but rigorous.  The code above took 10 hours to run on an old machine with four CPUs.  The `ldatuning` documentation has references and links to the underlying articles.

## Cross-validation

The abstracts of the (mostly paywalled unfortunately) articles implemented by `ldatuning` look like the metrics they suggest are based on assessing maximising [likelihood](https://en.wikipedia.org/wiki/Likelihood_function), minimising [Kullback-Leibler divergence](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence) or similar, using the same dataset that the model was trained on (rather than cross-validation).

While I was researching this I came across [this question on Stack Overflow](http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity) as well as a few mentions elsewhere on the web of cross-validation of topic models to choose optimal number of topics.  As part of my familiarisation with the whole approach to topic modelling I decided to look further into this.  For example, the [vignette for topicmodels](https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf) says the number of topics in their worked example was chosen by 10-fold cross-validation, but doesn't say how this was done other than pointing out that it is possible in "a few lines of R code".

A more complete description is given in this [conference paper by Zhao, Chen, Perkins, Liu, Ge, Ding and Zou](http://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-16-S13-S8):

> "Lacking such a heuristic to choose the number of topics, researchers have no recourse beyond an informed guess or time-consuming trial and error evaluation. For trial and error evaluation, an iterative approach is typical based on presenting different models with different numbers of topics, normally developed using cross-validation on held-out document sets, and selecting the number of topics for which the model is least perplexed by the test sets... Using the identified appropriate number of topics, LDA is performed on the whole dataset to obtain the topics for the corpus. We refer to this as the perplexity-based method.  Although the perplexity-based method may generate meaningful results in some cases, it is not stable and the results vary with the selected seeds even for the same dataset."

The key idea of cross-validation is that you divide the data into different numbers of subsets - conventionally 5 or 10, let's say 5 from now on - and take turns at using one of the five as a validation set while the remaining four are used as a training set.  This way each data point gets one turn as part of the hold-out validation, and four turns as part of the training set.  It's useful for assessing the overall validity of the model on data that wasn't involved in its training, and also for identifying good values of tuning hyper-parameters.  For today, I want to use it to find the best value of the number of topics hyperparameter "k".  

I'm going to use the `perplexity` measure for the applicability of a topic model to new data.  [Perplexity](https://en.wikipedia.org/wiki/Perplexity) is a measure of how well a probability model predicts a sample. Given the `perplexity` function comes in the `topicmodels` R package and is the obvious way supplied to test a trained model on new data, I think this is most likely what Grun and Hornick did for cross-validation in the `topicmodels` vignette; and it is mentioned by Zhao et al as the method used in "time-consuming trial and error evaluation".  I'm not particularly worried for now about what Zhao et al say about the stability of cross-validation with perplexity; I might come back to that in a later post (my intuition is that this is unlikely to be a problem, but I'll want to check that some time!).

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

Next step is do the validation, with a single value of k, but splitting the data into five so each row of the data gets a turn in the validation set.  Here's how I did that:

{% highlight R %}
#---------------5-fold cross-validation---------------------
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)

cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(topicmodels)
})
clusterExport(cluster, c("full_data", "k", "burnin", "iter", "keep", "splitfolds"))

results <- foreach(i = 1:folds) %dopar% {
   train_set <- full_data[splitfolds != i , ]
   valid_set <- full_data[splitfolds == i, ]
   
   fitted <- LDA(train_set, k = k, method = "Gibbs",
                 control = list(burnin = burnin, iter = iter, keep = keep) )
   return(perplexity(fitted, newdata = valid_set))
}
stopCluster(cluster)
{% endhighlight %}

The results aren't particularly interesting - just 5 numbers - so I won't show here.

### Cross validation with many candidate numbers of topics

Finally, we now do this cross-validation many times, with different values of the number of latent topics to estimate.  Here's how I did that:

{% highlight R %}
#----------------5-fold cross-validation, different numbers of topics----------------
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 3, 4, 5, 10, 20, 30, 40, 50, 75, 100, 200, 300) # candidates for how many topics
clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
   k <- candidate_k[j]
   results_1k <- matrix(0, nrow = folds, ncol = 2)
   colnames(results_1k) <- c("k", "perplexity")
   for(i in 1:folds){
      train_set <- full_data[splitfolds != i , ]
      valid_set <- full_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
   }
   return(results_1k)
}
})
stopCluster(cluster)

results_df <- as.data.frame(results)

ggplot(results_df, aes(x = k, y = perplexity)) +
   geom_point() +
   geom_smooth(se = FALSE) +
   ggtitle("5-fold cross-validation of topic modelling with the 'Associated Press' dataset",
           "(ie five different models fit for each candidate number of topics)") +
   labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
{% endhighlight %}

This took 75 minutes to run on a modern machine (different to that used for `ldatuning`) when I stopped the `candidate_k` at 100.  When I added the final values of 200 and 300 to `candidate_k` it took 210 minutes.  I did notice that towards the end, not all processors were being used at maximum capacity, which suggests the way I've parallelised it may not be optimal.

Exactly how number of documents, topics and words contribute to the time cost of fitting a topic model is a subject for a future post I think.

Here's the results from the cross-validation using perplexity:

![cv](/img/0077-cv.svg)

We get something that at least is consistent with the measures from the `ldatuning` package; there's a distinct flattening out of the cross-validated perplexity somewhere between 50 and 200 topics.  By the time we have 200 topics there has definitely been over-fitting, and the model is starting to get worse when tested on the hold-out validation sets.  There's still judgement required as to exactly how many topics to use, but 100 looks a good consensus number that all three methods tried from `ldatuning` support as well as the perplexity cross-validation measure.

## Presentation of final model

I opted to fit a model with 90 topics.  Here's an animation of the results:
![anim2](/img/0077-AssociatedPress.gif)

Code for creating the animation:

{% highlight R %}
#=====================Presentation of results===============
FinalModel <- LDA(full_data, k = 90, method = "Gibbs",
                  control = list(burnin = burnin, iter = iter, keep = keep) )

# approach to drawing word clouds of all topics from an object created with LDA,
# taken from /blog/2016/12/31/sparse-bags
n <- 100; palette = "Greens"; lda <- FinalModel

p <- posterior(lda)
w1 <- as.data.frame(t(p$terms)) 
w2 <- w1 %>%
   mutate(word = rownames(w1)) %>%
   gather(topic, weight, -word) 

pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]

wd <- setwd(tempdir())

# need to be careful for warnings that words didn't fit in, 
# in which case make the png device larger
# remove any PNG files sitting around in the temp folder
unlink("*.png")
# create a PNG for each frame of the animation
for(i in 1:ncol(w1)){
   png(paste0(i + 1000, ".png"), 8 * 100, 8 * 100, res = 100)
   par(bg = "grey95")
   w3 <- w2 %>%
      filter(topic == i) %>%
      arrange(desc(weight))
   with(w3[1:n, ], 
        wordcloud(word, freq = weight, random.order = FALSE, 
                  ordered.colors = TRUE, colors = pal))
   title(paste("Associated Press Topic", i))
   dev.off()
}

# combine all those png frames into a single animated GIF
system('magick -loop 0 -delay 70 *.png "AssociatedPress.gif"')

# move the asset over to where needed for the blog.  This is specific to my folder structure.
file.copy("AssociatedPress.gif", paste0(wd, "/../img/0077-AssociatedPress.gif"), overwrite = TRUE)

# cleanup
unlink(c("*.png", "AssociatedPress.gif"))
setwd(wd)
{% endhighlight %}

## A small note on `topicmodels` in linux.

In writing this post I used both Windows and Linux (Ubuntu) machines, and it was my first time using the R `topicmodels` pacakge on Linux.  I had some frustrations getting it installed; basically the same problem with missing GNU Scientific Library in [this blog post](http://tinyheero.github.io/2016/02/20/install-r-topicmodels.html) and [this Stack Overflow Q&A](http://stackoverflow.com/questions/24172188/how-can-i-install-topicmodels-package-in-r).  

The trick was I needed not just `gsl-bin` but also `libsg10-dev`.  Also the `mpfr` library which is "a GNU portable C library for arbitrary-precision binary floating-point computation with correct rounding, based on GNU Multi-Precision Library."  In Ubuntu:

{% highlight bash %}
sudo apt-get install gsl-bin libgsl0-dev
sudo apt-get install libmpfr-dev libmpfr-doc libmpfr4 libmpfr4-dbg
{% endhighlight %}

