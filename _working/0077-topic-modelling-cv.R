library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)

data("AssociatedPress", package = "topicmodels")

burnin = 1000
iter = 1000
keep = 50

# define our "full data" - during development I pretend the full dataset is 
# just the first 500 AP articles, which is enough for a reasonable test while not taking
# forever to run.  
full_data  <- AssociatedPress[1:nrow(AssociatedPress), ]
n <- nrow(full_data)
#-----------validation--------
k <- 5

splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]

fitted <- LDA(train_set, k = k, method = "Gibbs",
                          control = list(burnin = burnin, iter = iter, keep = keep) )
perplexity(fitted, newdata = train_set)
perplexity(fitted, newdata = valid_set)



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

#----------------5-fold cross-validation, different numbers of topics----------------
# set up a cluster for parallel processing
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

# load up the needed R package on all the parallel sessions
clusterEvalQ(cluster, {
   library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 3, 4, 5, 10, 20, 30, 40, 50, 75, 100, 200, 300) # candidates for how many topics

# export all the needed R objects to the parallel sessions
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


p1 <- ggplot(results_df, aes(x = k, y = perplexity)) +
   geom_point() +
   geom_smooth(se = FALSE) +
   ggtitle("5-fold cross-validation of topic modelling with the 'Associated Press' dataset",
           "(ie five different models fit for each candidate number of topics)") +
   labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
svg("../img/0077-cv.svg", 8, 6)
   print(p1)
dev.off()

png("../img/0077-cv.png", 8 * 600, 6 * 600, res = 600)
   print(p1)
dev.off()

#==============Alternative approach - using ldatuning==============
library(ldatuning)
library(topicmodels)
data("AssociatedPress", package="topicmodels")
full_data <- AssociatedPress[1:100, ]

system.time({
   tunes <- FindTopicsNumber(
      full_data,
      topics = c(1:10 * 10, 120, 140, 160, 180, 0:3 * 50 + 200),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
      method = "Gibbs",
      control = list(seed = 77),
      mc.cores = detectCores(),
      verbose = TRUE
   )
})


FindTopicsNumber_plot(tunes)

#=====================Presentation of results===============
FinalModel <- LDA(full_data, k = 90, method = "Gibbs",
                  control = list(burnin = burnin, iter = iter, keep = keep) )

#' function for drawing word clouds of all topics from an object created with LDA
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
showtext.opts(dpi = 100)
for(i in 1:ncol(w1)){
# for(i in 1:5){
   png(paste0(i + 1000, ".png"), 8 * 100, 8 * 100, res = 100)
   par(family = "myfont")
   par(bg = "grey95")
   w3 <- w2 %>%
      filter(topic == i) %>%
      arrange(desc(weight))
   with(w3[1:n, ], 
        wordcloud(word, freq = weight, random.order = FALSE, 
                  ordered.colors = TRUE, colors = pal))
   title(paste("Associated Press 1992 Topic", i))
   dev.off()
}

#
system('magick -loop 0 -delay 70 *.png "AssociatedPress.gif"')

# move the asset over to where needed for the blog.  This is specific to my folder structure.
file.copy("AssociatedPress.gif", paste0(wd, "/../img/0077-AssociatedPress.gif"), overwrite = TRUE)

# cleanup
unlink(c("*.png", "AssociatedPress.gif"))
setwd(wd)

