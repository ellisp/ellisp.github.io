library(ggplot2)
library(scales)
library(R.utils)    # for gunzip
library(tidyverse)
library(tidytext)
library(Matrix)     # for sparse matrices
library(data.table) # for fread, fast version of read.table
library(SnowballC)  # for wordStem
library(foreach)
library(doParallel)
library(testthat)   # used for various checks and tests along the way
library(topicmodels)
library(slam)       # for other types of sparse matrices, used with topicmodels
library(wordcloud)
library(grid)       # for annotating graphics 

# latest version of h2o needed at time of writing (December 2016) for sparse matrix support
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-tutte/1/R")))
library(h2o)

# https://archive.ics.uci.edu/ml/datasets/Bag+of+Words
# download the smallest example bag of words for first trial
if(!"docword.kos.txt" %in% list.files()){
   download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/docword.kos.txt.gz",
                 destfile = "docword.kos.txt.gz", mode = "wb")
   gunzip("docword.kos.txt.gz")
}
   
kos <- fread("docword.kos.txt", skip = 3, sep = " ")  
names(kos) <- c("doc", "word", "count")
expect_equal(sum(kos$count), 467714)

kos_vocab <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/vocab.kos.txt")$V1


kos2 <- kos %>%
   # attach the names of words back to the words
   mutate(word = factor(word, labels = kos_vocab)) %>%
   # stopwords are meant to be already removed but no harm in checking (note - makes no difference):
   anti_join(stop_words, by = "word") %>%
   # reduce words to their stems (otherwise abandon, abandoning, abandons all treated as different, etc),
   mutate(word = wordStem(word)) %>%
   mutate(word = gsub("^iraq$", "iraqi", word)) %>%
   # ... and aggregate back up by the new stemmed words to remove duplicates
   group_by(doc, word) %>%
   summarise(count = sum(count)) %>%
   # correct some names that went wrong in the stemming:
   mutate(word = gsub("^chenei$", "cheney", word),
          word = gsub("^kerri$", "kerry", word),
          word = gsub("^georg$", "george", word),
          word = gsub("^john$", "John", word))

# how many words per post?
wordcount <- kos2 %>%
   group_by(doc) %>%
   summarise(words = sum(count))

svg("../img/0076-words-per-post.svg", 7, 4)
par(family = "myfont", font.main = 1)
hist(wordcount$words, col = "grey", fg = "grey80", border = "grey95",
     main = "Words per post in the KOS data set", xlab = "Number of words")
dev.off()

# OK, quite a variety of words per post.
# Convert counts to *relative* count (ie proportion of words in each doc)
# need to do this as otherwise the main dimension in the data is just the 
# length of each blog post
kos3 <- kos2 %>%
   group_by(doc) %>%
   mutate(count = count / sum(count))

# Sparse version
kos_sparse <- kos3 %>%
   # arrange it so the colnames after cast_sparse are in alphabetical order:
   arrange(word) %>%
   cast_sparse(doc, word, count)


# Dense version
kos_df <- kos3 %>%
   spread(word, count, fill = 0)
kos_dense <- as.matrix(kos_df[ , -1])

expect_equal(colnames(kos_dense), colnames(kos_sparse))

dim(kos_sparse) # would be 3430 documents and 6906 words except that there's been more processing, so only 4566 words
object.size(kos) # about 4MB
object.size(kos_sparse) # about the same.  Slightly less efficient
object.size(kos_dense) # 120+ MB

#========k-means cluster analysis in R===========
# set up parallel cluster for the R version of analysis 
# (sorry for there being two uses of the word "cluster"...)
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)
clusterExport(cluster, "kos_dense")

# How to work out how many clusters to find in the data?
# great answer at http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters 
# But I only use the simplest method - visual inspection of a scree plot of the total within squares

# takes some time, even with the parallel processing:
max_groups <- 25
wss <- foreach (i = 1:max_groups) %dopar% {
   kmeans(kos_dense, centers = i)$tot.withinss
}

svg("../img/0076-base-scree.svg", 7, 4.5)
par(family = "myfont", font.main = 1)
plot(1:max_groups, wss, type="b", xlab="Number of groups",
     ylab="Within groups sum of squares", bty = "l")
grid()
dev.off()
# not clear how many are needed, but at least 20

stopCluster(cluster)

#==============cluster analysis in h2o======================
h2o.init(nthreads = -1, max_mem_size = "10G")

system.time({ kos_h1 <- as.h2o(kos_sparse)}) # 3 seconds
dim(kos_h1) # correct
colnames(kos_h1) # for some reason, only 100 columnames, c1:100
colnames(kos_h1) <- colnames(kos_sparse)

#--------------------checking we have the same data in all its different versions--------------------
# Compare the simplest single cluster case - should just be the mean of each column
simple_r <- kmeans(kos_dense, centers = 1)
simple_h <- h2o.kmeans(kos_h1, x = colnames(kos_h1), k = 1, standardize = FALSE)
simple_m1 <- apply(kos_dense, 2, mean)
simple_m2 <- t(apply(kos_h1, 2, mean))
simple_m3 <- apply(kos_sparse, 2, mean)

comparison <- data.frame(
   R = simple_r$centers[1 ,],
   manual1 = simple_m1,
   H2O = t(h2o.centers(simple_h))[,1],
   manual2 = as.vector(simple_m2[,1]),
   manual3 = simple_m3
)

png("../img/0076-pairs-centers.png", 7 * 600, 7 * 600, res = 600)
par(family = "myfont", font.main = 1)
pairs(comparison, main = "Different methods of estimating the center of the data")
dev.off()

#----------------Performing k means analysis for lots of different numbers of groups---------------------
# one advantage of h2o.kmeans is it lets you use cross-validation for assessing the within-group sum of squares,
# rather than getting it from the data that was used to set the centers.  It takse much longer though
max_groups <- 25
wss_h2o <- numeric(max_groups)
for(i in 1:max_groups){
   kos_km1 <- h2o.kmeans(kos_h1, x= colnames(kos_h1), estimate_k = FALSE, k = i, standardize = FALSE, 
                         nfolds = 5, max_iterations = 25)   
   wss_h2o[i] <- h2o.tot_withinss(kos_km1, xval = TRUE) # xval=TRUE means cross validation used
}

svg("../img/0076-h2o-scree.svg", 7, 4)
par(family = "myfont", font.main = 1)
plot(1:max_groups, wss_h2o, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", bty = "l")
grid()
dev.off()

#---------------------------------creating and present a final accepted model--------------------------
final_k <- 20
kos_km_final <- h2o.kmeans(kos_h1, x= colnames(kos_h1), 
                           estimate_k = FALSE, k = final_k, standardize = FALSE, 
                           init = "Random", max_iterations = 25, seed = 235)   
h2o.tot_withinss(kos_km_final)



sz  <- data_frame(
   size =  h2o.cluster_sizes(kos_km_final),
   group = paste0("V", 1:length(h2o.cluster_sizes(kos_km_final)))
)

# repeat this line from above, just so we remember what it is
simple_m3 <- apply(kos_sparse, 2, mean)
simple_df <- data_frame(word = names(simple_m3), center_simp = simple_m3)

kc_df <- h2o.centers(kos_km_final) %>%
    t %>%
    as.data.frame %>%
   mutate(word = colnames(kos_sparse)) %>%
   gather(group, center, -word) %>%
   group_by(group) %>%
   left_join(simple_df, by = "word") %>%
   mutate(center_diff = center - center_simp,
          center_ratio = center / center_simp) %>%
   arrange(desc(center_diff)) %>%
   summarise(distinctive_words = paste(word[1:8], collapse = " ")) %>%
   left_join(sz, by = "group") %>%
   arrange(desc(size)) 

knitr::kable(kc_df)



#------------visualise with PCA--------

rarewords <- kos2 %>%
   group_by(word) %>%
   summarise(count = sum(count)) %>%
   filter(count < 50) %>%
   arrange(count)

kos_smaller <- kos3 %>%
   anti_join(rarewords[, "word"], by = "word") %>%
   arrange(word) %>%
   cast_sparse(doc, word, count) 
dim(kos_smaller)

kos_h2 <- as.h2o(kos_smaller)

colnames(kos_h2) <- colnames(kos_smaller)

# not clear what k is here.  Seems mandatory, but the documentation hasn't caught up?
# This takes quite a long time to calculate:
kos_pc <- h2o.prcomp(kos_h2, k = 2)

kos_2d <- as.data.frame(h2o.predict(kos_pc, kos_h2, num_pc = 2))
kos_2d$group <- paste0("V", as.vector(predict(kos_km_final, kos_h1)) + 1)
head(kos_2d)

kc_df2 <- kc_df %>%
   mutate(lab = str_wrap(distinctive_words, 20),
          lab = factor(lab, levels = lab))

png("../img/0076-facet-groups.png", 9 * 600, 10 * 600, res = 600)
kos_2d %>%
   left_join(kc_df2, by = "group") %>%
   ggplot(aes(x = PC2, y = PC1)) +
   geom_point(size = 0.8, alpha = 0.5) +
   facet_wrap(~lab) +
   ggtitle("Political discussion in blogs at dailykos.com from the 2004 Presidential election",
           "Each point represents an individual blog post in the two dimensional space that best represents their variance.\nPosts have been clustered by k-means clustering based on counts of individual words.\nLabels show the stems of words most distinctively associated with the particular cluster of post.  ") +
   labs(x = paste0("PC2\n",
                   str_wrap("This graphic highlights how the 'curse of dimensionality' places severe limits on K-means clustering as an analytical technique.  With high dimensional data (in this case >3,000 columns), 'distances' between points tend to all being the same and clustering is difficult and.  A better method (not shown) is a more explicit model, as provided by topic modelling with latent dirichlet allocation.", 100)),
        caption = "Source: Bag of words data at https://archive.ics.uci.edu/ml/datasets/Bag+of+Words") +
   coord_equal() +
   theme(axis.text = element_blank())
dev.off()


# I didn't understand the strong pattern in the first two principal components, but got a similar
# result in base graphics
# kos_pc_base <- prcomp(kos_smaller)
# kos_2d <- as.data.frame(kos_pc_base$x[ , 1:2])

h2o.shutdown()


#======================topic modelling within R======================
# Other goes at this:
# https://datamicroscopes.github.io/topic.html

kos_counts <- kos2 %>%
   arrange(word) %>%
   # take advantage of factor() as a quick way to make number:level pairings
   mutate(word = factor(word))

# convert to triplet (row, column, cell) sparse matrix format:
kos_trip <- with(kos_counts, simple_triplet_matrix(i = doc, j = as.numeric(word), v = count))

dimnames(kos_trip)[[2]] <- levels(kos_counts$word)
# expect_equal(apply(kos_trip, 2, mean), apply(kos_sparse, 2, mean))

kos_small <- kos_trip
system.time(lda <- LDA(kos_small, k = 10))
# timings with k=5 and defaults
# 2 seconds with 100 rows
# 4.7 with 200
# 6.9 with 300
# 8.7 with 400
# 28.0  with 1000
# 130 with all 3500 rows and k = 5
# 332 with all rows and k = 10

apply(p$topics, 1, sum) # all ones.  So posterior()$topics gives probabilities of being in each of the five topics

topics(lda) # returns the numbers of the actual topics for each document

#' function for drawing word clouds of all topics from an object created with LDA
wclda <- function(lda, n = 100, palette = "Blues", ...){
   p <- posterior(lda)
   w1 <- as.data.frame(t(p$terms)) 
   w2 <- w1 %>%
      mutate(word = rownames(w1)) %>%
      gather(topic, weight, -word) 
   
   pal <- rep(brewer.pal(9, palette), each = ceiling(n / 9))[n:1]
   
   for(i in 1:ncol(w1)){
      w3 <- w2 %>%
         filter(topic == i) %>%
         arrange(desc(weight))
      with(w3[1:n, ], 
           wordcloud(word, freq = weight, random.order = FALSE, 
                     ordered.colors = TRUE, colors = pal, ...))
      title(paste("Topic", i))
   }
}


# system.time(lda2 <- LDA(kos_trip, k = 10)) # 330 seconds
# 
# svg("../img/0076-topics-10.svg", 12, 7)
# par(mfrow = c(2, 5), family = "myfont", font.main = 1)
# wclda(lda2)
# dev.off()


system.time(lda3 <- LDA(kos_trip, k = 8))

svg("../img/0076-topics-8.svg", 10, 7)
par(mfrow = c(2, 4), family = "myfont", font.main = 1)
wclda(lda3)
grid.text(0.5, 0.55, label = "Latent topics identified in 3,430 dailykos.com blog posts in 2004",
          gp = gpar(fontfamily = "myfont", fontface = "italic"))
grid.text(0.98, 0.02, hjust = 1,
          label = "Source: data from https://archive.ics.uci.edu/ml/datasets/Bag+of+Words\n           analysis from https://ellisp.github.io",
          gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7, col = "grey60"))

dev.off()



setwd("../img")
files <- list.files()
files <- files[grepl("^0076-topics.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 1200x800', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")
