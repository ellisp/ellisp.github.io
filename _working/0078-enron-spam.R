library(tidyverse)
library(tidytext)
library(tm)
library(testthat)
library(wordcloud)
library(data.table)
library(h2o)
library(stringr)
library(forcats)

#===========download files=============
# Files described at http://csmining.org/index.php/enron-spam-datasets.html
baseurl <- "http://csmining.org/index.php/enron-spam-datasets.html?file=tl_files/Project_Datasets/Enron-Spam%20datasets/Preprocessed/enron"

filenames <- paste0(baseurl, 1:6, ".tar.tar")


for(i in 1:6){
   message("Downloading ", i)
   dfile <- paste0("enron", i, ".tar.tar")
   download.file(filenames[i], destfile = dfile, mode = "wb")
   message("Un-archiving ", i)
   untar(dfile)
}
                             


#=============import to R================
# Adapting the example at http://tidytextmining.com/usenet.html
folders <- paste0("enron", 1:6)

spam <- data_frame(file = dir(paste0(folders, "/spam"), full.names = TRUE)) %>%
   mutate(text = map(file, readLines, encoding = "Latin-1")) %>%
   transmute(id = basename(file), text) %>%
   unnest(text) %>%
   mutate(SPAM = "spam")

ham <- data_frame(file = dir(paste0(folders, "/ham"), full.names = TRUE)) %>%
   mutate(text = map(file, readLines, encoding = "Latin-1")) %>%
   transmute(id = basename(file), text) %>%
   unnest(text) %>%
   mutate(SPAM = "ham")

enron_raw <- rbind(spam, ham)

#============error checks - failing!===================
# Check against the counts provided at http://csmining.org/index.php/data.html
# Turns out some 3013 spam messages have gone missing
# should be 20170 spam messages and 16545 ham messages:
# returns an error
expect_equal(length(unique(enron_raw$id)), 20170 + 16545) 

enron_raw %>%
   select(id, SPAM) %>%
   distinct() %>%
   summarise(spam_count = sum(SPAM == "spam"), ham_count = sum(SPAM == "ham"))
# For my purposes I decide not to worry about this.


#=================further processing==================
enron <- enron_raw %>%
   # will just remove the "Subject:" and "Subject :" and treat subject words like any other
   mutate(text = gsub("^Subject *: *", "", text),
          text = gsub("<U.....>", "", text, fixed = FALSE)) 

enron_words <- enron %>%
   unnest_tokens(word, text) %>%
   select(-SPAM)

# First I'm creating a summary, dense data frame with some numeric info on each document
enron_sum1 <- enron %>%
   mutate(number_characters = nchar(text)) %>%
   group_by(id, SPAM) %>%
   summarise(number_characters = sum(number_characters))


enron_sum2 <- enron_words %>%
   group_by(id) %>%
   summarise(number_words = length(word))

enron_sum3 <- enron_words %>%
   anti_join(stop_words, by = "word") %>%
   group_by(id) %>%
   summarise(number_nonstop_words = length(word))

enron_sum_total <- enron_sum1 %>%
   left_join(enron_sum2, by = "id") %>%
   left_join(enron_sum3, by = "id") %>%
   mutate(number_stop_words = number_words - number_nonstop_words,
          proportion_stop_words = number_stop_words / number_words) %>%
   select(-number_nonstop_words)


enron_sum_total %>%
   gather(variable, value, -SPAM, -id) %>%
   ggplot(aes(x = value, fill = SPAM)) +
   facet_wrap(~variable, scales = "free") +
   geom_density(alpha = 0.5) +
   scale_x_sqrt()


# Second is to make a sparse matrix with each word having a column

used_words <- enron_words %>%
   # knock out stop words:
   anti_join(stop_words, by = "word") %>%
   # knock out numerals, and words with only 2 or 1 letters:
   mutate(word = gsub("[0-9]", "", word),
          wordlength = nchar(word)) %>%
   filter(wordlength > 2) %>%
   group_by(word) %>%
   summarise(count = length(word)) %>%
   ungroup() %>%
   # knock out words used less than 10 times:
   filter(count >= 10)

enron_dtm <- enron_words %>%
   right_join(used_words, by = "word") %>%
   cast_dtm(id, word, count) 

# we need a version of enron_dense in the same column, to do a sort of 
# manual join of the sparse matrix with the dense one later in H2O.
rows <- data_frame(id = rownames(enron_dtm))
enron_dense <- left_join(rows, enron_sum_total, by = "id")
expect_equal(nrow(enron_dtm), nrow(enron_dense))
expect_equal(rownames(enron_dtm), enron_dense$id)

#================import to h2o and join up there============
h2o.init(nthreads = -1, max_mem_size = "10G")

# Load up the dense matrix:
enron_dense_h2o <- as.h2o(enron_dense)

# Load up the sparse matrix:
thefile <- tempfile()
write_stm_svm(enron_dtm, file = thefile)
enron_sparse_h2o <- h2o.uploadFile(thefile, parse_type = "SVMLight")
unlink(thefile)

# Number of rows should be equal:
expect_equal(nrow(enron_sparse_h2o), nrow(enron_dtm))
# Number of columns should be 1 extra in H2O, dummy variable of labels (1) added by write_stm_svm:
expect_equal(ncol(enron_sparse_h2o), ncol(enron_dtm) + 1)

# First column should be the dummy labels = all one
expect_equal(mean(enron_sparse_h2o[ , 1]), 1)

# give back the meaningful column names.  For some reason this doesn't work.
# colnames(enron_sparse_h2o) <- c("Intercept", make.names(colnames(enron_dtm)))

enron_fulldata <- h2o.cbind(enron_sparse_h2o, enron_dense_h2o)
dim(enron_fulldata) # takes a while, perhaps it is materializing it in the client or something?
dim(enron_sparse_h2o)
head(colnames(enron_fulldata), 10)

# Convert the target variable to a factor so h2o.glm and other modelling functions
# know what to do with it:
enron_fulldata[ , "SPAM"] <- as.factor(enron_fulldata[ , "SPAM"])


#=====================analysis in H2O=============

#-----------prep------------------------
# names of the explanatory variables - all the columns in the sparse matrix (which are individual words)
# except the first one which is the dummy "labels" created just for the SVMLight format.  And the
# four summary variables in the dense dataframe:
xnames <- c(colnames(enron_sparse_h2o)[-1], 
            "number_characters", "number_words", "number_stop_words", "proportion_stop_words")

# A lookup table of the column names that refer to words to the words they actually mean.
# Useful when we look at variable performance down the track.
wordcols <- data_frame(
   variable = colnames(enron_sparse_h2o),
   word = c("", colnames(enron_dtm))
)

# The slower models (ie apart from glm) take ages for cross-validation so 
# we'll settle for single-split validation
enron_split <- h2o.splitFrame(enron_fulldata, ratios = c(0.6, 0.2))
dim(enron_split[[1]])
#-------------------GLM---------------------
# Binomial GLM, with elastic net regularization.  This is the minimal baseline sort of model.
# 200 seconds
system.time({
mod.glm <- h2o.glm(x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
                   validation_frame = enron_split[[2]],
                   family = "binomial",
                   nfolds = 5, alpha = 0.5, lambda_search = TRUE)
})

h2o.varimp(mod.glm) %>%
   slice(1:30) %>%
   left_join(wordcols, by = c("names" = "variable")) %>%
   arrange(sign, desc(coefficients)) 

h2o.performance(mod.glm, valid = TRUE) # 131 / 6640

#--------------------Random Forest-------------------
# with ntrees = 50, nfolds = 10, max_depth  = 20, took 25 minutes to get 5% through so I stopped it
# Can view progress by going to http://127.0.0.1:54321/flow/index.html
# 1340 seconds
system.time({
mod.rf <- h2o.randomForest(x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
                           validation_frame = enron_split[[2]],
                           ntrees = 500, max_depth = 20,
                           stopping_tolerance = 0.0001, stopping_rounds = 3, score_tree_interval = 25)
})
# note from watching progress in flow, scoring every score_tree_interval models takes quite a lot of time.
# so I pushed out score_tree_interval.  On current settings it will score
# the latest model against the validation frame every 25 trees, and stop if it hasn't improved since 3*25 trees ago.
# If score_tree_interval is a small number, it stops growing trees too quickly

h2o.performance(mod.rf, valid = TRUE) # 172/6640 error rate


h2o.varimp(mod.rf) %>%
   slice(1:30) %>%
   left_join(wordcols) %>%
   arrange(desc(scaled_importance)) 

# This doesn't work for some reason:
# system.time({
# mod.rf.grid <- h2o.grid("randomForest",
#                         x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
#                         validation_frame = enron_split[[2]],
#                         hyper_params = list(
#                            max_depth = c(10, 20, 30),
#                            min_rows = 1:2))
# })

#-------------------gradient boosting machine-----------------
# 1240 seconds
system.time({
   mod.gbm <- h2o.gbm(x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
                              validation_frame = enron_split[[2]],
                              ntrees = 100, max_depth = 5,
                              stopping_tolerance = 0.0001, stopping_rounds = 3, score_tree_interval = 5)
})

h2o.performance(mod.gbm, valid = TRUE) # 240/6640 error rate

h2o.varimp(mod.gbm) %>%
   slice(1:30) %>%
   left_join(wordcols) %>%
   arrange(desc(scaled_importance)) 






#-------------------artificial neural network------------------------

# 900 seconds; much faster when sparse = TRUE
system.time({
   mod.dl <- h2o.deeplearning(x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
                      validation_frame = enron_split[[2]],
                      hidden = c(200, 200),
                      stopping_tolerance = 0.0001, stopping_rounds = 5, sparse = TRUE)
})
h2o.performance(mod.dl, valid = TRUE) # 82/6640


#-----------------Naive Bayes - doesn't work--------------------
# Conditional probabilities won't fit in the driver node's memory (20.99 GB > 6.06GB)
mod.nb <- h2o.naiveBayes(x = xnames, y = "SPAM", training_frame = enron_split[[1]], 
                              validation_frame = enron_split[[2]])


#==============presentation of results==========================

perf <- function(newdata){
   return(c(
      glm = as.character(h2o.confusionMatrix(mod.glm, newdata = newdata)$Rate[1]),
      rf = as.character(h2o.confusionMatrix(mod.rf, newdata = newdata)$Rate[1]),
      gbm = as.character(h2o.confusionMatrix(mod.gbm, newdata = newdata)$Rate[1]),
      dl = as.character(h2o.confusionMatrix(mod.dl, newdata = newdata)$Rate[1])
      ))
}

# this isn't the most efficient computing wise, because the performance on the training
# and validation sets could be extracted more directly but it is quick and easy to code:
perfs <- lapply(enron_split, perf)

perfsdf <- data_frame(value = unlist(perfs),
                      model = rep(c("Generalized Linear Model", "Random Forest", "Gradient Boosting Machine", "Deep Learning"), 3),
                      dataset = rep(c("Training", "Validation", "Testing"), each = 4),
                      abbrev = rep(names(perfs[[1]]), 3)) %>%
   mutate(errors = as.numeric(str_sub(str_extract(value, "=[0-9]+"), start = 2)),
          denom = as.numeric(str_sub(str_extract(value, "/[0-9]+"), start = 2)),
          errorrate = errors / denom,
          model = factor(model, levels = c("Deep Learning", "Generalized Linear Model", 
                                           "Random Forest", "Gradient Boosting Machine")),
          dataset = factor(dataset, levels = c("Training", "Validation", "Testing")))

perfsdf 

svg("../img/0078-results.svg", 8, 5)
perfsdf %>%
   ggplot(aes(x = errorrate, y = dataset, label = abbrev, colour = model)) +
   geom_path(aes(group = abbrev), alpha = 0.5, linetype = 2) +
   geom_text() +
   scale_colour_brewer(palette = "Set1") +
   scale_x_continuous("Error rate", label = percent) +
   labs(y = "", colour = "") +
   ggtitle("Error rates detecting ham from spam",
           "Enron email dataset, four different statistical learning methods")
dev.off()
