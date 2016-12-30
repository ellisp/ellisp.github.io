
# this relates to script 0076

#==============larger test=====================
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/docword.nytimes.txt.gz",
              destfile = "docword.nytimes.txt.gz", mode = "wb")


gunzip("docword.nytimes.txt.gz")

nyt <- fread("docword.nytimes.txt", skip = 3, sep = " ", nrows = -1)  
names(nyt) <- c("doc", "wordid", "count")

dim(nyt)
length(unique(nyt$doc)) #299752, should be about 300000
sum(nyt$count) # 99.542 million should be about 100 million

# 102660 words in the official vocab, but only 101636 distinct words in the data file.  So we need to be careful...
nyt_vocab <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/vocab.nytimes.txt")$V1
vocab_df <- data.frame(word = nyt_vocab, wordid = 1:length(nyt_vocab))

# the wordStem part of this next operation pushes my 12GB RAM machine to its limit and takes 5-10 minutes
nyt2 <- nyt %>%
   # attach the names of words back to the words
   left_join(vocab_df, by = "wordid") %>%
   select(-wordid) %>%
   # stopwords are meant to be already removed but no harm in checking:
   anti_join(stop_words, by = "word") %>%
   # reduce words to their stems (otherwise abandon, abandoning, abandons all treated as different, etc):
   mutate(word = wordStem(word)) %>%
   group_by(doc, word) %>%
   summarise(count = sum(count)) 

# how many words per article?
nyt2 %>%
   group_by(doc) %>%
   summarise(words = sum(count)) %>%
   ggplot(aes(x = words)) +
   geom_density() +
   scale_x_sqrt()

# OK, quite a variety of words per article.
# Convert counts to *relative* count (ie proportion of words in each doc)
# need to do this as otherwise the main dimension in the data is just the 
# length of each blog post
nyt3 <- nyt2 %>%
   group_by(doc) %>%
   mutate(count = count / sum(count))

# another expensive operation, about 2 minutes, just scrapes in with existing RAM:
nyt_M <- nyt3 %>%
   cast_sparse(doc, word, count)

# clean up
rm(nyt, nyt2, nyt3); gc()

h2o.init(nthreads = -1, max_mem_size = "10G")

# 'problem too large' 
nyt_h1 <- as.h2o(nyt_M) 
colnames(nyt_h1) <- colnames(nyt_M)


final_k <- 23
m1 <- h2o.kmeans(nyt_h1, x= colnames(nyt_h1), estimate_k = FALSE, k = final_k, standardize = FALSE, init = "Random")   


