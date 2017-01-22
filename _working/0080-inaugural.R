library(quanteda)
library(tidytext)
library(tidyverse)
library(stringr)
library(wordcloud)
library(viridis)

# originally copied this from http://abcnews.go.com/Politics/full-text-president-donald-trumps-inauguration-speech/story?id=44915821
trump <- paste(readLines("../data/trump_inauguration.txt"), collapse = " ")
trump_df <- data_frame(fulltext = trump,
                       inauguration = "2017-Trump")

inaugural <- data_frame(fulltext = data_char_inaugural,
   inauguration = names(data_char_inaugural)) %>%
   rbind(trump_df) %>%
   mutate(year = as.numeric(str_sub(inauguration, 1, 4)),
          president = str_sub(inauguration, start = 6)) %>%
   unnest_tokens(word, fulltext, token = "words") %>%
   group_by(inauguration) %>%
   mutate(sequence = 1:n()) 

palfun <- colorRampPalette(c("white", "brown"))


words <- inaugural %>%
   group_by(inauguration, word) %>%
   summarise(count = n()) %>%
   bind_tf_idf(word, inauguration, count) %>%
   ungroup() %>%
   arrange(desc(tf_idf)) %>%
   left_join(get_sentiments("bing"), by = "word") %>%
   mutate(sentiment_col = ifelse(sentiment == "negative", "red", "blue"),
          sentiment_col = ifelse(is.na(sentiment), "grey", sentiment_col))

# note - the sentiment matching seems very poor.  Many words with obvious sentiment
# are missed.

inaugs <- unique(inaugural$inauguration)

projdir <- setwd("C:/temp")
Cairo::CairoPDF("combined.pdf", 12, 12)
for(i in 1:length(inaugs)){
#for(i in 1:5){
   
   
   the_data <- subset(words, inauguration == inaugs[[i]]) %>%
      slice(1:80) %>%
      arrange(tf_idf) %>%
      mutate(fading_colour = palfun(80))
   
   # png(paste0(inaugs[[i]], ".png"), 4000, 4000, res = 100)
   par(family = "myfont")
   wordcloud(the_data$word, 
             freq = the_data$tf_idf * max(the_data$tf_idf)* 50, 
             colors = the_data$fading_colour,
             # uncomment the next line to put all words on the same scale, which makes many
             # individual wordclouds extremely small:
             # scale = c(6 * max(the_data$tf_idf) / max(words$tf_idf), 0.5),
             random.order = FALSE, random.color = FALSE, rot.per = 0)
             

   title(main = inaugs[[i]])
   
}
dev.off()
setwd(projdir)

#================

words %>%
   mutate(inauguration = gsub("-", " ", inauguration)) %>%
   group_by(inauguration) %>%
   arrange(desc(tf_idf)) %>%
   slice(1:3) %>%
   mutate(order = factor(1:3, 
                         labels = c("Most distinctive word", "Second most distinctive word", 
                                    "Third most distinctive word"))) %>%
   ungroup() %>%
   arrange(order, inauguration) %>%
   mutate(alternating = factor(rep(1:2, length.out = n()))) %>%
   ggplot(aes(x = count, y = inauguration, label = word, colour = alternating)) +
   geom_text(hjust = 0.5) +
   facet_wrap(~order, nrow = 1) +
   ggtitle("The most distinctive word from each President's inauguration speech") +
   xlim(-5, 35) +
   scale_colour_manual(values = c("darkgreen", "black")) +
   labs(x = "Number of times word used", y = "")  +
   theme(legend.position = "none")
