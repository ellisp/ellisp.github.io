library(quanteda)
library(tidytext)
library(tidyverse)
library(scales)
library(stringr)
library(wordcloud)
library(viridis)
library(grid)
library(testthat)

projdir <- getwd()

#==============data downloads and prep========================
# The Democratic-Republican party colours were red, white and blue but I need something distinctive.
parties <- c(None = "grey50", Federalist = "black", `Democratic-Republican` = "darkgreen",
             Whig = "orange", Republican = "red", Democrat = "blue")


# originally copied this from http://abcnews.go.com/Politics/full-text-president-donald-trumps-inauguration-speech/story?id=44915821
trump <- paste(readLines("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/trump_inauguration.txt"),
               collapse = " ")
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

words <- inaugural %>%
   group_by(inauguration, word, year, president) %>%
   summarise(count = n()) %>%
   bind_tf_idf(word, inauguration, count) %>%
   ungroup() 
# note - the sentiment matching seems very poor.  Many words with obvious sentiment
# are missed.  so not doing any sentiment analysis for now.

expect_equal(nrow(inaugural), sum(words$count))

# combine with the total count each word used in all speeches
all_usage <- words %>%
   group_by(word) %>%
   summarise(total_count = sum(count)) %>%
   arrange(desc(total_count))

expect_equal(sum(all_usage$total_count), sum(words$count))

words <- words %>%
   left_join(all_usage, by = "word")

inaugs <- unique(inaugural$inauguration)


#=====================references to particular words================
# time series chart

# these were originally sourced from http://www.enchantedlearning.com/history/us/pres/list.shtml and re-typed

presidents <- read.csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/presidents.csv",
                       skip = 3, stringsAsFactors = FALSE) %>%
   filter(!is.na(year)) %>%
   select(inauguration, party)

annotations <- data_frame(word = c("america", "democracy", "protect", "free"),
                          lab = c("Peaks post cold-war:",
                                  "First peaks with the war against fascism:",
                                  "Barely used in the 20th century.",
                                  "First peaks during the cold war:"),
                          y = c(2, .5, 0.4, 1.2) / 100
)

svg("../img/0080-overtime.svg", 9, 8.5)
words %>%
   mutate(word = ifelse(grepl("americ", word), "america", word),
          word = ifelse(grepl("democra", word), "democracy", word),
          word = ifelse(grepl("protect", word), "protect", word),
          word = ifelse(grepl("free", word), "free", word)) %>%
   group_by(inauguration, president, year, word) %>%
   summarise(count = sum(count)) %>% 
   group_by(inauguration, president, year) %>%
   mutate(relative_count = count / sum(count)) %>%
   filter(word %in% c("america", "free", "democracy", "protect")) %>%
   left_join(presidents, by = "inauguration") %>% 
   ggplot(aes(x = year, y = relative_count, label = president)) +
   #   geom_line(alpha = 0.3) +
   geom_text(size = 3, aes(colour = party)) +
   facet_wrap(~word, ncol = 1, scales = "free_y") +
   ggtitle("Changing use of selected words in inaugural Presidential addresses",
           "Presidents labelled if they used the word or a variant.") +
   labs(x = "", y = "Number of times used as a percentage of all words", caption = "http://ellisp.github.io") +
   scale_colour_manual("", values = parties) +
   scale_y_continuous(label = percent) +
   geom_text(data = annotations, x = 1935, aes(y = y, label = lab), colour = "grey50", hjust = 1) +
   theme(strip.text = element_text(size = 15, face = "bold"))
dev.off()



#===============wordcloud by unique words for each speech============
# ok analysis of distinct words at http://www.vocativ.com/394280/first-words-inaugural-address-trump-break-tradition/
# analysis of most common words at https://qz.com/889611/the-most-frequently-used-words-in-donald-trumps-inauguration-speech-and-every-previous-one/

set.seed(123)
svg("../img/0080-unique-words.svg", 12, 9)
par(family = "myfont", bg = "black", mfrow = c(2, 2))
our_inaugs <- c("1993-Clinton", "2001-Bush", "2009-Obama", "2017-Trump")
cols <- c("steelblue", "darkred", "steelblue", "darkred")
for(i in 1:length(our_inaugs)){
   newwords <- words %>%
      filter(total_count == count) %>%
      filter(inauguration == our_inaugs[[i]]) %$%
      wordcloud(words = .$word, 
                colors = terrain.colors(15), random.color = TRUE, scale = c(1.1, 0.9))
   title(main = gsub("-", " ", our_inaugs[[i]]), col.main = cols[i])
}

grid.text(0.5, 0.53, label = "Unique words in inauguration speeches of recent in-coming US Presidents",
          gp = gpar(col = "grey90", fontfamily = "myfont", fontface = "bold", cex = 1.5))
dev.off()



#=================wordcloud by tf-idf=================

Cairo::CairoPDF("../img/0080-distinctive-presid-words.pdf", 12, 12)
setwd("C:/temp")
for(i in 1:length(inaugs)){

   the_party <- presidents[presidents$inauguration == inaugs[[i]], "party"]   
   
   palfun <- colorRampPalette(c("white", parties[the_party]))
   
   the_data <- subset(words, inauguration == inaugs[[i]]) %>%
      arrange(desc(tf_idf)) %>%
      slice(1:80) %>%
      arrange(tf_idf) %>%
      mutate(fading_colour = palfun(80))
   
   # Uncomment next line if making PNG frames for a GIF
   # png(paste0(inaugs[[i]], ".png"), 1200, 1200, res = 100)
   showtext.opts(dpi = 100)
   par(family = "myfont")
   wordcloud(the_data$word, 
             freq = the_data$tf_idf * max(the_data$tf_idf)* 50, 
             colors = the_data$fading_colour,
             # uncomment the next line to put all words on the same scale, which makes many
             # individual wordclouds extremely small:
             # scale = c(6 * max(the_data$tf_idf) / max(words$tf_idf), 0.5),
             random.order = FALSE, random.color = FALSE, rot.per = 0)
   title(main = inaugs[[i]])
   grid.text(0.5, 0.05, label = "Most distinctive words used in US Presidential inauguration speeches",
             gp = gpar(fontfamily = "myfont", color = "grey50"))
   # Uncomment next line if making PNG frames for a GIF
   # dev.off()
}
dev.off()

system('magick -loop 0 -delay 200 *.png "distinctive-presid-words.gif"')

# move the asset over to where needed for the blog.  This is specific to my folder structure.
file.copy("distinctive-presid-words.gif", paste0(projdir, "/../img/0080-distinctive-presid-words.gif"), overwrite = TRUE)


setwd(projdir)



#================Three most distinctive words==============
# don't think I'll use this
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

#============save PNG versions=========
setwd("../img")
files <- list.files()
files <- files[grepl("^0080.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")

#=====checks==
words %>%
   filter(inauguration == "2017-Trump") %>%
   mutate(prop = round(count / sum(count) * 100, 2)) %>%
   arrange(desc(prop)) %>%
   select(word, count, prop, total_count) %>% 
   summarise(prop = sum(prop), count = sum(count))
