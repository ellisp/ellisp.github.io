---
layout: post
title: US Presidential inauguration speeches
date: 2017-01-23
tag: 
   - Text
   - History
   - R
description: I do some basic textual analysis and visualization with US Presidential inauguration speeches.
image: /img/0080-overtime.svg
socialimage: http://ellisp.github.io/img/0080-overtime.png
category: R
---

There's rightly been a lot of attention paid to USA President Trump's first speech as President at the time of his inauguration.  The speech broke with tradition by extending the acrimonious atmosphere of the election campaign into the Presidency.  There's been an instant flurry of analysis, with the Washington Post leading the way with [this analysis](https://www.washingtonpost.com/news/the-fix/wp/2017/01/20/trumps-inaugural-address-was-demonstrably-bleak/?utm_term=.387fc68317a7) of words that have not been used in any prior US presidential inauguration speeches.  All this, and the sense of major things going on in the world, prompted me to see what I could find myself in the historical speeches and President Trump's latest addition.

I got the historical texts from the `quanteda` R package, which includes the inauguration speeches up to 2013 as an example dataset.  There are plenty of human-readable alternatives such as [this one from the Yale Law School](http://avalon.law.yale.edu/subject_menus/inaug.asp).  I grabbed the transcript of Trump's speech directly from the web and just pasted it into a text document.  

I ended up with three main pieces of exploratory analysis:

- Tracking three key words *over time*
- *Unique* words
- *Distinctive* words

## Key words over time

I took four words that have particular resonances in US politics: freedom, democracy, protection and America.  

"America" featured very prominently in Trump's address, including the revival of a [1940s "America First" slogan](https://en.wikipedia.org/wiki/America_First_Committee) that was aimed at keeping the USA out of the European war against Nazism; but I didn't realise how prominently "America" was until I'd counted the words.  Used 35 times out of 1,455 words, fully one word in 40 of Trump's was "America" or a variant.  But while this was a record, it wasn't out of the league of precedent; Bill Clinton, George W. Bush and Richard Nixon all had more than 1% of their words as "America" or variant.  

As the chart below shows, Trump's use of "protect" and "protection" was more unusual and distinctive.  With uncomfortable economic connotations since being blamed for the 1930s Great Depression, and with additional negative connotations for some of a paternalist state, "protectionism" and its variants were barely mentioned in inauguration speeches in the century before Trump.  One has to go back to the [speech of President Polk in 1845](http://avalon.law.yale.edu/19th_century/polk.asp) to find comparable usage.  Protectionism was a hot topic in 1845, with the pro free-trade agricultural interests paticularly in the south challenged by infant industries, particularly in the northern states, and within twenty years it was an important contributing cause of the Civil War.  Polk managed to walk a thin and dangerous line between the two camps, and reading his inauguration speech it is interesting to see the multiple uses he puts the word "protect" to before bringing up the matter of tariffs.

<img src = '/img/0080-overtime.svg' width = '750'>

Of my remaining two words, President Trump made no reference to "democracy" and little to "free".  

It's interesting to see the historical ebb and flow in all these words, reflected in the four-yearly ritual of inaugurating a new president.  Discussion of "democracy" peaked under Roosevelt during the war with Germany and Japan, and had a revival at the end of the twentieth century in the period between the Cold War and the "War on Terror".  "Free" as a word was most popular in Cold War speeches, with a second spike in George W. Bush's 2005 inauguration.

## Unique words

The *Washington Post* analysis already referred to gave considerable emphasis to the unique words in President Trump's speech.  This prompted me to have a look at the first-term speeches of all of the last four Presidents, visualised in the graphic below: 

<img src = '/img/0080-unique-words.svg' width = '900'>

The stark imagery and agressive language and delivery of Trump's speech was confronting listening or reading: first use of the words sad, disrepair, bleed, ripped, stealing, windswept, depletion and of course "American carnage" which will most likely become the most remembered phrase from the speech.  We see that President Obama's first address in 2009 also introduced many words including icy, specter, sweatshops, stale, tanks, grudgingly, greed and grievance, but the humble wordclouds fail to communicate the difference in tone between President Trump and all three of his predecessors.  Overall, I'm not too pleased with this particular "first use of word" analysis as giving much in the way of insight.

## Distinctive words

Combining the two streams of analysis above, I wanted to do something a little more sophisticated than simply identifying the *unique* words used by particular Presidents.  In the graphic below I go the next step and identify words which are most *distinctive* to a particular Presidential address.  These are words that might be used elsewhere, but are most characteristic of a particular speech.  The size and colour of the words below are mapped to the ["tf-idf" ie "term frequency - inverse document frequency"](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) statistic, which compares the frequency of each word in a speech with how often it is used by other documents in the corpus (in this case, other speeches).

<img src = '/img/0080-distinctive-presid-words.gif' width = '750'>

Interested readers might prefer to [access this as a PDF](/img/0080-distinctive-presid-words.pdf), allowing you to linger over individual wordclouds.  Overall, I'm much happier with this as a piece of exploratory analysis, and in fact it gives a surprisingly compelling snapshot of a slice of US history.  Some interesting snippets:

- Madison's 1813 inauguration understandably made considerable mention of the 1812 war with Britain, part of the great global confrontation of the Napoleonic wars which led to both Moscow and Washington in flames, with prominence in the speech given to "british", "massacre", "cruel", "savage", "prisoners" and "captives"
- While we've seen Polk in 1845 made extensive mention of protectionism and its variants, in the graphic above we see that the really distinctive element of his speech was the discussion of Texas, which at that time was seeking annexation to the USA in the face of opposition from Mexico and from southern US states.
- McKinley's inauguration in 1901 shortly after the Spanish - USA war contains considerable mention of Cuba and the Philippines
- Hoover's 1929 speech included discussion of the 18th amendment to the Constitution, on prohibition of alcohol
- As we've already seen, democracy is prominent in several of F.D.R Roosevelt's speeches
- Truman's 1949 speech, to a post-War world entering both the Cold War and a period of unprecedented prosperity and growth, introduced both "communism" and the technocratic term "program", as well as terms such as "technical", "scientific", "regime", "economic" and "countries".
 
Plenty of other snippets of interesting detail hinted at too.  In fact, I do recommend browsing through the full texts of these interesting and often powerful historical documents.  No bag of words analysis like this will give you "the better angels of our nature" (Lincoln 1861), "the only thing we have to fear is fear itself" (Roosevelt 1933) and "ask not what your country can do for you; ask what you can do for your country" (Kennedy 1961).

## Code

The code to produce all this in R depends heavily on Julia Silge and David Robinson's `tidytext` package and the philosophy set out in their [Tidy Text Mining](http://tidytextmining.com/) book.

{% highlight R %}
library(quanteda)
library(tidytext)
library(tidyverse)
library(scales)
library(stringr)
library(wordcloud)
library(viridis)
library(grid)
library(testthat)

#==============data downloads and prep========================
# Create a named vector of colours for representing the different parties, using
# later in both ggplot and base graphics (wordcloud).
# The Democratic-Republican party colours were red, white and blue but I need something distinctive.
parties <- c(None = "grey50", Federalist = "black", `Democratic-Republican` = "darkgreen",
             Whig = "orange", Republican = "red", Democrat = "blue")


# Get the Trump speech
# originally copied this from http://abcnews.go.com/Politics/full-text-president-donald-trumps-inauguration-speech/story?id=44915821
trump <- paste(readLines("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/trump_inauguration.txt"),
               collapse = " ")
trump_df <- data_frame(fulltext = trump,
                       inauguration = "2017-Trump")

# Combine with all the other speeches, and break down into tokens (words),
# one row per word, in order:
inaugural <- data_frame(fulltext = data_char_inaugural,
			inauguration = names(data_char_inaugural)) %>%
   rbind(trump_df) %>%
   mutate(year = as.numeric(str_sub(inauguration, 1, 4)),
          president = str_sub(inauguration, start = 6)) %>%
   unnest_tokens(word, fulltext, token = "words") %>%
   group_by(inauguration) %>%
   mutate(sequence = 1:n()) 

# aggregate / count how many occurances of word in each speech:
words <- inaugural %>%
   group_by(inauguration, word, year, president) %>%
   summarise(count = n()) %>%
   bind_tf_idf(word, inauguration, count) %>%
   ungroup() 
# note - sentiment matching was very poor.  Many words with obvious sentiment
# are missed.  so not doing any sentiment analysis for now.

expect_equal(nrow(inaugural), sum(words$count))

# combine with the total count each word used in all speeches:
all_usage <- words %>%
   group_by(word) %>%
   summarise(total_count = sum(count)) %>%
   arrange(desc(total_count))

expect_equal(sum(all_usage$total_count), sum(words$count))

words <- words %>%
   left_join(all_usage, by = "word")

# vector of all inaugurations (eg '1961-Kennedy'), use later for looping through:
inaugs <- unique(inaugural$inauguration)

#=====================references to particular words================
# time series chart

# these presidential parties were originally sourced from http://www.enchantedlearning.com/history/us/pres/list.shtml and re-typed
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
   geom_text(size = 3, aes(colour = party)) +
   facet_wrap(~word, ncol = 1, scales = "free_y") +
   ggtitle("Changing use of selected words in inaugural Presidential addresses",
           "Presidents labelled if they used the word or a variant.") +
   labs(x = "", y = "Number of times used as a percentage of all words", caption = "http://ellisp.github.io") +
   scale_colour_manual("", values = parties) +
   scale_y_continuous(label = percent) +
   geom_text(data = annotations, x = 1935, aes(y = y, label = lab), colour = "grey50", hjust = 1) +
   theme(strip.text = element_text(size = 15, face = "bold"))

#===============wordcloud by unique words for each speech============

set.seed(123)
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

#=================wordcloud by tf-idf=================

for(i in 1:length(inaugs)){

   the_party <- presidents[presidents$inauguration == inaugs[[i]], "party"]   
   
   # create palette of colours, suitable for the particular party
   palfun <- colorRampPalette(c("white", parties[the_party]))
   
   the_data <- subset(words, inauguration == inaugs[[i]]) %>%
      arrange(desc(tf_idf)) %>%
      slice(1:80) %>%
      arrange(tf_idf) %>%
      mutate(fading_colour = palfun(80))
   
   png(paste0(inaugs[[i]], ".png"), 1200, 1200, res = 100)
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
   dev.off()
}

# tie the PNG frames together into a single animated GIF:
system('magick -loop 0 -delay 200 *.png "distinctive-presid-words.gif"')
{% endhighlight %}