library(foreign)
library(survey)
library(dplyr)
library(tidyr)
library(forcats)
library(gridExtra)

# Data downloaded from http://www.nzes.org/exec/show/data and because
# they want you to fill in a form to know who is using the data, I
# won't re-publish it myself
unzip("D:/Downloads/NZES2014GeneralReleaseApril16.sav.zip")


nzes <- read.spss("NZES2014GeneralReleaseApril16.sav", 
                  to.data.frame = TRUE, trim.factor.names = TRUE)

varlab <- cbind(attributes(nzes)$variable.labels)
varlab["dredincome",]

# info on weights here: http://www.nzes.org/data/NZES_weights.pdf
# weights:
# dsampwt            "Weight to correct over sampling" # ie of Maori                                                
# ddemwt             "Weight for age, gender, and education"                                                                       
# dwtfin             "Weight for all previous plus votes and nonvotes" ie after comparing to actual voting behaviour

sum(nzes$dwtfin) # weights are chosen to sum to sample size, rather than population
nrow(nzes)

# Main parties, in rough sequence of conservative to progressive
mainparties <- c("No Vote", "Conservative", "National", "NZ First", "Labour", "Green", "Internet?Mana Party", "Other party")
nzes <- nzes %>%
   mutate(vpartysum = ifelse(nzes$dvpartyvote %in% mainparties, as.character(nzes$dvpartyvote), "Other party"),
          vpartysum = factor(vpartysum, levels = mainparties),
          vpartysum = fct_recode(vpartysum, "Internet /\nMana Party" = "Internet?Mana Party"))

# create a survey design object for easy of analysing with correct weights, etc - 
# this is particularly important if doing modelling down the track
nzes_svy <- svydesign(~1, data = nzes, weights = ~dwtfin)

varlab["dfinance", ]
svytable(~dfinance, design = nzes_svy, round = TRUE)


#-------------Example analayis - attitudes to the Dirty Politics book---------------
dirtypol <- svytable(~ vpartysum + ddirtypol, nzes_svy)

# Mosaic plot.  Difficult for non-specialists to interpret
svg("../img/0057-mosaic.svg", 8, 8)
par(family = "myfont")
mosaicplot(dirtypol[ ,-5], shade = TRUE, las = 2, 
           ylab = varlab["ddirtypol", ], 
           xlab = "Party vote", main = "")
dev.off()


# Easier will be to draw two bar charts

# party colours taken from https://en.wikipedia.org/wiki/List_of_political_parties_in_New_Zealand
# except for "other or no vote" which matches "don't know" in p3 plot
party_cols <- c("grey90", "#00AEEF", "#00529F", "black", "#d82a20", "#098137", "#770808", "grey80", "#770808", 
                brewer.pal(5, "PuRd")[1])
names(party_cols) <- c(mainparties, "Internet /\nMana Party", "Other or no vote")

total1 <- dirtypol %>%
   as.data.frame() %>%
   group_by(ddirtypol) %>%
   summarise(Freq = sum(Freq)) %>%
   mutate(vpartysum = "TOTAL\nregardless of party")

p3 <- dirtypol %>%
   as.data.frame() %>%
   rbind(total1) %>%
   group_by(vpartysum) %>%
   mutate(ddirtypol = fct_rev(ddirtypol)) %>%
   mutate(prop = Freq / sum(Freq)) %>%
   mutate(ddirtypol = fct_relevel(ddirtypol, levels(ddirtypol)[2:5])) %>%
   ggplot(aes(x = vpartysum, weight = prop, fill = ddirtypol)) +
   geom_bar(position = "stack", width = 0.75) +
   geom_vline(xintercept = 9, colour = "grey60", size = 14, alpha = 0.3) +
   geom_bar(position = "stack", width = 0.75) +
   theme(legend.position = "right") +
   scale_fill_brewer("", palette = "PuRd", direction = -1, guide = guide_legend(reverse = FALSE)) +
   scale_y_continuous("", label = percent) +
   labs(x = "") +
   coord_flip() +
   ggtitle("Voter views on 'How much truth in Nicky Hager's Dirty Politics book'",
           subtitle = "Compared to party vote in the 2014 General Election\n\n
           Views about Mr Hagar's book, within each group of party voters") +
   theme(legend.margin = unit(.80, "lines")) 
  

total2 <- dirtypol %>%
   as.data.frame() %>%
   group_by(vpartysum) %>%
   summarise(Freq = sum(Freq)) %>%
   mutate(ddirtypol = "TOTAL regardless\nof views on book") 

p4 <- dirtypol %>%
   as.data.frame() %>%
   rbind(total2) %>%
   mutate(vpartysum = fct_collapse(vpartysum, "Other or no vote" = c("Other party", "No Vote"))) %>%
   mutate(vpartysum = fct_relevel(vpartysum, levels(vpartysum)[2:7])) %>%
   group_by(vpartysum, ddirtypol) %>%
   summarise(Freq = sum(Freq)) %>%
   group_by(ddirtypol) %>%
   mutate(prop = Freq / sum(Freq)) %>%
   ggplot(aes(x = ddirtypol, weight = prop, fill = vpartysum)) +
   geom_bar(position = "stack", width = 0.8) +
   geom_vline(xintercept = 6, colour = "grey60", size = 38, alpha = 0.3) +
   geom_bar(position = "stack", width = 0.8) +
   theme(legend.position = "right") +
   scale_fill_manual("Party vote", values = party_cols, guide = guide_legend(reverse = TRUE)) +
   scale_y_continuous("\n\n\n\nProportion voting for each party\n", label = percent) +
   labs(x = "", caption = "Source: New Zealand Election Study, analysed in http://ellisp.github.io") +
   ggtitle("",
           subtitle = "             Party votes, within each category of views about Mr Hagar's book")
   
svg("../img/0057-hagar.svg", 10, 10)
grid.arrange(p3, p4, ncol = 1)
dev.off()

png("../img/0057-hagar.png", 1000, 1000, res = 100)
grid.arrange(p3, p4, ncol = 1)
dev.off()

# Views about Mr Hagar's book, within each group of party voters (columns add to 100)
round(prop.table(xtabs(Freq ~ ddirtypol + vpartysum, data = as.data.frame(dirtypol)), margin = 2) * 100, 0)

# Party votes, within each category of views about Mr Hagar's book (rows add to 100)
round(prop.table(xtabs(Freq ~ ddirtypol + vpartysum, data = as.data.frame(dirtypol)), margin = 1) * 100, 0)




