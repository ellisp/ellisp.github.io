

library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(showtext)
library(GGally) # for ggpairs
library(RColorBrewer)
library(gridExtra) # for grid.arrange

devtools::install_github("ellisp/nzelect/pkg")
library(nzelect)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))


#============overall results==============
GE2014 %>%
   mutate(VotingType = paste0(VotingType, "Vote")) %>%
   group_by(Party, VotingType) %>%
   summarise(Votes = sum(Votes)) %>%
   spread(VotingType, Votes) %>%
   select(Party, PartyVote, CandidateVote) %>%
   ungroup() %>%
   arrange(desc(PartyVote))

GE2014 %>%
   filter(VotingType == "Candidate" & Electorate == "Wellington Central 60") %>%
   group_by(Candidate, Party) %>%
   summarise(Votes = sum(Votes)) %>%
   ungroup() %>%
   mutate(Proportion = round(Votes / sum(Votes) * 100, 1)) %>%
   arrange(desc(Votes))


br <- c(1000, 10000, 100000, 10^6)
p1 <- GE2014 %>%
   mutate(VotingType = paste0(VotingType, "Vote")) %>%
   group_by(Party, VotingType) %>%
   summarise(Votes = sum(Votes)) %>%
   spread(VotingType, Votes) %>%
   select(Party, PartyVote, CandidateVote) %>%
   ungroup() %>%
   arrange(desc(PartyVote)) %>%
   ggplot(aes(x = CandidateVote, y = PartyVote, label = Party)) +
   geom_abline(slope = 1, intercept = 0) +
   geom_text(colour = "rosybrown", angle = -10) +
   geom_point() +
   coord_equal() +
   scale_x_log10("First past the post vote for candidates", 
                 label = comma, breaks = br) +
   scale_y_log10("Proportional representation vote for parties", 
                 label = comma, breaks = br)
svg("../img/0035-p1.svg", 6, 5)
print(p1)
dev.off()

#--------------proportions----------
proportions <- GE2014 %>%
   group_by(VotingPlace, VotingType) %>%
   summarise(ProportionLabour = sum(Votes[Party == "Labour Party"]) / sum(Votes),
             ProportionNational = sum(Votes[Party == "National Party"]) / sum(Votes),
             ProportionGreens = sum(Votes[Party == "Green Party"]) / sum(Votes),
             ProportionNZF = sum(Votes[Party == "New Zealand First Party"]) / sum(Votes),
             ProportionMaori = sum(Votes[Party == "Maori Party"]) / sum(Votes))

svg("../img/0035-pairs.svg", 8, 8)
   ggpairs(proportions, aes(colour = VotingType), columns = 3:7)
dev.off()
#----------------------------candidate v party vote for Labour and Greens---------------
theme_set(theme_light(8, base_family = "myfont"))

p1 <- proportions %>%
   select(VotingPlace, VotingType, ProportionLabour) %>%
   spread(VotingType, ProportionLabour) %>%
   ggplot(aes(x = Candidate, y = Party)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, colour = "blue", size = 2) +
   scale_x_continuous(label = percent) + 
   scale_y_continuous(label = percent) +
   ggtitle("In the 2014 General Election by voting location,\nLabour candidate vote usually exceeded the party vote") +
   labs(x = "Proportion of Candidate Vote for Labour",
        y = "Proportion of Party Vote for Labour") 



p2 <- proportions %>%
   select(VotingPlace, VotingType, ProportionGreens) %>%
   spread(VotingType, ProportionGreens) %>%
   ggplot(aes(x = Candidate, y = Party)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, colour = "blue", size = 2) +
   scale_x_continuous(label = percent) + 
   scale_y_continuous(label = percent) +
   ggtitle("\nThe situation is reversed for Greens party") +
   labs(x = "Proportion of Candidate Vote for Greens",
        y = "Proportion of Party Vote for Greens") +
   annotate("text", x = 0.4, y = 0.1, label = "Blue line shows\nequality of the two\nvoting types",
            colour = "blue")

tmp <- proportions %>%
   select(VotingPlace, VotingType, ProportionNational) %>%
   spread(VotingType, ProportionNational) 
p3 <- tmp %>%
   ggplot(aes(x = Candidate, y = Party)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, colour = "blue", size = 2) +
   scale_x_continuous(label = percent) + 
   scale_y_continuous(label = percent) +
   ggtitle("For the National Party, candidate and\nparty vote are more closely aligned, but not always") +
   labs(y = "Proportion of Party Vote for National Party",
        x = "Proportion of Candidate Vote for National Party") +
   stat_ellipse(data = filter(tmp, Party > 2 * Candidate & Party > 0.5), colour = "red") 

tmp <- proportions %>%
   select(VotingPlace, VotingType, ProportionNZF) %>%
   spread(VotingType, ProportionNZF) 
p4 <- tmp %>%
   ggplot(aes(x = Candidate, y = Party)) +
   geom_point() +
   geom_abline(intercept = 0, slope = 1, colour = "blue", size = 2) +
   scale_x_continuous(label = percent) + 
   scale_y_continuous(label = percent) +
   ggtitle("New Zealand First generally does better\nfor the party than candidates, but not always") +
   labs(x = "Proportion of Candidate Vote for New Zealand First Party",
        y = "Proportion of Party Vote for New Zealand First Party") +
   stat_ellipse(data = filter(tmp, Candidate > 1.6 * Party & Candidate > 0.15), colour = "red") 

svg("../img/0035-cand-v-party.svg", 9, 9)
grid.arrange(p1, p2, p3, p4)
dev.off()

png("../img/0035-cand-v-party.png", 900, 900, res = 100)
grid.arrange(p1, p2, p3, p4)
dev.off()

#---------point locations----------
# theme_map proviaged by briatte@GitHub
source("https://gist.githubusercontent.com/briatte/4718656/raw/2c4e71efe6d46f37e7ea264f5c9e1610511bcb09/ggplot2-map-theme.R")

png("../img/0035-map.png", 700, 500, res = 100)
GE2014 %>%
   filter(VotingType == "Party") %>%
   group_by(VotingPlace) %>%
   summarise(ProportionNational = sum(Votes[Party == "National Party"] / sum(Votes))) %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   filter(VotingPlaceSuburb != "Chatham Islands") %>%
   mutate(MostlyNational = ifelse(ProportionNational > 0.5, 
                                  "Mostly voted National", "Mostly didn't vote National")) %>%
   ggplot(aes(x = WGS84Longitude, y = WGS84Latitude, colour = ProportionNational)) +
   geom_point() +
   facet_wrap(~MostlyNational) +
   coord_map() +
   borders("nz") +
   scale_colour_gradient2("Voted\nNational", label = percent, mid = "grey80", midpoint = 0.5) +
   theme_map(base_family = "myfont") +
   theme(legend.position = c(0.04, 0.55)) +
   ggtitle("Voting patterns in the 2014 General Election\n")
dev.off()

#------------------------rolling up---------------------
GE2014 %>%
   filter(VotingType == "Party") %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   group_by(REGC2014_N) %>%
   summarise(
      TotalVotes = sum(Votes),
      ProportionNational = round(sum(Votes[Party == "National Party"]) / TotalVotes, 3)) %>%
   arrange(ProportionNational)


p6 <- GE2014 %>%
   filter(VotingType == "Party") %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   group_by(TA2014_NAM) %>%
   summarise(
      TotalVotes = sum(Votes),
      ProportionNational = round(sum(Votes[Party == "National Party"]) / TotalVotes, 3)) %>%
   arrange(desc(ProportionNational)) %>%
   mutate(TA = ifelse(is.na(TA2014_NAM), "Special or other", as.character(TA2014_NAM)),
          TA = gsub(" District", "", TA),
          TA = gsub(" City", "", TA),
          TA = factor(TA, levels = TA)) %>%
   ggplot(aes(x = ProportionNational, y = TA, size = TotalVotes)) +
   geom_point() +
   scale_x_continuous("Proportion voting National Party", label = percent) +
   scale_size("Number of\nvotes cast", label = comma) +
   labs(y = "", title = "Voting in the New Zealand 2014 General Election by Territorial Authority")

svg("../img/0035-ta.svg", 6, 6)
   print(p6)
dev.off()
