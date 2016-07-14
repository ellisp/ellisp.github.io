
library(nzelect)
library(ggplot2)
library(scales)
library(dplyr)
library(ggmap)
library(ggthemes) # for theme_map

# download background map of Christchurch
background2 <- get_map("Christchurch", maptype = "toner-background")

# Extract just the votes physically case in Christchurch
the_data <- GE2014 %>%
   filter(VotingType == "Party") %>%
   group_by(VotingPlace) %>%
   summarise(VotesNational = sum(Votes[Party == "National Party"]) / sum(Votes),
             VotesLabour = sum(Votes[Party == "Labour Party"]) / sum(Votes),
             VotesGreens = sum(Votes[Party == "Green Party"]) / sum(Votes),
             TotalVotes = sum(Votes)) %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   filter(TA2014_NAM == "Christchurch City")

# draw a map
p1 <- ggmap(background2) +
   geom_point(data = the_data, aes(x = WGS84Longitude, y = WGS84Latitude, colour = VotesNational, size = TotalVotes)) +
   geom_point(data = the_data, aes(x = WGS84Longitude, y = WGS84Latitude, size = TotalVotes), colour = "grey35", shape = 1) +
   theme_map(base_family = "myfont") +
   theme(legend.position = "right") +
   coord_map(xlim = range(the_data$WGS84Longitude), ylim = range(the_data$WGS84Latitude)) +
   scale_colour_gradientn("Party vote\nNational", 
                          colours = c("darkred", "pink", "lightgoldenrod", "lightblue", "darkblue"), label = percent) +
   ggtitle("Party vote by voting place in Christchurch", subtitle = "New Zealand General Election 2014") +
   scale_size_area("Total votes", label = comma) +
   labs(caption = ("Analysis by http://ellis.github.io\nBackground tiles by Stamen Design under Creative Commons CC BY 3.0"))

svg("../img/0049-map.svg", 7, 6)
   print(p1)
dev.off()

png("../img/0049-map.png", 700, 600, res = 100)
print(p1)
dev.off()