#---------load up functionality and fonts------------
devtools::install_github("ellisp/nzelect/pkg")
devtools::install_github("hadley/ggplot2") # latest version needed for subtitles and captions

library(MASS) # for rlm().  Load before dplyr to avoid "select" conflicts
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


library(nzelect)




png("../img/0038-green-labour.png", 1000, 1000, res = 100)
GE2014 %>%
   filter(VotingType == "Party" &
             Party %in% c("Green Party", "Labour Party")) %>%
   group_by(VotingPlace) %>%
   summarise(PropGreens = sum(Votes[Party == "Green Party"]) / sum(Votes)) %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   left_join(Meshblocks2013, by = c("MB2014" = "MB")) %>%
   select(PropGreens, MeanBedrooms2013:PropStudentAllowance2013) %>%
   gather(Variable, Value, -PropGreens) %>%
   mutate(Variable = gsub("2013", "", Variable),
          Variable = gsub("Prop", "Prop ", Variable)) %>%
   ggplot(aes(x = Value, y = PropGreens)) +
   facet_wrap(~Variable, scales = "free_x") +
   geom_point() +
   geom_smooth(method = "rlm", se = FALSE) +
   scale_y_continuous("Percentage of Labour and Green voters who voted Green Party in party vote", 
                      label = percent) +
   scale_x_continuous("", label = comma) +
   labs(caption = "Note: horizontal scales vary; and some proportions exceed 1.0 due to confidentialising.\nBlue lines are outlier-resistant robust regressions.") +
   ggtitle("Choosing between Labour and Green in the 2014 New Zealand General Election",
      subtitle = "Each point represents an individual voting location (vertical axis) and the meshblock within which it is located (horizontal axis).") +    theme(panel.margin = unit(1.5, "lines"))
dev.off()

svg("../img/0038-national.svg", 8, 7)
GE2014 %>%
   filter(VotingType == "Party") %>%
   group_by(VotingPlace) %>%
   summarise(PropGreens = sum(Votes[Party == "National Party"]) / sum(Votes)) %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   left_join(Meshblocks2013, by = c("MB2014" = "MB")) %>%
   dplyr::select(PropGreens, MeanBedrooms2013:PropStudentAllowance2013) %>%
   gather(Variable, Value, -PropGreens) %>%
   mutate(Variable = gsub("2013", "", Variable),
          Variable = gsub("Prop", "Prop ", Variable)) %>%
   ggplot(aes(x = Value, y = PropGreens)) +
   facet_wrap(~Variable, scales = "free_x") +
   geom_point() +
   geom_smooth(method = "rlm", se = FALSE) +
   scale_y_continuous("Percentage of party votes for National Party", 
                      label = percent) +
   scale_x_continuous("note: horizontal scales vary; and some proportions exceed 1.0 due to confidentialising\nBlue lines are outlier-resistant robust regressions",
                      label = comma) +
   ggtitle("Choosing the National Party in the 2014 New Zealand General Election\nEach point represents an individual voting location (vertical axis) and the meshblock within which it is located (horizontal axis)") + 
   theme(panel.margin = unit(1.5, "lines"))
dev.off()


