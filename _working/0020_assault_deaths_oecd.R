library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(tidyr)
library(showtext) # for fonts

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_grey(base_family = "myfont"))

# violent assault leading to death
viol <- read.csv("../data/HEALTH_STAT_21112015041404959.csv", stringsAsFactors = FALSE) %>%
   mutate(Unit = gsub(" (standardised rates)", "", Unit, fixed = TRUE),
          Unit = factor(Unit, levels = unique(Unit)[c(2, 1, 3)])) %>%
   # not enough Turkey data so filter out:
   filter(Country != "Turkey")
   

viol_sum <- viol %>%
   filter(grepl("per 100 000", Unit))   %>%
   filter(Year > 1990) %>%
   group_by(Country, Unit) %>%
   summarise(Value = mean(Value)) %>%
   ungroup()

totals <- viol_sum %>%
   group_by(Country) %>%
   summarise(Value = mean(Value)) %>%
   arrange(Value)

viol_spread <- viol_sum %>%
   mutate(Unit = as.character(Unit),
          Unit = ifelse(grepl("female", Unit), "Female", Unit),
          Unit = ifelse(grepl(" males", Unit), "Male", Unit),
          Unit = ifelse(grepl("population", Unit), "Population", Unit)) %>%
   spread(Unit, Value) %>%
   mutate(Country = factor(Country, levels = totals$Country))

svg("../img/0020-deaths-trends.svg", 10, 10)
print(
   viol %>%
      filter(grepl("per 100 000", Unit))   %>%
      #filter(!Country %in% c("Colombia", "Russia", "Mexico", "Estonia", "Brazil", "Lithuania", "Latvia", "South Africa", "Chile", "Costa Rica", "United States")) %>%
      mutate(Country = factor(Country, levels = totals$Country)) %>%
      ggplot(aes(x = Year, y = Value, colour = Unit)) +
      facet_wrap(~Country, scales = "free_y", ncol = 5) +
      #facet_wrap(~Country) +
      geom_smooth(se = FALSE, method = "loess") +
      geom_point(alpha = 0.8, size = 1) +
      scale_colour_manual("", values = c("red", "grey10", "blue")) +
      theme(legend.position = "bottom") +
      labs(y = "Deaths per 100,000 per year - note changing vertical scale", 
           title = "Deaths from violent assault", x = "")
)
dev.off()



p1 <- viol_sum %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   mutate(Label = ifelse(grepl("population", Unit), as.character(Country), "|")) %>%
   ggplot(aes(x = Value, y = Country)) +
   geom_segment(data = viol_spread, aes(y = Country, yend = Country, x = Male, xend = Female),
                colour = "white", size = 3) +
   geom_text(size = 4, aes(label = Label, colour = Unit), alpha = 0.8,
             gp = gpar(fontfamily = "myfont")) +
   labs(y = "") +
   scale_x_log10(("Deaths per 100,000 (logarithmic scale)")) +
   theme(legend.position = "bottom") +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   labs(colour = "") +
   ggtitle("Mean annual deaths from violent assault 1990 to 2013") 
      

svg("../img/0020-assault-average.svg", 10, 10)
   print(p1)
dev.off()

png("../img/0020-assault-average.png", 1000, 1000, res = 100)
   print(p1)
dev.off()
