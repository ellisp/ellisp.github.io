library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggthemes)

www <- "http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv"
download.file(www, destfile = "../data/polls.csv")
polls_orig <- read.csv("../data/polls.csv", stringsAsFactors = FALSE)

grades = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "")

polls <- polls_orig %>%
   mutate(
      startdate = as.Date(startdate, "%m/%d/%Y"),
      enddate  = as.Date(enddate, "%m/%d/%Y"),
      grade = ordered(grade, levels = grades)
   )

names(polls)

table(polls$matchup)
table(polls$grade)

p1 <- polls %>%
   ggplot(aes(x = enddate, y = rawpoll_clinton)) +
   geom_point(alpha = 0.4, aes(size = samplesize, colour = grade)) +
   geom_smooth(aes(weight = poll_wt)) +
   scale_color_viridis(discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

p2 <- polls %>%
   ggplot(aes(x = rawpoll_clinton, y = adjpoll_clinton)) +
   geom_abline(intercept = 0, slope = 1, colour = "grey75") +
   geom_point(alpha = 0.4, aes(size = samplesize, colour = grade)) +
   scale_color_viridis(discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2)))

p3 <- polls %>%
   group_by(pollster) %>%
   summarise(grade = max(grade),
             meanweight = mean(poll_wt),
             number = length(poll_wt),
             weightednumber = number * meanweight,
             meansize = mean(samplesize)) %>%
   arrange(meanweight) %>%
   mutate(pollster = factor(pollster, levels = pollster)) %>%
   ggplot(aes(x = meanweight, y = pollster, label = pollster, colour = grade)) +
   geom_text(aes(size = weightednumber)) +
   theme_tufte(base_family = "myfont") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         text = element_text(colour = "grey50")) +
   theme(panel.background = element_rect(fill = "black", colour = NA)) +
   scale_color_viridis(discrete = TRUE, direction = -1, option = "viridis") +
   theme(legend.position = c(0.8, 0.3)) +
   scale_x_sqrt("Mean weight for polls - a combination of sample size and pollster grade",
                breaks = 0.15 * 2 ^ (0:4), limits = c(0, 2.6)) +
   labs(y = "Pollsters arranged by average weight that '538' gives to their polls",
        colour = "Pollster grade",
        size = "Overall influence\n(weighted number of polls)") +
   guides(colour =  guide_legend(override.aes = list(size = 4)),
          size = guide_legend(override.aes = list(colour = "grey80"))) +
   ggtitle("The pollsters used by 538")

svg("../img/0062-pollsters.svg", 8, 11)   
print(p3)
dev.off()


svg("../img/0062-clinton.svg", 7, 7)   
print(p1)
dev.off()

svg("../img/0062-raw-adj.svg", 7, 7)   
print(p2)
dev.off()


