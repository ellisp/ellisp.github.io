library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggthemes)

# this next code will need to be adapted if you don't have a ../data/ folder...
# alternatively, if that link stops working, there's a static copy at http://ellisp.github.io/data/polls.csv
www <- "http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv"
download.file(www, destfile = "../data/polls.csv")

# download data
polls_orig <- read.csv("../data/polls.csv", stringsAsFactors = FALSE)

table(table(polls_orig$poll_id))
table(polls_orig$type)

# create a vector of grades so we know what order they are in
grades = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D", "")

# parse the dates as proper dates, and add structure to the grade
polls <- polls_orig %>%
   mutate(
      startdate = as.Date(startdate, "%m/%d/%Y"),
      enddate  = as.Date(enddate, "%m/%d/%Y"),
      grade = ordered(grade, levels = grades)
   )

p0 <- polls %>%
   ggplot(aes(x = enddate, y = poll_wt, colour = type, size = samplesize)) +
   geom_point() +
   scale_y_sqrt() +
   facet_wrap(~grade)


pollsonly <- filter(polls, type == "polls-only")

# compare the raw values with the adjusted values for Clinton
p2 <- pollsonly %>%
   ggplot(aes(x = rawpoll_clinton, y = adjpoll_clinton)) +
   geom_abline(intercept = 0, slope = 1, colour = "grey75") +
   geom_point(alpha = 0.4, aes(size = samplesize, colour = grade)) +
   scale_color_viridis(discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
   coord_equal()

# For Trump:
p2a <- pollsonly %>%
   ggplot(aes(x = rawpoll_trump, y = adjpoll_trump)) +
   geom_abline(intercept = 0, slope = 1, colour = "grey75") +
   geom_point(alpha = 0.4, aes(size = samplesize, colour = grade)) +
   scale_color_viridis(discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
   coord_equal()

# Distribution of adjustment factors:
p2b <- pollsonly %>%
   mutate(Trump = adjpoll_trump / rawpoll_trump,
          Clinton = adjpoll_clinton / rawpoll_clinton) %>%
   select(Trump, Clinton) %>%
   gather(candidate, adj_ratio) %>%
   ggplot(aes(x = adj_ratio, colour = candidate, fill = candidate)) +
   geom_density(alpha = 0.3) +
   ggtitle("Density of adjustment ratios for two main candidates")

# show probability of voting Clinton and Trump over time, just the national polls:
p1 <- pollsonly %>%
   filter(state == "U.S.") %>%
   select(enddate, rawpoll_clinton, adjpoll_clinton, rawpoll_trump, adjpoll_trump, samplesize, grade) %>%
   gather(variable, value, -enddate, -samplesize, -grade) %>% 
   ggplot(aes(x = enddate, y = value / 100)) +
   facet_wrap(~variable) +
   geom_point(alpha = 0.4, aes(colour = grade, size = samplesize)) +
   geom_smooth(aes(weight = samplesize), span = 0.5) +
   scale_color_viridis("Pollster grade", discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
   scale_y_continuous("Percentage of intended vote\n", label = percent) +
   scale_size_area("Sample size", label = comma) +
   ggtitle("Intended vote in the US Presidential election",
           subtitle = "National surveys only") +
   labs(x = "End date of survey",
        caption = "Data compiled by FiveThirtyEight, analysis by http://ellisp.github.io")
   
# longitudinal version, with lines connecting pollsters:
p4 <- pollsonly %>%
   filter(state == "U.S.") %>%
   select(enddate, rawpoll_clinton, adjpoll_clinton, rawpoll_trump, adjpoll_trump, samplesize, grade, pollster) %>%
   gather(variable, value, -enddate, -samplesize, -grade, -pollster) %>% 
   ggplot(aes(x = enddate, y = value / 100, colour = grade)) +
   facet_wrap(~variable) +
   geom_line(aes(group = pollster)) +
   scale_color_viridis("Pollster grade", discrete = TRUE, option = "magma") + 
   guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2))) +
   scale_y_continuous("Percentage of intended vote\n", label = percent) +
   ggtitle("Intended vote in the US Presidential election",
           subtitle = "National surveys only, grouped by pollster") +
   labs(x = "End date of survey",
        caption = "Data compiled by FiveThirtyEight, analysis by http://ellisp.github.io")



pollsters <- pollsonly %>%
   group_by(pollster) %>%
   summarise(grade = max(grade),
             totalweight = sum(poll_wt),
             totalsample = sum(samplesize)) %>%
   arrange(totalweight) %>%
   mutate(pollster = factor(pollster, levels = pollster))

# infographic of the various pollsters and their weights
p3 <- pollsters %>%
   ggplot(aes(x = totalweight, y = pollster, label = pollster, colour = grade)) +
   geom_text(aes(size = totalsample), hjust = 0) +
   theme_tufte(base_family = "myfont") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         text = element_text(colour = "grey50")) +
   theme(panel.background = element_rect(fill = "black", colour = NA)) +
   scale_color_viridis(discrete = TRUE, direction = -1, option = "viridis") +
   theme(legend.position = c(0.8, 0.5)) +
   scale_x_continuous("Total weight towards forecasts",
                breaks = seq(from = 0, to = 200, by = 50), limits = c(0, 270)) +
   labs(y = "Pollsters arranged by weight that FiveThirtyEight gives to their surveys",
        colour = "Pollster grade") +
   guides(colour =  guide_legend(override.aes = list(size = 4)),
          size = guide_legend(override.aes = list(colour = "grey80"))) +
   scale_size("Combined sample size", label = comma, range = c(1, 9)) +
   ggtitle("The pollsters used by FiveThirtyEight",
           subtitle = "As at 29 October 2016")



svg("../img/0062-pollsters.svg", 11, 11)   
print(p3)
dev.off()

png("../img/0062-pollsters.png", 11 * 72, 11 * 72)   
print(p3)
dev.off()


svg("../img/0062-clinton.svg", 10, 10)   
print(p1)
dev.off()

png("../img/0062-clinton.png", 1000, 1000, res = 100)   
print(p1)
dev.off()

svg("../img/0062-pollster-lines.svg", 10, 10)   
print(p4)
dev.off()


svg("../img/0062-raw-adj.svg", 7, 7)   
print(p2)
dev.off()

svg("../img/0062-raw-adj-trump.svg", 7, 7)   
print(p2a)
dev.off()

svg("../img/0062-raw-adj-densities.svg", 7, 7)   
print(p2b)
dev.off()


svg("../img/0062-weights.svg", 8, 8)
print(p0)
dev.off()
