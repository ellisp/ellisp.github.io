library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(wordcloud)

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


#==========pollster infographics=============
# create summary pollster info.  Most pollsters always have the same grade, a few
# sometimes have different grades and sometimes have their grade missing
palette <- magma(length(grades))[length(grades):1]

pollsters <- pollsonly %>%
   group_by(pollster) %>%
   mutate(graden = ifelse(grade == "", NA, as.numeric(grade))) %>%
   summarise(grade = round(mean(graden, na.rm = TRUE)),
             totalweight = sum(poll_wt),
             totalsample = sum(samplesize)) %>%
   mutate(grade = ifelse(is.na(grade), 11, grade),
          grade = factor(grade, levels = 1:11, labels = grades),
          grade_colours = palette[as.numeric(grade)]) %>%
   ungroup() 


plotcloud <- function(){
   par(family = "myfont")   
   par(bg = "grey40")
   set.seed(223)
   wordcloud(words = pollsters$pollster,
             freq = pollsters$totalweight * 10000,
             colors = pollsters$grade_colours,
             random.order = FALSE,
             ordered.colors = TRUE, 
             family = "myfont")
   grid.text(0.55, 0.03, hjust = 0,
             label = "Size is mapped to total combined weight as at 29/10/2016;\nColour is mapped to pollster grade (lighter is better).", 
             gp = gpar(col = "grey90", fontfamily = "myfont", cex = 0.8))
   grid.text(0.03, 0.97, hjust = 0, label = "The pollster data used by FiveThirtyEight", 
             gp = gpar(col = "grey90", fontfamily = "myfont", cex = 1.5))
   legend("bottomleft", title = "", bg = "grey45", box.lty = 0,
          legend = c(levels(pollsters$grade)[1:10], "None"), text.col = palette)
}

svg("../img/0062-pollsters-cloud.svg", 9, 8)
   plotcloud()
dev.off()

png("../img/0062-pollsters-cloud.png", 900, 800, res = 100)
   plotcloud()
dev.off()



#==============save images==============


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
