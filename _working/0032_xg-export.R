#---------load up functionality and fonts------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(directlabels) # see http://stackoverflow.com/questions/13627735/stat-contour-with-data-labels-on-lines
library(caret)        # for cross-validation
library(RColorBrewer)
library(directlabels)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

pal <- brewer.pal(7, "Set1")[ 1:2]

#-----------import data----------------
# Open XG-Gammon, choose "players", "see profile results", "Results",
# "Copy to clipboard", "sessions list".  Paste result into Excel and save as a csv.
# xg_orig <- read_csv("../data/xg-export.csv")
# or you can use one I've prepared earlier, just with the column of opponent names deleted:
xg_orig <- read_csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/xg-export.csv")
names(xg_orig) <- gsub(" ", "_", names(xg_orig), fixed = TRUE)
xg <- xg_orig %>%
   filter(Match_Length <= 15) # knock out a 99999 outlier


svg("../img/0032-blunders.svg", 5, 3.4)
print(
   ggplot(xg, aes(x = Blunders)) + 
   geom_histogram(binwidth = 1) +
      labs(x = "Blunders per match",
           y = "Number of matches")
)
dev.off()


svg("../img/0032-jokers.svg", 5, 3.4)
print(
   ggplot(xg, aes(x = Jokers)) + 
      geom_histogram(binwidth = 1) +
      labs(x = "Jokers per match",
           y = "Number of matches")
)
dev.off()


p <- xg %>%
   mutate(bm = Blunders / Moves,
          jm = Jokers / Moves) %>%
   select(bm, jm) %>%
   gather(variable, value) %>%
   mutate(variable = ifelse(variable == "bm", "        Blunders per move", "  Jokers per move")) %>%
   ggplot(aes(x = value, colour = variable)) +
   geom_density() +
   labs(x = "Events per move")

svg("../img/0032-jokers-blunders.svg", 5, 3.4)
   direct.label(p)
dev.off()

#--------equity v Elo-----------
# PR = -(equity error per decision) * 500
# http://www.bgonline.org/forums/webbbs_config.pl?noframes;read=53424
# therefore e =  -PR / 500

# make a data frame I'll use for convertin equity per decision to Player Rating
# (PR) on various plots
pr_steps <- data.frame(PR = seq(from = 0, to = 40, by = 5)) %>%
   mutate(Eq_per_Decision = - PR / 500,
          PR = ifelse(PR == 40, "PR:", as.character(PR)))


# this plot shows that the "Elo_Level" is estimated by XG-Gammon
svg("../img/0032-Elo.svg", 6, 5)
ggplot(xg, aes(x = Eq_per_Decision, y = Elo_Level)) +
   geom_point() +
   scale_y_continuous(limits=c(670, 2100), 
                      breaks = seq(from = 800, to = 2000, by = 200)) +
   geom_text(data = pr_steps, y = 700, aes(label = PR), 
             colour = "grey50", family = "myfont") +
   labs(x = "Equity change per decision (negative means lost equity)",
        y = "Equivalent Elo level estimated by XG-Gammon",
        title = "XG-Gammon estimate of how equity per decision\nshould be related to Elo rating")
dev.off()


#-------------------skill v luck v winning------------------------
xg <- xg %>%
   mutate(Cost_luck_per_move = Cost_luck / Moves)

svg("../img/0032-luck-v-skill.svg", 6, 6)
xg %>%
   mutate(Result = factor(ifelse(Result == 0, "Loss", "Win"),
                          levels = c("Win", "Loss"))) %>%
   ggplot(aes(x = Eq_per_Decision, y = Cost_luck_per_move, colour = Result)) +
   geom_point(aes(size = Match_Length), shape = 2) +
   geom_text(data = pr_steps, y = -1.3, aes(label = PR), 
             colour = "grey50", family = "myfont") +
   scale_radius("Match\nlength", breaks = c(1, 3, 5, 7, 9, 11)) +
   scale_colour_manual(values = pal) +
   labs(y = "Match equity gained through luck, per move\n",
        x = "Skill: equity change per decision\nnegative means equity lost through poor choices",
        title = "Luck is more important than skill\nin any single backgammon match")
dev.off()


xg_with_skill <- xg %>%
   mutate(Result = factor(ifelse(Result == 0, "Loss", "Win"),
                          levels = c("Win", "Loss")),
          Opp_Eq_per_Decision = Opp_Eq_per_move * Opp_Roll / Opp_Decisions, 
          net_skill = Eq_per_Decision - Opp_Eq_per_Decision ) 

p3 <- xg_with_skill %>%
   ggplot(aes(x = net_skill, y = Cost_luck_per_move, colour = Result)) +
   geom_vline(xintercept = 0, colour = "grey45") +
   geom_hline(yintercept = 0, colour = "grey45") +
   geom_point(aes(size = Match_Length), shape = 2, alpha = 0.9) +
   scale_radius("Match\nlength", breaks = c(1, 3, 5, 7, 9, 11)) +
   scale_colour_manual(values = pal) +
   labs(y = "Match equity gained through luck, per move\n",
        x = "Skill: net equity change per decision from both players' decisions\nnegative means net equity loss through poor choices",
        title = "Luck is more important than skill\nin any single backgammon match") +
   annotate("text", x = 0.3, y = 0.04, label = "More skilled\nand luckier", 
            family = "myfont", colour = "grey40") +
   annotate("text", x = -0.05, y = -0.085, label = "Less skilled\nand unluckier", 
            family = "myfont", colour = "grey40") +
   
   annotate("text", x = 0.2, y = -0.01, label = "More skilled and\novercame bad luck",
            family = "myfont", colour = pal[1]) +
   annotate("segment", x = 0.14, xend = 0.09, y = -0.01, yend = -0.01,
            colour = pal[1], arrow = arrow(angle = 20)) +

   annotate("text", x = 0.17, y = -0.085, label = "More skilled\nbut luck won out",
            family = "myfont", colour = pal[2]) +
   annotate("segment", x = 0.12, xend = 0.04, y = -0.085, yend = -0.085,
            colour = pal[2], arrow = arrow(angle = 20))


svg("../img/0032-luck-v-net-skill.svg", 7, 6)
print(p3)
dev.off()


png("../img/0032-luck-v-net-skill.png", 700, 600, res = 100)
print(p3)
dev.off()

#---------------------------luck v skill in modelling----------
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod1 <- train(Result ~ Cost_luck_per_move, method = "glm", family = "binomial",
              data = xg_with_skill, trControl = ctrl)
mod1 # 98.0% accuracy

mod2 <- train(Result ~ net_skill, method = "glm", family = "binomial",
              data = xg_with_skill, trControl = ctrl)
mod2 # 65.0%


mod3 <- train(Result ~ net_skill + Cost_luck_per_move, method = "glm", family = "binomial",
              data = xg_with_skill, trControl = ctrl)
mod3 # 99.7%


