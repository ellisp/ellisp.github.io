#---------load up functionality and fonts------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(directlabels) # see http://stackoverflow.com/questions/13627735/stat-contour-with-data-labels-on-lines
library(caret)        # for cross-validation
library(RColorBrewer)

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
xg <- xg_orig


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

svg("../img/0032-luck-v-skill.svg", 6, 6)
xg %>%
   mutate(Result = factor(ifelse(Result == 0, "Loss", "Win"),
                          levels = c("Win", "Loss"))) %>%
   ggplot(aes(x = Eq_per_Decision, y = Cost_luck, colour = Result)) +
   geom_point(aes(size = Match_Length), shape = 2) +
   geom_text(data = pr_steps, y = -1.3, aes(label = PR), 
             colour = "grey50", family = "myfont") +
   scale_radius("Match\nlength", breaks = c(1, 3, 5, 7, 9, 11)) +
   scale_colour_manual(values = pal) +
   labs(y = "Match equity gained through luck\n-1 = certain loss, +1 = certain win",
        x = "Skill: equity change per decision\nnegative means equity lost through poor choices",
        title = "Luck is more important than skill\nin a single backgammon match")
dev.off()


xg_wth_skill <- xg %>%
   mutate(Result = factor(ifelse(Result == 0, "Loss", "Win"),
                          levels = c("Win", "Loss")),
          Opp_Eq_per_Decision = Opp_Eq_per_move * Opp_Roll / Opp_Decisions, 
          net_skill = Eq_per_Decision - Opp_Eq_per_Decision ) 

p3 <- xg_wth_skill %>%
   ggplot(aes(x = net_skill, y = Cost_luck, colour = Result)) +
   geom_vline(xintercept = 0, colour = "grey45") +
   geom_hline(yintercept = 0, colour = "grey45") +
   geom_point(aes(size = Match_Length), shape = 2) +
   scale_radius("Match\nlength", breaks = c(1, 3, 5, 7, 9, 11)) +
   scale_colour_manual(values = pal) +
   labs(y = "Match equity gained through luck\n-1 = certain loss, +1 = certain win",
        x = "Skill: net equity change from both players' decisions\nnegative means net equity loss through poor choices",
        title = "Luck is more important than skill\nin a single backgammon match") +
   annotate("text", x = 0.3, y = 0.9, label = "More skilled\nand luckier", 
            family = "myfont", colour = "grey40") +
   annotate("text", x = -0.05, y = -1, label = "Less skilled\nand unluckier", 
            family = "myfont", colour = "grey40") +
   
   annotate("text", x = 0.17, y = -0.17, label = "More skilled and\novercame bad luck", 
            family = "myfont", colour = pal[1]) +
   annotate("segment", x = 0.11, xend = 0.045, y = -0.17, yend = -0.17, 
            colour = pal[1], arrow = arrow(angle = 20)) +
   annotate("segment", x = 0.13, xend = 0.09, y = -0.3, yend = -0.65, 
            colour = pal[1], arrow = arrow(angle = 20)) +
   
   annotate("text", x = 0.17, y = -0.93, label = "More skilled\nbut luck won out", 
            family = "myfont", colour = pal[2]) +
   annotate("segment", x = 0.13, xend = 0.06, y = -0.93, yend = -0.93, 
            colour = pal[2], arrow = arrow(angle = 20))
   

svg("../img/0032-luck-v-net-skill.svg", 7, 6)
print(p3)
dev.off()


png("../img/0032-luck-v-net-skill.png", 700, 600, res = 100)
print(p3)
dev.off()

#---------------------------luck v skill in modelling----------

xg <- xg_orig %>%
   mutate(pr_me = -Eq_per_Decision * 500,
          pr_opp = -Opp_Eq_per_move * 500,
          Result = factor(Result)) %>%
   filter(!is.na(Result))

ggplot(xg, aes(y = pr_me, x = Result) )+
   geom_boxplot() +
   coord_flip() 


# xg_doubled <- xg %>%
#    mutate(tmp = pr_me,
#           pr_me = pr_opp,
#           pr_opp = tmp, 
#           Result = factor(1 - as.numeric(as.character(Result)))) %>%
#    select(-tmp) %>%
#    rbind(xg) 
# 
#=============prediction over a grid===========
mod1 <- glm(Result ~  pr_me + pr_opp, family = "binomial", data = xg_doubled)
mod2 <- glm(Result ~  Match_Length + pr_me + pr_opp, family = "binomial", data = xg_doubled)
mod3 <- glm(Result ~  Match_Length + pr_me * pr_opp, family = "binomial", data = xg)
mod4 <- glm(Result ~  Match_Length * pr_me * pr_opp, family = "binomial", data = xg)
mod5 <- glm(Result ~  Match_Length * (pr_me + pr_opp), family = "binomial", data = xg)
# on straight comparison of AIC we'd choose the simplest model:
AIC(mod1, mod2, mod3, mod4, mod5)

# but lets check with cross-validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

set.seed(123)
mod1.cv <- train(Result ~ pr_me + pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod2.cv <- train(Result ~ Match_Length + pr_me + pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod3.cv <- train(Result ~ Match_Length + pr_me * pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod4.cv <- train(Result ~ Match_Length * pr_me * pr_opp, method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
mod5.cv <- train(Result ~ Match_Length * (pr_me + pr_opp), method = "glm", family = "binomial",
                 data = xg, trControl = ctrl)
# mod3.cv actually seems the most accurate; but only just.  Would be good to have more data!

# Note that inference (confidence intervals etc) will be biased to 'significant'
# but ok for prediction


mls <- c(1, 3, 5, 7, 9, 11)

pred_grid <- expand.grid(
   Match_Length = mls,
   pr_me = 0:300 / 10,
   pr_opp = 0:300 / 10
   )

preds <- predict(mod3, newdata = pred_grid, type = "response")

pred_results <- cbind(pred_grid, preds)


p1 <- pred_results %>%
   mutate(Match_Length = paste0("Match length = ", Match_Length),
          Match_Length = factor(Match_Length, levels = paste0("Match length = ", mls))) %>%
   ggplot(aes(x = pr_me, y = pr_opp, z = preds,
              colour = ..level..)) +
   stat_contour() +
   geom_abline(slope = 1, intercept = 0, colour = "grey80") + # theoretically should be 50-50 along this line
   facet_wrap(~Match_Length) +
   labs(x = "How well I play (PR; lower is better)",
        y = "How well opponent plays (PR; lower is better)",
        title = "My chance of winning against opponent playing at different skill levels") +
   coord_equal()

svg("../img/0032-p1.svg", 8, 6)
direct.label(p1, "top.points")
dev.off()

p2 <- pred_results %>%
   filter(pr_opp %in% c(0, 5, 10, 20)) %>%
   mutate(pr_opp = paste0("Opponent PR = ", pr_opp)) %>%
   ggplot(aes(x = pr_me, y = Match_Length, z = preds,
              colour = ..level..)) +
   stat_contour() +
   facet_wrap(~pr_opp) +
   labs(x = "How well I play (PR; lower is better)",
        y = "Match length",
        title = "My chance of winning against opponent playing at different skill levels")

svg("../img/0032-p2.svg", 8, 6)
direct.label(p2, "top.points")
dev.off()
