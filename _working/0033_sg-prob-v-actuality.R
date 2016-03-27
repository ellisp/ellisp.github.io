#---------load up functionality and fonts------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(gam)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "Calibri"))
theme_set(theme_light())

#-----------import data----------------
# Open XG-Gammon, choose "players", "see profile results", "Results",
# "Copy to clipboard", "sessions list".  Paste result into Excel and save as a csv.
# xg_orig <- read_csv("../data/xg-export.csv")
# or you can use one I've prepared earlier, just with the column of opponent names deleted:
xg_orig <- read_csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/xg-export.csv")
names(xg_orig) <- gsub(" ", "_", names(xg_orig), fixed = TRUE)
xg <- xg_orig %>%
   filter(Match_Length <= 15) # knock out a 99999 outlier

head(xg)
names(xg)

ggplot(xg, aes(x = Opp_Elo, y = Elo_Level)) + geom_point()
# something wrong with how the opposition ELo is calculated
ggplot(xg, aes(x = Opp_Eq_per_move, y = Opp_Elo)) + geom_point()

# should look like this:
ggplot(xg, aes(x = Eq_per_Decision, y = Elo_Level)) + geom_point()


ggplot(xg, aes(x = Opp_Roll, y = Opp_Decisions)) + 
   geom_point() +
   geom_abline(intercept = 0, slope = 1)

model <- gam(Elo_Level ~ s(Eq_per_Decision), data = xg)

opp_data <- xg %>%
   select(Opp_Eq_per_move, Moves, Opp_Decisions) %>%
   mutate(Eq_per_Decision = Opp_Eq_per_move * Moves / Opp_Decisions)

opp_data$Opp_Elo_Modelled <- predict(model, newdata = opp_data)

ggplot(opp_data, aes(x = Eq_per_Decision, y = Opp_Elo_Modelled)) + geom_point()

ggplot(xg, aes(x = Opp_Eq_per_move, y = Opp_Elo_Modelled)) + geom_point()
