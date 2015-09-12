library(RODBC)
library(ineq)
library(dplyr)
library(ggplot2)
library(scales)
library(showtext)

font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- odbcConnect("PlayPen_Prod")

inc <- sqlQuery(PlayPen, "select income from vw_mainheader") 
# inc <- f_mainheader$income

?ineq

lorenz <- Lc(inc)
lorenz_df <- data.frame(prop_pop = lorenz$p, income = lorenz$L) %>%
   mutate(prop_equality = prop_pop)

svg("../img/0008-lorenz.svg", 5, 5)
ggplot(lorenz_df, aes(x = prop_pop, y = income)) +
   geom_ribbon(aes(ymax = prop_equality, ymin = income), fill = "yellow") +
   geom_line() +
   geom_abline(slope = 1, xintercept = 0) +
   scale_x_continuous("\nCumulative proportion of population", label = percent) +
   scale_y_continuous("Cumulative proportion of income\n", label = percent) +
   theme_minimal(base_family = "myfont") +
   coord_equal() +
   annotate("text", 0.5, 0.32, label = "Inequality\ngap", family = "myfont") +
   annotate("text", 0.5, 0.6, label = "Complete equality line", angle = 45, family = "myfont")
dev.off()

Gini(inc)
