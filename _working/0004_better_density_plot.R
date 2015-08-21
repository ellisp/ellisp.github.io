library(RODBC)
library(ggplot2)
library(scales)
library(showtext)

# comnect to database
PlayPen <- odbcConnect("PlayPen_prod")

# load fonts
font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")

k <- 0.2

inc <- sqlQuery(PlayPen, "select income from f_mainheader") %>%
   mutate(inc_trans = sign(income) * abs(income) ^ k)

breaks <- data.frame(labels = c(-5000, -1500, -200, -10, 0, 10, 345, 825, 5000) ) %>%
   mutate(points = sign(labels) * abs(labels) ^ k)

svg("../img/004_better_density_plot.svg", 8, 4)
inc %>%
   ggplot(aes(x = inc_trans)) +
   geom_density() +
   scale_x_continuous(breaks = breaks$points, labels = breaks$labels) +
   theme_minimal(base_family = "myfont") +
   labs(x= "\nWeekly income (transformed scale, $)",
        y = "Density\n")
dev.off()


