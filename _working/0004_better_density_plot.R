
power_trans <- function(x, k = 0.5){
   sign(x) * abs(x) ^ k
}

sim <- -5000:5000
svg("../img/0004_transformation.svg", 4, 4)
plot(sim, power_trans(sim), type = "l", bty = "l",
     xlab = "Original value", ylab = "Transformed value")
grid()
dev.off()


library(RODBC)
library(ggplot2)
library(scales)
library(showtext)
library(dplyr)
library(grid)

# comnect to database
PlayPen <- odbcConnect("PlayPen_prod")
sqlQuery(PlayPen, "use nzis11")

# load fonts
font.add.google("Poppins", "myfont")
showtext.auto()

# define the power to use in the transformation
k <- 0.2

# download the data from the database
inc <- sqlQuery(PlayPen, "select income from f_mainheader") %>%
   mutate(inc_trans = power_trans(income, k))

# define where to put the gridlines and labels on the x axis
breaks <- data.frame(labels = c(-5000, -1500, -200, -10, 0, 10, 345, 825, 5000) ) %>%
   mutate(points = power_trans(labels, k))


p1 <- inc %>%
   ggplot(aes(x = inc_trans)) +
   geom_density() +
   scale_x_continuous(breaks = breaks$points, labels = breaks$labels) +
   theme_minimal(base_family = "myfont") +
   labs(x= "\nWeekly income (transformed scale, $)",
        y = "Density\n",
        title = "Distribution of individual income in New Zealand 2011
showing the full range including zero and negative")

draw_plot <- function(){
   vp <- viewport(0.5, 0.55, height = 0.9)
   grid.newpage()
   print(p1, vp = vp)
   grid.text("Source: New Zealand Income Survey 2011 simulated unit record file, Statistics New Zealand\nGraphic by Peter Ellis", 
             0.9, 0.04, hjust = 1,
             gp = gpar(fontfamily = "myfont", fontsize = 9, col = "grey30"))
   
}


svg("../img/0004_better_density_plot.svg", 8, 5)
   draw_plot()
dev.off()

png("../img/0004_better_density_plot.png",  8 * 150, 5 * 150, res = 150)
   draw_plot()
dev.off()

