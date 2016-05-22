#===================setup=======================
library(showtext)
library(RMySQL)
library(MASS) 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(directlabels)

#================download and transform data==============
# See http://ellisp.github.io/blog/2015/08/15/importing-nzis-surf/ for
# instructions on setting up the database

sql <- "SELECT hours, income FROM vw_mainheader"
PlayPen <- dbConnect(RMySQL::MySQL(), username = "analyst", dbname = "nzis11")
nzis <- dbGetQuery(PlayPen, sql) 
dbDisconnect(PlayPen)

# An early version of the analysis used a cube root transform; this was dropped
# as it reduces interpretability and is not really germane to the illustration
# of robust regression methods
# cuberoot <- function(x){
#    sign(x) * abs(x)^(1/3)
# }
# nzis <- nzis %>%
#    mutate(income2 = cuberoot(income),
#           hours2 = cuberoot(hours))

# create the variables to be used for x and y (as finally used, this
# is just income and hours on their original scale):
nzis <- nzis %>%
   mutate(income2 = income,
          hours2 = hours)

#==================simulations of many samples and estimated models==============
# create empty lists to hold the samples and the model results
points_samp <- list()
mod_lm <- list()
mod_rlm <- list()
mod_lqs <- list()

# create the samples, estimate the models and store the results
reps <- 10000
for (i in 1:reps){
   nzis_samp <- nzis[sample(1:nrow(nzis), 30), ]   
   points_samp[[i]] <- nzis_samp
   mod_lm[[i]] <- lm(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   mod_rlm[[i]] <- rlm(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   mod_lqs[[i]] <- lqs(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   
}

#======================reporting results==============

#------------setup-----------------
# Fonts:
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))



# define a generic plot function that does the background pale grey
# plot fo the full dataset and draws the axes:
draw_plot <- function(){
   with(nzis, plot(hours2, income2, cex = 0.5, col = "grey80", bty = "l",
                   xlab = "Hours worked", 
                   ylab = "Weekly income",
                   axes = FALSE))
   
   axis(2, at = axTicks(2), labels = paste0("$", str_trim(format(
      axTicks(2), big.mark = ","))))
   axis(1, at = axTicks(1), labels = axTicks(1))
}



#-----------animation-------------
# This code is dependent on the particular folder structure.  File locations
# will need to be changed if people want to reproduce the analysis.
for(i in 1:100){
   
   png(paste0("_output/0041-rlm-lqs/", i + 20000, ".png"),
       650, 650, res = 100)
   par(family = "myfont")
   draw_plot()
   title(main = "Comparison of different estimation methods\nfor linear models on a subset of income data")
   with(points_samp[[i]], points(hours2, income2, cex = 2))
   abline(mod_lm[[i]], col = "black")
   abline(mod_rlm[[i]], col = "blue")
   abline(mod_lqs[[i]], col = "red")
   
   legend("topright", legend = c("lm", "rlm", "lts"), lty = 1, 
          col = c("black", "blue", "red"), text.col = c("black", "blue", "red"),
          bty = "n")
   
   legend("bottomright", legend = c("Full data", "Sampled data"),
          pch = 1, col = c("grey50", "black"), pt.cex = 1:2, 
          bty = "n", text.col = c("grey50", "black"))
   
   dev.off()
}

# next sequence turns the various static frames into an animated GIF
# and depends on the actual location of ImageMagick:
projdir <- setwd("_output/0041-rlm-lqs/")
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 65 *.png "../../../img/0041-rtm-lqs.gif"') 
setwd(projdir)


#--------------visual static comparison-----------------
png("../img/0041-compare-lqs-rlm.png", 800, 500, res = 100)
par(mfrow = c(1, 2), family = "myfont")

# draw graph for lqs
draw_plot()
title(main = "Least trimmed squares")


for(i in 1:reps){
   abline(mod_lqs[[i]], col = adjustcolor("red", alpha.f = 0.02))
}
   
# draw graph for rlm
draw_plot()
title(main = "M-estimator")
for(i in 1:reps){
   abline(mod_rlm[[i]], col = adjustcolor("blue", alpha.f = 0.02))
}
dev.off()


#-----------------compare estimated slopes-------------------
slopes <- data.frame(
   lm = unlist(lapply(mod_lm, function(x){coef(x)[2]})),
   rlm = unlist(lapply(mod_rlm, function(x){coef(x)[2]})),
   lqs = unlist(lapply(mod_lqs, function(x){coef(x)[2]}))
) %>%
   gather(method, value)

# summary table:
slopes %>%
   mutate(method = factor(method, levels = c("lm", "rlm", "lqs"))) %>%
   group_by(method) %>%
   summarise(
      mean = round(mean(value), 1),
      median = round(median(value), 1),
      sd = round(sd(value), 1)
   )

# density plot, taking care to use same colours for each method
# as in previous graphics:
colours <- c("lm" = "black", "rlm" = "blue", "lqs" = "red")

svg("../img/0041-slope-densities.svg", 6, 3.5)
print(direct.label(
ggplot(slopes, aes(x = value, fill = method, colour = method)) +
   geom_density(alpha = 0.3) +
   coord_cartesian(xlim = c(-25, 75)) +
   scale_x_continuous("Estimated marginal return of extra hour of work",
                      label = dollar) +
   labs(y = "Density of estimates from\nsamples of 30 each") +
   scale_fill_manual(values = colours) +
   scale_colour_manual(values = colours)
))
dev.off()
