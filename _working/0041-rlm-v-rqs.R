#------------------setup------------------------
library(showtext)
library(RMySQL)
library(MASS) # for stepAIC.  Needs to be before dplyr to avoid "select" namespace clash
library(dplyr)
library(tidyr)
library(stringr)

font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- dbConnect(RMySQL::MySQL(), username = "analyst", dbname = "nzis11")



#---------------------------download and transform data--------------------------
# This query will include double counting of people with multiple ethnicities
sql <-
   "SELECT sex, agegrp, occupation, qualification, region, hours, income, 
a.survey_id, ethnicity FROM
f_mainheader a                                               JOIN
d_sex b           on a.sex_id = b.sex_id                     JOIN
d_agegrp c        on a.agegrp_id = c.agegrp_id               JOIN
d_occupation e    on a.occupation_id = e.occupation_id       JOIN
d_qualification f on a.qualification_id = f.qualification_id JOIN
d_region g        on a.region_id = g.region_id               JOIN
f_ethnicity h     on h.survey_id = a.survey_id               JOIN
d_ethnicity i     on h.ethnicity_id = i.ethnicity_id
ORDER BY a.survey_id, ethnicity"

nzis <- dbGetQuery(PlayPen, sql) 
dbDisconnect(PlayPen)

cuberoot <- function(x){
   sign(x) * abs(x)^(1/3)
}


nzis <- nzis %>%
   mutate(income2 = cuberoot(income),
          hours2 = cuberoot(hours))


nzis <- nzis %>%
   mutate(income2 = income,
          hours2 = hours)


points_samp <- list()
mod_lm <- list()
mod_rlm <- list()
mod_lqs <- list()

reps <- 10000
for (i in 1:reps){
   nzis_samp <- nzis[sample(1:nrow(nzis), 30), ]   
   points_samp[[i]] <- nzis_samp
   mod_lm[[i]] <- lm(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   mod_rlm[[i]] <- rlm(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   mod_lqs[[i]] <- lqs(income2 ~ hours2 + (hours2 == 0), data = nzis_samp)
   
}

draw_plot <- function(){
   with(nzis, plot(hours2, income2, cex = 0.5, col = "grey80", bty = "l",
                   xlab = "Hours worked", 
                   ylab = "Weekly income",
                   axes = FALSE))
   
   axis(2, at = axTicks(2), labels = paste0("$", str_trim(format(
      axTicks(2), big.mark = ","))))
   axis(1, at = axTicks(1), labels = axTicks(1))
}

png("../img/0041-compare-lqs-rlm.png", 1000, 650, res = 100)
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
(
projdir <- setwd("_output/0041-rlm-lqs/")
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 65 *.png "../../../img/rtm-lqs.gif"') 
setwd(projdir)


mod_lm[[1]]
unlist(lapply(mod_lm, function(x){coef(x)[2]}))
