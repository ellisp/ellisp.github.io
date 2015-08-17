library(showtext)
library(RODBC)
library(ggplot2)
library(scales)
library(dplyr)
library(Cairo)

PlayPen <- odbcConnect("PlayPen_prod")
font.add.google("Poppins", "myfont")
showtext.auto()
sqlQuery(PlayPen, "use nzis11")



#==========tables==============
#---------------------test against summary statistics in data dictionary-------------

# what's the implicit weight for each row?
3461.1 / nrow(f_mainheader) # .1174409

tab1 <- sqlQuery(PlayPen, "SELECT 
                 sex,
                 ROUND(AVG(income))          as Mean, 
                 COUNT(1)                    as Sample,
                 ROUND(COUNT(1) * .11744, 1) as Population
                 FROM vw_mainheader
                 GROUP BY sex")

tab1



tab2 <- sqlQuery(PlayPen, "SELECT 
                 agegrp,
                 ROUND(AVG(income))          as Mean, 
                 COUNT(1)                    as Sample,
                 ROUND(COUNT(1) * .11744, 1) as Population
                 FROM vw_mainheader
                 GROUP BY agegrp")

tab2


# qualification summary in data dictionary uses a different classification to that in data
tab3 <- sqlQuery(PlayPen, "SELECT 
                 qualification,
                 ROUND(AVG(income))          as Mean, 
                 COUNT(1)                    as Sample,
                 ROUND(COUNT(1) * .11744, 1) as Population
                 FROM vw_mainheader
                 GROUP BY qualification
                 ORDER BY Mean")

tab3

# occupation summary not given in data dictionary
tab4 <- sqlQuery(PlayPen, "SELECT 
                 occupation,
                 ROUND(AVG(income))          as Mean, 
                 COUNT(1)                    as Sample,
                 ROUND(COUNT(1) * .11744, 1) as Population
                 FROM vw_mainheader
                 GROUP BY occupation
                 ORDER BY Mean")

tab4

# region summary not given in data dictionary
tab5 <- sqlQuery(PlayPen, "SELECT 
                             region,
                             ROUND(AVG(income))          as Mean, 
                             COUNT(1)                    as Sample,
                             ROUND(COUNT(1) * .11744, 1) as Population
                           FROM vw_mainheader
                           GROUP BY region
                           ORDER BY Mean ")

tab5


tab6 <- sqlQuery(PlayPen,
                 "SELECT
                     ethnicity,
                     ROUND(AVG(income))          as Mean,
                     COUNT(1)                    as Sample,
                     ROUND(COUNT(1) * .11744, 1) as Population
                  FROM f_mainheader m
                  JOIN f_ethnicity e ON m.survey_id = e.survey_id
                  JOIN d_ethnicity d ON e.ethnicity_id = d.ethnicity_id
                  GROUP BY ethnicity
                  ORDER BY Mean")

tab6                 

#=======graphics==================

dtf <- sqlQuery(PlayPen,
                "SELECT
                ethnicity,
                income
                FROM f_mainheader m
                JOIN f_ethnicity e ON m.survey_id = e.survey_id
                JOIN d_ethnicity d ON e.ethnicity_id = d.ethnicity_id")

p1 <- dtf %>%
   filter(ethnicity %in% c("Asian", "European", "Maori", "Pacific Peoples")) %>%
   ggplot(aes(x = income, colour = ethnicity)) +
   geom_density(size = 1.1) +
   scale_x_log10("Weekly income from all sources", label = dollar, breaks = c(10, 100, 345, 825, 10000)) +
   theme_minimal(base_family = "myfont") +
   theme(legend.position = "bottom") +
   scale_colour_brewer("", palette = "Set1")

svg("../img/0003-nzis-ethnicity-density.svg", 7, 5)
   print(p1)
dev.off()

# facebook version
CairoPNG("../img/0003-nzis-ethnicity-density.png", 7 * 100, 5 * 100, res = 100)
   print(p1 + theme_minimal())
dev.off()

svg("../img/0003-nzis-ethnicity-boxplot.svg", 6, 6)
dtf %>%
   ggplot(aes(y = income, x = ethnicity, colour = ethnicity)) +
   geom_boxplot() +
   geom_rug() +
   scale_y_log10("Weekly income from all sources", label = dollar, breaks = c(10, 100, 1000, 10000)) +
   theme_minimal() +
   labs(x = "") +
   coord_flip() +
   scale_colour_brewer(palette = "Set2") +
   theme(legend.position = "none")
dev.off()

# how did I choose to mark $345 and $825 on the scale
# ripped off from http://stackoverflow.com/questions/27418461/calculate-the-modes-in-a-multimodal-distribution-in-r
find_modes<- function(data, ...) {
   dens <- density(data, ...)
   y <- dens$y
   modes <- NULL
   for ( i in 2:(length(y) - 1) ){
      if ( (y[i] > y[i - 1]) & (y[i] > y[i + 1]) ) {
         modes <- c(modes,i)
      }
   }
   if ( length(modes) == 0 ) {
      modes = 'This is a monotonic distribution'
   }
   return(dens$x[modes])
}

x <- dtf$income
x[x < 1] <- 1 # not interested in negative income just now

# where are those modes?
exp(find_modes(log(x)))


dtf2 <- sqlQuery(PlayPen, "select hours, income from vw_mainheader")

p3 <- ggplot(dtf2, aes(x = hours, y = income)) +
   geom_jitter(alpha = 0.05) +
   scale_x_log10("Hours worked", breaks = c(1, 10, 20, 40, 80)) +
   scale_y_log10("Weekly income from all sources", label = dollar, breaks = c(10, 100, 345, 825, 10000)) +
   theme_minimal(base_family = "myfont")
svg("../img/0003-nzis-ethnicity-scatterplot.svg", 6, 6)
print(p3)
dev.off()

#=======================exporting tables to the actual post========
library(xtable)
options(xtable.type = "html")
options(xtable.include.rownames = FALSE)

print(xtable(tab1), file = "../_tables/0003-tab1.html")
print(xtable(tab2), file = "../_tables/0003-tab2.html")
print(xtable(tab3), file = "../_tables/0003-tab3.html")
print(xtable(tab4), file = "../_tables/0003-tab4.html")
print(xtable(tab5), file = "../_tables/0003-tab5.html")
print(xtable(tab6), file = "../_tables/0003-tab6.html")

alltables <- paste0("../_tables/0003-tab", 1:6, ".html")

thispost <- "2015-08-15-importing-nzis-surf.html"

# read in the human-edited post from the _knitr directory
tmp0 <- readLines(paste0("../_knitr/", thispost))

for(i in 1:6){
   thistable <- alltables[i]
   tmp1 <- paste(readLines(thistable), collapse ="\n")
   tmp0 <- gsub(thistable, tmp1, tmp0, fixed = TRUE)
}
writeLines(tmp0, con =(paste0("../_posts/", thispost)))
