library(mbie)

library(RODBC)
library(ineq)
library(dplyr)
library(ggplot2)
library(scales)
library(showtext) # for fonts
library(stringr)  # for str_wrap
library(tidyr)

font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- odbcConnect("PlayPen_Prod")
sqlQuery(PlayPen, "use nzis11")

inc <- sqlQuery(PlayPen, "select income from vw_mainheader where hours > 30 and income > 0")$income

#---------distribution of various measures from small smaple

sim_ineq <- function(inc, n, reps = 1000){
   # note Kolm doesn't work
   #      and entropy and Atkinson are identical
   results <- data.frame(
      trial = 1:reps,
      P90P10 = numeric(reps),
      P80P20 = numeric(reps),
      Gini = numeric(reps),
      Theil = numeric(reps),
      Atkinson = numeric(reps),
      RS = numeric(reps),
      PropCent = numeric(reps),
      PropDec = numeric(reps))
   
   set.seed(123) # for reproducibility
   for(i in 1:reps){
      x <- sample(inc, n, replace = TRUE)
      x <- sort(x)
      
      results[i, "P90P10"] <- quantile(x, .9) / quantile(x, .1)
      results[i, "P80P20"] <- quantile(x, .8) / quantile(x, .2)
      results[i, "Gini"] <- Gini(x)
      results[i, "Theil"] <- Theil(x)
      results[i, "Atkinson"] <- Atkinson(x)
      results[i, "RS"] <- RS(x)
      results[i, "PropCent"] <- sum(x[(n * .99) : n]) / sum(x) * 100
      results[i, "PropDec"] <- sum(x[(n * .90) : n]) / sum(x) * 100
   }
   
   return(results)
}
   
results_list <- list()
for (i in c(500, 1000, 5000, 10000)){
   results_list[[i]] <- sim_ineq(inc = inc, n = i)[ , -1]
}



png("../img/0016-pairs-n500.png", 800, 800, res = 100)
   pairs(results_list[[500]])
dev.off()

png("../img/0016-pairs-n1000.svg", 800, 800, res = 100)
   pairs(results_list[[1000]])
dev.off()


png("../img/0016-pairs-n5000.png", 800, 800, res = 100)
   pairs(results_list[[5000]])
dev.off()

png("../img/0016-pairs-n10000.png", 800, 800, res = 100)
   pairs(results_list[[10000]])
dev.off()


print(results_list[[500]] %>%
   gather("Method", "Value") %>%
   ggplot(aes(x = Value)) +
   facet_wrap(~Method, scales = "free", ncol = 4) +
   labs(x = "") +
   geom_density())