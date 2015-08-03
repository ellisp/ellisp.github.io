library(dplyr)
library(showtext)
library(RColorBrewer)
library(directlabels)
library(ggplot2)
library(scales)
library(extrafont)


#==================helper functions=====================
# in blog, move this script to bottom
fibs_p <- function(a, b, ml){
   # function to determine the expected probability of player a winning a bachgammon match
   # against player b of length ml, with a and b representing their FIBS Elo ratings
   tmp <- 1 - (1 / (10 ^ ((a - b) * sqrt(ml) / 2000) + 1))
   return(tmp)
}


fibs_scores <- function(a, b, winner = "a", ml = 1, axp = 500, bxp = 500){
   # a is Elo rating of player A before match
   # b is Elo rating of player B before match
   # ml is match length
   # axp is total match lengths (experience) of player a until this match
   # bxp is total match lengths (experience) of player b until this match
   # see http://www.fibs.com/ratings.html for formulae
   
   # calculate experience-correction multipliers:
   multa <- ifelse(axp < 400, 5 - ((axp + ml) / 100), 1)
   multb <- ifelse(bxp < 400, 5 - ((axp + ml) / 100), 1)
   
   # probability of A winning:
   winproba <- fibs_p(a = a, b = b, ml = ml)
   
   # match value (points to be distributed between the two players):
   
   matchvalue <- 4 * sqrt(ml)
   
   # who gets them?:
   if(winner == "b"){
      a <- a - matchvalue * winproba * multa
      b <- b + matchvalue * winproba * multb
   } else {
      a <- a + matchvalue * (1 - winproba) * multa 
      b <- b - matchvalue * (1- winproba) * multb
   }
   
   return(list(a = a, b = b, axp = axp + ml, bxp = bxp + ml))
}

# test against "baptism by fire" example at http://www.fibs.com/ratings.html:
round(fibs_scores(a = 1500, b = 1925, ml = 7, axp = 0, bxp = 10000, winner = "a")$a, 2) # should be 1540.95





#================heat maps showing probability of winning
a <- b <- seq(from = 1000, to = 2000, by = 5)

mat <- expand.grid(a, b) %>%
   rename(a = Var1, b = Var2)


df1 <- mat %>%
   mutate(probs = fibs_p(a = a, b = b, ml = 5))

df2 <- mat %>%
   mutate(probs = fibs_p(a = a, b = b, ml = 23))


p1 <- ggplot(df1,  aes(x = a, y = b, z = probs)) +
   geom_tile(aes(fill = probs)) +
   theme_minimal(base_family = "Calibri") +
   scale_fill_gradientn(colours = brewer.pal(10, "Spectral"), limits = c(0, 1)) +
   stat_contour(aes(colour = ..level..)) +
   labs(x = "Player A Elo rating", y = "Player B Elo rating") +
   theme(legend.position = "none") +
   ggtitle("Probability of Player A winning a 5 point match")

p2 <- p1 %+% data.frame(df2)

vp1 <- viewport(0.25, 0.5, width = 0.5)
vp2 <- viewport(0.75, 0.5, width = 0.5)

res <- 150
png("../img/0002-ml5-23-elo.png", res * 10, res * 5, res = res, bg = "transparent")
   print(direct.label(p1), vp = vp1)
   print(direct.label(p2), vp = vp2)
dev.off()

#================simulations of how long it takes for scores to stablise================


# two people playing eachother 5 point games with 0.6 chance of winning


A <- B <- data_frame(rating = 1500, exp = 0)

timeseries <- data_frame(A = A$rating, B = B$rating)


for (i in 1:10000){
   result <- fibs_scores(a = A$rating, b = B$rating, 
               winner = ifelse(runif(1) > 0.4, "a", "b"),
               axp = A$exp, bxp = B$exp, ml = 5)
   timeseries[i, "A"] <- A$rating <- result$a
   timeseries[i, "B"] <-    B$rating <- result$b
   A$exp <- result$axp
   B$exp <- result$bxp
   
}

timeseries$A <- ts(timeseries$A)
timeseries$B <- ts(timeseries$B)
plot(timeseries$A)
plot(timeseries$B)

acf(timeseries$B, type = "partial")
acf(diff(timeseries$B))


# six people - 3 good, 3 bad - 5 point games 0.6 chance of good people winning






# chance of winning changes according to an ARIMA process, on a logistic transformation dimension





