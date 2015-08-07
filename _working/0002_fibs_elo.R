library(dplyr)
library(showtext) # for fonts
library(RColorBrewer)
library(directlabels) # for labels on contour lines
library(ggplot2)
library(scales)
library(forecast) # for auto.arima

font.add.google("Poppins", "myfont")
showtext.auto()
#==================helper functions=====================

fibs_p <- function(a, b, ml){
   # function to determine the expected probability of player a winning a bachgammon match
   # against player b of length ml, with a and b representing their FIBS Elo ratings
   tmp <- 1 - (1 / (10 ^ ((a - b) * sqrt(ml) / 2000) + 1))
   return(tmp)
}


#================heat maps showing probability of winning

# create a matrix of players A and B's possible Elo ratings
a <- b <- seq(from = 1000, to = 2000, by = 5)
mat <- expand.grid(a, b) %>%
   rename(a = Var1, b = Var2)

# create a folder to hold the images and navigate to it
dir.create("tmp0002")
owd <- setwd("tmp0002")

# cycle through a range of possible match lengths, drawing a plot for each
matchlengths <- seq(from = 1, to = 23, by = 1)
res <- 150

for(i in 1:length(matchlengths)){
   df1 <- mat %>% mutate(probs = fibs_p(a = a, b = b, ml = matchlengths[i]))
   
   p1 <- ggplot(df1,  aes(x = a, y = b, z = probs)) +
      geom_tile(aes(fill = probs)) +
      theme_minimal(base_family = "myfont") +
      scale_fill_gradientn(colours = brewer.pal(10, "Spectral"), limits = c(0, 1)) +
      scale_colour_gradientn(colours = "black") +
      stat_contour(aes(colour = ..level..), binwidth = .1) + # force contours to be same distance each image
      labs(x = "Player A Elo rating", y = "Player B Elo rating") +
      theme(legend.position = "none") +
      coord_equal() +
      ggtitle(paste0("Probability of Player A winning a match to ", matchlengths[i]))
   
   png(paste0(letters[i], ".png"), res * 5, res * 5, res = res, bg = "white")
      print(direct.label(p1)) # direct labels used to label the contour points
   dev.off()
}

# compile the images into a single animated Gif with ImageMagick.  Note that
# the {animation} package provides R wrappers to do this but they often get
# confused (eg clashes with Windows' convert) and it's easier to do it explicitly
# yourself by sending an instruction to the system:
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 150 *.png "EloProbs.gif"')

# move the asset over to where needed for the blog
file.copy("EloProbs.gif", "../../img/0002-EloProbs.gif", overwrite = TRUE)

# clean up
setwd(owd)
unlink("tmp0002", recursive = TRUE)



   #=================determine change of Elo rating ================
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
      
      # probability of A winning, using fibs_p function defined earlier:
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
   
   #================simulations of how long it takes for scores to stablise================
   # two people playing eachother 5 point games with 0.6 chance of winning
   
   
   A <- B <- data_frame(rating = 1500, exp = 0)
   
   timeseries <- data_frame(A = A$rating, B = B$rating)
   
   set.seed(123) # for reproducibility
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
   timeseries$time <- 1:nrow(timeseries)
   
   # theoretical value:
   tv <- (log(1/ 0.4 - 1) / log(10)  * 2000 / sqrt(5) + 3000 ) / 2
   
   
   svg("../img/0002-elo-rating.svg", 8, 5)
   ggplot(timeseries, aes(x = time, y = A)) +
      geom_line(colour = "grey50") +
      theme_minimal(base_family = "myfont") +
      scale_y_continuous("Player A's Elo rating") +
      geom_hline(yintercept = tv, colour = "blue") +
      ggtitle("Elo rating of Player A in two player, constant skill simulation") +
      scale_x_continuous("\nNumber of matches played", label = comma) +
      annotate("text", y = tv - 10, x = max(timeseries$time) - 100, 
               label ="Theoretical\nvalue", colour  = "blue", 
               family = "myfont", size = 3)
   dev.off()

sd(as.numeric(timeseries[ -(1:400), "A"]))
both <- cbind(timeseries$A, timeseries$B)
plot(both)
dygraph(timeseries$A) %>% dyRangeSelector()
plot(timeseries$B)

par(mfrow = c(1, 2), family = "myfont")
acf(timeseries$A)
acf(timeseries$A, type = "partial")




model1 <- auto.arima(timeseries$A, stepwise = FALSE)
model1

model2 <- arima(timeseries$A, order = c(1, 0, 0))
plot(residuals(model2)); grid() # why are they larger negative? limited number of ways to lose points
model2
