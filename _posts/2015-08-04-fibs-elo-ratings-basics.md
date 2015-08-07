---
layout: post
title: Simulating backgammon players' Elo ratings
date: 2015-08-07
tag: 
   - R
   - Backgammon
   - Animations
description: I show how to convert from the Elo ratings of a backgammon player and their opponent and match length to the theoretical probability of winning.  I simulate a simple two player backgammon forum to show how Elo ratings vary at random around players "true" skill level.  Along the way, I demonstrate making animated graphics for a webpage.
image: /img/0002-EloProbs.gif
category: R
---

##Probabilities of winning for players with a given rating
Backgammon clubs and on-line forums use a modified form of the [Elo rating system](https://en.wikipedia.org/wiki/Elo_rating_system) to keep track of how well individuals have played and draw inferences about their underlying strength.  The higher the rating, the stronger the player.  Players with higher ratings are inferred to be stronger than those with lower ratings and hence are expected to win; and the longer the match, the more likely the greater skill level will overcome the random chance of the dice.  The animated plot below shows the expected probabilities of two players in the [FIBS (First Internet Backgammon Server)](http://www.fibs.com/) internet forum, where players start at 1500 and the very best reach over 2000.

<img src = "/img/0002-EloProbs.gif" alt="Animated heatmap of player probabilities of winning" style ="width: 500px;"/>

When the two players have the same rating, they are expected to have a 50/50 chance of winning, and this causes the constant diagonal line in the animation above.  When one player is better than the other they have a higher chance of winning, represented for Player A by the increasingly blue space in the bottom right of the plot and the numbers (which are estimated probabilities) labelling the contour lines.  The actual formula used on FIBS and illustrated above is [defined by on the FIBS site](http://www.fibs.com/ratings.html) as:

   Winning prob. = 1 - (1 / (10 ^ ((YOU - HIM) * SQRT(ML) / 2000) + 1))

(As an aside, I don't think there is a strong theoretical reason for the SQRT(ML) in that formula.  With the complications of the doubling cube I very much doubt that the relationship of the probability winning to match length is as simple as that.  But it's a reasonable empirical approximation; I might blog more about that another time.)
   
Now, here's how I made that animation.  In the R code below, first, I load up some functionality and the Google font I use for this blog.  Then I define a function that estimates the probability of a player winning using the FIBS formula above.

{% highlight R linenos %}
library(dplyr)
library(showtext) # for fonts
library(RColorBrewer)
library(directlabels) # for labels on contour lines
library(ggplot2)
library(scales)
library(forecast) # for auto.arima later on

font.add.google("Poppins", "myfont")
showtext.auto()
#==================helper functions=====================

fibs_p <- function(a, b, ml){
   # function to determine the expected probability of player a winning a bachgammon match
   # against player b of length ml, with a and b representing their FIBS Elo ratings
   tmp <- 1 - (1 / (10 ^ ((a - b) * sqrt(ml) / 2000) + 1))
   return(tmp)
}   
{% endhighlight %}

The strategy to create the animation is simple:

* Create all the combinations of two players' Elo ratings.
* Loop through 23 possible match lengths, calculating the probabilities of A winning for each combination of Elo ratings at that match length
* Draw a still image heatmap for each of those 23 match lengths using {ggplot2}
* Compile the images into a single animated Gif using [ImageMagick](http://studio.imagemagick.org/script/index.php).

{% highlight R linenos %}
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
{% endhighlight %}

##Actual ratings varying while "true" rating is constant
The estimated probability of winning against a certain opponent at a given match length might be of interest to backgammon players (I'm surprised it's not referred to more often), but its most common use is embedded in the calculation of how much each player's Elo rating changes after a match is decided.  That formula is well explained by FIBS and a little involved so I won't spell it out here, but you can see it in action in the function I provide later in this post.  A key point for our purposes is that as the win/loss of a match is a random event, players' Elo ratings which are based on those events are random variables.  Even if we grant the existence of a "real" value for each player, it is unobservable, and can only be estimated by their actual Elo rating in a tournament or internet forum.

In fact, my original motivation for this blog post was to see how much Elo ratings fluctuate due to randomness of individual games.  In a later post I'll have a more complex and realistic simulation, but the chart below shows what happens to a player's Elo ratings over time in the following situation:

* Only two players
* They only ever play 5 point matches, and they play 10,000 of them
* Player A's chance of winning the 5 point match is 0.6, and this does not change over time (ie no player is improving in skill relative to the other)

The results are shown in the chart below, and in this unrealistically simple scenario there's more variation than I'd realised there would be.  Player A's actual Elo rating fluctuates fairly wildly around the true value (which can be calculated as 1578.75), hitting 1650 several times and dropping nearly all the way to 1500 at one point.

![Ratings of Player A in 2 player simulation](/img/0002-elo-rating.svg)

Here's the code for that simulation, including my R function that provides FIBS-style Elo ratings, adjusted for experience as set out by FIBS.

{% highlight R linenos %}
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
{% endhighlight %}

In this situation, a player's Elo rating is quite well modelled by an autoregressive AR(1) time series model.  With an autoregression parameter of around 0.98, the rating at any point in time is obviously very closely related to the previous rating; almost but not quite a random walk..Showing how and why would make this post too long, but it's a useful factoid to note for future work when we come to more realistic and complex simulations of Elo ratings on FIBS.

