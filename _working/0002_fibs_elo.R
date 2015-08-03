library(dplyr)
library(showtext)
library(RColorBrewer)
library(directlabels)
library(ggplot2)
library(scales)
library(extrafont)



fibs_p <- function(a, b, ml){
   # function to determine the expected probability of player a winning a bachgammon match
   # against player b of length ml, with a and b representing their FIBS Elo ratings
   tmp <- 1 - (1 / (10 ^ ((a - b) * sqrt(ml) / 2000) + 1))
   return(tmp)
}


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