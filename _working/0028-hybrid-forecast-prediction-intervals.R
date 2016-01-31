#------------------setup------------------------
library(showtext)
library(ggplot2)
library(scales)
library(forecast) # probably should rebuild this as a fork
library(Mcomp)
library(tidyr)
library(dplyr)
library(knitr) # for kable

source("https://raw.githubusercontent.com/ellisp/forecast/dev/R/hybridf.R")

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))


svg("../img/0028-USAccDeaths.svg", 6, 7)
fc <- hybridf(USAccDeaths)
par(mfrow = c(3, 1), bty = "l", family = "myfont")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)
dev.off()

png("../img/0028-USAccDeaths.png", 600, 700, res = 100)
fc <- hybridf(USAccDeaths)
par(mfrow = c(3, 1), bty = "l", family = "myfont")
plot(fc)
plot(fc$fc_ets)
plot(fc$fc_aa)
dev.off()



pi_accuracy <- function(fc, yobs){
   # checks the success of prediction intervals of an object of class 
   # forecast with actual values
   if(length(yobs) != length(fc$mean)){
      stop("yobs needs to be the same length as the forecast period.")
   }
   n <- length(yobs)
   yobsm <- cbind(yobs, yobs)
   In <- (yobsm > fc$lower & yobsm < fc$upper) # relies on recycling yobs
   colnames(In) <- c("Series 1", "Series 2")
   Success <- colMeans(In)
   return(list(In = In, Success = Success, n = n))
   
}

#============forecasting with default values===============
num_series <- length(M3) # ie 3003
results <- matrix(0, nrow = num_series, ncol = 7)

for(i in 1:num_series){
   cat(i, " ")        # let me know how it's going as it loops through...
   series <- M3[[i]]
   x <- series$x      # ie the data to be fitted
   xx <- series$xx    # ie the true, actual values of the forecast period
   h <- length(xx)    # ie the length of the forecast period
   
   fc3 <- hybridf(x, h = h)
   results[i, 5:6] <- pi_accuracy(fc3, xx)$Success
   
   fc1 <- fc3$fc_ets
   results[i, 1:2] <- pi_accuracy(fc1, xx)$Success
   
   fc2 <- fc3$fc_aa
   results[i, 3:4] <- pi_accuracy(fc2, xx)$Success
   
   results[i, 7] <- h
}

results <- as.data.frame(results)

names(results) <- c("ets_p80", "ets_p95", "auto.arima_p80", "auto.arima_p95",
                    "hybrid_p80", "hybrid_p95", "h")

# The results are saved as percentages that were in the intervals,
# and the forecast lengths are different, so we need to weight by
# forecast length (h) to get the actual total percentage of observations
# that were within prediction interval.  This code produces the table
# reproduced in the blog post above:
results %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2)) %>%
   kable()


svg("../img/0028-indiv-error.svg", 9, 7)
print(
results %>%
   gather(variable, value, -h) %>%
   mutate(Level = ifelse(grepl("p80", variable), "80%", "95%"),
          Level = factor(Level, levels = c("95%", "80%")),
          variable = gsub("_p[0-9].", "", variable)) %>%
   ggplot(aes(x = h, y = value, colour = Level)) +
   facet_grid(Level~variable) +
   scale_y_continuous("Percentage of actual results within forecast prediction interval\n",
                      label = percent, breaks = c(0, .25, .5, .75, .8, .95, 1)) +
   labs(x = "Forecast period", colour = "Desired level") +
   ggtitle("Prediction interval success for three forecasting\nmethods on 3,003 M3 timeseries") +
   geom_jitter(alpha = 0.2, width = 1.3, height = 0.1, shape = 1) +
   geom_smooth(se = FALSE, method = "lm") +
   theme(panel.grid.minor = element_blank())
)
dev.off()




#=====with bootstrapping instead of formulae for the prediction intervals=============

num_series <- length(M3)
resultsb <- matrix(0, nrow = num_series, ncol = 7)

for(i in 1:num_series){
   cat(i, " ")
   series <- M3[[i]]
   x <- series$x
   xx <- series$xx
   h <- length(xx)
   
   fc3 <- hybridf(x, h = h, simulate = TRUE, bootstrap.ets = TRUE, bootstrap.aa = TRUE)
   resultsb[i, 5:6] <- pi_accuracy(fc3, xx)$Success
   
   fc1 <- fc3$fc_ets
   resultsb[i, 1:2] <- pi_accuracy(fc1, xx)$Success
   
   fc2 <- fc3$fc_aa
   resultsb[i, 3:4] <- pi_accuracy(fc2, xx)$Success
   
   resultsb[i, 7] <- h
}

resultsb <- as.data.frame(resultsb)

names(resultsb) <- c("ets_p80", "ets_p95", "auto.arima_p80", "auto.arima_p95",
                    "hybrid_p80", "hybrid_p95", "h")

resultsb %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2)) %>% 
   kable()
   


save.image("0028.RData")


#========================draw example images===================
results %>%
   mutate(series = 1:3003) %>%
   arrange(hybrid_p95) %>%
   head()

series <- M3[[482]]

x <- series$x
xx <- series$xx
h <- length(xx)

fc3 <- hybridf(x, h = h)

plot(fc3); grid()
lines(xx, col = "red")
