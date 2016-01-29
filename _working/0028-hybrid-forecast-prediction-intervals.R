#------------------setup------------------------
library(showtext)
library(ggplot2)
library(scales)
library(forecast) # probably should rebuild this as a fork
library(Mcomp)
library(tidyr)
library(dplyr)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))



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

num_series <- length(M3)
results <- matrix(0, nrow = num_series, ncol = 7)

for(i in 1:num_series){
   cat(i, " ")
   series <- M3[[i]]
   x <- series$x
   xx <- series$xx
   h <- length(xx)
   
   fc1 <- forecast(ets(x), h = h)
   results[i, 1:2] <- pi_accuracy(fc1, xx)$Success
   
   fc2 <- forecast(auto.arima(x), h = h)
   results[i, 3:4] <- pi_accuracy(fc2, xx)$Success
   
   
   fc3 <- hybridf(x, h = h)
   results[i, 5:6] <- pi_accuracy(fc3, xx)$Success
   
   results[i, 7] <- h
}

results <- as.data.frame(results)

names(results) <- c("ets_p80", "ets_p95", "auto.arima_p80", "auto.arima_p95",
                    "hybrid_p80", "hybrid_p95", "h")

results %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2))

par(mfrow = c(3, 1))
plot(fc1)
lines(xx, col = "red")
plot(fc2)
lines(xx, col = "red")
plot(fc3)
lines(xx, col = "red")


colMeans(results)
