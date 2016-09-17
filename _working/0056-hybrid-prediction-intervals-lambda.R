#------------------setup------------------------
library(ggplot2)
library(scales)
library(forecast)
library(fpp) # for example data
library(Mcomp)
library(tidyr)
library(dplyr)
library(forecastHybrid)
library(knitr) # for kable




x <- austourists # international tourists to Australia, visitor nights
bcl <- BoxCox.lambda(x)
fc <- hybridModel(x, models = "ae", lambda = bcl)
p1 <- autoplot(forecast(fc, 10)) + 
   ggtitle("Visitor nights in Australia", 
      subtitle = "Forecast with hybrid of ETS and auto.arima,\nwith automated choice of Box-Cox lambda") +
   labs(fill = "Prediction interval\nconfidence level",
        y = "Visitor nights per quarter (millions)")

svg("../img/0056-austourists.svg", 6, 5)
   print(p1)
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
)
num_series <- length(M3) # ie 3003
# num_series <- 100 # during dev.  Comment out when doing for full dataset
# make matrix to hold results for ets, auto.arima, and hybrid - 
# with and without Box-Cox transforms - and for thetaf
# Two columns per method as we look at 80% and 95% intervals
results <- matrix(0, nrow = num_series, ncol = 7 * 2 + 1)

for(i in 1:num_series){
   cat(i, " ")        # let me know how it's going as it loops through...
   series <- M3[[i]]
   x <- series$x      # ie the data to be fitted
   xx <- series$xx    # ie the true, actual values of the forecast period
   h <- length(xx)    # ie the length of the forecast period
   
   # If the data are all positive, we estimate an optimal value of lambda
   # for a Box-Cox transformation
   if(min(x) >=0){
      bcl <- BoxCox.lambda(x)      
   } else {
      bcl <- 1
   }

   
   mod <- hybridModel(x, models = "ae")
   mod_bc <- hybridModel(x, models = "ae", lambda = bcl)
   
   fc_ets <- forecast(mod$ets, h = h)
   fc_aa <- forecast(mod$auto.arima, h = h)
   fc_hyb <- forecast(mod, h = h)
   
   fc_bc_ets <- forecast(mod_bc$ets, h = h)
   fc_bc_aa <- forecast(mod_bc$auto.arima, h = h)
   if(bcl < 0 &&
      (with(fc_bc_ets, sum(is.na(cbind(mean, upper, lower)))) > 0) ||
      (with(fc_bc_aa, sum(is.na(cbind(mean, upper, lower)))) > 0)){
      # refit model with lambda of 0 instead of the negative value,
      # as the original negative value returned NAs
      bcl <- 0
      mod_bc <- hybridModel(x, models = "ae", lambda = bcl)
      fc_bc_ets <- forecast(mod_bc$ets, h = h)
      fc_bc_aa <- forecast(mod_bc$auto.arima, h = h)
   }
   
   fc_bc_hyb <- forecast(mod_bc, h = h)
   
   fc_th <- thetaf(x, h = h)
   
   results[i, 1:2] <- pi_accuracy(fc_ets, xx)$Success
   results[i, 3:4] <- pi_accuracy(fc_aa, xx)$Success
   results[i, 5:6] <- pi_accuracy(fc_hyb, xx)$Success
   
   results[i, 7:8] <- pi_accuracy(fc_bc_ets, xx)$Success
   results[i, 9:10] <- pi_accuracy(fc_bc_aa, xx)$Success
   results[i, 11:12] <- pi_accuracy(fc_bc_hyb, xx)$Success
   
   results[i, 13:14] <- pi_accuracy(fc_th, xx)$Success
   
   results[i, 15] <- h
}

results <- as.data.frame(results)

names(results) <- c("ets80", "ets95", "aa80", "aa95", "hyb80", "hyb95", 
                    "etsbc80", "etsbc95", "aabc80", "aabc95", "hybbc80", "hybbc95", 
                    "theta80", "theta95", "h")

# The results are saved as percentages that were in the intervals,
# and the forecast lengths are different, so we need to weight by
# forecast length (h) to get the actual total percentage of observations
# that were within prediction interval.  This code produces the table
# reproduced in the blog post above:
res_sum <- results %>% 
   gather(variable, value, -h) %>%
   mutate(weighted_value = value * h) %>%
   group_by(variable) %>%
   summarise(Success = round(sum(weighted_value) / sum(h), 2)) %>%
   mutate(model = ifelse(grepl("aa", variable), "auto.arima", "theta"),
          model = ifelse(grepl("ets", variable), "ets", model),
          model = ifelse(grepl("hyb", variable), "hybrid", model),
          transform = ifelse(grepl("bc", variable), "Box-Cox", "None"),
          level = ifelse(grepl("80", variable), "80%", "95%")
   ) 

kable(res_sum)

res_sum %>%
   ggplot(aes(x = Success, y = model, colour = transform)) +
   facet_wrap(~level, scales = "free_x") +
   geom_point() +
   scale_x_continuous(label = percent)

svg("../img/0028-indiv-error.svg", 9, 7)
print(
   results %>%
      gather(variable, value, -h) %>%
      mutate(Level = ifelse(grepl("80", variable), "80%", "95%"),
             Level = factor(Level, levels = c("95%", "80%")),
             variable = gsub("[0-9]", "", variable)) %>%
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


