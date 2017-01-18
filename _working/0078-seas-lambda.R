library(tidyverse)
library(Mcomp)
library(Tcomp)
library(foreach)
library(doParallel)


#=========================analysis functions=======================

# Set up a cluster for parallel computing
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(forecast)
})


#' fit 8 models (all combinations of ets/arima, seasonally adjusted / not, BoxCox transformed / not)
#' to all data series in data_collection.  data_collection needs to be an object of class Mcomp
#' @value data frame with columns for mase (mean absolute scaled error), series number, and
#' three columns with information on the characteristics of the fit model
eval_forecasts <- function(data_collection){
   clusterExport(cluster, "data_collection")
   
   results <- foreach(i = 1:length(data_collection), .combine = rbind) %dopar% {
      the_data <- data_collection[[i]]
   
      x <- the_data$x
      xx <- the_data$xx
      h <- the_data$h
      
      l <- BoxCox.lambda(x)
      l <- max(min(l, 1), 0)
      
      # some time series with 0 in them fail catastrophically with stlm if lambda is close
      # to zero, so we force them to be at most a square rootish transformation
      if(min(x) <= 0){ l <- max(l, 0.5)}
      
      fc <- list()
      ac <- numeric()
      
      # no seasonally adjustment, Box Cox transform
      fc[[1]] <- forecast(auto.arima(x, lambda = l), h = h)
      fc[[2]] <- forecast(ets(x, lambda = l), h = h)
      
      # no seasonally adjustment, no Box Cox transform
      fc[[3]] <- forecast(auto.arima(x), h = h)
      fc[[4]] <- forecast(ets(x), h = h)
      
      # seasonally adjust first, Box Cox transform
      fc[[5]] <- forecast(stlm(x, method = "arima", lambda = l), h = h)
      fc[[6]] <- forecast(stlm(x, method = "ets", lambda = l), h = h)
      
      # seasonally adjust first, no transform
      fc[[7]] <- forecast(stlm(x, method = "arima"), h = h)
      fc[[8]] <- forecast(stlm(x, method = "ets"), h = h)
      
      ac <- sapply(fc, function(mod){
         accuracy(mod, xx)["Test set", "MASE"]
      })
      
      data.frame(mase = ac,
                 model = rep(c("ARIMA", "ETS"), 4),
                 transform = rep(c("Transformed", "Transformed", "Untransformed", "Untransformed"), 2),
                 seasadj = rep(c("Seasonally adjusted", "Unadjusted"), each = 4),
                 series = i)
   }
   return(results)
}

#=============plotting and summary functions====================
# Functions for summarising the results of eval_forecasts()

p1 <- function(results){
   results %>%
      ggplot(aes(y = mase, x = transform, colour = seasadj)) +
      facet_wrap(~model) +
      geom_boxplot() +
      coord_flip() +
      scale_y_log10()
}

p2 <- function(results){
   results %>%
      mutate(seasadj = ifelse(grepl("Seasonal", seasadj), "SeasonallyAdjustedFirst", "Unadjusted")) %>%
      spread(seasadj, mase) %>% 
      ggplot(aes(y = SeasonallyAdjustedFirst, x = Unadjusted, colour = transform)) +
      geom_point(alpha = 0.3) +
      geom_abline(slope = 1, intercept = 0) +
      facet_wrap(~model) +
      scale_x_log10() +
      scale_y_log10()
}

t1 <- function(results){
   results %>%
      group_by(seasadj, transform, model) %>%
      summarise(mase = round(mean(mase, tr = 0.1), 2)) %>%
      spread(seasadj, mase) 
}

#=============apply to data=============

m_results_monthly <- eval_forecasts(subset(M3, "MONTHLY"))
m_results_quarterly <- eval_forecasts(subset(M3, "QUARTERLY"))
t_results_monthly <- eval_forecasts(subset(tourism, "MONTHLY"))
t_results_quarterly <- eval_forecasts(subset(tourism, "QUARTERLY"))

p1(m_results_monthly)
p2(m_results_monthly)
t1(m_results_monthly)

p1(m_results_quarterly)
p2(m_results_quarterly)
t1(m_results_quarterly)

p1(t_results_monthly)
p2(t_results_monthly)
t1(t_results_monthly)

p1(t_results_quarterly)
p2(t_results_quarterly)
t1(t_results_quarterly)
