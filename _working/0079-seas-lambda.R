library(tidyverse)
library(Mcomp)
library(Tcomp)
library(foreach)
library(doParallel)
library(scales)
library(broom)
library(stringr)
library(stargazer)
library(lme4)
library(forcats)


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
                 transform = rep(c("BoxCox", "BoxCox", "None", "None"), 2),
                 seasadj = rep(c("Seasonal in model", "Seasonally adjusted first"), each = 4),
                 series = i)
   }
   return(results)
}

#=============plotting and summary functions====================
# Functions for summarising the results of eval_forecasts()
p2 <- function(results){
   results %>%
      spread(seasadj, mase) %>% 
      ggplot(aes(y = `Seasonally adjusted first`, x = `Seasonal in model`, colour = transform)) +
      geom_point(alpha = 0.2) +
      geom_abline(slope = 1, intercept = 0) +
      geom_smooth(se = FALSE) +
      facet_wrap(~model) +
      scale_x_log10() +
      scale_y_log10() +
      coord_equal() +
      labs(x = "Mean absolute scaled error from fitting a model to the original data",
           y = "Error from fitting a model to data\nthat was first seasonally adjusted",
           colour = "")
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

#-----------------plots of individual results--------------

p2(m_results_monthly);
t1(m_results_monthly)

p2(m_results_quarterly)
t1(m_results_quarterly)

svg("../img/0079-m-results-quarterly-2.svg", 8, 5)
p2(m_results_quarterly ) +
   ggtitle("Marginally better results on average from seasonally adjusting\na series prior to modelling and forecasting",
           "... and little obvious change from choosing to Box-Cox transform or not") +
   labs(caption = "Quarterly data from the M3 forecasting competition")
dev.off()

p2(t_results_monthly)
t1(t_results_monthly)

p2(t_results_quarterly)
t1(t_results_quarterly)


#--------------------combined results-------------------
all_results <- rbind(
   m_results_monthly,
   m_results_quarterly,
   t_results_monthly,
   t_results_quarterly
) %>%
   mutate(
      collection = rep(c("M3", "Tourism"), 
                       c(nrow(m_results_monthly) + nrow(m_results_quarterly),
                         nrow(t_results_monthly) + nrow(t_results_quarterly))),
      frequency = rep(c("monthly", "quarterly", "monthly", "quarterly"), 
                      c(nrow(m_results_monthly) , nrow(m_results_quarterly),
                        nrow(t_results_monthly) , nrow(t_results_quarterly))),
      dataset_series = paste(collection, frequency, series),
      collection_frequency = paste(collection, frequency)
   ) %>%
   as_tibble()

svg("../img/0079-all-boxplot.svg", 8, 6)
ggplot(all_results, aes(x = transform, colour = seasadj, y = mase)) +
   facet_grid(model ~ collection_frequency) +
   geom_boxplot()  +
   coord_flip() +
   scale_y_log10("Mean absolute scaled error") +
   labs(x = "", colour = "", 
        caption = "2,977 quarterly and monthly datasets from the M3 and Tourism forecasting competitions") +
   ggtitle("Comparison of variants in forecasting methods",
           "Box-Cox transformation or not; seasonally adjust the data before model fitting or not.")
dev.off()

#==========modelling of results======================
model <- lmer(mase ~ model + transform + seasadj + frequency + collection + (1|dataset_series), data = all_results)
model_inters <- lmer(mase ~ model * (transform * seasadj + frequency + collection) + (1|dataset_series), data = all_results)

AIC(model_inters, model)

stargazer(model, type = "html")
stargazer(model_inters, type = "html")



# Interpreting all those interactions is basically impossible as-is, so just for
# illustrating results I fit models separately to ETS and ARIMA
model_ets <- lmer(mase ~ transform * seasadj + frequency + collection + (1|dataset_series), 
                  data = subset(all_results, model == "ETS"))
model_arima <- lmer(mase ~ transform * seasadj + frequency + collection + (1|dataset_series), 
                  data = subset(all_results, model == "ARIMA"))


stargazer(model_ets, type = "html")
stargazer(model_arima, type = "html")

effects_ets <- cbind(tidy(confint(model_ets))[-(1:3), ],
                 tidy(model_ets)[2:6, ],
                 model = "ETS")

effects_arima <- cbind(tidy(confint(model_arima))[-(1:3), ],
                     tidy(model_arima)[2:6, ],
                     model = "ARIMA")

effects <- rbind(effects_ets, effects_arima)

names(effects)[1:3] <- c("variable", "lower", "upper")

svg("../img/0079-lmer-results.svg", 11, 5)
effects %>%
   ggplot(aes(x = variable, y = estimate)) +
   facet_wrap(~model) +
   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4)  +
   geom_point() +
   coord_flip() +
   geom_hline(yintercept = 0, colour = "red") +
   labs(y = "Impact on forecasting, measured by mean absolute scaled error", x = "",
        caption = "Tested on all 2,977 quarterly and monthly datasets from the M3 and Tourism forecasting competitions") +
   ggtitle("Small but noticeable impact of different forecasting methods",
"ARIMA works best when seasonally adjusted beforehand rather than seasonality included in the model.
For ETS, no significant evidence either seasonal adjustment strategy is better.
For both models, it seems better not to automatically use a Box-Cox transformation.")
dev.off()

all_results %>%
   group_by(model, transform, seasadj) %>%
   summarise(mase = round(mean(mase, tr = 0.1), 3)) %>%
   spread(model, mase) %>%
   knitr::kable("html")


#=========save PNG versions=======
setwd("../img")
files <- list.files()
files <- files[grepl("^0079.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\"', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")


summary(sapply(subset(tourism, "MONTHLY"), function(x){x$h}))
summary(sapply(subset(tourism, "QUARTERLY"), function(x){x$h}))
summary(sapply(subset(M3, "MONTHLY"), function(x){x$h}))
summary(sapply(subset(M3, "QUARTERLY"), function(x){x$h}))
