library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext) # for fonts
library(forecast)
library(tseries)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

#-------------------how good is auto.arima at picking order of the ARMA process------------
arima_fit_sim <- function(model, 
                          correct_params, 
                          n = 2 ^ (1:13) * 10, 
                          reps = 10,
                          stepwise = FALSE,
                          verbose = TRUE){
   require(forecast)
   if(verbose){require(dplyr)}
   
   results <- data.frame(n = rep(n, each = reps),
                      correct = FALSE,
                      fitted = "",
                      stringsAsFactors = FALSE)
   counter <- 0

   for(j in 1:length(n)){
      if(verbose){message(n[j])}
      for(i in 1:reps){
         counter <- counter + 1
         true_series <- arima.sim(model, n = n[j])
         fit <- auto.arima(true_series, stepwise = stepwise)
         results[counter, "fitted"] <- paste(names(fit$coef), collapse = " ")
         if(length(coef(fit)) == length(correct_params) && sum(names(fit$coef) %in% correct_params) == length(correct_params) ){
            results[counter, "correct"] <- TRUE
            
         }   else {
            results[counter, "correct"] <- FALSE
         }
         
      }
   }
   
   if(verbose){
      print(
         results %>%
            group_by(n) %>%
            summarise(correct = sum(correct) / length(correct) * 100)
         )
   }

   return(results)
   }


our_reps <- 5

results_ar1 <- arima_fit_sim(model = list(ar = c(0.5)),
                             correct_params = c("ar1"), 
                             reps = our_reps,
                             n = 2 ^ (1:8) * 10)

results_ma1 <- arima_fit_sim(model = list(ma = c(0.5)),
                             correct_params = c("ma1"), 
                             reps = our_reps)


results_arma22 <- arima_fit_sim(model = list(ar = c(0.5, -0.1), ma = c(-0.3, 0.2)),
                                correct_params = c("ar1", "ar2", "ma1", "ma2"), 
                                reps = our_reps)

results_arma33 <- arima_fit_sim(model = list(ar = c(0.5, -0.1, 0.3), ma = c(-0.3, 0.2, 0.4)),
                                correct_params = c("ar1", "ar2", "ar3", "ma1", "ma2", "ma3"), 
                                reps = our_reps)



results_ar1 %>%
   group_by(n) %>%
   summarise(correct = sum(correct) / length(correct) * 100)


