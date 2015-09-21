library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext) # for fonts
library(forecast)
library(tseries)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

set.seed(123)
true_series <- arima.sim(model = list(ar = c(0.5, -0.1), ma = c(-0.3, 0.2)), n = 5000)

ar_series <- arma(true_series, order = c(4, 1), include.intercept = FALSE)
ma_series <- arma(true_series, order = c(1, 4), include.intercept = FALSE)

ar_series
ma_series


#-------------similar ACF, different models----------------
set.seed(123)
series1 <- arima.sim(model = list(ar = c(0.0215, 0.212, 0.033,-0.041), ma = 0.16), n = 500)
series2 <- arima.sim(model = list(ar = -0.05, ma = c(0.236, 0.225, .086, .014)), n = 500)
par(mfrow = c(1, 2))
acf(series1, type = "partial")
acf(series2, type = "partial")


#-------------------how good is auto.arima at picking order of the ARMA process------------
reps <- 1000
n <- 2 ^ (1:10) * 10
results <- data.frame(n = rep(n, each = reps),
                      correct = logical(reps),
                      fitted = character())
counter <- 0
for(j in 1:length(n)){
   message(n[j])
   for(i in 1:reps){
      counter <- counter + 1
      true_series <- arima.sim(model = list(ar = c(0.5, -0.1), ma = c(-0.3, 0.2)), n = n[j])
      fit <- auto.arima(true_series, stepwise = FALSE)
      results[counter, "fitted"] <- paste(names(fit$coef), collapse = " ")
      if(length(coef(fit)) == 4 && sum(names(fit$coef) %in% c("ar1", "ar2", "ma1", "ma2")) == 4 ){
         results[counter, "correct"] <- TRUE
         
      }   else {
         results[counter, "correct"] <- FALSE
      }
      
   }
}

results %>%
   group_by(n) %>%
   summarise(correct = sum(correct) / length(correct) * 100)


results %>%
   group_by(fitted) %>%
   summarise(count = length(fitted)) %>%
   arrange(count) %>%
   data.frame()

