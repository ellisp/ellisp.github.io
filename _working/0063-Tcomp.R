library(Tcomp)
library(forecastHybrid)
library(tidyr)
library(dplyr)
library(parallel)
library(english)
library(directlabels)

#' Function to perform five forecasts to all members of a set of series
#' ARIMA, ETS, Theta, Naive and ensemble of ETS and ARIMA
#' @param the_series a list of class Mcomp containing series of class Mdata
#' @param tests a list of horizons over which to return the MASE of the forecast
accuracy_point <- function (the_series, tests = list(the_series$h)) {
   x <- the_series$x
   xx <- the_series$xx
   h <- the_series$h
   
   # determine best value for BoxCox transformation, 
   # but limit it to between 0 and 1
   bc <- BoxCox.lambda(x)
   bc <- min(max(0, bc), 1)
   
   # fit ARIMA and ETS models
   mod1 <- hybridModel(x, models ="ae", 
                       a.args = list(lambda = bc), e.args = list(lambda = bc))
   
   # create forecasts
   fc1 <- forecast(mod1$auto.arima, h = h)
   fc2 <- forecast(mod1$ets, h = h)
   fc3 <- thetaf(x, h = h)
   fc4 <- snaive(x, h = h)
   fc5 <- forecast(mod1, h = h)
   
   # estimate MASE for all the values of tests
   MASEs <- matrix(0, nrow = 5, ncol = length(tests))
   for (j in 1:length(tests)) {
      this_test <- tests[[j]]
      MASEs[, j] <- c(accuracy(fc1, xx, test = this_test)["Test set", "MASE"], 
                      accuracy(fc2, xx, test = this_test)["Test set", "MASE"], 
                      accuracy(fc3, xx, test = this_test)["Test set", "MASE"], 
                      accuracy(fc4, xx, test = this_test)["Test set", "MASE"],
                      accuracy(fc5, xx, test = this_test)["Test set", "MASE"])                                                                                                                                                            
   }
   
   colnames(MASEs) <- gsub(":", "-", as.character(tests))
   rownames(MASEs) <- c("ARIMA", "ETS", "Theta", "Naive", "ARIMA-ETS average")
   return(MASEs)
}

#------------Set up multi core cluster----------------
# set up cluster with number of cores minus one
cores <- detectCores()
cluster <- makePSOCKcluster(max(1, cores - 1))

# set up the functionality on the cluster
clusterEvalQ(cluster, {
   library(Tcomp)
   library(forecastHybrid)
})
clusterExport(cluster, "accuracy_point")


#-----------------Fit to tourism data----------------------
# takes a few minutes on my laptop:
results_point <- parLapply(cluster,
                           subset(tourism, "quarterly"),
                           accuracy_point,
                           tests = list(2, 4, 6, 8))

stopCluster(cluster)

#----------------presenting results-----------
results_point_df <- as.data.frame(do.call("rbind", results_point))
results_point_df$model <- row.names(results_point_df)
results_point_df <- results_point_df %>%
   gather(horizon, MASE, -model) %>%
   group_by(model, horizon) %>%
   summarise(MASE = round(mean(MASE), 2)) %>%
   mutate(horizon = as.numeric(horizon)) %>%
   ungroup() %>%
   mutate(model = gsub("ARIMA-ETS average", "Hybrid", model))

p <- results_point_df %>%
   ggplot(aes(x = horizon, y = MASE, colour = model)) +
   geom_point(size = 2) +
   geom_line() +
   scale_colour_brewer(palette = "Set1") +
   labs(x = "Forecasting horizon",
        y = "Mean Absolute Scaled Error (lower is better)",
        caption = "Source: 2010 tourism competition data in Tcomp R package") +
   ggtitle("It's hard to beat a naive forecast or a Theta forecast",
           subtitle = "Comparison of forecasting methods with tourism competition data")

svg("../img/0063-results.svg", 7, 6)
direct.label(p, "last.qp")
dev.off()

png("../img/0063-results.png", 700, 600, res = 100)
direct.label(p, "last.qp")
dev.off()
