library(ggplot2)
library(forecast)
library(Tcomp)
library(doParallel)
library(dplyr)
library(tidyr)
library(Mcomp)
#============set up cluster for parallel computing===========
cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(Tcomp)
   library(Mcomp)
   library(forecast)
})

#==========the actual analytical function==============
competition <- function(collection, maxfors = length(collection)){
   if(class(collection) != "Mcomp"){
      stop("This function only works on objects of class Mcomp, eg from the Mcomp or Tcomp packages.")
   }
   nseries <- length(collection)
   mases <- foreach(i = 1:maxfors, .combine = "rbind") %dopar% {
      thedata <- collection[[i]]  
      fc1 <- forecast(ets(thedata$x), h = thedata$h)
      fc2 <- forecast(stlm(thedata$x), h = thedata$h)
      fc3 <- thetaf(thedata$x, h = thedata$h)
      mase <- c(accuracy(fc1, thedata$xx)[2, "MASE"],
                accuracy(fc2, thedata$xx)[2, "MASE"],
                accuracy(fc3, thedata$xx)[2, "MASE"])
      return(mase)
   }
   colnames(mases) <- c("ets", "stlm", "thetaf")
   return(mases)
}

## Test on a small set of data, useful during dev
small_collection <- list(tourism[[100]], tourism[[200]], tourism[[300]], tourism[[400]], tourism[[500]], tourism[[600]])
class(small_collection) <- "Mcomp"
test1 <- competition(small_collection)
round(test1, 1)

#===========application=================
# takes a few minutes to run
t4 <- competition(subset(tourism, "quarterly"))
t12 <- competition(subset(tourism, "monthly"))
m4 <- competition(subset(M1, "quarterly"))
m12 <- competition(subset(M1, "monthly"))



# shut down cluster to avoid any mess:
stopCluster(cluster)

#===========results=============

results <- as.data.frame(rbind(t4, t12, m4, m12) ) %>%
   mutate(dataset = rep(c("Tourism", "M1"), 
                        times = c(nrow(t4) + nrow(t12), nrow(m4) + nrow(m12)))) %>%
   mutate(period = rep(c("Quarterly", "Monthly", "Quarterly", "Monthly"),
                       times = c(nrow(t4), nrow(t12), nrow(m4), nrow(m12)))) %>%
   gather(method, MASE, -period, -dataset)

results %>%
   ggplot(aes(x = MASE, colour = method, fill = method)) +
   geom_density(alpha = 0.1) + 
   geom_rug() +
   facet_grid(period ~ dataset) +
   scale_x_sqrt()

results %>%
   group_by(period, dataset, method) %>%
   summarise(MASE = mean(MASE, tr = 0.1)) %>%
   ggplot(aes(x = period, y = MASE, label = method, colour = method)) +
   facet_wrap(~dataset) +
   geom_text() +
   scale_colour_brewer(palette = "Set1") +
   theme(legend.position = "none") +
   labs(x = "", y = "Trimmed mean of Mean Absolute Scaled Error", colour = "") +
   ggtitle("Comparison of three related forecasting methods",
           subtitle = "'Tourism' and 'M1' competition datasets")


results %>%
   group_by(period, dataset, method) %>%
   summarise(MASE = mean(MASE, tr = 0)) %>%
   ggplot(aes(x = period, y = MASE, label = method, colour = method)) +
   facet_wrap(~dataset) +
   geom_text() +
   scale_colour_brewer(palette = "Set1") +
   theme(legend.position = "none") +
   labs(x = "", y = "Mean of Mean Absolute Scaled Error", colour = "") +
   ggtitle("Comparison of three related forecasting methods",
           subtitle = "'Tourism' and 'M1' competition datasets")
