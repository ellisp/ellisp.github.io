library(ggplot2)
library(forecast)
library(Tcomp)
library(doParallel)
library(dplyr)
library(tidyr)
library(Mcomp)


#======example datasets=============

svg("../img/0070-eg1.svg", 8, 8)
# Example 1
x <- tourism[[150]]$x
h <- tourism[[150]]$h
par(mfrow = c(4, 1), bty = "l", font.main = 1, family = "myfont")
plot(forecast(ets(x), h = h)); grid()
plot(forecast(stlm(x), h = h)); grid()
plot(thetaf(x, h = h)); grid()
plot(tourism[[150]], main = "Actual results - `tourism` data series 150"); grid()
dev.off()

svg("../img/0070-eg2.svg", 8, 8)
# Example 2
x <- tourism[[600]]$x
h <- tourism[[600]]$h
par(mfrow = c(4, 1), bty = "l", font.main = 1, family = "myfont")
plot(forecast(ets(x), h = h)); grid()
plot(forecast(stlm(x), h = h)); grid()
plot(thetaf(x, h = h)); grid()
plot(tourism[[600]], main = "Actual results - `tourism` data series 600"); grid()
dev.off()

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

# shut down cluster
stopCluster(cluster)

#===========results=============

results <- as.data.frame(rbind(t4, t12, m4, m12) ) %>%
   mutate(dataset = rep(c("Tourism", "M1"), 
                        times = c(nrow(t4) + nrow(t12), nrow(m4) + nrow(m12)))) %>%
   mutate(period = rep(c("Quarterly", "Monthly", "Quarterly", "Monthly"),
                       times = c(nrow(t4), nrow(t12), nrow(m4), nrow(m12)))) %>%
   gather(method, MASE, -period, -dataset)

svg("../img/0070-density.svg", 8, 5)
results %>%
   ggplot(aes(x = MASE, colour = method, fill = method)) +
   geom_density(alpha = 0.1) + 
   geom_rug() +
   scale_colour_brewer("", palette = "Set1") +
   scale_fill_brewer("", palette = "Set1") +
   facet_grid(period ~ dataset) +
   scale_x_sqrt()
dev.off()

svg("../img/0070-trmean.svg", 8, 4.5)
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
dev.off()

svg("../img/0070-mean.svg", 8, 4.5)
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
dev.off()