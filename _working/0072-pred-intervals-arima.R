library(forecast)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(grid)
library(colorspace)

library(extrafont)
theme_set(theme_light(base_family = "Calibri") + 
             theme(legend.position = "bottom") +
             theme(plot.caption = element_text(colour = "grey50"))) 
update_geom_defaults("text", list(family = "Calibri"))
#=============prediction interval functions===================
#' takes a forecast object, and the actual results, and returns a data frame
#' binary indicator of success as well as the horizon for each row and an optional
#' "type" column.
accuracy_pi <- function(fc, actual, type = NULL, labs = c("80%", "95%")){
   if(nrow(fc$upper) != length(actual)){
      stop("`fc` must be a forecast object with `upper` and `lower` objects that are matrices with number of rows equal to length of `actual`")
   }
   actual <- as.vector(actual)
   h <- length(actual)
   # take advantage of actual being a vector that will recycle, going down each
   # column of the two matrices of logical conditions we superimpose here:
   tmp <- as.data.frame(fc$lower < actual & fc$upper > actual)
   names(tmp) <- labs
   tmp <- cbind(tmp, 
                "h" = 1:h,
                "type" = rep(type, h))
   return(tmp)
}

#===================simulations=======================
n <- 100 # size of full series
h <- 10  # how much of the series to forecast forward
R <- 10000 # how many datasets to try this with
the_model <- list(ar = 0.5, ma = 0.5)

cluster <- makeCluster(7) # only any good if you have at least 7 processors :)
registerDoParallel(cluster)

clusterEvalQ(cluster, {
   library(forecast)
   
})

results <- foreach(1:R, .combine = rbind) %dopar% {
   sim_data <- ts(cumsum(arima.sim(model = the_model, n = n)))
   
   x <- sim_data[1:(n - h)]
   actual <- sim_data[(n - h + 1):n]
   mod1 <- auto.arima(x)
   fc1 <- forecast(mod1, h = h)
   
   mod2 <- Arima(x, order = c(1, 1, 1))
   fc2 <- forecast(mod2, h = h)
   rbind(
      accuracy_pi(fc1, actual = actual, type = "auto"),
      accuracy_pi(fc2, actual = actual, type = "pre-knowledge")
   )
}


stopCluster(cluster)

#==========present results==================
pal <- rainbow_hcl(2)

svg("../img/0072-results.svg", 7, 5)
results %>%
   gather(limit, success, -h, -type) %>%
   group_by(h, type, limit) %>%
   summarise(success = mean(success)) %>%
   ggplot(aes(x = h, y = success, colour = type)) +
   facet_wrap(~limit) +
   geom_line() +
   scale_y_continuous(label = percent) +
   ggtitle("Prediction interval coverage of two modelling methods",
           subtitle = "Comparing auto.arima with pre-knowledge of the true data generating process") +
   labs(x = "Number of periods forecast forward",
        caption = "Data generated with an ARIMA(1,1,1) process, which is much more regular than real life data.",
        colour = "Modelling approach:") +
   scale_colour_manual(values = pal) +
   scale_x_continuous(breaks = 0:5 * 2)

grid.text(0.3, y= 0.65,
          label = str_wrap("When the true model family, and the true meta-parameters p, d, q are known, coverage is close to the 80% and 95% promised.", 33),
          gp = gpar(family = "myfont", col = pal[2], cex = 0.8))
grid.text(0.8, y= 0.45,
          label = str_wrap("When the meta parameters p, d and q are estimated from the data, coverage is materially less.", 33),
          gp = gpar(family = "myfont", col = pal[1], cex = 0.8))

dev.off()   
 
#=================demo data============


svg("../img/0072-examples.svg", 8, 7)
# four example datasets
set.seed(123) # for reproducibility
par(mfrow = c(2, 2), bty = "l")
for(i in 1:4){
   
   sim_data <- ts(cumsum(arima.sim(model = the_model, n = n)))
   
   x <- sim_data[1:(n - h)]
   actual <- sim_data[(n - h + 1):n]
   
   mod2 <- Arima(x, order = c(1, 1, 1))
   fc2 <- forecast(mod2, h = h)   
   plot(fc2, main = paste0("Simulated ARIMA(1,1,1) data: ", i))
   lines((n - h + 1):n, actual, col = "red")
}
dev.off()

setwd("../img")
files <- list.files()
files <- files[grepl("^0072.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 1200x1200', " ", i, " ", output)
   system(cmd)
   
}