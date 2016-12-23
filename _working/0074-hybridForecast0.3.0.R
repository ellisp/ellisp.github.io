library(forecastHybrid)
library(ggplot2)
library(scales)
library(tidyverse)

# median duration of unemployment, in weeks
uempmed <- ts(economics$uempmed, start = c(1967, 7), frequency = 12)

BoxCox.lambda(uempmed)

m1 <- hybridModel(uempmed, lambda = 0, weights = "equal")

m2 <- hybridModel(uempmed, lambda = 0, weights = "cv.errors")

f1 <- forecast(m1, 24)
f2 <- forecast(m2, 24)

svg("../img/0074-base-forecast.svg", 7, 8)
par(family = "myfont")
par(mfrow = c(2, 1), font.main = 1, bty = "l", mar = c(4,3,6,2) + 0.1, cex.main = 0.6)
plot(f1)
plot(f2)
dev.off()

svg("../img/0074-six-forecasts.svg", 8, 6)
par(mfrow = c(3, 2), bty = "l", cex = 0.8)
plot(f2$thetam)
plot(f2$ets)
plot(f2$stlm)
plot(f2$auto.arima)
plot(f2$nnetar)
plot(f2$tbats)
dev.off()

svg("../img/0074-gg-forecast.svg", 6, 4.5)
autoplot(f2) +
   ggtitle("Forecast median length of unemployment",
           subtitle = "Six component models, weights chosen by cross-validation") +
   labs(y = "Weeks", x = "",
        caption = "Source: 'economics' in ggplot2 R package,\nultimately from http://research.stlouisfed.org/fred2")
dev.off()

svg("../img/0074-six-forecasts.svg", 8, 6)
par(mfrow = c(3, 2), bty = "l", cex = 0.8)
plot(f2$thetam)
plot(f2$ets)
plot(f2$stlm)
plot(f2$auto.arima)
plot(f2$nnetar)
plot(f2$tbats)
dev.off()

#----------demo more arguments-----------------
# just ARIMA and Theta models
m3 <- hybridModel(uempmed, models = "af", weights = "cv.errors",
                  cvHorizon = 24, horizonAverage = TRUE, 
                  windowSize = 60)


#------------demo theta----------------
f3 <- thetaf(uempmed, h = 24)
f4 <- forecast(thetam(uempmed), h = 24)

svg("../img/0074-theta.svg")
data.frame(thetaf = f3$mean, thetam = f3$mean) %>%
   mutate_each("as.numeric") %>%
   ggplot(aes(x = thetaf, y = thetam)) +
   geom_abline(intercept = 0, slope = 1) +
   geom_point() +
   labs(x = "thetaf()", y = "forecast(thetam())",
        caption = "Forecasts are of median length of unemployment in the USA") +
   ggtitle("Separating thetaf into a modelling function with a forecast method",
           subtitle = "The two methods give identical results")
dev.off()



setwd("../img")
files <- list.files()
files <- files[grepl("^0074.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 800x800', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")