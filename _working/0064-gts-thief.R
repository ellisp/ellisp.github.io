library(dplyr)
library(tidyr)
library(forecast)
library(hts)
library(thief)
library(forecastHybrid)
library(directlabels)
library(RColorBrewer)

itm_orig <- read.csv("../data/ITM339501_20161029_031330_76.csv", skip =4, stringsAsFactors = FALSE)

itm <- itm_orig
names(itm) <- c("Month", "ArrivalsAustralia", "ArrivalsTotal", "DeparturesAustralia", "DeparturesTotal")

itm <- itm %>%
   mutate(ArrivalsOther = ArrivalsTotal - ArrivalsAustralia,
          DeparturesOther = DeparturesTotal - DeparturesAustralia) %>%
   select(-ArrivalsTotal, -DeparturesTotal) %>%
   mutate(DeparturesAustralia = -DeparturesAustralia,
          DeparturesOther = -DeparturesOther) %>%
   filter(!is.na(ArrivalsAustralia))

itm_ts <- ts(as.matrix(itm[ , 2:5]), frequency = 12, start = c(1978, 4))
itm_net <- ts(apply(itm_ts, 1, sum), frequency = 12, start = c(1978, 4))
plot(itm_ts)
plot(itm_net)
monthplot(itm_net, bty = "l"); grid()

#------------------grouped forecast--------------------------
groups <- rbind(
   c("Arrivals", "Departures", "Arrivals", "Departures"),
   c("Australia", "Australia", "Other", "Other")
)
rownames(groups) <- c("Direction", "Location")
itm_gts <- gts(itm_ts, groups = groups)
plot(itm_gts, bty = "l")


itm_fc1a <- forecast(itm_gts, 36, weights = "mint", fmethod = "ets",
                   parallel = TRUE, num.cores = 7)

itm_fc1b <- forecast(itm_gts, 36, weights = "mint", fmethod = "arima",
                    parallel = TRUE, num.cores = 7)

plot(itm_fc1b, bty = "l", col = brewer.pal(4, "Set2"), color_lab = TRUE)

#---------------thief forecast--------------------------
itm_aggs <- tsaggregates(itm_net)
autoplot(itm_aggs)

itm_fc3a <- thief(itm_net, h = 36, usemodel = "ets")
itm_fc3b <- thief(itm_net, h = 36, usemodel = "arima")


#----------------more traditional methods-------------------
itm_mods2 <- hybridModel(itm_net, models = "ae")
itm_fc2a <- forecast(itm_mods2$ets, 36) # just ETS
itm_fc2b <- forecast(itm_mods2$auto.arima, 36) # just auto.arima
itm_fc2c <- forecast(itm_mods2, 36)     # ensemble model
itm_mods5 <- hybridModel(itm_net) # all five models
itm_fc2d <- forecast(itm_mods5, 36) # all five

#------------compare them all------------------
results <- data.frame(grouped_ets  = apply(itm_fc1a$bts, 1, sum), 
              grouped_arima = apply(itm_fc1b$bts, 1, sum), 
              thief_ets = itm_fc3a$mean,
              thief_arima = itm_fc3b$mean,
              ets = itm_fc2a$mean, 
              arima = itm_fc2b$mean, 
              hybrid2 = itm_fc2c$mean,
              hybrid5 = itm_fc2d$mean)

p1 <- results %>%
   mutate(TimePeriod = 1:36) %>%
   gather(variable, value, -TimePeriod) %>%
   ggplot(aes(x = TimePeriod, y = value, colour = variable)) +
  # theme_grey() +
   geom_line() +
   geom_point() +
   scale_colour_brewer(palette = "Set2")

direct.label(p1)


#============prediction intervals==============

nsim <- 10000
h <- 36 
sim <- numeric(nsim)

fit1 <- itm_mods5$auto.arima
fit2 <- itm_mods5$ets

for(i in seq_len(nsim))
   sim1 <- simulate(fit1, future = TRUE, nsim = h)
   sim2 <- simulate(fit2, future = TRUE, nsim = h)
   sim3 <- sim1 + sim2 / 2
   sim[i] <- sum(simulate(fit, future = TRUE, nsim = h))
meanagg <- mean(sim)
