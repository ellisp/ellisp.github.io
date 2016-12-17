# devtools::install_github("Ather-Energy/ggTimeSeries")
# devtools::install_github("masalmon/usaqmindia")

library(ropenaq) # sourced from OpenAQ, see https://github.com/ropensci/ropenaq
library(ggplot2)
library(scales)
library(tidyverse)
library(usaqmindia) # sourced from US Air Quality Monitoring, see https://github.com/masalmon/usaqmindia
library(forecastHybrid)
library(seasonal)   # for Diwali dates
library(ggseas)
library(ggmap)
library(lubridate)
library(png)
library(RColorBrewer)

data(holiday) # will be using this for "diwali"

#============openaq experiemtn================
how_many <- attr(aq_measurements(city = "Ahmedabad"), "meta")
n <- how_many$found # unfortunately only goes back to mid 2015
results_ls <- list()
for(i in 1:ceiling(n / 1000)){
   results_ls[[i]] <- aq_measurements(country = "IN", city = "Ahmedabad", 
                                      date_from = "2010-01-01", 
                                      limit = 1000, page = i)   
   cat(i)
   
}

results_df <- do.call("rbind", results_ls) %>%
   arrange(dateLocal)

svg("../img/0073-openaq.svg", 8, 8)
results_df %>%
   ggplot(aes(x = dateLocal, y = value)) + 
   facet_wrap(~parameter, ncol = 1, scales = "free_y") +
   geom_line() +
   ggtitle("Air pollutants in Ahmedabad, 2016") +
   scale_y_continuous("Value", label = comma) +
   labs(x = "", caption = "Source: OpenAQ")
dev.off()


#=========map of india========
cities <- data_frame(
   city = c("Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai"),
   lat = c(13.067439, 28.644800, 17.387140, 22.572645, 19.0759837),
   long = c(80.2784700, 77.216721, 78.491684	, 88.363892,	72.8776559)
)

india <- get_map("India", zoom = 5, maptype = "satellite")

svg("../img/0073-map.svg", 6, 6)
ggmap(india) +
   geom_text(data = cities,  aes(x = long, y = lat, label = city), colour = "white") +
   theme_nothing()
dev.off()


#=========US Air Quality Monitoring===========
data("pm25_india")


svg("../img/0073-six-cities-orig.svg", 8, 8)
pm25_india %>%
   ggplot(aes(x = datetime, y = conc)) +
   facet_wrap(~city, ncol = 1) +
   geom_line()
dev.off()

svg("../img/0073-six-cities-daily.svg", 8, 8)
# daily aggregation
pm25_india %>%
   mutate(day = as.Date(datetime)) %>%
   group_by(day, city) %>%
   summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>%
   ggplot(aes(x = day, y = avvalue)) +
   facet_wrap(~city, ncol = 1, scales = "free_y") +
   geom_line()
dev.off()

svg("../img/0073-six-cities-monthly.svg", 8, 8)
# monthly aggregation
pm25_india %>%
   mutate(mon = substring(datetime, 1, 7)) %>%
   group_by(mon, city) %>%
   summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>% 
   ungroup() %>%
   mutate(mon = as.Date(paste0(mon, "-15"))) %>%
   ggplot(aes(x = mon, y = avvalue)) +
   facet_wrap(~city, ncol = 1, scales = "free_y") +
   geom_line(colour = "grey50", linetype = 2) +
   stat_stl(s.window = 7, frequency = 12, colour = "steelblue", size = 1.2) +
   ggtitle("Airborne fine particulate matter in Indian cities (PM2.5)",
           subtitle = "Showing original and seasonally adjusted") +
   labs(x = "Seasonal adjustment does not take Diwali into account", 
        y = "Trimmed mean monthly PM2.5 concentration",
        caption = "Data: U.S. Embassy and Consulates in India")
dev.off()

#=======analysis=========
pm25_monthly_df <- pm25_india %>%
   mutate(mon = substring(datetime, 1, 7)) %>%
   group_by(mon, city) %>%
   summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>% 
   ungroup() %>%
   spread(city, avvalue) 

pm25_monthly_ts <- pm25_monthly_df %>%
   select(-mon) %>%
   map(function(x){ts(x, start = c(2013, 1), frequency = 12)}) %>%
   do.call(cbind, .)

palette <- brewer.pal(5, "Set1")
names(palette) <- c("Delhi", "Kolkata", "Mumbai", "Hyderabad", "Chennai")

svg("../img/0073-decomp.svg", 8, 6)
pm25_monthly_ts %>%
   as.data.frame() %>%
   mutate(time = time(pm25_monthly_ts),
          month = time - floor(time)) %>%
   gather(city, value, -time, -month) %>% 
   # imputation: if value is missing, give it mean for that month and city:
   group_by(city, month) %>%
   mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)) %>%
   ungroup() %>%
   # order city levels for drawing legend (there are better ways for doing this programmatically):
   mutate(city = factor(city, levels = c("Delhi", "Kolkata", "Mumbai", "Hyderabad", "Chennai"))) %>%
   ggsdc(aes(x = time, y = value, colour = city), s.window = 7) +
   geom_line(size = 1) +
   theme(legend.position = "right")+
   labs(colour = "", x = "Vertical lines show the month of Diwali, and any impact is seen in the 'irregular' series.", 
        y = "Trimmed mean monthly PM2.5 concentration",
        caption = "Data: U.S. Embassy and Consulates in India") +
   scale_colour_manual(values = palette) +
   ggtitle("Airborne fine particulate matter in Indian cities (PM2.5)",
           subtitle = "Diwali makes an impact but is part of a broader seasonality")  +
   geom_vline(xintercept = ((month(diwali) - 1) / 12 + year(diwali))[114:116], colour = "grey50")
dev.off()   

svg("../img/0073-decomp-index.svg", 8, 6)
pm25_monthly_ts %>%
   as.data.frame() %>%
   mutate(time = time(pm25_monthly_ts),
          month = time - floor(time)) %>%
   gather(city, value, -time, -month) %>% 
   # imputation: if value is missing, give it mean for that month and city:
   group_by(city, month) %>%
   mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)) %>%
   ungroup() %>%
   # order city levels for drawing legend (there are better ways for doing this programmatically):
   mutate(city = factor(city, levels = c("Chennai", "Hyderabad", "Delhi", "Mumbai", "Kolkata"))) %>%
   ggsdc(aes(x = time, y = value, colour = city), s.window = 7, index.ref = 1) +
   geom_line(size = 1) +
   theme(legend.position = "right")+
   labs(colour = "", x = "Vertical lines show the month of Diwali, and any impact is seen in the 'irregular' series.", 
        y = "Trimmed mean monthly PM2.5 concentration as an index\n(January 2013 = 100)",
        caption = "Data: U.S. Embassy and Consulates in India") +
   scale_colour_manual(values = palette) +
   ggtitle("Airborne fine particulate matter in Indian cities (PM2.5)",
           subtitle = "Viewing the data as an index reveals overall growth patterns")  +
   geom_vline(xintercept = ((month(diwali) - 1) / 12 + year(diwali))[114:116], colour = "grey50")
dev.off()   

# Diwali was in November 2013, October 2014, November 2015
diwali[114:116]

svg("../img/0073-monthplot.svg", 8, 5)
par(family = "myfont")
par(mfrow = c(2, 3), font.main = 1)
for(i in 1:5){
   monthplot(pm25_monthly_ts[ , i], bty = "l",
             ylab = "",
             main = colnames(pm25_monthly_ts)[i])   
   grid()
}
dev.off()


#===========analysis of Delhi only================
delhi <- pm25_monthly_ts[ , "Delhi"]

svg("../img/0073-acf.svg", 8, 4.5)
par(family = "myfont")
par(mfrow = c(1, 2), bty = "l", font.main = 1)
acf(delhi)
pacf(delhi)
dev.off()

BoxCox.lambda(delhi) # about 0.2.  Note that if we don't use this in forecasting (below) there is a tendency to forecast < 0


diwalix <- window(genhol(diwali), start = start(delhi), end = end(delhi))

mod0 <- auto.arima(delhi, xreg = diwalix, lambda = 0.21)
mod0


mod1 <- hybridModel(pm25_monthly_ts[ , "Delhi"], models = "ae", a.args = list(xreg = diwalix), lambda = BoxCox.lambda(delhi))
mod1$auto.arima

diwalix_future <- as.matrix(window(genhol(diwali), start = end(delhi) + c(0, 1), end = end(delhi) + c(2, 0)))

fc1 <- forecast(mod1, 24, xreg = diwalix_future)

svg("../img/0073-forecast.svg", 8, 6)
autoplot(fc1) +
   ggtitle("Airborne fine particulate matter in Delhi (PM2.5)",
      subtitle = "Forecasts from combination of auto.arima and ets, taking Diwali into account") +
   labs(x = "", y = "Trimmed mean monthly PM2.5 concentration", fill = "Confidence level",
        caption = "Data: U.S. Embassy and Consulates in India, tidied in the usaqmindia R package")
dev.off()


#=========convert to png=========

setwd("../img")
files <- list.files()
files <- files[grepl("^0073.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 800x800', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")