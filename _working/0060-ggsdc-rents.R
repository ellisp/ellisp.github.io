
# redo this with just Dunedin and Auckland, not regions

library(tidyr)
library(dplyr)
# We use the dev version of ggplot2 for subtitle and captions, 
# so need the following line if you don't already have it
# devtools::install_github("hadley/ggplot2")
library(ggseas)

www <- "http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data/documents-images/by-ta/ta-mean-rents.csv"
rents_orig <- read.csv(www, check.names = FALSE)

rents <- rents_orig %>%
   gather(TA, MeanRent, -Month) %>%
   mutate(Month = as.Date(Month))

p1 <- rents %>%
   filter(TA %in% c("Auckland", "Dunedin")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = TA), 
         frequency = 12, method = "seas", start = c(1993, 1),
         facet.titles = c("Original series", "Underlying trend",
                          "Regular seasonal impacts", "Residual randomness")) +
   geom_line() +
   labs(colour = "", x = "Month lodged") +
   scale_y_continuous("Decomposition of mean rent", label = dollar) +
   ggtitle("Rent in Dunedin has a strong seasonal pattern",
           subtitle = "Seasonal decomposition with X13-SEATS-ARIMA via ggsdc") +
   labs(caption = "Source: Ministry of Business, Innovation and Employment")

svg("../img/0060-seas-rents.svg", 7, 5.8)
print(p1)
dev.off()

png("../img/0060-seas-rents.png", 7 * 100, 5.8 * 100, res = 100)
print(p1)
dev.off()

p2 <- rents %>%
   filter(TA %in% c("Auckland", "Dunedin")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = TA), 
         frequency = 12, method = "decompose", type = "multiplicative") +
   geom_line()

svg("../img/0060-decomp-rents.svg", 7, 4.6)
print(p2)
dev.off()

# monthplot

dun_ts <- rents %>%
   filter(TA %in% c("Dunedin")) %>%
   mutate(LogRent = log(MeanRent)) %>%
   select(LogRent) %>%
   ts(start = c(1993, 1), frequency = 12) 

svg("../img/0060-monthplot.svg", 7, 5)
par(bty = "l", family = "myfont")
# plot of months, including trend
monthplot(dun_ts)
grid()
dev.off()

# de-trended version
svg("../img/0060-monthplot-detrended.svg", 7, 5)
par(bty = "l", family = "myfont")
dun_stl <- stl(dun_ts[ ,1], s.window = 7)
monthplot(with(as.data.frame(dun_stl$time.series), seasonal + remainder))
grid()
dev.off()

