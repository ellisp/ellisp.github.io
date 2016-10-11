library(tidyr)
library(dplyr)
# We use the dev version of ggplot2 for subtitle and captions, 
# so need the following line if you don't already have it
# devtools::install_github("hadley/ggplot2")
library(ggseas)

www <- "http://www.mbie.govt.nz/info-services/housing-property/sector-information-and-statistics/rental-bond-data/documents-images/by-region/region-mean-rents.csv"
rents_orig <- read.csv(www, check.names = FALSE)

rents <- rents_orig %>%
   gather(Region, MeanRent, -Month) %>%
   mutate(Month = as.Date(Month))

p1 <- rents %>%
   filter(Region %in% c("Auckland", "Otago")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = Region), 
         frequency = 12, method = "seas", start = c(1993, 1),
         facet.titles = c("Original series", "Underlying trend",
                          "Regular seasonal impacts", "Residual randomness")) +
   geom_line() +
   labs(colour = "", x = "") +
   scale_y_continuous("", label = dollar) +
   ggtitle("Mean rent in two New Zealand regional councils",
           subtitle = "Seasonal decomposition with X13-SEATS-ARIMA via ggsdc") +
   labs(caption = "Source: Ministry of Business, Innovation and Employment")

svg("../img/0060-seas-rents.svg", 7, 5)
print(p1)
dev.off()

png("../img/0060-seas-rents.png", 7 * 100, 5 * 100, res = 100)
print(p1)
dev.off()

p2 <- rents %>%
   filter(Region %in% c("Auckland", "Otago")) %>%
   ggsdc(aes(x = Month, y = MeanRent, colour = Region), 
         frequency = 12, method = "decompose", type = "additive") +
   geom_line()

svg("../img/0060-decomp-rents.svg", 7, 4)
print(p2)
dev.off()