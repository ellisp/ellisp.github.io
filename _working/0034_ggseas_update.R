#---------look and feel------------
library(showtext)
library(ggplot2)
library(xtable)
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))


#---------functionality------
library(dplyr)
library(ggseas)
library(scales)
library(grid) # for grid.text

#--------tsdf-----------
ap_df <- tsdf(AirPassengers)
print(xtable(
   head(ap_df)
), type = "html", inlcude.rownames = FALSE)


#-----rolling averages---
svg("../img/0034-rolling.svg", 7, 5)
ggplot(ldeaths_df, aes(x = YearMon, y = deaths)) +
   geom_point() +
   facet_wrap(~sex) +
   stat_rollapplyr(width = 12, FUN = median) +
   ggtitle("Lung deaths in the UK\n") +
   labs(x = "", y = "Deaths (showing 12 month moving median\n")
dev.off()


#--------------convert to an index-----------
svg("../img/0034-index.svg", 7, 5)
ggplot(ldeaths_df, aes(x = YearMon, y = deaths, colour = sex)) +
   stat_seas(index.ref = 1:12) +
   ggtitle("Indexed lung deaths in the UK\n") +
   labs(x = "", y = "Seasonally adjusted deaths\n(first 12 months average = 100)\n") 
dev.off()

#---------------deduce frequency and start------------
# SEATS with defaults:
svg("../img/0034-deduce-frequency.svg", 7, 5)
ggplot(ldeaths_df, aes(x = YearMon, y = deaths, colour = sex)) +
   geom_point() +
   facet_wrap(~sex) +
   stat_seas() +
   labs(x = "") +
   scale_y_continuous("Number of deaths", label = comma) +
   ggtitle("Seasonally adjusted lung deaths in the UK\n") +
   theme(legend.position = "none")
dev.off()


#------------------ggsdc---------------

serv <- subset(nzbop, Account == "Current account" & 
                  Category %in% c("Services; Exports total", "Services; Imports total"))

svg("../img/0034-ggsdc.svg", 7, 7)
ggsdc(serv, aes(x = TimePeriod, y = Value, colour = Category),
      method = "seas", start = c(1971, 2), frequency = 4) +
   geom_line() +
   labs(x = "   \n  ", colour = "") +
   scale_y_continuous("Value in millions of New Zealand dollars\n", label = comma) +
   ggtitle("New Zealand service exports and imports") +
   theme(legend.position = c(0.17, 0.92))
grid.text("Source: Statistics New Zealand, Balance of Payments", 0.7, 0.03,
          gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7))   
dev.off()

png("../img/0034-ggsdc.png", 7 * 150, 7 * 150, res = 150)
ggsdc(serv, aes(x = TimePeriod, y = Value, colour = Category),
      method = "seas", start = c(1971, 2), frequency = 4) +
   geom_line() +
   labs(x = "   \n  ", colour = "") +
   scale_y_continuous("Value in millions of New Zealand dollars\n", label = comma) +
   ggtitle("New Zealand service exports and imports") +
   theme(legend.position = c(0.17, 0.92))
grid.text("Source: Statistics New Zealand, Balance of Payments", 0.7, 0.03,
          gp = gpar(fontfamily = "myfont", fontface = "italic", cex = 0.7))   
dev.off()