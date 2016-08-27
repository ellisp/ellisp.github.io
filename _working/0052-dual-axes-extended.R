library(dplyr)
library(tidyr)
library(quantmod) # for getSymbols for share price data
library(ggseas)   # for nzbop data


source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/9ab547bff18f73e783aaf30a7e4851c9a2f95b80/dualplot.R")     





#=================Different starting points==================
# Fonterra were publicly listed only recently.  BHP are the same unit ($ per share)
# but different magnitued
fonterra <- getSymbols('FCG.NZ', src='yahoo', auto.assign = FALSE) 
bhp <- getSymbols('BHP.AX', src='yahoo', auto.assign = FALSE) 

# default - a cross over point happen at the earliest point of the shorter series
svg("../img/0052-starts2.svg", 8, 5)
par(family = "myfont")
dualplot(x1 = time(bhp), y1 = bhp$BHP.AX.Close,
         x2 = time(fonterra), y2 = fonterra$FCG.NZ.Close, 
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", main = "Price per share of two key firms")
dev.off()


svg("../img/0052-starts3.svg", 8, 5)
par(family = "myfont")
# or can override eg - each one begins at the same vertical height on its earliest point
dualplot(x1 = time(bhp), y1 = bhp$BHP.AX.Close,
         x2 = time(fonterra), y2 = fonterra$FCG.NZ.Close, 
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", 
         main = "Price per share of two key firms\n(starting at same vertical position)",
         ylim.ref = c(1, 1), silent = TRUE)
dev.off()

svg("../img/0052-starts4.svg", 8, 5)
par(family = "myfont")
# or other ways of forcing the cross over point eg at the final point
dualplot(x1 = time(bhp), y1 = bhp$BHP.AX.Close,
         x2 = time(fonterra), y2 = fonterra$FCG.NZ.Close, 
         ylab1 = "BHP", ylab2 = "Fonterra", 
         legx = "topright", 
         main = "Price per share of two key firms\n(finishing at same vertical position)",
         ylim.ref = c(nrow(bhp), nrow(fonterra)))
dev.off()

#============dairy prices========
dairy <- Quandl("GDT/WMP_PI")

milkplot <- function(){
   par(family = "myfont")
   dualplot(x1 = dairy[ , 1], y1 = dairy[, 8], 
         x2 = time(fonterra), y2 = fonterra$FCG.NZ.Close,
         ylab1 = "Whole milk powder price index\n",
         ylab2 = "Fonterra Cooperative Group\nshare price ($)",
         col = colorspace::rainbow_hcl(2, c = 80, l = 50, start = 45, end = 270),
         colgrid = "grey90", cex = 0.8,
         main = "Fonterra share prices are less volatile than global milk prices")
}

svg("../img/0052-milk-price.svg", 8, 5)
milkplot()
dev.off()

png("../img/0052-milk-price.png", 8 * 110, 5 * 110)
milkplot()
dev.off()

#===========both different frequencies and starting periods=========

airnz <- getSymbols('AIR.NZ', src='yahoo', auto.assign = FALSE, from = "2000-01-01") 

services <- nzbop %>%
   filter(Category == "Services; Exports total") %>%
   filter(TimePeriod > as.Date("1997-12-30"))

the_plot <- function(){
   par(family = "myfont")
   dualplot(x1 = services$TimePeriod, y1 = services$Value, x2 = time(airnz), y2 = airnz$AIR.NZ.Close,
         ylab1 = "New Zealand service exports ($m)\n", ylab2 = "Air New Zealand share price",
         yleg1 = "All service exports ($m) (left axis)", lwd = c(4,2), col = c("rosybrown", "steelblue"),
         main ="New Zealand service exports and Air New Zealand share price over time",
         colgrid = "white", bty = "o", bg = "white", box.col = "white")
   }

svg("../img/0052-frequency1.svg", 8, 5)
the_plot()
dev.off()

png("../img/0052-frequency1.png", 8 * 110, 5 * 110)
the_plot()
dev.off()


#================negative values=================
set.seed(123)
data1 <- data.frame(x = 1:100, y = arima.sim(list(ar = c(0.9)), 100))
data2 <- data.frame(x = 1:100, y = arima.sim(list(ar = c(0.9)), 100) * 50 - 3)

# when series have values that are very small in absolute value or negative, 
# converting to an index is problematic so 
svg("../img/0052-negatives.svg", 8, 5)
par(family = "myfont")
dualplot(x1 = data1$x, y1 = data1$y, x2= data1$x, y2 = data2$y)
dev.off()






#============not for serious anlaysis================
# automated y limit choice has its limits:
dualplot(x1 = time(airnz), y1 = airnz$AIR.NZ.Volume, y2 = airnz$AIR.NZ.Close)

dualplot(x1 = time(airnz), y1 = airnz$AIR.NZ.Volume, y2 = airnz$AIR.NZ.Close, ylim2 = c(0, 4 ))

dualplot(x1 = time(airnz), y1 = sqrt(airnz$AIR.NZ.Volume), y2 = airnz$AIR.NZ.Close)


# so beter to take another approach
airnz3 <- airnz %>%
   as.data.frame() %>%
   mutate(TimePeriod = time(airnz),
          PeakVol = AIR.NZ.Volume > 2 * 10 ^ 7) 

peaks <- filter(airnz3, PeakVol)$TimePeriod


p1 <- airnz3 %>%
   select(TimePeriod, AIR.NZ.Close, AIR.NZ.Volume) %>%
   mutate(SquareRootOfVolume = sqrt(AIR.NZ.Volume)) %>%
   rename(ClosingPrice = AIR.NZ.Close) %>%
   gather(variable, value, -TimePeriod, -AIR.NZ.Volume) %>%
   ggplot(aes(x = TimePeriod, y = value)) +
   facet_wrap(~variable, scales = "free_y", ncol = 1) +
   geom_line() +
   geom_vline(xintercept = as.numeric(peaks), colour = "steelblue", size = 2.5, alpha = 0.1) +
   geom_line(colour = "brown") +
   labs(x = "", y = "", title = "Four big trading events for Air New Zealand shares since 2000")

svg("../img/0052-airnz.svg", 8, 5)
print(p1)
dev.off()   

png("../img/0052-airnz.png", 8 * 110, 5 * 110)
print(p1)
dev.off()   

   
# not for barcharts




# where to put the dual plot function


