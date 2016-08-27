library(dplyr)
library(tidyr)
library(quantmod) # for getSymbols
library(ggseas)   # for nzbop data



source("https://gist.github.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/0e5dc52262e86b5d21b914db3553e58b0b8d1d20/dualplot.R")     

# Issues
# improve automatic legend titles (done but needs regression testing)
# axis labels for x axis when it is a time (done but needs regression testing)
# indexing work even for xts or zoo objects
# work with time series of different lengths

# different starting periods


#================negative values=================
set.seed(123)
data1 <- data.frame(x = 1:100, y = arima.sim(list(ar = c(0.9)), 100))
data2 <- data.frame(x = 1:100, y = arima.sim(list(ar = c(0.9)), 100) * 50 - 3)

# when series have values that are very small in absolute value or negative, 
# converting to an index is problematic so 
dualplot(x1 = data1$x, y1 = data1$y, x2= data1$x, y2 = data2$y)

dualplot(x1 = data1$x, y1 = data1$y, x2= data1$x, y2 = data2$y, ylim1 =- c(-4, 4))





#=================Different starting points==================
# Fonterra were publicly listed only recently
fonterra <- getSymbols('FCG.NZ', src='yahoo', auto.assign = FALSE) 
airnz <- getSymbols('AIR.NZ', src='yahoo', auto.assign = FALSE) 

# default - a cross over point happen at the earliest point of the shorter series
dualplot(x1 = time(fonterra), x2 = time(airnz), y1 = fonterra$FCG.NZ.Close, y2 = airnz$AIR.NZ.Close)

# or the other way around:
dualplot(x1 = time(airnz), y1 = airnz$AIR.NZ.Close, x2 = time(fonterra), y2 = fonterra$FCG.NZ.Close, silent = TRUE)

# or can override eg - each one begins at the same vertical height on its earliest point
dualplot(x1 = time(fonterra), y1 = fonterra$FCG.NZ.Close, x2 = time(airnz), y2 = airnz$AIR.NZ.Close,
         ylim.ref = c(1, 1), silent = TRUE)

# or other ways of forcing the cross over point eg at the final point
dualplot(x1 = time(fonterra), y1 = fonterra$FCG.NZ.Close, x2 = time(airnz), y2 = airnz$AIR.NZ.Close,
         ylim.ref = c(nrow(fonterra), nrow(airnz)))

         




#===========both different frequencies and starting periods=========

airnz2 <- getSymbols('AIR.NZ', src='yahoo', auto.assign = FALSE, from = "2000-01-01") 

services <- nzbop %>%
   filter(Category == "Services; Exports total") %>%
   filter(TimePeriod > as.Date("1997-12-30"))

dualplot(x1 = services$TimePeriod, y1 = services$Value, x2 = time(airnz2), y2 = airnz2$AIR.NZ.Close,
         ylab1 = "New Zealand service exports ($m)\n", ylab2 = "Air New Zealand share price",
         yleg1 = "All service exports ($m) (left axis)", lwd = c(4,2), col = c("brown", "steelblue"))



#============not for serious anlaysis================
# automated y limit choice has its limits (or is this a feature?)
dualplot(x1 = time(airnz2), y1 = airnz2$AIR.NZ.Volume, y2 = airnz2$AIR.NZ.Close)

dualplot(x1 = time(airnz2), y1 = airnz2$AIR.NZ.Volume, y2 = airnz2$AIR.NZ.Close, ylim2 = c(0, 4 ))

dualplot(x1 = time(airnz2), y1 = sqrt(airnz2$AIR.NZ.Volume), y2 = airnz2$AIR.NZ.Close)

airnz3 <- airnz2 %>%
   as.data.frame() %>%
   mutate(TimePeriod = time(airnz2),
          PeakVol = AIR.NZ.Volume > 2 * 10 ^ 7) 

peaks <- filter(airnz3, PeakVol)$TimePeriod

airnz3 %>%
   select(TimePeriod, AIR.NZ.Close, AIR.NZ.Volume) %>%
   mutate(SquareRootOfVolume = sqrt(AIR.NZ.Volume)) %>%
   rename(ClosingPrice = AIR.NZ.Close) %>%
   gather(variable, value, -TimePeriod, -AIR.NZ.Volume) %>%
   ggplot(aes(x = TimePeriod, y = value)) +
   facet_wrap(~variable, scales = "free_y", ncol = 1) +
   geom_line() +
   geom_vline(xintercept = as.numeric(peaks), colour = "steelblue", size = 2.5, alpha = 0.1) +
   geom_line(colour = "brown") +
   labs(x = "", y = "", title = "Air New Zealand share volume and price")
   
   
# not for barcharts




# where to put the dual plot function


