library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggseas) # for stat_index
library(grid)
library(gridExtra)
library(source.gist)

# Download data from the Reserve Bank of New Zealand
download.file("http://www.rbnz.govt.nz/-/media/ReserveBank/Files/Statistics/Key%20graphs/graphdata.xlsx?la=en",
              destfile = "rbnz.xlsx", mode = "wb")

# Import some of that data into R and create a numeric TimePeriod variable from the original
# string that shows year and month:
forex <- read_excel("rbnz.xlsx", sheet = "8_NZDUSD", skip = 4) %>%
   mutate(year = as.numeric(substring(DATE, 1, 4)),
          month = as.numeric(substring(DATE, 6, 7)),
          TimePeriod = year + (month - 0.5) / 12) %>%
   select(-DATE, -year, -month)

# Tidy up names:
names(forex)[1:2] <- c("NZDUSD", "TWI")

# Create a long, thin ("tidy") version for use with ggplot2:
forex_m <- forex %>%  gather(variable, value, -TimePeriod) 

# Set the basic foundation of the coming ggplot graphics:
basicplot <- ggplot(forex_m, aes(x = TimePeriod, y = value, colour = variable)) +
   labs(x = "", caption = "Data from RBNZ; graphic by http://ellisp.github.io", colour = "")


#-----------------facet versions-------------------
# Good facet plot:
svg("../img/0051-facets.svg", 8, 6)
basicplot +
   geom_line() +
   facet_wrap(~variable, scales = "free_y", ncol = 1) +
   ggtitle("Comparing two time series with facets may reduce comparability")
dev.off()   

# Misleading facet plot from playing around with one of the scales:
p1 <- forex_m %>%
   filter(variable == "NZDUSD") %>%
   ggplot(aes(x = TimePeriod, y = value)) +
   geom_line() +
   labs(x = "", y = "USD purchased for one NZD") +
   ggtitle("NZDUSD")

p2 <- forex_m %>%
   filter(variable != "NZDUSD") %>%
   ggplot(aes(x = TimePeriod, y = value)) +
   geom_line() +
   labs(x = "", y = "Trade weighted index") +
   ylim(c(0, 100)) +
   ggtitle("TWI")

svg("../img/0051-facets-bad.svg", 8, 6)
grid.arrange(p1, p2, ncol = 1)
dev.off()
#--------------index--------------

# Good index plot:
svg("../img/0051-index1.svg", 8, 5)
basicplot +
   stat_index(index.ref = 1) +
   labs(y = "Index (January 1984 = 100)") +
   ggtitle("Usually accepted version of comparing two time series",
           subtitle = "Converted to an index, reference period first point in time")
dev.off()

# Also a good index plot, but showing that arbitrary choices are still being made:
svg("../img/0051-index360.svg", 8, 6)
basicplot + 
   stat_index(index.ref = 361) +
   labs(y = "Index (January 2014 = 100)") +
   ggtitle("But then, a different picture?",
           subtitle = "Converted to an index, reference period chosen arbitrarily later in the series")
dev.off()

#---------------connected scatterplot-------------

svg("../img/0051-csp.svg", 8, 8)
forex %>%
   mutate(label = ifelse(round(TimePeriod - floor(TimePeriod), 3) == 0.042, substring(TimePeriod, 1, 4), "")) %>%
   ggplot(aes (x = NZDUSD, y = TWI, label = label, colour = TimePeriod)) +
   geom_path() +
   geom_text(fontface = "bold") +
   scale_colour_gradientn("", colours = c("grey75", "darkblue")) +
   ggtitle("Connected scatter plot may be the best analytically\nbut is intimidating to non-specialists")
dev.off()   


#------------dual axis version-------------
# As we're drawing a number of these, we want a function to make it easier.
# Here's one I prepared earlier:
# source("https://gist.githubusercontent.com/ellisp/4002241def4e2b360189e58c3f461b4a/raw/9ab547bff18f73e783aaf30a7e4851c9a2f95b80/dualplot.R")     

# bad:
svg("../img/0051-dualbad1.svg", 8, 5)
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, 
                     colgrid = "grey90", ylim2 = c(20, 200)))
dev.off()

# bad:
svg("../img/0051-dualbad2.svg", 8, 5)
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, 
                     colgrid = "grey90", ylim1 = c(0, 1)))
dev.off()


# verybad:
forex2 <- forex %>%
   mutate(NZDUSD_growth = c(NA, diff(NZDUSD)) / NZDUSD * 100)
svg("../img/0051-dualbad3.svg", 8, 5)
par(family = "myfont")
with(forex2[-1, ], dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = NZDUSD_growth, 
                            colgrid = "grey90", ylim2 = c(-15, 15)))   
dev.off()


# ok:
theplot <- function(){
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, lwd = 1.2, colgrid = "grey90", 
                     main = "NZ dollar exchange rate & trade-weighted index",
                     ylab1 = "US dollars for one NZ dollar",
                     ylab2 = "Index",
                     yleg1 = "NZD / USD exchange rate (left axis)",
                     yleg2 = "Trade-weighted index (right axis)",
                     mar = c(5,6,3,6)))

mtext(side = 1, "Data from RBNZ; graphic by http://ellisp.github.io", 
      adj = 1, cex = 0.8, line = 3)
   
}

svg("../img/0051-dualgood.svg", 8, 5)
theplot()
dev.off()

png("../img/0051-dualgood.png", 800, 500)
theplot()
dev.off()

# ok again, equivalent to reference point for indexing of January 2014
theplot2 <- function(){
   par(family = "myfont")
   with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, lwd = 1.2, colgrid = "grey90", 
                     main = "NZ dollar exchange rate & trade-weighted index",
                     ylim.ref = c(361, 361), 
                     ylab1 = "US dollars for one NZ dollar",
                     ylab2 = "Index",
                     yleg1 = "NZD / USD exchange rate (left axis)",
                     yleg2 = "Trade-weighted index (right axis)",
                     mar = c(5,6,3,6)))
   mtext(side = 1, "Data from RBNZ; graphic by http://ellisp.github.io", 
         adj = 1, cex = 0.8, line = 3)
}

svg("../img/0051-dualgood2.svg", 8, 5)
theplot2()
dev.off()

png("../img/0051-dualgood2.png", 800, 500)
theplot2()
dev.off()

#--------tidy up-----------
unlink("rbnz.xlsx")

