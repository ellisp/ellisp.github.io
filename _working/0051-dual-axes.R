library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggseas) # for stat_index
library(grid)
library(gridExtra)

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
# As we're drawing a number of these, we want a function to make it easier:

dualplot <- function(x1, y1, y2, x2 = x1, col1 = "red1", col2 = "steelblue",
                     lwd = 1, colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = substitute(y1), ylab2 = substitute(y2), nxbreaks = 5,
                     yleg1 = paste(ylab1, "(LHS)"), yleg2 = paste(ylab2, "(RHS)"),
                     ylim1 = NULL, ylim2 = NULL, xlab = "",
                     main = NULL, legx = "topleft", legy = NULL, ...){
   # Base graphics function for drawing dual axis line plot.
   # Assumed to be two time series on a conceptually similar, non-identical scale 
   #
   # Use with caution! 
   # Please don't use to show growth rates and the original
   # series at the same time!
   #
   # Peter Ellis, 16 August 2016, GNU GPL-3
   # most parameters should be obvious:
   # x1 and y1 are the x and y coordinates for first line
   # x2 and y2 are the x and y coordinates for second line.  Often x2 will == x1, but can be overridden
   # nbreaks is number of breaks in horizontal axis
   # lwd and mar are graphics parameters (see ?par)
   # colgrid is colour of gridlines; if NULL there are no gridlines
   # ylab1 and ylab2 are the labels for the two y axes
   # yleg1 and yleg2 are the labels for the two series in the legend
   # xlab and main are for x label and main title as in plot()
   # legx and legy are x and y position fed through to legend()
   # ... is parameters to pass to legend()
   oldpar <- par(mar = mar)
   xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
   
   # draw first series - with no axes.
   plot(x1, y1, type = "l", axes = FALSE, lwd = lwd,
        xlab = xlab, ylab = "", col = col1, main = main, 
        xlim = range(xbreaks), ylim = ylim1)
   
   # add in the gridlines if wanted:
   if(!is.null(colgrid)){
      grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
      abline(v = xbreaks, col = colgrid)
   }
   
   # add in the left hand vertical axis and its label
   axis(2, col = col1, col.axis= col1, las=1 )  ## las=1 makes horizontal labels
   mtext(paste0("\n", ylab1, "\n"), side = 2, col = col1, line = 1.5) 
   
   # Allow a second plot on the same graph
   par(new=TRUE)
   
   # Plot the second series:
   plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd,
        col = col2, xlim = range(xbreaks), ylim = ylim2)
   
   ## add second vertical axis (on right) and its label
   mtext(paste0("\n", ylab2, "\n"), side = 4, col = col2, line = 4.5) 
   axis(4,  col = col2, col.axis = col2, las=1)
   
   # Draw the horizontal time axis
   axis(1, at = xbreaks)
   
   # Add Legend
   legend(x = legx, y = legy, legend=c(yleg1, yleg2),
          text.col = c(col1, col2), lty = c(1, 1), col = c(col1, col2),
          bty = "n", ...)
   
   par(oldpar)
}     




# bad:
svg("../img/0051-dualbad1.svg", 8, 5)
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, colgrid = "grey90", ylim2 = c(20, 200)))
dev.off()

# bad:
svg("../img/0051-dualbad2.svg", 8, 5)
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, colgrid = "grey90", ylim1 = c(0, 1)))
dev.off()


# verybad:
forex2 <- forex %>%
   mutate(NZDUSD_growth = c(NA, diff(NZDUSD)) / NZDUSD * 100)
svg("../img/0051-dualbad3.svg", 8, 5)
par(family = "myfont")
with(forex2, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = NZDUSD_growth, colgrid = "grey90", ylim2 = c(-15, 15)))   
dev.off()


# ok:
theplot <- function(){
par(family = "myfont")
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, lwd = 1.2, colgrid = "grey90", 
                     main = "NZ dollar exchange rate & trade-weighted index",
                     ylab1 = "NZ dollars for one US dollar",
                     ylab2 = "Index",
                     yleg1 = "NZD / USD exchange rate (left axis)",
                     yleg2 = "Trade-weighted index (right axis)",
                     mar = c(5,6,3,6)))

mtext(side = 1, "Data from RBNZ; graphic by http://ellisp.github.io", adj = 1, cex = 0.8, line = 3)
   
}

svg("../img/0051-dualgood.svg", 8, 5)
theplot()
dev.off()

png("../img/0051-dualgood.png", 800, 500)
theplot()
dev.off()


#--------tidy up-----------
unlink("rbnz.xlsx")

