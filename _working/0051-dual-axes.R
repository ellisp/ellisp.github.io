library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggseas) # for stat_index
library(grid)

download.file("http://www.rbnz.govt.nz/-/media/ReserveBank/Files/Statistics/Key%20graphs/graphdata.xlsx?la=en",
              destfile = "rbnz.xlsx", mode = "wb")


forex <- read_excel("rbnz.xlsx", sheet = "8_NZDUSD", skip = 4) %>%
   mutate(year = as.numeric(substring(DATE, 1, 4)),
          month = as.numeric(substring(DATE, 6, 7)),
          TimePeriod = year + (month - 0.5) / 12) %>%
   select(-DATE, -year, -month)


names(forex)[1:2] <- c("NZDUSD", "TWI")

forex_m <- forex %>%  gather(variable, value, -TimePeriod) 


basicplot <- ggplot(forex_m, aes(x = TimePeriod, y = value, colour = variable)) +
   labs(x = "", caption = "Data from RBNZ; graphic by http://ellisp.github.io", colour = "")


#-----------------facet-------------------
basicplot +
   geom_line() +
   facet_wrap(~variable, scales = "free_y", ncol = 1) +
   ggtitle("Preferred version of comparing two time series",
           subtitle = "Vertically aligned facets")
   
   
#--------------index--------------

basicplot +
   stat_index(index.ref = 1) +
   labs(y = "Index (January 1984 = 100)") +
   ggtitle("Usually accepted version of comparing two time series",
           subtitle = "Converted to an index, reference period first point in time")

basicplot + 
   stat_index(index.ref = 360) +
   labs(y = "Index (January 2014 = 100)") +
   ggtitle("But then, what stops us doing this?",
           subtitle = "Converted to an index, reference period chosen arbitrarily later in the series")

#---------------connected scatterplot-------------

forex %>%
   mutate(label = ifelse(round(TimePeriod - floor(TimePeriod), 3) == 0.042, substring(TimePeriod, 1, 4), "")) %>%
   ggplot(aes (x = NZDUSD, y = TWI, label = label, colour = TimePeriod)) +
   # geom_point() +
   geom_path() +
   geom_text(fontface = "bold") +
   scale_colour_gradientn("", colours = c("grey75", "darkblue"))
   


#------------dual axis version-------------

dualplot <- function(x1, y1, y2, x2 = x1, col1 = "red1", col2 = "steelblue",
                     lwd = 1, colgrid = NULL,
                     mar = c(3, 6, 3, 6) + 0.1, 
                     ylab1 = substitute(y1), ylab2 = substitute(y2), nxbreaks = 5,
                     yleg1 = paste(ylab1, "(LHS)"), yleg2 = paste(ylab2, "(RHS)"),
                     ylim1 = NULL, ylim2 = NULL, xlab = "",
                     main = NULL, legx = "topleft", legy = NULL, ...){
   # function for drawing dual axis line plot
   # most parameters should be obvious eg x1 and y1 are the x and y coordinates for first line
   # ... is parameters to pass to legend()
   oldpar <- par(mar = mar)
   xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1, x2)), length.out = nxbreaks))
   
   plot(x1, y1, type = "l", axes = FALSE, lwd = lwd,
        xlab = xlab, ylab = "", col = col1, main = main, 
        xlim = range(xbreaks), ylim = ylim1)
   
   if(!is.null(colgrid)){
      grid(lty = 1, nx = NA, ny = NULL, col = colgrid)   
      abline(v = xbreaks, col = colgrid)
   }
   

   
   axis(2, col = col1, col.axis= col1, las=1 )  ## las=1 makes horizontal labels
   mtext(paste0("\n", ylab1, "\n"), side = 2, col = col1, line = 1.5) 
   
   ## Allow a second plot on the same graph
   par(new=TRUE)
   
   ## Plot the second plot and put axis scale on right
   plot(x2, y2,   xlab="", ylab="", axes = FALSE, type = "l", lwd = lwd,
        col = col2, xlim = range(xbreaks), ylim = ylim2)
   
   ## add second vertical axis and its label
   mtext(paste0("\n", ylab2, "\n"), side = 4, col = col2, line = 4.5) 
   axis(4,  col = col2, col.axis = col2, las=1)
   
   ## Draw the time axis
   axis(1, at = xbreaks)
   
   
   ## Add Legend
   legend(x = legx, y = legy, legend=c(yleg1, yleg2),
          text.col = c(col1, col2), lty = c(1, 1), col = c(col1, col2),
          bty = "n", ...)
   
   par(oldpar)
}     




# bad:
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, colgrid = "grey90", ylim2 = c(20, 200)))

# bad:
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, colgrid = "grey90", ylim1 = c(0, 1)))


# ok:
with(forex, dualplot(x1 = TimePeriod, y1 = NZDUSD, y2 = TWI, lwd = 1.2, colgrid = "grey90", 
     main = "NZ dollar exchange rate & trade-weighted index",
     ylab1 = "NZ dollars for one US dollar",
     ylab2 = "Index",
     yleg1 = "NZD / USD exchange rate (left axis)",
     yleg2 = "Trade-weighted index (right axis)",
     mar = c(5,6,3,6)))

mtext(side = 1, "Data from RBNZ; graphic by http://ellisp.github.io", adj = 1, font = 3, line = 3)
