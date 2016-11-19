library(testthat) # for testing the energy conversion
#============what is the relation between magnitude, energy in ergs, and energy in megatonnes?

#' function that converts an earthquake magnitude approximately to an energy estimate
#' in megatonnes of TNT (ie millions of tonnes)
magn_to_megat <- function(magnitude){
   # Formula from http://alabamaquake.com/energy.html#/
   ergs <- 10 ^ (11.8 + 1.5 * magnitude)
   
   # Conversion rates from http://www.atmosp.physics.utoronto.ca/people/codoban/PHY138/Mechanics/dimensional.pdf.
   grammes_tnt <- ergs / (4 * 10 ^ 10)
   megatonnesTNT <- grammes_tnt / 10 ^ 6 / 10 ^ 6
   return(megatonnesTNT)
}

# checks against the rough conversions in a table at 
# http://www.openhazards.com/faq/earthquakes-faults-plate-tectonics-earth-structure-user-submitted-questions/what-energy
expect_lt(abs(magn_to_megat(6) - 0.015), 0.01) 
expect_lt(abs(magn_to_megat(7) - 0.5), 0.1) 
expect_lt(abs(magn_to_megat(8) - 15), 1)


#=============download snapshot of quake data, current as at 19 November 2016=======
library(dplyr)
library(ggplot2)
library(viridis) # for colours
library(ggseas)  # for stat_rollaplyr, rolling average on fly
library(scales)
library(ggrepel)

# About 12 MB
download.file("https://github.com/ellisp/ellisp.github.io/raw/master/data/quakes_df.rda", destfile = "quakes_df.rda")
load("quakes_df.rda")
# cleanup:
unlink("quakes_df.rda")

#==================analysis=============

# facet map
m1 <- quakes_df %>%
   filter(day >= as.Date("2016-11-09")) %>%
   ggplot(aes(x = longitude, y = latitude, size = energy, colour = depth)) +
   borders('nz', colour = NA, fill = terrain.colors(7)[7]) +
   geom_point(alpha = 0.5) +
   coord_map() +
   ggmap::theme_nothing(legend = TRUE) +
   theme(legend.position = "none") +
   scale_size_area("Energy",max_size = 25) +
   facet_wrap(~day, ncol = 5) +
   scale_color_viridis("Depth") +
   ggtitle("Earthquakes in New Zealand, ten days in November 2016",
           subtitle = "Circle area is proportionate to energy released,.\nDates are based on Greenwich mean time.") +
   labs(caption = "Data from quakesearch.geonet.org.nz\nAnalysis from ellisp.github.io")

svg("../img/0068-m1.svg", 10, 6)
print(m1)
dev.off()

#=======aggregate by day===============
quakes_g <- quakes_df %>%
   filter(eventtype == "earthquake") %>%
   filter(magnitude >= 2) %>%
   group_by(day) %>%
   summarise(NumberOver2 = length(magnitude),
             NumberOver4 = sum(magnitude >= 4),
             NumberOver5 = sum(magnitude >= 5),
             NumberOver6 = sum(magnitude >= 6),
             NumberOver7 = sum(magnitude >= 7),
             MeanMagnitude = mean(magnitude, na.rm = TRUE),
             MaxMagnitude = max(magnitude, na.rm = TRUE),
             MeanDepth = mean(depth),
             TotalEnergy = sum(energy)) # ie megatons of TNT


quakes_g %>%
   arrange(desc(TotalEnergy))


#==============graphics of daily energy, magnitude, counts===============
# note the period in 1977 - 1988 when many days had no earthquake > 3
p1 <- quakes_g %>%
   filter(day > as.Date("1949-12-31")) %>%
   ggplot(aes(x = day, y = MaxMagnitude, colour = MaxMagnitude)) + 
   geom_line() +
   geom_point() + 
   theme_grey(base_family = "myfont") +
   scale_color_viridis(option = "A", direction = -1) +
   ggtitle("Maximum quake magnitude by day",
      subtitle = "The late 1970s and early 1980s saw an unusual number of days with only low magnitude earthquakes") +
   labs(x = "", y ="Magnitude of largest earthquake each day", color = "",
        caption = "Data from quakesearch.geonet.org.nz\nAnalysis from ellisp.github.io")

png("../img/0068-quakes-p1.png", 900, 600, res = 100)
print(p1)
dev.off()


# six months after the 2010 and 2011 earthquakes a sudden big drop in the number of quakes > 2
p2 <- quakes_g %>%
   filter(day > as.Date("1969-12-31")) %>%
   ggplot(aes(x = day, y = NumberOver2, colour = MaxMagnitude)) + 
   geom_line() +
   geom_point() + 
   theme_grey(base_family = "myfont") +
   scale_color_viridis(option = "A", direction = -1) +
   scale_y_sqrt() +
   ggtitle("Number of quakes over magnitude 2",
      subtitle = "In early 2012 there was a sudden drop off in the number of noticeable quakes") +
   labs(x = "", y = "Number of earthquakes of magnitude two or higher\n(square root transformed scale)",
        color = "Magnitude of\nlargest quake\neach day",
        caption = "Data from quakesearch.geonet.org.nz\nAnalysis from ellisp.github.io")

png("../img/0068-quakes-p2.png", 900, 600, res = 100)
   print(p2)
dev.off()


p_basis <- quakes_g %>%
   filter(day > as.Date("1750-12-31")) %>%
   ggplot(aes(x = day, y = TotalEnergy)) + 
   geom_point() 

set.seed(123)   
small_data <- quakes_g %>%
   filter(day > as.Date("1750-12-31")) %>%
   filter(MaxMagnitude > 7.5) %>%
   mutate(lab = paste0(day, "\nMax magnitude: ", round(MaxMagnitude, 2)))

p3 <- p_basis +
   geom_text_repel(data = small_data, aes(label = lab), size = 3, colour = "steelblue") +
   scale_y_continuous(label = comma) +
   ggtitle("Total energy released by earthquakes per day",
           subtitle = "The days of the 2009 Dusky Sounds and 2016 Kaikoura earthquakes were the biggest since 1855") +
   labs(x = "", y = "Energy in Megatons of TNT", 
        caption = "Data from quakesearch.geonet.org.nz\nAnalysis from ellisp.github.io") +
   annotate("text", x = as.Date("1800-01-01"), y = 15, size = 3, colour = "steelblue",
            label = "Data from before the 1950s fails to \ninclude many smaller events")

png("../img/0068-quakes-p3.png", 9 * 100, 6 * 100, res = 100)
print(p3)
dev.off()



p4 <- quakes_g %>%
   filter(day > as.Date("1750-12-31")) %>%
   ggplot(aes(x = day, y = TotalEnergy * 10 ^ 6)) + 
   geom_line(colour = "grey50") +
   geom_point() +
   scale_y_log10(label = comma) + 
   stat_rollapplyr(width = 60, colour = "steelblue", size = 1.2)+
   coord_cartesian(xlim = as.Date(c("1945-01-01", "2016-12-30"))) +
   labs(x = "", y = "Energy in tons of TNT\n(logarithmic scale)") +
   ggtitle("Total energy released by earthquakes per day",
           subtitle = "The low energy release in the late 1970s and early 1980s is apparent on the log scale")

png("../img/0068-quakes-p4.png", 900, 600, res = 100)
print(p4)
dev.off()


p5 <- p4 + 
   coord_cartesian(xlim = as.Date(c("2008-01-01", "2016-12-30"))) +
   ggtitle("Total energy released by earthquakes per day",
           subtitle = "The drop in earthquake energy from early 2012 is apparent")

png("../img/0068-quakes-p5.png", 900, 600, res = 100)
print(p5)
dev.off()

p6 <- p4 + 
   coord_cartesian(xlim = as.Date(c("2016-01-01", "2016-12-30"))) +
   ggtitle("Total energy released by earthquakes per day",
           subtitle = "There is no obvious build up to big events like the Gisborne quakes in September and Kaikoura quakes in November")
png("../img/0068-quakes-p6.png", 900, 600, res = 100)
print(p6)
dev.off()




