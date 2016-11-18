library(dplyr)
library(ggplot2)
library(viridis)
library(ggseas)

#================download data==================
urls <- readLines("../data/earthquake-data-urls.txt")

tmp <- list()

# this takes a few minutes to download all the data from GNS and isn't necessarily fair on them,
# so maybe download my pre-done version first, and find a way to just add the last year?
for(i in 1:length(urls)){
   cat(i)
   tmp[[i]] <- read.csv(urls[i])   
   Sys.sleep(1) # to avoid hitting the server before it's had a breather
}
quakes_orig <- tmp
save(quakes_orig, file = "../data/quakes_orig.rda")

quakes_bound <- do.call("rbind", quakes_orig)

# note they seem to be using -9 as an NA value
quakes_df <- quakes_bound %>%
   mutate(day = as.Date(substring(origintime, 1, 10))) %>%
   filter(longitude > 166 & longitude < 180 & latitude > -48 & latitude < -32) %>%
   select(longitude, latitude, magnitude, depth, day, evaluationmethod, eventtype) %>%
   mutate(magnitude = ifelse(magnitude < 0, NA, magnitude),
          energy = (10 ^ 1.5) ^ magnitude)
save(quakes_df, file = "../data/quakes_df.rda")

summary(quakes_df)
#==================analysis=============

# facet map
m1 <- quakes_df %>%
   filter(day >= as.Date("2016-11-09")) %>%
   ggplot(aes(x = longitude, y = latitude, size = energy / 10 ^ 9, colour = depth)) +
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

# grouped by day
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
             TotalEnergy = sum(energy))


quakes_g %>%
   arrange(desc(TotalEnergy))

# period in 1977 - 1988 when many days had no earthquake > 3
quakes_g %>%
   filter(day > as.Date("1949-12-31")) %>%
   ggplot(aes(x = day, y = MaxMagnitude, colour = MaxMagnitude)) + 
   geom_line() +
   geom_point() + 
   theme_grey() +
   scale_color_viridis(option = "A", direction = -1) +
   scale_y_sqrt()


# six months after the 2010 and 2011 earthquakes a sudden big drop in the number of quakes > 2
quakes_g %>%
   filter(day > as.Date("1969-12-31")) %>%
   ggplot(aes(x = day, y = NumberOver2, colour = MaxMagnitude)) + 
   geom_line() +
   geom_point() + 
   theme_grey() +
   scale_color_viridis(option = "A", direction = -1) +
   scale_y_sqrt()


p <- quakes_g %>%
   filter(day > as.Date("1969-12-31")) %>%
   ggplot(aes(x = day, y = TotalEnergy)) + 
   geom_line(colour = "grey50") +
   geom_point() 
p

p +
   scale_y_log10() + 
   stat_rollapplyr(width = 60, colour = "lightblue", size = 2)

p + 
   scale_y_log10() + 
   stat_rollapplyr(width = 60, colour = "lightblue", size = 2) +
   coord_cartesian(xlim = as.Date(c("2010-01-01", "2016-12-30")))

p + 
   scale_y_log10() + 
   stat_rollapplyr(width = 7, colour = "lightblue", size = 2) +
   coord_cartesian(xlim = as.Date(c("2016-01-01", "2016-12-30")))

