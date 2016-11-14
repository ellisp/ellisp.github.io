library(ggplot2)
library(scales)
library(dplyr)

# http://www.stuff.co.nz/national/86458731/Cheviot-earthquake-Tracing-the-source-of-the-7-5-magnitude-quake-and-its-aftermath?utm_source=dlvr.it&utm_medium=twitter

# Energy released in all NZ Earthquakes, 2010-2016

eqs <- data.frame(
   energy = c(48.99, 13.34, 2.67, 6.25, 4.93, 2.71, 4.73, 16.38),
   period = c("Nov 14 2016", "Earlier 2016", paste("All", 2015:2010)),
   stringsAsFactors = FALSE)

eqs <- eqs %>%
   mutate(period = factor(period, levels = period)) %>%
   arrange(desc(period)) %>%
   mutate(cumenergy = cumsum(energy),
          centres = cumenergy - energy / 2)
             

leg <- guide_legend(reverse = TRUE)

p <- ggplot(eqs, aes(x = 1, weight = energy, fill = period)) +
   geom_bar(width = 1, colour = "grey50") +
   geom_text(x = 1.3, aes(y = centres, label = paste0(energy, "%"), colour = period)) +
   coord_polar(theta = "y") +
   scale_fill_brewer(palette = "Oranges", direction = -1, guide = leg) +
   scale_color_brewer(palette = "Blues", direction = 1, guide = leg)    +
   theme_minimal(base_family = "myfont") +
   theme(axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank(),
         plot.caption = element_text(hjust = 0.5)) +
   labs(fill = "", colour = "", 
        caption = "Source: http://www.stuff.co.nz/national/86458731/\nCheviot-earthquake-Tracing-the-source-of-the-7-5-magnitude-quake-and-its-aftermath\nSupplied by John Holdaway") +
   ggtitle("Half the earthquake energy released since 2010 came in a single day",
           subtitle = "Energy released in all New Zealand earthquakes, 2010-2016")

svg("../img/0067-pie.svg", 8, 8)
print(p)
dev.off()


png("../img/0067-pie.png", 800, 800, res = 100)
print(p)
dev.off()
