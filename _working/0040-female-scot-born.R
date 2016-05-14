library(ggplot2)
library(scales)
library(ggrepel)
library(ggthemes)


# Ratio of females to males in the Scotland- and Ireland- born population in 1881
fem_ratio <- data.frame(
   province = c("Auckland", "Taranaki", "Hawke's Bay", "Wellington", "Marlborough", "Nelson", "West Coast", "Canterbury", "Otago"), 
   scots = c(68.14, 55.24, 58.74, 71.07, 51.56, 52.66, 38.95, 67.80, 75.16) / 100,
   irish = c(84.96, 43.40, 65.87, 87.73, 69.98, 50.20, 67.92, 81.28, 81.52) / 100
)
   

sanitise_ticks <- function(x, digits = 4){
   ticks <- sort(unique(round(x, digits = digits)))
   for(i in 2:length(ticks)){
      if(ticks[i] - ticks[i-1] < 0.004){
         ticks[i-1] = mean(ticks[(i-1):i])
         ticks[i] <- 0
      }
   }
   ticks <- ticks[ticks != 0] 
}

yticks <- sanitise_ticks(fem_ratio$irish)
xticks <- sanitise_ticks(fem_ratio$scots)

linecol <- "steelblue"



p1 <- ggplot(fem_ratio, aes(x = scots, y = irish, label = province)) +
   theme_tufte() +
   geom_abline(intercept = 0, slope = 1, colour = linecol, alpha = 0.5) +
   geom_point() +
   geom_rug(colour = "steelblue") +
   geom_text_repel(colour = "grey60", segment.color = NA, family = "serif") +
   scale_x_continuous(breaks = xticks, label = percent) +
   scale_y_continuous(breaks = yticks, label = percent) +
   theme(axis.text = element_text(colour = "steelblue"),
         axis.ticks = element_line(colour = "steelblue")) +
   theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)) +
   labs(x = "\nScottish born\n", y = "Irish born\n", 
        caption = "Source: Census, analysed in Lenihan 'From Alba to Aotearoa'") +
   ggtitle("Females as a percentage of males in New Zealand 1881",
           subtitle = "Selected places of birth shown") +
   annotate("text", x = 0.68, y = 0.60, colour = linecol, family = "serif",
            label = "Line shows same ratio\nfor Scots- and Irish- born",
            alpha = 0.5) +
   coord_equal()

set.seed(234)
svg("../img/0040-females.svg", 6, 7)
   print(p1)
dev.off()

set.seed(234)
png("../img/0040-females.png", 600, 700, res = 100)
   print(p1)
dev.off()