library(devtools)
install_github("ellisp/ggseas/pkg")

library(ggseas)

# make demo data
ap_df <- data.frame(
   x = as.numeric(time(AirPassengers)),
   y = as.numeric(AirPassengers)
)


# SEATS with defaults
svg("../img/0030-p1.svg", 7, 4)
print(
ggplot(ap_df, aes(x = x, y = y)) +
   geom_line(colour = "grey80") +
   stat_seas(start = c(1949, 1), frequency = 12) +
   ggtitle("SEATS seasonal adjustment - international airline passengers") +
   ylab("International airline passengers per month")
)
dev.off()

# X11 with no outlier treatment
svg("../img/0030-p2.svg", 7, 4)
print(
ggplot(ap_df, aes(x = x, y = y)) +
   geom_line(colour = "grey80") +
   stat_seas(start = c(1949, 1), frequency = 12, x13_params = list(x11 = "", outlier = NULL)) +
   ggtitle("X11 seasonal adjustment - international airline passengers") +
   ylab("International airline passengers per month")
)
dev.off()


p3 <- ggplot(ldeaths_df, aes(x = YearMon, y = deaths, colour = sex)) +
   geom_point(colour = "grey80") +
   geom_line(colour = "grey80") +
   facet_wrap(~sex) +
   stat_seas(start = c(1974, 1), frequency = 12, size = 2) +
   ggtitle("Seasonally adjusted lung deaths in the UK 1974 - 1979") +
   ylab("Deaths") +
   xlab("(light grey shows original data;\ncoloured line is seasonally adjusted)") +
   theme(legend.position = "none")

svg("../img/0030-p3.svg", 8, 5)
   print(p3) # note - the call to seas() isn't run until each and every time you print the plot
dev.off()

png("../img/0030-p3.png", 800, 500, res = 100)
   print(p3) # note - the call to seas() isn't run until each and every time you print the plot
dev.off()


svg("../img/0030-p4.svg", 8, 5)
# seasonality varies a bit over time, works better:
print(
ggplot(ap_df, aes(x = x, y = y)) +
   stat_stl(frequency = 12, s.window = 7)
)
dev.off()