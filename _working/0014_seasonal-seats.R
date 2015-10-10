# load and prepare data
library(mbieDBmisc)
TRED <- odbcConnect("TRED_Prod")

ect <-ImportTS2(TRED, "Values - Electronic card transactions A/S/T by industry group (Monthly)") %>%
   filter(CV1 == "Actual" & !is.na(Value)) %>%
   rename(group = CV2) %>%
   select(-Obs_Status, -CV1)

save(ect, file= "../data/Electronic card transactions by industry group Monthly.rda")




#=============to be used in blog==================
library(seasonal)
library(tidyr)
library(dplyr)
library(showtext)
library(ggplot2)
library(scales)

# set up fonts etc
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

# set path to X13-SEATS and check it's working
Sys.setenv(X13_PATH = "c:/winx13/x13as")
checkX13()

# download file with data ultimately from Statistics New Zealand's Infoshare, prepared 
# earlier: "Values - Electronic card transactions A/S/T by industry group (Monthly)"
#
download.file("https://github.com/ellisp/ellisp.github.io/blob/master/data/Electronic card transactions by industry group Monthly.rda?raw=true",
              mode = "wb",
              destfile = "tmp.rda")
load("tmp.rda")
head(ect)

apparel <- ect %>%
   filter(group == "Apparel")

apparel_ts <- ts(apparel$Value, start = c(2002, 10), frequency = 12)

svg("../img/0014-apparel-ts-basics.svg", 7, 7)
par(mfrow = c(2, 2), family = "myfont")
plot(apparel_ts, xlab = "")
acf(apparel_ts, main = "")
pacf(apparel_ts, main = "")
spectrum(apparel_ts, main = "")
dev.off()

svg("../img/0014-apparel-decompose.svg", 7, 7)
plot(decompose(apparel_ts, type = "multiplicative"))
dev.off()

mod <- seas(apparel_ts)

svg("../img/0014-apparel-ts-seats.svg", 7, 7)
par(mfrow = c(2, 2), family = "myfont")
plot(mod)
plot(resid(mod), main = "Residuals")
qqnorm(resid(mod), main = "Residuals compared to Normal")
pacf(resid(mod), "Partial ACF of residuals")
dev.off()

summary(mod)

inspect(mod)

apparel_sa <- data_frame(
   Time = time(apparel_ts),
   Original = apparel_ts,
   SA = final(mod))

svg("../img/0014-apparel-compare.svg", 7, 7)
print(
ggplot(apparel_sa, aes(x = Original, y = SA, colour = Time)) +
   geom_abline(intercept = 0, slope = 1, colour = "grey50") +
   geom_path() +
   geom_point() +
   coord_equal() +
   scale_x_continuous(label = comma) +
   scale_y_continuous("Seasonally adjusted", label = comma) +
   ggtitle("Comparison of original and seasonally adjusted\n electronic card transactions on apparel in New Zealand")
)
dev.off()

svg("../img/0014-apparel-adjusted-ggplot.svg", 7, 5)
print(
apparel_sa %>%
   gather("variable", "value", -Time) %>% 
   mutate(variable = gsub("SA", "Seasonally adjusted by SEATS", variable)) %>%
   ggplot(aes(x = Time, y = value, colour = variable)) +
   geom_line() +
   labs(colour = "", x = "") +
   scale_y_continuous("Value of transactions ($m)", label = dollar) +
   ggtitle("Electronic card transactions on apparel in New Zealand") +
   theme(legend.position = "bottom")
)
dev.off()

#----------------making a new ggplot2 stat-----------------

library(proto)

StatSeas <- proto(ggplot2:::Stat, {    
   required_aes <- c("x", "y")
   default_geom <- function(.) GeomLine
   objname <- "seasadj"
   calculate_groups <- function(., data, scales, ...){
      .super$calculate_groups(., data, scales, ...)
   }
   calculate <- function(., data, scales, frequency, start, ...) {
      y_ts <- ts(data$y, frequency = frequency, start = start)
      y_sa <- seasonal::final(seasonal::seas(y_ts, ...))
      result <- data.frame(x = data$x, y = as.numeric(y_sa))
      return(result)
   }
}) 
stat_seas <- StatSeas$new 

svg("../img/0014-apparel-ts-new-stat.svg", 7, 5)
print(
ggplot(apparel, aes(x = TimePeriod, y = Value)) +
   # original:
   geom_line(colour = "red") +
   # seasonally adjusted:
   stat_seas(frequency = 12, start = c(2002, 10))
)
dev.off()

# the beauty is we can now use with facets and other groupings:
p1 <- ect %>%
   ggplot(aes(x = TimePeriod, y = Value, colour = group)) +
   geom_line(alpha = 0.3) +
   stat_seas(frequency = 12, start = c(1978, 4)) +
   facet_wrap( ~ group, scales = "free_y", ncol = 2) +
   labs(x = "", y = "Seasonally adjusted monthly transaction value ($m)",
        title = "Electronic card transactions in New Zealand") +
   theme(legend.position = "none")


svg("../img/0014-faceted.svg", 7, 8)
   print(p1)
dev.off()