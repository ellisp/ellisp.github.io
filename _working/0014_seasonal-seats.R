# load and prepare data
library(mbieDBmisc)
TRED <- odbcConnect("TRED_Prod")
dataset <- sqlFetch(TRED, "timeseries.dataset")
dataset[grep("arrival", dataset$dataset), "dataset"] %>% View

itm <-ImportTS2(TRED, "Visitor arrivals by country of residence, purpose and NZ port (Monthly)") %>%
   rename(country = CV1, purpose = CV2, airport = CV3) %>%
   select(-Obs_Status,-CountryGrouped, -Magnitude)

save(itm, file= "../data/arrivals-country-purpose-airport-20151010.rda")




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

# download file from Statistics New Zealand's International Travel and Migration,
# prepared by me earlier.  Ultimately, this was from Infoshare as the series
# "Visitor arrivals by country of residence, purpose and NZ port (Monthly)"
download.file("https://github.com/ellisp/ellisp.github.io/blob/master/data/arrivals-country-purpose-airport-20151010.rda?raw=true",
              mode = "wb",
              destfile = "tmp.rda")
load("tmp.rda")
head(itm)

uk_all <- itm %>%
   filter(country == "United Kingdom" & 
             purpose == "TOTAL ALL TRAVEL PURPOSES" &
             airport == "TOTAL NEW ZEALAND PORTS"
             )

uk_ts <- ts(uk_all$Value, start = c(1978, 4), frequency = 12)

svg("../img/0014-uk-ts-basics.svg", 7, 7)
par(mfrow = c(2, 2), family = "myfont")
plot(uk_ts, xlab = "")
acf(uk_ts, main = "")
pacf(uk_ts, main = "")
spectrum(uk_ts, main = "")
dev.off()

svg("../img/0014-uk-decompose.svg", 7, 7)
plot(decompose(uk_ts, type = "multiplicative"))
dev.off()

mod <- seas(uk_ts)

svg("../img/0014-uk-ts-seats.svg", 7, 7)
par(mfrow = c(2, 2), family = "myfont")
plot(mod)
plot(resid(mod), main = "Residuals")
qqnorm(resid(mod), main = "Residuals compared to Normal")
pacf(resid(mod), "Partial ACF of residuals")
dev.off()

summary(mod)

inspect(mod)

uk_sa <- data_frame(
   Time = time(uk_ts),
   Original = uk_ts,
   SA = final(mod))

svg("../img/0014-uk-compare.svg", 7, 7)
print(
ggplot(uk_sa, aes(x = Original, y = SA, colour = Time)) +
   geom_abline(intercept = 0, slope = 1, colour = "grey50") +
   geom_path() +
   geom_point() +
   coord_equal() +
   scale_x_continuous(label = comma) +
   scale_y_continuous("Seasonally adjusted", label = comma) +
   ggtitle("Comparison of original and seasonally adjusted\n arrivals from the UK to New Zealand")
)
dev.off()

svg("../img/0014-uk-adjusted-ggplot.svg", 7, 5)
print(
uk_sa %>%
   gather("variable", "arrivals", -Time) %>% 
   mutate(variable = gsub("SA", "Seasonally adjusted by SEATS", variable)) %>%
   ggplot(aes(x = Time, y = arrivals, colour = variable)) +
   geom_line() +
   labs(colour = "", x = "") +
   scale_y_continuous("Number of arrivals", label = comma) +
   ggtitle("Visitor arrivals from the UK to New Zealand") +
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

svg("../img/0014-uk-ts-new-stat.svg", 7, 5)
print(
ggplot(uk_all, aes(x = TimePeriod, y = Value)) +
   # original:
   geom_line(colour = "red") +
   # seasonally adjusted:
   stat_seas(frequency = 12, start = c(1978, 4))
)
dev.off()

# the beauty is we can now use with facets and other groupings:

p1 <- itm %>%
   filter(country %in% c("United Kingdom", "Australia", "Japan") &
             airport %in% c("Auckland airport", "Christchurch airport", "Wellington airport") &
             purpose %in% c("Holiday/Vacation", "Business", "Visit Friends/Relatives")) %>%
   ggplot(aes(x = TimePeriod, y = Value, colour = airport)) +
   stat_seas(frequency = 12, start = c(1978, 4)) +
   facet_grid(country ~ purpose, scales = "free_y") +
   labs(x = "", y = "Seasonally adjusted arrivals",
        title = "Visitor arrivals to New Zealand, selected origins and ports")


svg("../img/0014-faceted.svg", 10, 7)
   print(p1)
dev.off()