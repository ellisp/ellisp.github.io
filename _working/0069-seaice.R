library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(seasonal)
library(ggseas)
library(forecast)
library(ggthemes)
library(testthat)
library(tibble)
library(lubridate) # for yday
#=============download===========


mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# https://nsidc.org/data/docs/noaa/g02135_seaice_index/#daily_data_files

# This is the latest incomplete year's "near real time" data:
download.file("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_nrt_v2.csv",
              destfile = "seaice_nrt.csv")

# And this is the earlier, fully definitive years' data
download.file("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/NH_seaice_extent_final_v2.csv",
              destfile = "seaice_final.csv")

seaice_nrt <- read.csv("seaice_nrt.csv", skip = 2, header = FALSE)[ , 1:5]
seaice_final <- read.csv("seaice_final.csv", skip = 2, header = FALSE)[ , 1:5]

seaice <- rbind(seaice_final, seaice_nrt)
names(seaice) <- c("year", "month", "day", "extent", "missing")
expect_equal(sum(seaice$missing == 0), nrow(seaice))
   
seaice <- seaice %>%
   mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
   group_by(month) %>%
   mutate(monthday = month + day / max(day)) %>%
   ungroup() %>%
   mutate(month = factor(month, labels = mon)) %>%
   arrange(year, month, day) %>%
   mutate(timediff = c(NA, diff(date)),
          dayofyear = yday(date))

# clean up (unless you want to keep the csvs)
unlink("seaice_nrt.csv")
unlink("seaice_final.csv")   

#================graphing================
# try to improve on https://www.washingtonpost.com/news/energy-environment/wp/2016/11/17/the-north-pole-is-an-insane-36-degrees-warmer-than-normal-as-winter-descends/?postshare=4511479671672405&tid=ss_tw&utm_term=.7268a7dc9692

# difference between area and extent explained at https://nsidc.org/arcticseaicenews/faq/#area_extent
# Basically, scientists prefer extent, and that is the only one in the daily data (monthly has area too).


# There is a trick in the below, equally spacing the months.  this only slightly warps
# the visual though(makes February look about 8% longer than it is), not noticeable
p3 <- seaice %>%
   mutate(latestyear = (year == max(year))) %>%
   ggplot(aes(x = monthday, y = extent, group = as.factor(year), colour = year, alpha = latestyear)) +
   geom_line() +
   scale_alpha_discrete(range = c(0.25, 1), guide = "none") +
   scale_color_viridis("", direction = -1, guide = guide_legend(reverse = FALSE))  +
   scale_x_continuous(breaks = 1:12, labels = mon) +
   theme_tufte(base_family = "myfont") +
   theme(legend.position = c(0.2, 0.3)) +
   # x coordinate in the next line is a magic number, would need manual updating if done with more data:
   annotate("text", x = 12, y = seaice[nrow(seaice), ]$extent, label = maxyear) +
   ggtitle("The extent of Arctic sea ice has been steadily declining...",
           subtitle = "...but this year is special.") +
   labs(y = "Area of ocean with at least 15% sea ice \n(millions of square kilometres)",
        x = "",
        caption = "Source: USA National Snow and Ice Data Center\nhttps://nsidc.org/data/docs/noaa/g02135_seaice_index/#daily_data_files") +
   theme(plot.caption = element_text(colour = "grey50"))

svg("../img/0069-seaice-final.svg", 8, 4)
print(p3)
dev.off()

ggsave("../img/0069-seaice-final.png", p3, width = 8, height = 4, dpi = 300)

#===============timeseries analysis==========
# the data is not actuall daily but starts as every second day until about 1987.
# Latest date that is 2 days ahead of the previous date:
seaice %>%
   filter(timediff == 2)  %>%
   filter(date == max(date)) %>%
   select(date)

seaice_daily <- seaice %>%
   filter(date > as.Date("1987-08-20")) 

svg("../img/0069-decomposition.svg", 7, 6) 
ggsdc(seaice_daily, aes(x = date, y = extent), method = "stl", s.window = 7, frequency = 365.25) +
   geom_line() +
   ggtitle("Seasonal decomposition of Arctic seasonal ice coverage",
           subtitle = "Some difficult patterns when daily data was new, then a steady decline") +
   labs(x = "", y = "Sea ice extent\n",
        caption = "Analysis by http://ellisp.github.io; data from National Snow and Ice Data Center")
dev.off()

# adapting the method at http://robjhyndman.com/hyndsight/dailydata/
# going to pretend no frequency in the call to ts we use later in auto.arima
seaice_ts1 <- ts(seaice_daily$extent, frequency = 1, start = 1987 + (8 + 20 / 31) / 12 )
seaice_ts365 <- ts(seaice_daily$extent, frequency = 365.25, start = 1987 + (8 + 20 / 31) / 12 )

# and instead create a set of fourier seasonal terms
z <- fourier(seaice_ts365, K = 5)
zf <- fourier(seaice_ts365, K = 5, h = 2 * 365)

model <- auto.arima(seaice_ts1, xreg = z)

seaice_fc <- forecast(model, 2 * 365, xreg = zf)


svg("../img/0069-forecast.svg", 7, 3.5) 
   autoplot(seaice_fc) + 
      labs(x = "", y = "Arctic ice extent") +
      ggtitle("Forecast Actic ice coverage to late 2018") +
      labs(caption = "Analysis by http://ellisp.github.io; data from National Snow and Ice Data Center",
           x = "Days since 20 August 1987 (when daily measurements began)")
dev.off()


system('"C:\\Program Files\\ImageMagick-7.0.2-Q16\\convert" ../img/0069-forecast.svg ../img/0069-forecast.png')
