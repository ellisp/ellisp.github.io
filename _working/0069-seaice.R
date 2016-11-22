library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(tidyr)
library(seasonal)
library(ggseas)
library(forecast)
library(forecastHybrid)
library(directlabels)
library(ggthemes)
#=============download===========
# https://nsidc.org/data/docs/noaa/g02135_seaice_index/#monthly_data_files
# 3.1.6 Monthly Sea Ice Extent and Area Data Files
# 
# file naming convention h_mm_area.txt
# 
# ftp from here: ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/
   
mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monnum <- gsub(" ", 0, format(1:12, width = 2))
urls <- paste0(
      "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/",
      mon,
      "/N_",
      monnum,
      "_area_v2.txt"  
)


tmp <- tempdir() 

# only about 60KB of downloading:
for(i in 1:12){
   download.file(urls[i], destfile = paste0(tmp, "/seaice", i, ".txt"))
}

seaice_list <- list(12)
maxyear <- 2016 # better would be to automate this
for(i in 1:12){
   # next line returns lots of warnings
   seaice_list[[i]] <- read_fwf(paste0(tmp, "/seaice", i, ".txt"), 
            col_positions = fwf_widths(c(5, 6, 10, 7, 7, 6),
                                       col_names = c("year", "mo", "data_type", "region", "extent", "area")),
            skip = 1, na = c("", "NA", "-9999"), n_max = maxyear - 1977
            )
}

seaice <- do.call("rbind", seaice_list) %>%
   mutate(
      year = as.numeric(year),
      mo = as.numeric(mo),
      extent = as.numeric(extent),
      area = as.numeric(area)
   ) %>%
   filter(!is.na(year)) %>%
   arrange(year, mo)


#================graphing================
# try to improve on https://www.washingtonpost.com/news/energy-environment/wp/2016/11/17/the-north-pole-is-an-insane-36-degrees-warmer-than-normal-as-winter-descends/?postshare=4511479671672405&tid=ss_tw&utm_term=.7268a7dc9692
p1 <- seaice %>%
   select(mo, year, extent, area) %>%
   gather(variable, value, -mo, -year) %>%
   mutate(latestyear = (year == max(year))) %>%
   ggplot(aes(x = mo, y = value, group = as.factor(year), colour = year, alpha = latestyear)) +
   facet_wrap(~variable, ncol = 1) +
   geom_line() +
   scale_alpha_discrete(range = c(0.25, 1), guide = "none") +
   scale_color_viridis(direction = -1)  +
   scale_x_continuous("", breaks = 1:12, labels = mon)
p1


p2 <- ggplot(seaice, aes(x = extent, y = area, colour = year + mo / 12)) +
   geom_point() +
   geom_path() +
   coord_equal() +
   scale_color_viridis(direction = -1)  
p2

# difference between area and extent explained at https://nsidc.org/arcticseaicenews/faq/#area_extent
# Basically, scientists prefer extent.

p3 <- seaice %>%
   mutate(latestyear = (year == max(year))) %>%
   ggplot(aes(x = mo, y = extent, group = as.factor(year), colour = year, alpha = latestyear)) +
   geom_line() +
   scale_alpha_discrete(range = c(0.25, 1), guide = "none") +
   scale_color_viridis("", direction = -1, guide = guide_legend(reverse = FALSE))  +
   scale_x_continuous("", breaks = 1:12, labels = mon) +
   theme_tufte(base_family = "myfont") +
   theme(legend.position = c(0.2, 0.3)) +
   # "10.5" in the next line is a magic number, would need manual updating if done with more data:
   annotate("text", x = 10.5, y = seaice[nrow(seaice), ]$extent, label = maxyear) +
   ggtitle("The extent of arctic sea ice has been steadily declining",
           subtitle = "Area of ocean with at least 15% sea ice, from the USA National Snow and Ice Data Center") +
   labs(y = "Extent of Arctic sea ice\n(millions of square kilometres)",
        caption = "Source: https://nsidc.org/data/docs/noaa/g02135_seaice_index/#monthly_data_files") +
   theme(plot.caption = element_text(colour = "grey50"))

svg("../img/0069-seaice-final.svg", 8, 4)
print(p3)
dev.off()

png("../img/0069-seaice-final.png", 800, 400, res = 100)
print(p3)
dev.off()


#===============timeseries analysis==========
seaice_extent <- seaice  %>%
   select(extent) %>%
   ts(frequency = 12, start = c(1978, 11)) %>%
   tsdf()

seaice_extent$imputed <- FALSE
for(i in 2:nrow(seaice_extent)){
   if(is.na(seaice_extent[i, "y"])){
      seaice_extent[i, "y"] <- mean(c(seaice_extent[i - 2, "y"], seaice_extent[i + 2, "y"]))
      seaice_extent[i, "imputed"] <- TRUE
   }
}
# this method makes a small dip in 1988 but is better than nothing

# stl works best as the decomposition method in this case - better than X13, not sure why
ggsdc(seaice_extent, aes(x = x, y = y), method = "stl", s.window = 7) +
   geom_line() 
# NRTSI is an interim measure used until the GSFC data are available.

# error potential - more magic numbers
seaice_ts <- ts(seaice_extent$y, start = c(1978, 11), frequency = 12)

svg("../img/0069-tsdisplay.svg", 7, 5)
par(family = "myplot", bty = "l")
tsdisplay(seaice_ts)
dev.off()

svg("../img/0069-monthplot.svg", 7, 5)
par(family = "myplot", bty = "l")
monthplot(seaice_ts, ylab = "Arctic ice extent",
          main = "Declining ice coverage in all twelve months")
grid()
dev.off()

model <- hybridModel(seaice_ts, models = "aefs") # using the GitHub version of forecastHybrid
seaice_fc <- forecast(model, 24)

svg("../img/0069-forecast.svg", 7, 2.7) 
   autoplot(seaice_fc) + labs(x = "", y = "Arctic ice extent")
dev.off()



