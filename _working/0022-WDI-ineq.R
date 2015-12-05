library(WDI)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(GGally)   # for ggplot pairs plot
library(showtext) # for fonts

# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()


# import data from the World Bank
gini <- WDI(indicator = "SI.POV.GINI", end = 2015, start = 1960)
p90 <- WDI(indicator = "SI.DST.10TH.10", end = 2015, start = 1960)
p80 <- WDI(indicator = "SI.DST.05TH.20", end = 2015, start = 1960)
p10 <- WDI(indicator = "SI.DST.FRST.10", end = 2015, start = 1960)
p20 <- WDI(indicator = "SI.DST.FRST.20", end = 2015, start = 1960)


# merge and tidy up
inequality <- merge(gini, p10, all = TRUE) %>%
   merge(p20, all = TRUE) %>%
   merge(p80, all = TRUE) %>%
   merge(p90, all = TRUE) %>%
   # create synthetic variables
   mutate(P90P10 = SI.DST.10TH.10 / SI.DST.FRST.10,
          P80P20 = SI.DST.05TH.20 / SI.DST.FRST.20) %>%
   select(-iso2c) %>%
   # gather into long form
   gather(variable, value, -country, -year) %>%
   filter(!is.na(value)) %>%
   # rename the variables:
   mutate(variable = gsub("SI.DST.FRST.", "P", variable),
          variable = gsub("SI.DST.10TH.10", "P90 (Share of richest 10%)", variable),
          variable = gsub("SI.DST.05TH.20", "P80 (Share of richest 20%)", variable),
          variable = gsub("SI.POV.GINI", "Gini", variable, fixed = TRUE))

all_countries <- unique(inequality$country) # 158 countries


#---------------all 7 measures over time?-------------
# example plot of 6 random countries over time
svg("../img/0022-eg-countries.svg", 8, 8)
set.seed(127)
inequality %>%
   filter(country %in% sample(all_countries, 6, replace = FALSE)) %>%
   mutate(variable = str_wrap(variable, 20)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_point() +
   geom_line() +
   facet_grid(variable ~ country, scales = "free_y")
dev.off()


# average observations per country and variable:
inequality %>%
   group_by(variable, country) %>%
   summarise(ObsPerCountry = length(value)) %>%
   group_by(variable) %>%
   summarise(AveObsPerCountry = mean(ObsPerCountry),
             Countries = length(ObsPerCountry))



#---------trimmed mean for each country on each variable, in wide format----------
inequality_aves <- inequality %>%
   group_by(country, variable) %>%
   summarise(value = mean(value, tr = 0.2)) %>%
   spread(variable, value) %>%
   # make the column names legal
   data.frame(stringsAsFactors = FALSE, check.names = TRUE)
names(inequality_aves) <- gsub(".", "", names(inequality_aves), fixed = TRUE)
   

# what are the extreme values of those averages of ratios:
inequality_aves %>%
   arrange(P90P10) %>%
   tail()


# Draw pairs plots
svg("../img/0022-pairs.svg", 12, 12)
inequality_aves %>%
   # Bhutan has an average P90P10 of 600 so we exclude it
   filter(country != "Bhutan") %>%
   select(-country) %>%
   ggpairs() 
dev.off()

png("../img/0022-pairs.png", 1200, 1200, res = 100)
inequality_aves %>%
   # Bhutan has an average P90P10 of 600 so we exclude it
   filter(country != "Bhutan") %>%
   select(-country) %>%
   ggpairs() 
dev.off()

