devtools::install_github("jmarshallnz/statsNZ")

library(statsNZ)
library(dplyr)
library(ggseas)
library(stringr)

available_stats()

get_groups("ESM")

esm <- get_stats("ESM", "Industry by variable - Subannual Financial Collection")
head(esm)
summary(esm)

unique(esm$SeriesTitle1) # 15 levels
unique(esm$SeriesReference) # 225 levels and the code contains information
unique(esm$status) # 3 levels
str(esm)

# SeriesReference MFGQ.XXX1KA (ie last three letters 1K1) means
# Sales in volume terms ie adjusted for price changes, 
# the September 2010 quarter prices of industry XXX

# from the original Hot Off the Press release
# http://www.stats.govt.nz/~/media/Statistics/Browse%20for%20stats/EconomicSurveyofManufacturing/HOTPDec15qtr/esm-dec-2015-tables.xlsx
# we find 


# taking a punt on status "F" meaning final (the alternatives are C and R.  There don't seem
# to be any values of R in this subset, and C seems identical to F.)
the_data <- esm %>%
   filter(status == "F") %>%
   filter(grepl("1KA$", SeriesReference)) %>%
   mutate(SeriesTitle1 = str_wrap(SeriesTitle1, 30))

# what's the order biggest to largest, so we can sort the facets:
sorted <- the_data %>%
   filter(Period == max(Period)) %>%
   arrange(desc(DataValues))

p1 <- the_data %>%
   mutate(SeriesTitle1 = factor(SeriesTitle1, levels = sorted$SeriesTitle1)) %>%
   ggplot(aes(x= Period, y = DataValues)) +
   facet_wrap(~SeriesTitle1, scales = "free_y", ncol = 3) +
   geom_line(colour = "grey70") +
   stat_stl(s.window = 7, frequency = 4, colour = "steelblue", size = 0.9) +
   scale_y_continuous("Sales per quarter, September 2010 prices, millions of dollars\n", 
                      label = dollar) +
   labs(x = "", caption = "Source: Statistics New Zealand experimental API\nhttp://innovation.stats.govt.nz/initiatives/time-series-api-prototype/") +
   ggtitle("Economic Survey of Manufacturing, New Zealand",
           subtitle = "Different fortunes in industries' manufacturing trends")

svg("../img/0061-manufacturing.svg", 8, 8)
print(p1)
dev.off()

png("../img/0061-manufacturing.png", 800, 800)
print(p1)
dev.off()


