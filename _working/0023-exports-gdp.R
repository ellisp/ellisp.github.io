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



#-------------------data imports, first explore, and clean-----------------
# Exports of goods and services (% of GDP)
exports <- WDI(indicator = "NE.EXP.GNFS.ZS", end = 2015, start = 1960)
# GDP per capita (constant 2000 US$)
gdp <- WDI(indicator = "NY.GDP.PCAP.KD", end = 2015, start = 1960)

both <- merge(exports, gdp) %>%
   rename(exports = NE.EXP.GNFS.ZS,
          gdp = NY.GDP.PCAP.KD) %>%
   # removing any year-country combos missing eith gdp or exports:
   filter(!(is.na(exports) | is.na(gdp))) %>%
   arrange(country, year)

# let's look at 16 countries at a time
all_countries <- unique(both$country)
sampled <- both    %>%
   filter(country %in% sample(all_countries, 16))
   
# connected scatter plot:
sampled %>%
   ggplot(aes(y = gdp, x = exports, colour = year)) +
   facet_wrap(~country, scales = "free") +
   geom_path()

# univariate time series plots
sampled %>%
   gather(variable, value, -(iso2c:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")


# how to get rid of the groups?
unique(both[ , c("iso2c", "country")]) %>% arrange(iso2c)
# they have a number as first or second digit, or X or Z as first digit (but ZW ZA ZM legit)

both2 <- both %>%
   filter(!grepl("X.", iso2c) & !grepl("[0-9]", iso2c)) %>%
   filter(!grepl("Z.", iso2c) | iso2c %in% c("ZW", "ZA", "ZM")) %>%
   filter(!iso2c %in% c("OE", "EU"))
dim(both2)
dim(both)


#------------------------simple cross section-------------------
country_sum <- both2 %>%
   group_by(country) %>%
   summarise(gdp = mean(gdp),
             exports = mean(exports))

country_sum %>%
   ggplot(aes(x = exports, y = gdp, label = country)) +
   geom_text(size = 3) +
   geom_smooth(method = "lm") +
   scale_x_log10() +
   scale_y_log10()




#-------------------time series---------------------
# we're interested not only is there an overall relation but does it change over time?

# first we want to know the value of exports at the beginning of each series
first_values <- both2 %>%
   group_by(country) %>%
   filter(year == min(year)) %>%
   ungroup() %>%
   rename(exports_starter = exports,
          first_year = year) %>%
   select(iso2c, exports_starter, first_year)

# all those individual time series have problem of non-stationarity and of course autocorrelation
# so when we merge with the starting values of exports we also calculate first
# differences of logarithms
both3 <- both2 %>%
   left_join(first_values, by = "iso2c") %>%
   arrange(country, year) %>%
   group_by(country) %>%
   mutate(exports_g = c(NA, diff(log(exports))),
          gdp_g = c(NA, diff(log(gdp)))) %>%
   ungroup()


all_countries2 <- unique(both3$country)

both3 %>%
   filter(country %in% sample(all_countries2, 15)) %>%
   select(country, year, exports_g, gdp_g) %>%
   gather(variable, value, -(country:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")

