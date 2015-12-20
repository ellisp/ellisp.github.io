library(WDI)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(nlme)

# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


#-------------------data imports, first explore, and clean-----------------
if(!exists("exports")){
   # Exports of goods and services (% of GDP)
   exports <- WDI(indicator = "NE.EXP.GNFS.ZS", end = 2015, start = 1950)
   # GDP per capita (constant 2000 US$)
   gdp <- WDI(indicator = "NY.GDP.PCAP.KD", end = 2015, start = 1950)
}

both <- merge(exports, gdp) %>%
   rename(exports = NE.EXP.GNFS.ZS,
          gdp = NY.GDP.PCAP.KD) %>%
   # removing any year-country combos missing eith gdp or exports:
   filter(!(is.na(exports) | is.na(gdp))) %>%
   arrange(country, year)

set.seed(234)
# let's look at 12 countries at a time
all_countries <- unique(both$country)
sampled <- both    %>%
   filter(country %in% sample(all_countries, 12))
   
# connected scatter plot:
p1 <- sampled %>%
   ggplot(aes(y = gdp, x = exports, colour = year)) +
   facet_wrap(~country, scales = "free") +
   geom_path() +
   labs(x = "Exports as a percentage of GDP") +
   scale_y_continuous("GDP per capita, constant 2000 US dollars", label = dollar) +
   ggtitle("Exports and GDP over time, selected countries")

# univariate time series plots
p2 <- sampled %>%
   gather(variable, value, -(iso2c:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")


# how to get rid of the country groups?
unique(both[ , c("iso2c", "country")]) %>% arrange(iso2c)
# they have a number as first or second digit, or X or Z as first digit (but ZW ZA ZM legit)

both2 <- both %>%
   filter(!grepl("X.", iso2c) & !grepl("[0-9]", iso2c)) %>%
   filter(!grepl("Z.", iso2c) | iso2c %in% c("ZW", "ZA", "ZM")) %>%
   filter(!iso2c %in% c("OE", "EU"))

#------------------------simple cross section-------------------
country_sum <- both2 %>%
   group_by(country) %>%
   filter(year == max(year))

p3 <- country_sum %>%
   ggplot(aes(x = exports / 100, y = gdp, label = country)) +
   geom_text(size = 3) +
   geom_smooth(method = "lm") +
   scale_y_log10(label = dollar) +
   scale_x_log10(label = percent) +
   labs(x = "Exports as a percentage of GDP",
        y = "GDP per capita, constant 2000 US dollars",
        title = "Cross section of exports and GDP, latest years when data are present")




#-------------------time series---------------------
# we're interested not only is there an overall relation but does it change over time?

# first we want to know the value of exports at the beginning of each series
first_values <- both2 %>%
   group_by(country) %>%
   filter(year == min(year)) %>%
   ungroup() %>%
   mutate(exports_starter = log(exports),
          first_year = year) %>%
   select(iso2c, exports_starter, first_year)

p4 <- ggplot(first_values, aes(x = exports_starter)) + geom_density()

# all those individual time series have problem of non-stationarity and of course autocorrelation
# so when we merge with the starting values of exports we also calculate first
# differences of logarithms
both3 <- both2 %>%
   left_join(first_values, by = "iso2c") %>%
   arrange(country, year) %>%
   group_by(country) %>%
   mutate(exports_g = c(NA, diff(log(exports))),
          gdp_g = c(NA, diff(log(gdp))),
          exports_lag = lag(exports_g)) %>%
   ungroup() %>%
   filter(!is.na(gdp_g)) %>%
   filter(!is.na(exports_lag))

all_countries2 <- unique(both3$country)

set.seed(123)
sampled <- both3 %>%
   filter(country %in% sample(all_countries2, 12)) 

p5 <- sampled %>%
   select(country, year, exports_g, gdp_g) %>%
   gather(variable, value, -(country:year)) %>%
   ggplot(aes(x = year, y = value)) +
   geom_line() +
   facet_wrap(~ country + variable, scales = "free_y")

p6 <- sampled %>%
   select(country, year, exports_g, gdp_g) %>%
   ggplot(aes(x = exports_g, y = gdp_g, colour = year)) +
   geom_smooth(colour = "red", method = "lm") +
   geom_path() +
   geom_point() +
   facet_wrap(~ country, scales = "free") +
   labs(x = "change in logarithm of exports share of GDP",
        y = "change in logarithm of GDP per capita, constant 2000 USD",
        title = "Changing importance of exports and GDP growth, selected countries")



# we start with some exploratory fixed effects models
# fixed effects model
model0 <- lm(gdp_g ~ country + exports_g, data = both3)
model1 <- lm(gdp_g ~ country * exports_g, data = both3)
model2 <- lm(gdp_g ~ country * (exports_g + exports_lag), data = both3)
summary(model0)
summary(model1)
anova(model0)
anova(model1)
anova(model2)

# then a more appropriate model that allows country-level variation to be a
# random effect, and auto-correlation of the 
model4 <- lme(fixed = gdp_g ~ ordered(first_year) + exports_g + exports_lag + exports_starter + year,
              random = ~ exports_g + exports_lag + year | country, 
              correlation = corAR1(form = ~ year | country),
              data = both3)
round(tail(summary(model4)$tTable, 4), 3)

# p values are less "significant" than in the fixed effects models:
anova(model4)

cf <- coef(model4)

# let's look at just the last four coefficients ie exclude the nuisance of first_year
cf_df <- cf[, (ncol(cf) - 3):ncol(cf)] %>%
   as.data.frame() %>%
   mutate(country = rownames(cf),
          combined = exports_lag + exports_g) %>%
   arrange(combined)


p7 <- cf_df %>%
   gather(variable, value, -country, -exports_starter) %>%
   mutate(variable = gsub("combined", "exports_g + exports_lag", variable, fixed = TRUE)) %>%
   ggplot(aes(x = value)) +
   facet_wrap(~variable, scales = "free") +
   geom_density() +
   geom_rug()

#----------------------save charts--------------

svg("../img/0023-p1.svg", 9, 7)
print(p1)
dev.off()

png("../img/0023-p1.png", 900, 700, res = 100)
print(p1)
dev.off()


svg("../img/0023-p2.svg", 9, 7)
print(p2)
dev.off()

svg("../img/0023-p3.svg", 8, 7)
print(p3)
dev.off()

png("../img/0023-p3.png", 800, 700, res = 100)
print(p3)
dev.off()


svg("../img/0023-p4.svg", 5, 4)
print(p4)
dev.off()

svg("../img/0023-p5.svg", 9, 7)
print(p5)
dev.off()

svg("../img/0023-p6.svg", 9, 7)
print(p6)
dev.off()

svg("../img/0023-p7.svg", 7, 6)
print(p7)
dev.off()
