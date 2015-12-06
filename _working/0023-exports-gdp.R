library(WDI)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(GGally)   # for ggplot pairs plot
library(showtext) # for fonts
library(nlme)

# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


#-------------------data imports, first explore, and clean-----------------
if(!exists("exports")){
   # Exports of goods and services (% of GDP)
   exports <- WDI(indicator = "NE.EXP.GNFS.ZS", end = 2015, start = 1960)
   # GDP per capita (constant 2000 US$)
   gdp <- WDI(indicator = "NY.GDP.PCAP.KD", end = 2015, start = 1960)
}

both <- merge(exports, gdp) %>%
   rename(exports = NE.EXP.GNFS.ZS,
          gdp = NY.GDP.PCAP.KD) %>%
   # removing any year-country combos missing eith gdp or exports:
   filter(!(is.na(exports) | is.na(gdp))) %>%
   arrange(country, year)

# let's look at 16 countries at a time
all_countries <- unique(both$country)
sampled <- both    %>%
   filter(country %in% sample(all_countries, 12))
   
# connected scatter plot:
p1 <- sampled %>%
   ggplot(aes(y = gdp, x = exports, colour = year)) +
   facet_wrap(~country, scales = "free") +
   geom_path()

# univariate time series plots
p2 <- sampled %>%
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

p3 <- country_sum %>%
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
   mutate(exports_starter = log(exports),
          first_year = year) %>%
   select(iso2c, exports_starter, first_year)

p4 <- ggplot(both3, aes(x = exports_starter)) + geom_density()

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

head(both3)

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
model0 <- lm(gdp_g ~ country +  exports_g, data = both3)
summary(model0)
anova(model0)
AIC(model0)

# alternative spec of fixed effects model
model1 <- lm(gdp_g ~ ordered(first_year) + exports_g + exports_starter, data = both3)
summary(model1)
anova(model1)
AIC(model1)

model2 <- lme(fixed = gdp_g ~ ordered(first_year) + exports_g + exports_starter,
              random = ~ 1 | country, 
              data = both3)

summary(model2)$tTable
anova(model2)
ACF(model2) # special ACF function for objects of class lme
AIC(model2)

model3 <- lme(fixed = gdp_g ~ ordered(first_year) + exports_g + exports_starter,
              random = ~ 1 | country, 
              correlation = corAR1(),
              data = both3)
anova(model3)
AIC(model3)

# now that we have an appropriate error structure, the p values are further from zero
# ie less significant.  We look at our four models, each one closer to what we 
# really believe is a good fit, just focusing on the parameters of interest
round(tail(summary(model0)$coefficients, 1), 3)
round(tail(summary(model1)$coefficients, 2), 3)
round(tail(summary(model2)$tTable, 2), 3)
round(tail(summary(model3)$tTable, 2), 3)

model4 <- lme(fixed = gdp_g ~ ordered(first_year) + exports_g + exports_lag + exports_starter,
              random = ~ 1 | country, 
              correlation = corAR1(),
              data = both3)

tail(round(summary(model4)$tTable, 3), 3)

#----------------------save charts--------------

svg("../img/0023-p1.svg", 9, 7)
print(p1)
dev.off()

svg("../img/0023-p2.svg", 9, 7)
print(p2)
dev.off()

svg("../img/0023-p3.svg", 8, 7)
print(p3)
dev.off()

svg("../img/0023-p4.svg", 8, 7)
print(p4)
dev.off()

svg("../img/0023-p5.svg", 9, 7)
print(p5)
dev.off()

svg("../img/0023-p6.svg", 9, 7)
print(p6)
dev.off()

