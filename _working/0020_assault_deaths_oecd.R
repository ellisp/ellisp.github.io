library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(rsdmx)    # for importing OECD data
library(ISOcodes) # for merging country codes with names
library(rvest)    # for screen scraping from the Vatican
library(gridExtra)

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_grey(base_family = "myfont"))



#----------Import and mung data---------------
# load deaths by assault from OECD.Stat
if(!exists("viol_sdmx")){
   myUrl <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/HEALTH_STAT/CICDHOCD.TXCMFETF+TXCMHOTH+TXCMILTX.AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+NMEC+BRA+CHN+COL+CRI+IND+IDN+LVA+LTU+RUS+ZAF/all?startTime=1960&endTime=2014"
   dataset <- readSDMX(myUrl) # takes about 30 seconds on a slow hotel internet connection
   viol_sdmx <- as.data.frame(dataset) # takes about 10 seconds on an i5
}


# load Country codes from ISOcodes R package
data("ISO_3166_1")

# mung:
viol <- viol_sdmx %>%
   # match country codes to country names:
   left_join(ISO_3166_1[ , c("Alpha_3", "Name")], by = c("COU" = "Alpha_3")) %>%
   # more friendly names:
   rename(Country = Name,
          Year = obsTime,
          Value = obsValue) %>%
   # limit to columns we want:
   select(UNIT, Country, Year, Value) %>%
   # make graphics-friendly versions of Unit and Year variables:
   mutate(Unit = ifelse(UNIT == "TXCMILTX", 
                        "Deaths per 100 000 population", "Deaths per 100 000 males"),
          Unit = ifelse(UNIT == "TXCMFETF", "Deaths per 100 000 females", Unit),
          Unit = factor(Unit, levels = unique(Unit)[c(2, 3, 1)]),
          Year = as.numeric(Year)) %>%
   # not enough data for Turkey to be useful so we knock it out:
   filter(Country != "Turkey")


# create country x Unit summaries   
viol_sum <- viol %>%
   filter(Year > 1990) %>%
   group_by(Country, Unit) %>%
   summarise(Value = mean(Value)) %>%
   ungroup()

# create country totals (for ordering in charts)
totals <- viol_sum %>%
   group_by(Country) %>%
   summarise(Value = mean(Value)) %>%
   arrange(Value)

# create wider version, with one column per variable:
viol_spread <- viol_sum %>%
   mutate(Unit = as.character(Unit),
          Unit = ifelse(grepl("female", Unit), "Female", Unit),
          Unit = ifelse(grepl(" males", Unit), "Male", Unit),
          Unit = ifelse(grepl("population", Unit), "Population", Unit)) %>%
   spread(Unit, Value) %>%
   mutate(Country = factor(Country, levels = totals$Country))

#------------------Graphics---------------------


p1 <- viol_sum %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   mutate(Label = ifelse(grepl("population", Unit), as.character(Country), "|")) %>%
   ggplot(aes(x = Value, y = Country)) +
   geom_segment(data = viol_spread, aes(y = Country, yend = Country, x = Male, xend = Female),
                colour = "white", size = 3) +
   geom_text(size = 4, aes(label = Label, colour = Unit), alpha = 0.8,
             family = "myfont") +
   labs(y = "") +
   scale_x_log10(("Deaths per 100,000 (logarithmic scale)")) +
   theme(legend.position = "bottom") +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   labs(colour = "") +
   ggtitle("Mean annual deaths from assault 1990 to 2013") 


svg("../img/0020-assault-average.svg", 10, 10)
print(p1)
dev.off()

png("../img/0020-assault-average.png", 1000, 1000, res = 100)
print(p1)
dev.off()



svg("../img/0020-deaths-trends.svg", 10, 10)
print(
   viol %>%
      #filter(!Country %in% c("Colombia", "Russia", "Mexico", "Estonia", "Brazil", "Lithuania", "Latvia", "South Africa", "Chile", "Costa Rica", "United States")) %>%
      mutate(Country = factor(Country, levels = totals$Country)) %>%
      ggplot(aes(x = Year, y = Value, colour = Unit)) +
      facet_wrap(~Country, scales = "free_y", ncol = 5) +
      #facet_wrap(~Country) +
      geom_smooth(se = FALSE, method = "loess") +
      geom_point(alpha = 0.8, size = 1) +
      scale_colour_manual("", values = c("red", "grey10", "blue")) +
      theme(legend.position = "bottom") +
      labs(y = "Deaths per 100,000 per year - note changing vertical scale", 
           title = "Deaths from assault", x = "")
)
dev.off()


#--------------gender digression------------
viol_gender <- viol %>%
   filter(!grepl("population", Unit)) %>%
   select(UNIT, Value, Year, Country) %>%
   spread(UNIT, Value) %>% 
   mutate(ratio = TXCMHOTH / TXCMFETF)  %>%
   group_by(Country) %>%
   summarise(ratio = mean(ratio, tr = 0.2)) %>%
   arrange(ratio) %>%
   # knock out Luxembourg and Iceland, too many NAs:
   filter(!is.na(ratio)) %>%
   mutate(Country = factor(Country, levels = Country))

svg("../img/0020-gender-ratios.svg", 6, 10)
print(
   viol_gender %>%
   ggplot(aes(x = ratio, y = Country)) +
   geom_point() +
   labs(x = "Trimmed mean annual ratio of male to female rates\nof death from assault over whole period", y = "")
   )
dev.off()

# download catholic proportion data
cath_page <- read_html("http://www.catholic-hierarchy.org/country/sc1.html")
cath_data <- html_table(cath_page)[[1]] 

names(cath_data) <- gsub(" ", "_", names(cath_data))

lat_american <- c("Colombia", "Brazil", "Mexico", "Chile", "Costa Rica")
american <- c(lat_american, c("Canada", "United States"))


cath_data <- cath_data %>%
   mutate(Country = gsub("M.+xico", "Mexico", Country),
          Country = ifelse(Country == "USA", "United States", Country),
          Country = ifelse(Country == "Great Britain", "United Kingdom", Country),
          Country = ifelse(Country == "Korea (South)" , "Korea, Republic of", Country)) %>%
   select(Country, Percent_Catholic) %>%
   mutate(Percent_Catholic = as.numeric(gsub("%", "", Percent_Catholic)),
          Continent1 = ifelse(Country %in% american, "Americas", "Other"),
          Continent2 = ifelse(Country %in% lat_american, "Latin American", "Other"))

viol_gender_cath <- viol_gender %>%
   left_join(cath_data, by = "Country")

cath1 <-    viol_gender_cath %>%
   ggplot(aes(x = Percent_Catholic, y = ratio, label = Country))  +
   geom_smooth(method = "lm") +
   geom_text(family = "myfont", size = 3, alpha = 0.8) +
   labs(x = "Percentage of country reported to be Catholic", 
        y = "Ratio of female to male\nassault death rates") +
      xlim(-5, 110)

cath2 <- cath1 + aes(colour = Continent1)
cath3 <- cath1 + aes(colour = Continent2)

svg("../img/0020-gender-catholic.svg", 8, 8)
grid.arrange(cath1, cath2, cath3)
dev.off()

mod1 <- lm(ratio ~ Percent_Catholic, data = viol_gender_cath)
mod2 <- lm(ratio ~ Percent_Catholic * Continent1, data = viol_gender_cath)
mod3 <- lm(ratio ~ Percent_Catholic * Continent2, data = viol_gender_cath)

summary(mod1)
summary(mod2)
summary(mod3)
