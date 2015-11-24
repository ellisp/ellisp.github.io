library(ggplot2)
library(scales)
library(grid)
library(dplyr)
library(tidyr)
library(showtext) # for fonts
library(rsdmx)
library(ISOcodes)

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
           title = "Deaths from violent assault", x = "")
)
dev.off()



p1 <- viol_sum %>%
   mutate(Country = factor(Country, levels = totals$Country)) %>%
   mutate(Label = ifelse(grepl("population", Unit), as.character(Country), "|")) %>%
   ggplot(aes(x = Value, y = Country)) +
   geom_segment(data = viol_spread, aes(y = Country, yend = Country, x = Male, xend = Female),
                colour = "white", size = 3) +
   geom_text(size = 4, aes(label = Label, colour = Unit), alpha = 0.8,
             gp = gpar(fontfamily = "myfont")) +
   labs(y = "") +
   scale_x_log10(("Deaths per 100,000 (logarithmic scale)")) +
   theme(legend.position = "bottom") +
   scale_colour_manual("", values = c("red", "grey10", "blue")) +
   labs(colour = "") +
   ggtitle("Mean annual deaths from violent assault 1990 to 2013") 
      

svg("../img/0020-assault-average.svg", 10, 10)
   print(p1)
dev.off()

png("../img/0020-assault-average.png", 1000, 1000, res = 100)
   print(p1)
dev.off()

svg("../img/0020-gender-ratios.svg", 6, 10)
print(viol %>%
   filter(!grepl("population", Unit)) %>%
   select(UNIT, Value, Year, Country) %>%
   spread(UNIT, Value) %>% 
   mutate(ratio = TXCMHOTH / TXCMFETF)  %>%
   group_by(Country) %>%
   summarise(ratio = mean(ratio, tr = 0.2)) %>%
   arrange(ratio) %>%
   # knock out Luxembourg and Iceland, too many NAs:
   filter(!is.na(ratio)) %>%
   mutate(Country = factor(Country, levels = Country)) %>%
   ggplot(aes(x = ratio, y = Country)) +
   geom_point() +
   labs(x = "Trimmed mean annual ratio of male to female rates\nof violent death over whole period", y = ""))
dev.off()

