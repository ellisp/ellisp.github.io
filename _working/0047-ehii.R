
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(openxlsx)
library(ggseas)


download.file("http://utip.lbj.utexas.edu/data/EHII-UPDATED-10-30-2013.xlsx",
              destfile = "ehii.xlsx", mode = "wb")

ehii <- read.xlsx("ehii.xlsx")[ , -1] # don't need the first column
    
ehii_tidy <- ehii %>%
   gather(Year, Gini, -Country, -Code) %>%
   mutate(Year = as.numeric(Year))

ggplot(ehii_tidy, aes(x = Year, y = Gini, colour = Country)) +
   geom_line() +
   theme(legend.position = "none")


full_countries <- ehii_tidy %>%
   filter(Year = min(Year) & !is.na(Gini))

final_result <- ehii_tidy %>%
   filter(Country %in% full_countries$Country & !is.na(Gini)) %>%
   group_by(Country) %>%
   mutate(Gini_index = Gini / Gini[1] * 100) %>%
   filter(Year == max(Year)) %>%
   mutate(Year = Year + 1,
          label = paste(Code, round(Gini)))


ehii_tidy %>%
   filter(Country %in% full_countries$Country) %>%
   ggplot(aes(x = Year, y = Gini, colour = Country)) +
   stat_index(index.ref = 1, alpha = 0.3) +
   theme(legend.position = "none") +
   geom_text(data = final_result, aes(label = label, y = Gini_index)) +
   ggtitle("Biggest increases in inequality since 1963",
           subtitle = "(for countries with data from 1963)") +
   labs(y = "Index of Gini coefficient, set to be 100 in 1963",
        x = "", caption = "UTIP Estimated Household Income Inequality dataset") +
   xlim(1960, 2012) +
   annotate("text", x = 1970, y = 150, 
            label = "Country codes appear at their final data point; 
numbers are the latest available Gini coefficient")


