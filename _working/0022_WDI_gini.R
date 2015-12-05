library(WDI)
library(ggplot2)
library(scales)
library(dplyr)

search_results <- WDIsearch("")
good_series <- search_results[0, ]

# create an empty object to hold all the data.  Each data frame is an element
# in the wdi[[]] list.
wdi <- list()

# this takes 10 hours to run as you're downloading *every* dataset in the WDI
for(i in 1:nrow(search_results)){
   tmp <- WDI(indicator = search_results[i, 1], end = 2015, start = 1960)
   if(!is.null(tmp) && nrow(tmp) > 0){
      good_series <- rbind(good_series, search_results[i, ])
      wdi[[nrow(good_series)]] <- tmp
   } else {
      message(search_results[i, 1], " | ", search_results[i, 2]," is bad, skipping this dataset")
   }
   cat(i, " ")
}

# we might want meaningful names for each element of the wdi[[]] list, so might
# as well give them just in case.
names(wdi) <- good_series[ , 1]

save(wdi, file = "../data/wdi.rda")
save(good_series, file = "../data/good_series.rda")



chosen <- sample(1:nrow(good_series), 1)
series <- good_series[chosen ,1]
x <- WDI(indicator = series, end = 2015, start = 1960)


names(x)[names(x) == series] <- "value"

selected_countries <- x %>%
   group_by(country) %>%
   summarize(n = sum(!is.na(value)),
             ave = mean(value, na.rm = TRUE, tr = 0.2)) %>%
   filter(n > 5) %>%
   arrange(ave)

print(
   x %>%
   filter(country %in% selected_countries$country) %>%
   filter(!is.na(value)) %>%
   mutate(country = factor(country, levels = selected_countries$country)) %>%
   ggplot(aes(x = year, y = value, colour = country)) +
   geom_point() +
   geom_smooth(se = FALSE, span = 2, method = "loess") +
   facet_wrap(~country) +
   theme(legend.position = "none") +
   ggtitle(good_series[chosen, 2])
)




