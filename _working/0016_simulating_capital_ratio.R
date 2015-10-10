library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(tidyr)
library(mbie)
#======================exogenous saving rate and growth in production==========
duration <- 1000
economy <- data.frame(
   year = 1:duration,
   capital = 1000,
   production = 1000,
   saving_rate = 0.12 + arima.sim(model = list(ar = 0.9), n = duration) / 60,
   growth = 1.02 + arima.sim(model = list(ar = 0.9), n = duration) / 70
   )


for(i in 2:duration){
   economy[i, "capital"] <- economy[i - 1, "capital"] + 
      economy[i, "saving_rate"] * economy[i - 1, "production"]
   
   economy[i, "production"] <- economy[i - 1, "production"] * economy[i, "growth"]
   
}

# capital expressed as number of years of production:
economy$CapIncRatio <- with(economy, capital / production)

p1 <- economy %>%
   mutate(LogProduction = log10(production)) %>%
   select(year, CapIncRatio, LogProduction) %>%
   gather("variable", "value", -year) %>%
   mutate(variable = gsub("CapIncRatio", "Capital as a proportion of a year's production", variable),
          variable = gsub("LogProduction", "Production (logarithm)", variable)) %>%
   ggplot(aes(x = year, y = value)) +
   facet_wrap(~variable, ncol = 1, scales = "free_y") +
   geom_line()

p2 <- ggplot(economy, aes(x = year, y = saving_rate)) +
   geom_line() +
   scale_y_continuous("Savings as a percent of last year's production", label = percent)

p3 <- ggplot(economy, aes(x = year, y = (growth - 1))) +
   geom_line() +
   scale_y_continuous("Growth in production", label = percent)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(p1, vp = vplayout(1:2, 1))
print(p3, vp = vplayout(1, 2))
print(p2, vp = vplayout(2, 2))
