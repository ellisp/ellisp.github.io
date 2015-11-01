library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(tidyr)
library(mbie)
#=================helper function============
draw_plots <- function(economy){
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
   }



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

draw_plots(economy)


#===================more complex endogenous model================
duration <- 1000
economy <- data.frame(
   year = 1:duration,
   capital = 1000,
   labour = 1000,
   production = 1000,
   saving_rate = 0.12,
   growth = 1.02,
   labour_growth = 1.01 + arima.sim(model = list(ar = 0.9), n = duration) / 200,
   randomness1 = 1 + arima.sim(model = list(ar = 0.9), n = duration) / 300, # productivity
   randomness2 = arima.sim(model = list(ar = 0.9), n = duration) / 100,     # savings
   war = arima.sim(model = list(ar = 0.93), n = duration)            
   )
war_threshhold <- quantile(economy$war, 0.03)

for(i in 2:duration){
   economy[i, "saving_rate"] <- economy[1, "saving_rate"] +
      (economy[i - 1, "growth"] - 1.02) * rnorm(1, 3, 0.2) +
      economy[i, "randomness2"]
   
   economy[i, "capital"] <- economy[i - 1, "capital"] + 
      economy[i, "saving_rate"] * economy[i - 1, "production"]
   economy[i, "labour"] <- economy[i - 1, "labour"] * 
      economy[i, "labour_growth"]
   
   if(economy[i, "war"] < war_threshhold){
      economy[i, "capital"] <- economy[i, "capital"] * runif(1, 0.75, 0.95)
      economy[i, "labour"] <- economy[i, "labour"] * runif(1, 0.90, 0.95)
   }
   
   K <- economy[i, "capital"] # for ease of use
   L <- economy[i, "labour"]
   Y <- 1.95 * K ^ 0.3 * L ^ 0.6 * economy[i, "randomness1"]
   economy[i, "production"] <- Y
   economy[i, "growth"] <- Y / economy[i - 1, "production"] 
   
}

draw_plots(economy)
head(economy, 10)

economy %>%
   mutate(lag_growth = c(NA, growth[-duration])) %>%
   filter(!is.na(lag_growth)) %>%
   ggplot(aes(x = lag_growth, y = saving_rate)) + geom_path() + geom_smooth(method = "lm")

