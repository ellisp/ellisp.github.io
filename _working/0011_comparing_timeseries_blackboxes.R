library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(forecast)
library(mgcv)
library(showtext) # for fonts

font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

bb1 <- function(n = 1000){
   # ARIMA(2, 1, 1) with drift
   cumsum(arima.sim(model = list(ar = c(0.5, -0.2), ma = 0.3), n = n) + 0.10)
}

bb2 <- function(n = 1000){
   # ARIMA(1, 1, 2) with drift
   cumsum(arima.sim(model = list(ar = c(0.5), ma = c(0.3, -0.1)), n = n) + 0.04)
}

plot(bb1())
plot(bb2())


#--------begin simulation---------

# turn this into a function that takes two data generating functions, reps and n and compares them

the_data <- data_frame(blackbox1 = numeric(), blackbox2 = numeric(), trial = numeric(), time = numeric())

reps <- 100
n <- 500
set.seed(123) # for reproducibility
for(i in 1:reps){
   tmp <- data_frame(blackbox1 = bb1(n), blackbox2 = bb2(n), trial = i, time = 1:n)
   the_data <- rbind(the_data, tmp)
}

the_data_m <- the_data %>%
   gather(source, value, -trial, -time) %>%
   mutate(trial = as.character(trial),
          sourcetrial = paste(source, trial))

the_data_m %>%
   ggplot(aes(x = time, y = value, colour = trial)) +
      facet_wrap(~source) +
      geom_line(alpha = 0.3) +
   geom_smooth(se = FALSE)

#-----------------------comparison-----------


difference <- the_data_m %>%
   left_join(
      the_data %>%
         group_by(time) %>%
         summarise(centre_1 = mean(blackbox1),
                   sd_1 = sd(blackbox1)),
      by = "time"
      ) %>%
   group_by(trial, source)%>%
   summarise(
      meandiff = mean(abs(value - centre_1) / sd_1)) # this biases down those from blackbox1.  How to correct?

summary(difference)
ggplot(difference, aes(x=meandiff)) +geom_density()
model <- lm(meandiff ~ source, data = difference)
par(mfrow=c(2, 2))
plot(model)
anova(model)
summary(model)



