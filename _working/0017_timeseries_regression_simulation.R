library(dplyr)
library(tidyr)
library(ggplot2)
library(Cairo)

n <- 1000

df1 <- data.frame(x = scale(cumsum(arima.sim(list(ar = 0.5), n = n)))) %>%
   mutate(y = 1 + 0.3 * x + scale(arima.sim(list(ar = 0.6), n)),
          ind = 1:n,
          type = "TimeSeries")


df2 <- data.frame(x = rnorm(n)) %>%
   mutate(y = 1 + 0.3 * x + rnorm(n),
          ind = 1:n,
          type = "CrossSection")


ggplot(df1, aes(x, y, colour = ind)) +
   geom_path() +
   geom_point() +
   geom_smooth(method = "lm")

df1 %>%
   gather(variable, value, -(ind:type)) %>%
   ggplot(aes(x = ind, y = value, colour = variable)) +
   geom_line()

ggplot(df2, aes(x, y, colour = ind)) +
   geom_path() +
   geom_point() +
   geom_smooth(method = "lm")


df_both <- rbind(df1, df2)

ggplot(df_both, aes(x, y, colour = ind)) +
   facet_wrap(~type, ncol = 2) +
   geom_path() +
   geom_point() +
   geom_abline(intercept = 1, slope = 0.3) +
   geom_smooth(method = "lm", se = FALSE, size = 2, colour = "red")


CairoPDF("tmp.pdf")
for(i in 1:n){
   
   print(ggplot(df_both[1:n, ], aes(x, y, colour = ind)) +
            facet_wrap(~type, ncol = 2) +
            geom_path() +
            geom_point() +
            geom_abline(intercept = 1, slope = 0.3) +
            geom_smooth(method = "lm", se = FALSE, size = 2, colour = "red"))
   
}
dev.off()
