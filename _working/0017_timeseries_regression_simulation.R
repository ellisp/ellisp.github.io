library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(showtext)


setwd(old_dir) # delete for final version

font.add.google("Poppins", "myfont")
showtext.auto()

theme_set(theme_light(base_family = "myfont"))

n <- 200
popn <- n * 10

set.seed(123)

df1 <- data.frame(x = rnorm(popn)) %>%
   mutate(y = 1 + 0.3 * x + scale(arima.sim(list(ar = 0.99), popn)),
          ind = 1:popn,
          type = "TimeSeries")
df1 <- df1[1:n, ]


df2 <- data.frame(x = rnorm(n)) %>%
   mutate(y = 1 + 0.3 * x + rnorm(n),
          ind = 1:n,
          type = "CrossSection")


svg("../img/0017-original.svg", 6, 4)
print(
   df1 %>%
   ggplot(aes(x = ind, y = y)) +
   geom_line() +
   labs(x = "Time") +
   ggtitle("Simulated response variable from linear model\nwith time series random element")
)
dev.off()


df_both <- rbind(df1, df2)


old_dir <- setwd("_output/0017-timeseries-regression")

for(i in 5:n){

   # I name the images i + 1000 so alphabetical order is also numeric
   png(paste0(i + 1000, ".png"), 600, 600, res = 100)
   
   
   
   
   
   df1_tmp <- df1[1:i, ]
   df2_tmp <- df2[1:i, ]
   
   residuals1 <- data.frame(res = residuals(lm(y ~ x, data = df1_tmp)), 
                            ind = 1:i, 
                            type = "TimeSeries")
   residuals2 <- data.frame(res = residuals(lm(y ~ x, data = df2_tmp)), 
                            ind = 1:i, 
                            type = "CrossSection")

      

   
   p1 <- ggplot(df_both[c(1:i, (n + 1) : (n + i)), ], aes(x, y, colour = ind)) +
      facet_wrap(~type, ncol = 2) +
      geom_path() +
      geom_point() +
      geom_abline(intercept = 1, slope = 0.3) +
      geom_smooth(method = "lm", se = FALSE, size = 2, colour = "red") +
      theme(legend.position = "none") +
      xlim(range(df_both$x)) +
      ylim(range(df_both$y)) +
      ggtitle(paste("Connected scatterplot showing regression on first", i, "points")) +
      
   
   
   p2 <- residuals1 %>%
      rbind(residuals2) %>%
      mutate(type = factor(type, levels = c("CrossSection", "TimeSeries"))) %>%
      ggplot(aes(x = ind, y = res)) +
      scale_x_continuous(limits = c(0, n)) +
      facet_wrap(~type) +
      geom_line() +
      geom_point() +
      ggtitle("Residuals from regression so far") +
      labs(x = "Time", y = "Residuals")
   
   grid.arrange(p1, p2)
   
   dev.off()
   
}


# combine them into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 10 *.png "timeseries.gif"')

# move the asset over to where needed for the blog
file.copy("timeseries.gif", "../img/0017-timeseries.gif", overwrite = TRUE)

setwd(old_dir)
