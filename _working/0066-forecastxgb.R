
library(forecastxgb)
library(dplyr)

leg <- "f: Theta; forecast::thetaf\na: ARIMA; forecast::auto.arima
n: Neural network; forecast::nnetar\nx: Extreme gradient boosting; forecastxgb::xgbts"

p1 <- Tcomp_results %>%
   ggplot(aes(x = model, y =  MASE, colour = Frequency, label = model)) +
   geom_text(size = 4) +
   geom_line(aes(x = as.numeric(model)), alpha = 0.25) +
   scale_y_continuous("Mean scaled absolute error\n(smaller numbers are better)") +
   annotate("text", x = 2, y = 3.5, label = leg, hjust = 0) +
   ggtitle("Average error of four different timeseries forecasting methods\n2010 Tourism Forecasting Competition data") +
   labs(x = "Model, or ensemble of models\n(further to the left means better overall performance)")

svg("../img/0066-tcomp.svg", 8, 6)
print(p1)
dev.off()

png("../img/0066-tcomp.png", 800, 600, res = 100)
print(p1)
dev.off()