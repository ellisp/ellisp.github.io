library(showtext) # for fonts

font.add.google("Poppins", "myfont")
showtext.auto()

set.seed(123)
arma_1 <- arima.sim(model = list(ar = c(0.5, -0.1), ma = 0.3), n = 1000)

#---------------a linear combination of a stationary time series has the same acf-------
series1 <- ts(arma_1)
series2 <- ts(arma_1 * 3 + 2)

svg("../img/0010-ts-acf-1.svg", 6, 7)
par(mfrow = c(3, 2), family = "myfont", bty = "l")
plot(series1, main = "Original series")
plot(series2, main = "Original * 3 + 2")
acf(series1, main = "Autocorrelation function")
acf(series2, main = "Autocorrelation function")
acf(series1, type = "partial", main = "Partial autocorrelation function")
acf(series2, type = "partial", main = "Partial autocorrelation function")
dev.off()

#-------different instances of the same non-stationary time series----------------
# will have similar ACFs, as will the differenced versions, but can go in wildly different
# directions

set.seed(123)
arma_1 <- arima.sim(model = list(ar = c(0.5, -0.1), ma = 0.3), n = 1000)
set.seed(234)
arma_2 <- arima.sim(model = list(ar = c(0.5, -0.1), ma = 0.3), n = 1000)
series3 <- ts(cumsum(cumsum(arma_1)))
series4 <- ts(cumsum(cumsum(arma_2)))


svg("../img/0010-ts-acf-2.svg", 6, 7)
par(mfrow = c(3, 2), family = "myfont", bty = "l")
plot(series3, main = "First instance of ARIMA(2, 2, 1)")
plot(series4, main = "Second instance of same ARIMA(2, 2, 1)")
acf(series3, main = "Autocorrelation function")
acf(series4, main = "Autocorrelation function")
acf(series3, type = "partial", main = "Partial autocorrelation function")
acf(series4, type = "partial", main = "Partial autocorrelation function")
dev.off()

svg("../img/0010-ts-acf-3.svg", 6, 7)
par(mfrow = c(3, 2), family = "myfont", bty = "l")
plot(series3, main = "First instance of ARIMA(2, 2, 1)")
plot(series4, main = "Second instance of same ARIMA(2, 2, 1)")
acf(diff(diff(series3)), main = "Autocorrelation of second differences")
acf(diff(diff(series4)), main = "Autocorrelation of second differences")
acf(diff(diff(series3)), type = "partial", main = "Partial autocorrelation\nof second differences")
acf(diff(diff(series4)), type = "partial", main = "Partial autocorrelation\nof second differences")
dev.off()