library(forecast)

set.seed(234)

n <- 80

# white noise
wn1 <- ts(rnorm(n))
plot(wn1)

# random walk - remember a random walk is a cumulative sum of an iid random variable
# such as our white noise:
rw <- ts(cumsum(wn1))
plot(rw)

# so the first difference of the random walk recovers the original white noise
plot(wn1, diff(rw))


#------------seasonality - unchanging-----------------------
# simple, unchanging seasonality over a random walk.  First quarter gets +4, second +2, etc.
seasonality1 <- rep(c(4, 2, -1, 0), (n / 4))

seas1 <- ts(rw + seasonality1, frequency = 4)
plot(seas1)
plot(decompose(seas1))

# lots of strong autocorrelations:
acf(seas1)
pacf(seas1)

# taking differences at lag 4 removes most of the seasonality (this is equivalent
# to converting it to year on year differences)
#
diffed <- diff(seas1, 4)
plot(diffed)
# the annual seasonality autocorrelation (ie at lag "1" in the plot below, which
# means 1 year ie 4 quarters) is gone, but the autocorrelation from the original
# random walk is still there:
acf(diffed)
pacf(diffed)

# taken successive differences can address this to a degree:
diffed <- diff(diff(seas1, 4), 1)
plot(diffed)
# but now some negative correlation seasonality has crept in:
pacf(diffed)

# try for yourself - does it make any difference if you do the lag 1 diff
# before the lag 4 diff?


#-----------------seasonality - itself a time series------------------
# what if the seasonality for each quarter is itself an AR(1) ie moving around 
# the original value:
seasonality2 <- ts(seasonality1 )
seasonality2[(1:(n / 4)) * 4 - 3] <- seasonality1[1] + arima.sim(n / 4, model = list(ar = c(0.1)))
seasonality2[(1:(n / 4)) * 4 - 2] <- seasonality1[2] + arima.sim(n / 4, model = list(ar = c(0.1)))
seasonality2[(1:(n / 4)) * 4 - 1] <- seasonality1[3] + arima.sim(n / 4, model = list(ar = c(0.1)))
seasonality2[(1:(n / 4)) * 4 - 0] <- seasonality1[4] + arima.sim(n / 4, model = list(ar = c(0.1)))
   
seas2 <- ts(rw + seasonality2, frequency = 4)
plot(seasonality2)
plot(seas2)

acf(seas2)
pacf(seas2)

diffed <- diff(diff(seas2, 4), 1)
plot(diffed)
pacf(diffed)

# decompose cannot identify the changing seasonality
plot(decompose(seas2))

# stl cannot identify it unless you let the s.window parameter use loess for
# seasonal extraction:
plot(stl(seas2, s.window = "periodic")) # seasonality is same each cycle
plot(stl(seas2, s.window = 4))          # seasonality changes over time, but not as much as our reality


#--------------------seasonality - a more complex time series-------------
# we perturb the seasonality with ARMA(1, 1)[4] (notice how this is specified
# for arima.sim())
seasonality3 <- ts(seasonality1) +
   arima.sim(n = n, model = list(ar = c(0, 0, 0, 0.5), ma = c(0, 0, 0, -0.5)))
plot(seasonality3)
seas3 <- ts(rw + seasonality3, frequency = 4)
plot(stl(seas3, s.window = 4))

#------------modelling with SARIMA--------------
# auto.arima is poor at recovering the original data generating process of
# the series, particularly with these small sample sizes.  But it does
# well at decompoising into a moving

library(forecast)
model2 <- auto.arima(seas2)
model2

model3 <- auto.arima(seas3)
model3


#================================X13-SEATS-ARIMA===================
# X13-SEATS-ARIMA
