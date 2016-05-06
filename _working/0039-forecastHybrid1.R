library(showtext)
library(ggplot2)
library(scales)
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


install.packages("forecastHybrid")
library(forecastHybrid)


mod1 <- hybridModel(AirPassengers)
fc1 <- forecast(mod1)
fc1
class(fc1)

svg("../img/0039-airpassengers1.svg", 6, 6)
par(family = "myfont")
par(mar = c(5, 4, 8, 2))
plot(fc1, ylab = "International airline passengers")
dev.off()

png("../img/0039-airpassengers1.png", 600, 600, res = 100)
par(family = "myfont")
par(mar = c(5, 4, 8, 2))
plot(fc1, ylab = "International airline passengers")
dev.off()

#----------custom model---------
mod2 <- hybridModel(AirPassengers, models = "ae",
                    weights = "insample.errors")
fc2 <- forecast(mod2)

svg("../img/0039-airpassengers2.svg", 6, 5)
par(family = "myfont")
par(mar = c(5, 4, 4, 2))
plot(fc2, ylab = "International airline passengers")
dev.off()


#------------unemp example-----------------
# adapted from http://biba.etsii.upm.es/web/tiki-view_blog_post.php?postId=26 
wi<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/WIUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
us<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/UNRATE.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
il<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/ILUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
mi<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/MIUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
mn<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/MNUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))
io<-read.csv('http://www.quandl.com/api/v1/datasets/FRED/IAUR.csv?&auth_token=gigXwpxd6Ex91cjgz1B7&trim_start=1976-01-01&trim_end=2013-04-01&sort_order=desc', colClasses=c('Date'='Date'))


#merging the data into one dataframe
unemp <- merge(wi, io, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io')
unemp <- merge(unemp, mi, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi')
unemp <- merge(unemp, mn, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn')
unemp <- merge(unemp, us, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn', 'us')
unemp <- merge(unemp, il, by = 'DATE'); colnames(unemp) <- c('DATE', 'wi', 'io', 'mi', 'mn', 'us', 'il')

# break into historical and forecast periods
n <- nrow(unemp)
unemp_hist <- unemp[1:(n - 12), ]
unemp_fc <- unemp[(n - 11):448,]
unemp_hist_ts <- ts(unemp_hist$wi, start = c(1976, 1), frequency = 12)

# fit hybrid model, passing arguments to auto.arima with a.args and to
# nnetar with n.args
mod4 <- hybridModel(unemp_hist_ts, models = "aen", 
                    a.args = list(xreg = unemp_hist[ , 3:7]),
                    n.args = list(xreg = unemp_hist[ , 3:7]))

# fit the forecast - note that you have to supply the future xreg values,
# but you only need to do it once (both nnetar and auto.arima use the same 
# ones):                    
fc4 <- forecast(mod4, h = 12, xreg = unemp_fc[ , 3:7])

svg("../img/0039-unemployment.svg", 7, 5)
par(family = "myfont")
par(mar = c(5, 4, 5, 2))
plot(fc4, ylab = "Unemployment in Wisonsin", 
     sub = "Forecasts based on unemployment in\nUSA, Iowa, Michigan, Minnesota, and Illinois")
dev.off()


png("../img/0039-unemployment.png", 700, 500, res = 100)
par(family = "myfont")
par(mar = c(5, 4, 5, 2))
plot(fc4, ylab = "Unemployment in Wisonsin", 
     sub = "Forecasts based on unemployment in\nUSA, Iowa, Michigan, Minnesota, and Illinois")
dev.off()


