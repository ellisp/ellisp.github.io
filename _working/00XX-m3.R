library(devtools)
#devtools::install_github("christophsax/seasonal", ref = "error-parsing-fixes") 


library(Mcomp)
library(dplyr)
library(tidyr)
library(seasonal)

# set path to X13-SEATS and check it's working
Sys.setenv(X13_PATH = "c:/winx13/x13as")
checkX13()

nseries <- length(M3)

# nseries <- 500 # for development

# define number of new methods
number_methods <- 7 + 4 # 7 original, 3 from Hyndman, plus X13

theta <- as.matrix(M3Forecast$THETA)[1:nseries, ]
fpro <- as.matrix(M3Forecast$ForecastPro)[1:nseries, ]
fcx <- as.matrix(M3Forecast$ForcX)[1:nseries, ]
bjauto <- as.matrix(M3Forecast$`B-J auto`)[1:nseries, ]
ab1 <- as.matrix(M3Forecast$AutoBox1)[1:nseries, ]
ab2 <- as.matrix(M3Forecast$AutoBox2)[1:nseries, ]
ab3 <- as.matrix(M3Forecast$AutoBox3)[1:nseries, ]

ets1 <- aarima <- hybrid <- seats <- x11 <- matrix(NA, nrow = nseries, ncol = 18)

for(i in 1:nseries){
   print(i)
   SkipIt <- FALSE
   ets1[i, ] <- forecast(ets(M3[[i]]$x), h = 18, PI = FALSE)$mean
   aarima[i, ] <- forecast(auto.arima(M3[[i]]$x), h = 18)$mean
   hybrid[i, ] <- 0.5 * (aarima[i, ] + ets1[i, ])
   
   if(M3[[i]]$period != "YEARLY"){
      try(m <- seas(M3[[i]]$x, forecast.maxlead = 18)) # 801 fails to converge ; 807 runs but produces no data; 
      # 1154  and a bunch aferhas singular x matrix problem because of trading days 
      #1405 fails for 'unknown reason'
   } else {
      try(m <- seas(M3[[i]]$x, regression.aictest = NULL, forecast.maxlead = 18)) # about 25% of annual series fail to start
   }
   try(seats[i, ] <- series(m, "forecast.forecasts")[1:18 , 1]      ) # sometimes generates 36 or more... forecast.maxlead obviously means something i don't understand
}


# 92 failures in first 500 series; 413 in total, various reasons
message(sum(is.na(seats[ , 1])), " series failed for SEATS, replacing them with auto.arima's results")
bad <- is.na(seats)
seats[bad] <- aarima[bad]

# Compute accuracy
mase <- mape <- smape <- matrix(NA, nrow = number_methods, ncol = nseries)
f <- matrix(NA, nrow = number_methods, ncol = 18)
for(i in 1:nseries)
{
   x <- M3[[i]]$xx
   n <- length(x)
   f[1, 1:n] <- theta[i, 1:n]
   f[2, 1:n] <- fpro[i, 1:n]
   f[3, 1:n] <- fcx[i, 1:n]
   f[4, 1:n] <- bjauto[i, 1:n]
   f[5, 1:n] <- ab1[i, 1:n]
   f[6, 1:n] <- ab2[i, 1:n]
   f[7, 1:n] <- ab3[i, 1:n]
   f[8, 1:n] <- ets1[i, 1:n]
   f[9, 1:n] <- aarima[i, 1:n]
   f[10, 1:n] <- hybrid[i, 1:n]
   f[11, 1:n] <- seats[i, 1:n]
   scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(x))))
   for(j in 1:number_methods)
   {
      mape[j, i] <- mean(abs((x - f[j, 1:n]) / x)) * 100
      smape[j, i] <- mean(abs(x - f[j, 1:n]) / (abs(x) + abs(f[j, 1:n]))) * 200
      mase[j, i] <- mean(abs(x - f[j, 1:n]) / scale)
   }
}

# All series
m3table <- matrix(NA, nrow = number_methods, ncol = 3)
m3table[,1] <- rowMeans(mape, na.rm = TRUE)
m3table[,2] <- rowMeans(smape)
m3table[,3] <- rowMeans(mase)
m3table <- as.data.frame(m3table)

names(m3table) <- c("MAPE", "sMAPE", "MASE")

m3table$method <- c("Theta", "ForecastPro", "ForecastX", "BJauto",
                       "Autobox1", "Autobox2", "Autobox3",
                       "ETS", "AutoARIMA", "Hybrid", "X13-SEATS")

m3table %>%
   arrange(MASE)
