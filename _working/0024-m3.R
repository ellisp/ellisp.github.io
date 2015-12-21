library(devtools)
#devtools::install_github("christophsax/seasonal", ref = "error-parsing-fixes") 


library(Mcomp)
library(dplyr)
library(tidyr)
library(seasonal)
library(showtext)
library(ggplot2)
library(scales)
library(RColorBrewer)


# import fonts
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))


# set path to X13-SEATS and check it's working
Sys.setenv(X13_PATH = "c:/winx13/x13as")
checkX13()

nseries <- length(M3)


# forecasts from the original competition
theta <- as.matrix(M3Forecast$THETA)[1:nseries, ]
fpro <- as.matrix(M3Forecast$ForecastPro)[1:nseries, ]
fcx <- as.matrix(M3Forecast$ForcX)[1:nseries, ]
bjauto <- as.matrix(M3Forecast$`B-J auto`)[1:nseries, ]
ab1 <- as.matrix(M3Forecast$AutoBox1)[1:nseries, ]
ab2 <- as.matrix(M3Forecast$AutoBox2)[1:nseries, ]
ab3 <- as.matrix(M3Forecast$AutoBox3)[1:nseries, ]

# set up matrices to hold the forecasts from new methods
ets1 <- aarima <- theta_fc <- hybrid2a <- hybrid2b <- 
   hybrid2c <- hybrid3 <- seats <- 
   matrix(NA, nrow = nseries, ncol = 18)

# produce new forecasts
for(i in 1:nseries){
   print(i)
   ets1[i, ] <- forecast(ets(M3[[i]]$x), h = 18, PI = FALSE)$mean
   aarima[i, ] <- forecast(auto.arima(M3[[i]]$x), h = 18)$mean
   theta_fc[i, ] <- thetaf(M3[[i]]$x, h = 18)$mean
   hybrid2a[i, ] <- 0.5 * (aarima[i, ] + ets1[i, ])
   hybrid2b[i, ] <- 0.5 * (aarima[i, ] + theta_fc[i, ]) 
   hybrid2c[i, ] <- 0.5 * (theta_fc[i, ] + ets1[i, ])
   hybrid3[i, ]  <- (aarima[i, ] + ets1[i, ] + theta_fc[i, 1]) / 3
   
   if(M3[[i]]$period != "YEARLY"){
      try(m <- seas(M3[[i]]$x, forecast.maxlead = 18)) # 801 fails to converge ; 807 runs but produces no data; 
      # 1154  and a bunch aferhas singular x matrix problem because of trading days 
      #1405 fails for 'unknown reason'
   } else {
      try(m <- seas(M3[[i]]$x, 
                    regression.aictest = NULL, 
                    regression.variables = c("const"), 
                    forecast.maxlead = 18)) 
   }
   try(seats[i, ] <- series(m, "forecast.forecasts")[1:18 , 1]) 
}


# 92 failures in first 500 series; 413 in total, various reasons
message(sum(is.na(seats[ , 1])), " series failed for SEATS, replacing them with auto.arima's results")
bad <- is.na(seats)
bad[ ,1]
save(bad, seats, file = "tmp_bad_and_seats.rda")

# create a copy of seats that uses auto arima when it's bad
seats2 <- seats
seats2[bad] <- aarima[bad]

# which series are the bad ones?
bad_series <- which(bad[, 1])


# Compute accuracy
number_methods <- 15
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
   f[10, 1:n] <- theta_fc[i, 1:n]
   f[11, 1:n] <- hybrid2a[i, 1:n]
   f[12, 1:n] <- hybrid2b[i, 1:n]
   f[13, 1:n] <- hybrid2c[i, 1:n]
   f[14, 1:n] <- hybrid3[i, 1:n]
   f[15, 1:n] <- seats2[i, 1:n]
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
                       "ETS", "AutoARIMA", "Theta_fc", "Hybrid AutoARIMA ETS", 
                       "Hybrid AutoARIMA Theta_fc", "Hybrid Theta_fc-ETS",
                       "Hybrid 3", "X13-SEATS")

m3table %>%
   arrange(MASE)

p1 <- m3table %>%
   arrange(-MASE) %>%
   mutate(method = factor(method, levels = method)) %>%
   mutate(hybrid = ifelse(grepl("Hybrid", method), "Hybrid", "Single")) %>%
   ggplot(aes(x = MASE, y = method, colour = hybrid)) +
   geom_point() +
   geom_segment(aes(yend = method), xend = 1) +
   labs(x = "Mean Absolute Scaled Error (smaller is better)", colour = "") +
   ggtitle("Comparison of selected forecasting methods on the M3 data")
svg("../img/0024-dots.svg", 6, 5)
print(p1)
dev.off()

png("../img/0024-dots.png", 600, 500, res = 100)
print(p1)
dev.off()

#=================further investigation===============

#-------------example series with unit root--------------
# Series Y434, Marital status (numbers in thousands), 1974 to 1988
svg("../img/0024-prob1.svg", 6, 4)
plot(M3[[434]]$x, bty = "l", main = "Y434 Marital status (numbers in thousands)")
grid()
dev.off()

# this works and decides it's ARIMA(0,1,0) with drift:
auto.arima(M3[[434]]$x)

# this fails
seas(M3[[434]]$x, regression.aictest = NULL)
seas(M3[[434]]$x, regression.aictest = NULL, x11 = "")

# but if we manually specify the model
m <- seas(M3[[434]]$x, 
          regression.aictest = NULL, 
          regression.variables = c("const"),
          arima.model = c(0,1,0), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]    

# which is the same as auto.arima's original forecast:
aarima[434, ]

#-----------trading day problem--------------
# quarterly Denmark GDP by expenditure
svg("../img/0024-prob2.svg", 6, 4)
plot(M3[[1154]]$x, bty = "l", main = "Q509 Denmark GDP by expenditure")
grid()
dev.off()

# fails because of singularity to do with number of Saturdays:
seas(M3[[1154]]$x)


# if we specify manually which regression variables to choose 
# and we drop trading days, we get a result:
m <- seas(M3[[1154]]$x, 
          regression.variables = c("const", "easter[14]", "seasonal"), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]    

aarima[1154, ]


#---------------runs but produces no data------
# quarterly cash flow
svg("../img/0024-prob3.svg", 6, 4)
plot(M3[[807]]$x, bty = "l", main = "Q162 Cash Flow" )
grid()
dev.off()

# following runs but produces no data
seas(M3[[807]]$x)

# this works and concludes MA(1):
auto.arima(M3[[807]]$x)

# but if we manually specify the model
m <- seas(M3[[807]]$x, 
          regression.variables = c("const"),
          arima.model = c(0,0,1), 
          forecast.maxlead = 18)
series(m, "forecast.forecasts")[1:18 , 1]  
aarima[807, ]


#---------cannot process spec file for an unknown reason---------
# Monthly shipments
svg("../img/0024-prob4.svg", 6, 4)
plot(M3[[1485]]$x, main = "M84, Shipments")
grid()
dev.off()

# runs fine:
auto.arima(M3[[1485]]$x)

# takes a long time and eventually fails for unknown reason:
seas(M3[[1485]]$x)


# still fails:
m <- seas(M3[[1485]]$x, 
          regression.variables = c("const"),
          arima.model = c(0,1,1), 
          forecast.maxlead = 18)

# can't work this one out


#-----------can't handle phony date/times----------------
# Telecommunications data, start = "1"
svg("../img/0024-prob5.svg", 6, 4)
plot(M3[[3000]]$x, main = "O171 Telecommunication data")
grid()
dev.off()

# fails because X13 can't handle abstract times, or things 
# starting in year 1.  No obvious way around this.
seas(M3[[3000]]$x)


#=================when X13 runs, but does different than auto.arima=============
# 15th row of mase is for X13 and 9th is for auto arima


mases <- data_frame(
      x13 = mase[15, ],
      aa = mase[9, ],
      id = 1:ncol(mase)) %>%
   mutate(diff = x13 - aa) 


#------------X13 much better---------------
mases %>% arrange(diff)
# stock shares sold on NYSE 1947 to 1987
# note that basically the crash of 1987 is what stuffed auto arima
# X13 seems to make an inspired guess!
m <- seas(M3[[335]]$x, regression.aictest = NULL, regression.variables = "const")
summary(m) # ARIMA(1,0,0)
auto.arima(M3[[335]]$x) # ARIMA(0,2,0)

comp_x13_seats <- function(s){
            
   orig <- M3[[s]]$x
   act  <- M3[[s]]$xx
   
   tmp1 <- rbind(
      data.frame(
         time = as.numeric(time(orig)),
         value = as.numeric(orig),
         method = "Actual result",
         stringsAsFactors = FALSE),
      data.frame(
         time = as.numeric(time(act)),
         value = as.numeric(act),
         method = "Actual result",
         stringsAsFactors = FALSE)
   )
   
   tmp2 <- data_frame(
      x13 = seats[s, 1:length(act)],
      aa  = aarima[s, 1:length(act)],
      time = time(act)
   ) %>%
      gather(method, value, -time) 
      
   tmp <- rbind(tmp1, tmp2) %>%
      mutate(method = gsub("aa", "auto.arima()", method),
             method = gsub("x13", "X13-SEATS", method))
   
   # we want a named vector of colours so each method gets the same colour each time
   pal <- brewer.pal(3, "Set1")
   names(pal) <- unique(tmp$method)
   
   # need the order to help order the legend
   latest <- tmp %>%
      filter(time == max(time)) %>%
      arrange(-value) 
   
   p1 <- tmp %>%
      mutate(method = factor(method, levels = latest$method)) %>%
      ggplot(aes (x = time, y = value, colour = method)) +
      geom_line() +
      geom_point() +
      scale_y_log10() +
      ggtitle(M3[[s]]$description) +
      scale_colour_manual(values = pal)
   
   svg(paste0("../img/0024-x13-comp-", s, ".svg"), 6, 3)
      print(p1)
   dev.off()
}

comp_x13_seats(335)
comp_x13_seats(334)
comp_x13_seats(332)
comp_x13_seats(49)
comp_x13_seats(171)


#-----------X13 much worse eg 1------------
mases %>% arrange(-diff)
comp_x13_seats(73)
comp_x13_seats(64)
comp_x13_seats(1388)
comp_x13_seats(120)



plot(M3[[73]]$x)

aarima[73, ]
seats[73, ] # completely explosive

m <- seas(M3[[73]]$x, 
          regression.aictest = NULL, 
          regression.variables = c("const"),
          forecast.maxlead = 18)
# SEATS gives ARIMA(1,1,2), log transform


# auto.arima gives a simpler ARIMA(0,1,0) without the log transform
auto.arima(M3[[73]]$x)



#-----------------X13 much worse eg 2----------------
mases %>% arrange(-diff)

plot(M3[[1388]]$x)

aarima[1388, ]
seats[1388, ] # again, completely explosive

m <- seas(M3[[1388]]$x, forecast.maxlead = 18)
summary(m) # ARIMA(1,1,0)(1,1,0), log transform


# auto.arima gives ARIMA(1,1,0)(0,1,0) on original
auto.arima(M3[[1388]]$x)


#=========================theta method==================
# http://stats.stackexchange.com/questions/67036/does-thetaf-in-the-forecast-package-in-r-detect-seasonality
# Hyndman says it does not handle seasonality.  Then why did it score so well in M3?


thetas <- function(s){
   forecast_theta <- ts(c(M3[[s]]$x, theta_fc[s, ]))
   original_theta <- ts(c(M3[[s]]$x, theta[s, ]))
   plot(cbind(forecast_theta, original_theta), main = M3[[s]]$description, plot.type = "single", col = 1:2)
}

thetas(1388)



for(i in 1:3003){
   png(paste0("_output/0024-thetas/", i, ".png"), 600, 400, res = 100)
   thetas(i)
   cat(i, " ")
   dev.off()
}

