library(Tcomp)
library(Mcomp)

test_fc <- function(period, verbose = TRUE){
   # subset to just the monthly, quarterly or yearly data, using subset method from Mcomp
   the_data <- subset(tourism, period)
   
   # object to hold results
   mases <- matrix(0, ncol = 4, nrow = length(the_data))
   
   
   for(i in 1:length(the_data)){
      if(verbose){
         # for tracking progress
         if(i / 10 == round(i / 10)){cat(paste0(i, " "))}  
      }
      
      the_series <- the_data[[i]]
      
      x <- the_series$x   # training set
      xx <- the_series$xx # test set
      h <- the_series$h
      
      mod1 <- auto.arima(x)
      fc1 <- forecast(mod1, h = h)
      fc2 <- forecast(ets(x), h = h)
      fc3 <- thetaf(x, h = h)
      
      
      mases[i, 1] <-  accuracy(fc1, xx)["Test set", "MASE"]
      mases[i, 2] <-  accuracy(fc2, xx)["Test set", "MASE"]
      mases[i, 3] <-  accuracy(fc3, xx)["Test set", "MASE"]
      mases[i, 4] <- h
   }
   
   cat("\n")
   colnames(mases) <- c("ARIMA", "ETS", "Theta", "Horizon")
   
   return(apply(mases, 2, mean))
}

test_fc("monthly")
test_fc("quarterly")
test_fc("yearly")