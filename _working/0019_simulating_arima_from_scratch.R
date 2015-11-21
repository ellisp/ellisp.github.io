library(latex2exp)
library(extrafont)

setwd(old_dir)
#-------------generate data-------------
set.seed(123)
n <- 1000

wn <- ts(rnorm(n))


ar1 <- ma1 <- arma11 <- arma22 <- wn[1:2]

for(i in 3:n){
   ar1[i]      <- ar1[i - 1] * 0.8 + wn[i]
   ma1[i]      <- wn[i - 1]  * 0.8 + wn[i]
   arma11[i]   <- arma11[i - 1] * 0.8 + wn[i - 1] * 0.8 + wn[i] 
   arma22[i]   <- arma22[i - 1] * 0.8 + arma22[i - 2]  * (-0.3) + 0.8 * wn[i-1] - 0.3 * wn[i-2] + wn[i]
}


ar1 <- ts(ar1)
ma1 <- ts(ma1)
arma11 <- ts(arma11)
arima111 <- ts(cumsum(arma11))
arima222 <- ts(cumsum(cumsum(arma22)))

#----------------create plots------------------
old_dir <- setwd("_output/0019-arima-simulation-scratch")

for(i in 3:n){
   png(paste0(i + 1000, ".png"), 800, 800, res = 100)
      par(mfrow = c(3, 2), cex.main = 1.5, cex = 0.8, family = "Calibri")
      plot(wn[1:i], main = latex2exp("$\\epsilon ~ N(0, \\sigma)"), 
           bty = "l", type = "l", ylab = "x = white noise", xlab = "")
      
      plot(ar1[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + \\epsilon_t$"), 
           bty = "l", type = "l", ylab = "x =AR (1)", xlab = "")
      
      plot(ma1[1:i], main = latex2exp("$x_t = 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
           bty = "l", type = "l", ylab = "x = MA(1)", xlab = "")
      
      plot(arma11[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + 0.8\\epsilon_{t-1} + \\epsilon_t$"),
           bty = "l", type = "l", ylab = "x = ARMA(1, 1)", xlab = "")
      
      plot(arima111[1:i], main = latex2exp("$x_t = 0.8x_{t-1} + 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
           bty = "l", type = "l", ylab = "y = ARIMA(1, 1, 1)", xlab = "")
      mtext(latex2exp("$y_t = x_t + x_{t-1} + ... + x_0$"), cex = 1.3, line = -0.5)
      
      plot(arima222[1:i], main =  latex2exp(
         "$x_t = 0.8x_{t-1} - 0.3x_{t-2} - 0.3\\epsilon_{t-2} + 0.8\\epsilon_{t-1} + \\epsilon_t$"), 
         bty = "l", type = "l", ylab = "z = ARIMA(2, 2, 2)", xlab = "")
      mtext(latex2exp("$y_t = x_t + x_{t-1} + ... + x_0$"), cex = 1.3, line = -0.5)
      mtext(latex2exp("$z_t = y_t + y_{t-1} + ... + y_0$"), cex = 1.3, line = -2.0)
   dev.off()
   
}


# combine them into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 10 *.png "arima-sims.gif"')



# move the asset over to where needed for the blog
file.copy("arima-sims.gif", "../../../img/0019-arima-sims.gif", overwrite = TRUE)
file.copy("1900.png", "../../../img/0019-snapshot.gif", overwrite = TRUE)

setwd(old_dir)
