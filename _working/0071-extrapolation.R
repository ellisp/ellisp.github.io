library(xgboost)
library(nnet)
library(ranger)

# can nnet extrapolate

x <- 1:100 + rnorm(100)
y <-   3 + 0.3 * x + rnorm(100)

extrap <- data.frame(x = c(x, 1:5 * 10 + 100))


mod_lm <- lm(y ~ x)

mod_rf <- ranger(y ~ x)
fc_rf <- predict(mod_rf, data = extrap)

mod_nn <- nnet(y ~ x, size = 8, linout = TRUE)

xg_params <- list(objective = "reg:linear", max.depth = 2)
mod_cv <- xgb.cv(label = y, params = xg_params, data = as.matrix(x), nrounds = 40, nfold = 10) # choose nrounds that gives best value of RMSE
mod_xg <- xgboost(label = y, params = xg_params, data = as.matrix(x), nrounds = 25)


par(bty = "l")
p <- function(){
   plot(x, y, xlim = c(0, 150), ylim = c(0, 50))
   grid()
}

par(mfrow = c(2, 2))
p(); points(extrap$x, predict(mod_lm, newdata = extrap), col = "red")
p(); points(extrap$x, predict(mod_nn, newdata = extrap), col = "blue")
p(); points(extrap$x, predict(mod_xg, newdata = as.matrix(extrap)), col = "purple")
p(); points(extrap$x, fc_rf$predictions, col = "pink") 

points(x, predict(mod_xg, newdata = as.matrix(x)), col = "purple")

