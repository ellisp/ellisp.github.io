library(xgboost)  # extreme gradient boosting
library(nnet)     # neural network
library(ranger)   # for random forests
library(rpart)    # for demo single tree
library(rpart.plot)
library(viridis) # for palette of colours
library(grid)    # for annotations

# The questions is, can "machine learning" methods extrapolate as well as a simple linear regression?

# sample data - training set
set.seed(134) # for reproducibility
x <- 1:100 + rnorm(100)
y <-   3 + 0.3 * x + rnorm(100)

# extrapolation / test set, has historical data plus some more extreme values
extrap <- data.frame(x = c(x, 1:5 * 10 + 100))


mod_lm <- lm(y ~ x)

mod_rf <- ranger(y ~ x)
fc_rf <- predict(mod_rf, data = extrap)

mod_nn <- nnet(y ~ x, size = 8, linout = TRUE)

# XG boost.  This is a bit more complicated as we need to know how many rounds
# of trees to use.  Best to use cross-validation to estimate this.  Note - 
# I use a maximum depth of 2 for the trees which I identified by trial and error
# with different values of max.depth and cross-validation, not shown
xg_params <- list(objective = "reg:linear", max.depth = 2)
mod_cv <- xgb.cv(label = y, params = xg_params, data = as.matrix(x), nrounds = 40, nfold = 10) # choose nrounds that gives best value of  root mean square error on the training set
best_nrounds <- which(mod_cv$test.rmse.mean == min(mod_cv$test.rmse.mean))
mod_xg <- xgboost(label = y, params = xg_params, data = as.matrix(x), nrounds = best_nrounds)


p <- function(title){
   plot(x, y, xlim = c(0, 150), ylim = c(0, 50), pch = 19, cex = 0.6,
        main = title, xlab = "", ylab = "", font.main = 1)
   grid()
}

predshape <- 1

svg("../img/0071-four-methods.svg", 8, 8)
par(family = "myfont")
par(mfrow = c(2, 2), bty = "l", mar = c(7, 4, 4, 2) + 0.1)

p("Linear regression")
points(extrap$x, predict(mod_lm, newdata = extrap), col = "red", pch = predshape)

p("Neural network")
points(extrap$x, predict(mod_nn, newdata = extrap), col = "blue", pch = predshape)

p("Extreme gradient boosting")
points(extrap$x, predict(mod_xg, newdata = as.matrix(extrap)), col = "darkgreen", pch = predshape)

p("Random forest")
points(extrap$x, fc_rf$predictions, col = "plum3", pch = predshape) 

grid.text(0.5, 0.54, gp = gpar(fontfamily = "myfont", col = "steelblue"), 
          label = "Tree-based learning methods (like xgboost and random forests)\nhave a particular challenge with out-of-sample extrapolation.")
grid.text(0.5, 0.04, gp = gpar(fontfamily = "myfont", col = "steelblue"), 
          label = "In all the above plots, the black points are the original training data,\nand coloured circles are predictions.")
dev.off()


#==============draw an example tree===================
# this is to illustrate the fundamental limitation of tree-based methods
# for out-of-sample extrapolation
tree <- rpart(y ~ x)

svg("../img/0071-tree.svg", 7, 5)
par(family = "myfont")
par(font.main = 1, col.main = "steelblue")
rpart.plot(tree, digits = 1, 
           box.palette = viridis(10, option = "D", begin = 0.85, end = 0), shadow.col = "grey65", col = "grey99", 
           main = "Tree-based methods will give upper and lower bounds\nfor predicted values; in this example, the highest possible\npredicted value of y is 31, whenever x>84.")
dev.off()


setwd("../img")
files <- list.files()
files <- files[grepl("^0071.+svg$", files)]
for(i in files){
   output <- gsub("svg$", "png", i)
   cmd <- paste0('\"C:\\Program Files\\ImageMagick-7.0.2-Q16\\magick\" -size 1200x1200', " ", i, " ", output)
   system(cmd)
   
}
setwd("../_working")
