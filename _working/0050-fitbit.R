library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)

fitbit <- read.csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/fitbit_export_20160806.csv", 
                   skip = 1, stringsAsFactors = FALSE) %>%
   mutate(
      Calories.Burned = as.numeric(gsub(",", "", Calories.Burned)),
      Steps = as.numeric(gsub(",", "", Steps)),
      Activity.Calories = as.numeric(gsub(",", "", Activity.Calories)),
      Date = as.Date(Date, format = "%d/%m/%Y")
   )

# look at everything, except the response variable.  Resulting 
# chart not shown in blog.
pairs(fitbit[ , -2])

# Distance is redundant - predictable from Steps.  Remove variables we don't want
fitbit2 <- fitbit
fitbit2$Distance <- NULL
fitbit2$Activity.Calories <- NULL
fitbit2$Date <- NULL
fitbit2$Steps <- fitbit2$Steps / 1000 # so coefficients will be calories per thousand steps, easier to descri

plot_pairs <- function(){
   par(family = "myfont")
   # let's look at all the candidate variables
   panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
      # function for calculating correlation coefficients and using
      # as panels in pairs() function.  Taken from ?pairs
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
   }
   pairs(fitbit2[ , -1], lower.panel = panel.cor, main = "Pairwise relationships of Fitbit's measured activities")
}



svg("../img/0050-pairs.svg", 9, 9)
   plot_pairs()
dev.off()

png("../img/0050-pairs.png", 9 * 72, 9 * 72)
   plot_pairs()
dev.off()


# turns out the sum of those minutes is the time you are awake:
# Not shown in blog
minutes <- fitbit2[ , 4:7]
plot(24 - round(apply(minutes, 1, sum) / 60, 1) # ie sleeping times (approximately))

# Model the basic average calories per step, over all average intensities
mod0 <- lm(Calories.Burned ~ Steps, data = fitbit2)

svg("../img/0050-1var-resid.svg", 7, 5)
plot(mod0, which = 1, bty = "l"); grid()
dev.off()

#----check autocorrelation--------
svg("../img/0050-1var-resid-pacf.svg", 7, 5)
pacf(resid(mod2), main = "Partial autocorrelation of residuals from single variable regression")
dev.off()

round(coef(mod0)) # ie 1926 calories plus 69 calories per 1,000 steps (regardless whether walking, running, etc)
# Compare to 50 calories per 1000 steps at http://www.livestrong.com/article/320124-how-many-calories-does-the-average-person-use-per-step/

# create X matrix and Y vector for use with glmnet which doesn't take R formulae
X <- as.matrix(fitbit2[ , -1]) # standardisation happens as part of glmnet
Y <- fitbit2$Calories.Burned

set.seed(123)
alphas <- seq(from = 0, to  = 1, length.out = 10)
res <- matrix(0, nrow = length(alphas), ncol = 2)
for(i in 1:length(alphas)){
   cvmod <- cv.glmnet(X, Y, alpha = alphas[i])
   res[i, ] <- c(alphas[i], sqrt(min(cvmod$cvm)))
}
res <- res[order(res[ ,2]), ]
res
# best alpha varies according to the random seed set earlier but with seed 123 it is 0.8889
bestalpha <- res[1, 1]

cvmod <- cv.glmnet(X, Y, alpha = bestalpha)
mod1 <- glmnet(X, Y, alpha = bestalpha)


mod2 <- lm(Calories.Burned ~ ., data = fitbit2)

coefs <- data.frame(original = coef(mod2), 
                  shrunk =  as.vector(coef(mod1, s = cvmod$lambda.min)),
                    very.shrunk = as.vector(coef(mod1, s = cvmod$lambda.1se)))

round(coefs, 3)

# RMSE from full data ie overfit models
RMSE(Y, fitted(mod0))
RMSE(Y, predict(mod1, newx = X, s = cvmod$lambda.min))
RMSE(Y, fitted(mod2))


#----------------validation--------
# first, check that with lambda = 0 you get the same results with both, other than minor calculation diffs
mod3 <- glmnet(X, Y, alpha = bestalpha, lambda = 0)
round(data.frame("elastic, lambda = 0" = as.vector(coef(mod3)), "lm" = coef(mod2), check.names = FALSE), 3)

# function to feed to boot that does elastic modelling
mod1f <- function(data, i){
   X <- as.matrix(data[i , -1])
   Y <- data[i , 1]
   cvmod <- cv.glmnet(X, Y, alpha = 1, nfolds = 30)
   mod1 <- glmnet(X, Y, alpha = 1)
   RMSE(predict(mod1, newx = as.matrix(data[ , -1]), s = cvmod$lambda.min), data[ , 1])
}

elastic_boot <- boot(fitbit2, statistic = mod1f, R = 99)


# function to feed to boot that does OLS modelling
mod2f <- function(data, i){
   mod2 <- lm(Calories.Burned ~ ., data = data[i, ])
   RMSE(predict(mod2, newdata = data), data[ , 1])
}

lm_boot <- boot(fitbit2, statistic = mod2f, R = 99)

# for boot with OLS modelling, only one explanatory variable
mod0f <- function(data, i){
   mod0 <- lm(Calories.Burned ~ Steps, data = data[i, ])
   RMSE(predict(mod0, newdata = data), data[ , 1])
}
lm0_boot <- boot(fitbit2, statistic = mod0f, R = 99)

round(c("elastic" = mean(elastic_boot$t), 
        "lm_allvar" = mean(lm_boot$t),
        "lm_1var" = mean(lm0_boot$t)), 1)







#----variables coming in with different importance--
# working with scaled variables just for visual distinguishing
# shouldn't make a difference as under the hood glmnet does it anyway
mod1s <- glmnet(scale(X), Y, alpha = bestalpha)

ordering <- c(6,4,5,1,2,3)

svg("../img/0050-elastic-coefs.svg", 7, 6)
par(mar = c(5.1, 4.1, 6.5, 1))
plot(mod1s, xvar = "dev", label = TRUE, col = 1:6, lwd = 2, main = 
        "Increasing contribution of different explanatory variables\nas penalty for including them is relaxed")

legend("topleft", legend = colnames(X)[ordering], text.col = (1:6)[ordering], 
       lwd = 2, bty = "n", col = (1:6)[ordering])

dev.off()




