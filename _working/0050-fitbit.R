library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(RColorBrewer)

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


fitbit2 <- fitbit
fitbit2$Activity.Calories <- NULL
fitbit2$Date <- NULL
fitbit2$Steps <- fitbit2$Steps / 1000 # so coefficients will be calories per thousand steps, easier to describe

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

# metres per step - not quite 71cm every time, due to rounding or other jitter:
svg("../img/0050-stridelenth.svg", 7, 4)
print(ggplot(fitbit2, aes(x = Distance / Steps)) + 
   geom_rug() + 
   geom_density() +
      ggtitle("Stride length reverse-engineered from Fitbit data",
              subtitle = "Not all strides identical, due to rounding or other jitter")
)
dev.off()

# turns out the sum of those minutes is the time you are awake:
# Not shown in blog
minutes <- fitbit2[ , 4:7]
round(apply(minutes, 1, sum) / 60, 1)

plot(24 - round(apply(minutes, 1, sum) / 60, 1)) # ie sleeping times (approximately)

# Model the basic average calories per step, over all average intensities
mod0 <- lm(Calories.Burned ~ Steps, data = fitbit2)

svg("../img/0050-1var-resid.svg", 7, 5)
par(family = "myfont")
plot(mod0, which = 1, bty = "l"); grid()
dev.off()

#----check autocorrelation--------
svg("../img/0050-1var-resid-pacf.svg", 7, 5)
par(family = "myfont")
pacf(resid(mod0), main = "Partial autocorrelation of residuals\nfrom single variable regression")
dev.off()

round(coef(mod0)) # ie 1926 calories plus 69 calories per 1,000 steps (regardless whether walking, running, etc)
# Compare to 50 calories per 1000 steps at http://www.livestrong.com/article/320124-how-many-calories-does-the-average-person-use-per-step/


#============full model=================

#===========elastic net regularisation=============
# create X matrix and Y vector for use with glmnet, which doesn't take Rmodel formulae
# First column of fitbit2 is the response variable "Calories burned" so we exclude from X:
X <- as.matrix(fitbit2[ , -1]) # standardisation happens as part of glmnet
Y <- fitbit2$Calories.Burned

set.seed(123)
alphas <- seq(from = 0, to  = 1, length.out = 10)
res <- matrix(0, nrow = length(alphas), ncol = 6) # five columns for results
for(i in 1:length(alphas)){
   for(j in 2:6){
      cvmod <- cv.glmnet(X, Y, alpha = alphas[i])
      res[i, c(1, j)] <- c(alphas[i], sqrt(min(cvmod$cvm)))
   }
}
res <- data.frame(res)
res$average_rmse <- apply(res[ , 2:6], 1, mean)
res <- res[order(res$average_rmse), ]
names(res)[1] <- "alpha"

svg("../img/0050-cv-alpha.svg", 7, 5)
res %>%
   select(-average_rmse) %>%
   gather(trial, rmse, -alpha) %>%
   ggplot(aes(x = alpha, y = rmse)) +
   geom_point() +
   geom_smooth(se = FALSE) +
   labs(y = "Root mean square error") +
   ggtitle("Cross validation best RMSE for differing values of alpha")
dev.off()

# best alpha varies according to the random seed set earlier but with seed 123 it is 0.667
bestalpha <- res[1, 1]

#--------

# Cross-validated version at best value of alpha, for use later
# when we want to use the best value of lambda given that alpha
cvmod <- cv.glmnet(X, Y, alpha = bestalpha)

# the model itself
mod1 <- glmnet(X, Y, alpha = bestalpha)

# the OLS model
mod2 <- lm(Calories.Burned ~ ., data = fitbit2)

coefs <- data.frame(original = coef(mod2), 
                  shrunk =  as.vector(coef(mod1, s = cvmod$lambda.min)),
                    very.shrunk = as.vector(coef(mod1, s = cvmod$lambda.1se)))

round(coefs, 3)

# RMSE from full data ie overfit models
RMSE(Y, fitted(mod0)) # steps the only explanatory variable
RMSE(Y, predict(mod1, newx = X, s = cvmod$lambda.min))
RMSE(Y, fitted(mod2))



# check that with lambda = 0 you get the same results with both, other than minor calculation diffs
mod3 <- glmnet(X, Y, lambda = 0)
round(data.frame("elastic, lambda = 0" = as.vector(coef(mod3)), "lm" = coef(mod2), check.names = FALSE), 3)
# coefficients are different even with lambda = 0.  

# what about a simpler case with less numerical instability - 
# drop "distance" (which is the second column)
mod4 <- glmnet(X[ , -2], Y, lambda = 0)
mod5 <- lm(Y ~ X[ , -2])
round(data.frame("elastic, lambda = 0" = as.vector(coef(mod4)), "lm" = coef(mod5), check.names = FALSE), 3)


#----------------validation--------
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
# refit the model with scaled variables just for the graphic.
# working with scaled variables just for visual distinguishing
# shouldn't make a difference as under the hood glmnet does it anyway
# in the models fit earlier.
mod1s <- glmnet(scale(X), Y, alpha = bestalpha)

# set a palette
thepal <- brewer.pal(7, "Set1")

# set the ordering for ease of reading on graphic
ordering <- c(7,5,6,2,1,3,4)

svg("../img/0050-elastic-coefs.svg", 7, 6)
par(family = "myfont")
par(mar = c(5.1, 4.1, 6.5, 1), bg = "grey90")
plot(mod1s, xvar = "dev", label = TRUE, col = thepal, lwd = 2, main = 
        "Increasing contribution of different explanatory variables\nas penalty for including them is relaxed")

legend("topleft", legend = colnames(X)[ordering], text.col = thepal[ordering], 
       lwd = 2, bty = "n", col = thepal[ordering])

dev.off()




