library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)


fitbit <- read.csv("../data/fitbit_export_20160806.csv", skip = 1, stringsAsFactors = FALSE) %>%
   mutate(
      Calories.Burned = as.numeric(gsub(",", "", Calories.Burned)),
      Steps = as.numeric(gsub(",", "", Steps)),
      Activity.Calories = as.numeric(gsub(",", "", Activity.Calories)),
      Date = as.Date(Date, format = "%d/%m/%Y")
   )
# pairs, excluding Date and Calories consumed
pairs(fitbit[ -(1:2)])

# so Distance is redundant - predictable from Steps
fitbit2 <- fitbit
fitbit2$Distance <- NULL
fitbit2$Activity.Calories <- NULL
fitbit2$Date <- NULL

pairs(fitbit2[ -1])

# turns out the sum of those minutes is the time you are awake:
minutes <- fitbit2[ , 4:7]
24 - round(apply(minutes, 1, sum) / 60, 1)


mod0 <- lm(Calories.Burned ~ Steps + Floors, data= fitbit2)
RMSE(Y, fitted(mod0)) # surprisngly bad
plot(mod0, which = 1, bty = "l"); grid()

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
# best alpha varies according to the random seed set earlier
bestalpha <- res[1, 1]

cvmod <- cv.glmnet(X, Y, alpha = bestalpha)
mod1 <- glmnet(X, Y, alpha = bestalpha)


mod2 <- lm(Calories.Burned ~ ., data = fitbit2)

coefs <- data.frame(original = coef(mod2), 
           shrunk = as.vector(coef(mod1, s = cvmod$lambda.min)))

round(coefs, 3)

# RMSE from full data ie overfit models
RMSE(Y, fitted(mod0))
RMSE(Y, predict(mod1, newx = X, s = cvmod$lambda.min))
RMSE(Y, fitted(mod2))


#----------------validation--------
# first, check that with lambda = 0 you get the same results with both
mod3 <- glmnet(X, Y, alpha = bestalpha, lambda = 0)
round(data.frame("elastic, lambda = 0" = as.vector(coef(mod3)), "lm" = coef(mod2), check.names = FALSE), 3)


mod1f <- function(data, i){
   X <- as.matrix(data[i , -1])
   Y <- data[i , 1]
   cvmod <- cv.glmnet(X, Y, alpha = 1, nfolds = 30)
   mod1 <- glmnet(X, Y, alpha = 1)
   RMSE(predict(mod1, newx = as.matrix(data[ , -1]), s = cvmod$lambda.min), data[ , 1])
}

elastic_boot <- boot(fitbit2, statistic = mod1f, R = 99)


mod2f <- function(data, i){
   mod2 <- lm(Calories.Burned ~ ., data = data[i, ])
   RMSE(predict(mod2, newdata = data), data[ , 1])
}

lm_boot <- boot(fitbit2, statistic = mod2f, R = 99)

mod0f <- function(data, i){
   mod0 <- lm(Calories.Burned ~ Steps + Floors, data = data[i, ])
   RMSE(predict(mod0, newdata = data), data[ , 1])
}
lm0_boot <- boot(fitbit2, statistic = mod0f, R = 99)

round(c("elastic" = mean(elastic_boot$t), 
        "lm_allvar" = mean(lm_boot$t),
        "lm_2var" = mean(lm0_boot$t)), 1)
#----autocorrelation--------
pacf(resid(mod2))

round(coef(mod0), 3)
round(coef(lm(Calories.Burned ~ Steps, data = fitbit2)), 3) 
# 69 calories per 1000 steps.  Compare to 50 calories per 1000 steps at http://www.livestrong.com/article/320124-how-many-calories-does-the-average-person-use-per-step/
confint(mod0)


#----variables coming in with different importance--
# working with scaled variables just for visual distinguishing
# shouldn't make a difference as under the hood glmnet does it anyway
mod1s <- glmnet(scale(X), Y, alpha = bestalpha)
plot(mod1s, xvar = "dev", label = TRUE)
colnames(X)
coef(mod1s)
