
#===================setup=======================
library(MASS) # must be loaded before dplyr
library(ggplot2)
library(scales)
library(boot)
library(caret)
library(dplyr)
library(tidyr)
library(directlabels)

theme_set(theme_light(10, base_family = "myfont") )

set.seed(123)

# install nzelect package that has census data
# devtools::install_github("ellisp/nzelect/pkg")
library(nzelect)

# drop some variables so the simple approaches tested here can work:
au <- AreaUnits2013 %>%
   # drop the columns with areas' code and name:
   select(-AU2014, -AU_NAM) %>%
   # drop geographical variables:
   select(-(NZTM2000Easting:WGS84Latitude)) %>%
   # drop some perfectly collinear variables:
   select(-PropWorked40_49hours2013, -Prop35to39_2013) %>%
   # drop some rarely informative variables:
   select(-PropWorkedOver60hours2013)

# give meaningful rownames, helpful for some diagnostic plots later
row.names(au) <- AreaUnits2013$AU_NAM

# remove some repetition from the variable names
names(au) <- gsub("_2013", "", names(au), fixed = TRUE)
names(au) <- gsub("2013", "", names(au), fixed = TRUE)

# restrict to areas with no missing data.  If this was any more complicated (eg
# imputation),it would need to be part of the validation resampling too; but
# just dropping them all at the beginning doesn't need to be resampled; the only
# implication would be sample size which would be small impact and complicating.
au <- au[complete.cases(au), ]

#==================functions for two of the modelling strategies=====================
# The stepwise variable selection:
model_process_step <- function(the_data){
   model_full <- lm(MedianIncome ~ ., data = the_data)
   model_final <- stepAIC(model_full, direction = "both", trace = 0)
   return(model_final)
}

# The dropping of highly collinear variables, based on Variance Inflation Factor:
model_process_vif <- function(the_data){
   # remove the collinear variables based on vif
   x <- 20
   
   while(max(x) > 10){
      mod1 <- lm(MedianIncome ~ . , data = the_data)
      x <- sort(car::vif(mod1) , decreasing = TRUE)
      the_data <- the_data[ , names(the_data) != names(x)[1]]
      # message(paste("dropping", names(x)[1]))
   }
   
   model_vif <- lm(MedianIncome ~ ., data = the_data)
   return(model_vif)
}

# The third strategy, full model, is only a one-liner with standard functions
# so I don't need to define a function separately for it.

#==================Different validation methods=================

#------------------simple bootstrap comparison-------------------------
# create a function suitable for boot that will return the goodness of fit
# statistics testing models against the full original sample.
compare <- function(orig_data, i){
   # create the resampled data
   train_data <- orig_data[i, ]
   test_data <- orig_data # ie the full original sample
   
   # fit the three modelling processes
   model_step <- model_process_step(train_data)
   model_vif  <- model_process_vif(train_data)
   model_full <- lm(MedianIncome ~ ., data = train_data)
   
   # predict the values on the original, unresampled data
   predict_step <- predict(model_step, newdata = test_data)
   predict_vif  <- predict(model_vif, newdata = test_data)
   predict_full  <- predict(model_full, newdata = test_data)
   
   # return a vector of 6 summary results
   results <- c(
      step_R2 = R2(predict_step, test_data$MedianIncome),
      vif_R2  = R2(predict_vif, test_data$MedianIncome),
      full_R2  = R2(predict_full, test_data$MedianIncome),
      step_RMSE = RMSE(predict_step, test_data$MedianIncome),
      vif_RMSE  = RMSE(predict_vif, test_data$MedianIncome),
      full_RMSE  = RMSE(predict_full, test_data$MedianIncome)
   )
   return(results)
}

# perform bootstrap
Repeats <- 100
res <- boot(au, statistic = compare, R = Repeats)

# restructure results for a graphic showing root mean square error, and for
# later combination with the other results.  I chose just to focus on RMSE;
# the messages are similar if R squared is used.
RMSE_res <- as.data.frame(res$t[ , 4:6])
names(RMSE_res) <- c("AIC stepwise selection", "Remove collinear variables", "Use all variables")

p1 <- RMSE_res %>%
   mutate(trial = 1:Repeats) %>%
   gather(variable, value, -trial) %>% 
   # re-order levels:
   mutate(variable = factor(variable, levels = c(
      "Remove collinear variables", "AIC stepwise selection", "Use all variables"
   ))) %>%
   ggplot(aes(x = trial, y = value, colour = variable)) +
   geom_line() +
   geom_point() +
   ggtitle("'Simple' bootstrap of model fit of three different regression strategies",
           subtitle = "Predicting areas' median income based on census variables") +
   labs(x = "Resample id (there no meaning in the order of resamples)\n",
        y = "Root Mean Square Error (higher is worse)\n",
        colour = "Strategy",
        caption = "Data from New Zealand Census 2013")

png("../img/0043a-boot-results.png", 800, 500, res = 100)
print(p1)
dev.off()

svg("../img/0043a-boot-results.svg", 8, 5)
print(p1)
dev.off()

# store the three "simple bootstrap" RMSE results for later
simple <- apply(RMSE_res, 2, mean)

#-----------------------enhanced (optimism) bootstrap comparison-------------------
# for convenience, estimate the models on the original sample of data
orig_step <- model_process_step(au)
orig_vif <- model_process_vif(au)
orig_full <- lm(MedianIncome ~ ., data = au)

# create a function suitable for boot that will return the optimism estimates for
# statistics testing models against the full original sample.
compare_opt <- function(orig_data, i){
   # create the resampled data
   train_data <- orig_data[i, ]

   # fit the three modelling processes
   model_step <- model_process_step(train_data)
   model_vif  <- model_process_vif(train_data)
   model_full <- lm(MedianIncome ~ ., data = train_data)
   
   # predict the values on the original, unresampled data
   predict_step <- predict(model_step, newdata = orig_data)
   predict_vif  <- predict(model_vif, newdata = orig_data)
   predict_full  <- predict(model_full, newdata = orig_data)
   
   # return a vector of 6 summary optimism results
   results <- c(
      step_R2 = R2(fitted(model_step), train_data$MedianIncome) - R2(predict_step, orig_data$MedianIncome),
      vif_R2  = R2(fitted(model_vif), train_data$MedianIncome) - R2(predict_vif, orig_data$MedianIncome),
      full_R2  = R2(fitted(model_full), train_data$MedianIncome) - R2(predict_full, orig_data$MedianIncome),
      step_RMSE = RMSE(fitted(model_step), train_data$MedianIncome) - RMSE(predict_step, orig_data$MedianIncome),
      vif_RMSE  = RMSE(fitted(model_vif), train_data$MedianIncome) - RMSE(predict_vif, orig_data$MedianIncome),
      full_RMSE  = RMSE(fitted(model_full), train_data$MedianIncome) - RMSE(predict_full, orig_data$MedianIncome)
   )
   return(results)
}

# perform bootstrap
res_opt <- boot(au, statistic = compare_opt, R = Repeats)

# calculate and store the results for later
original <- c(
   RMSE(fitted(orig_step), au$MedianIncome), 
   RMSE(fitted(orig_vif), au$MedianIncome),
   RMSE(fitted(orig_full), au$MedianIncome)
)

optimism <- apply(res_opt$t[ , 4:6], 2, mean)
enhanced <- original - optimism


#------------------repeated cross-validation------------------
# The number of cross validation repeats is the number of bootstrap repeats / 10:
cv_repeat_num <- Repeats / 10

# use caret::train for the two standard models:
the_control <- trainControl(method = "repeatedcv", number = 10, repeats = cv_repeat_num)
cv_full <- train(MedianIncome ~ ., data = au, method = "lm", trControl = the_control)
cv_step <- train(MedianIncome ~ ., data = au, method = "lmStepAIC", trControl = the_control, trace = 0)

# do it by hand for the VIF model:
results <- numeric(10 * cv_repeat_num)
for(j in 0:(cv_repeat_num - 1)){
   cv_group <- sample(1:10, nrow(au), replace = TRUE)
   for(i in 1:10){
      train_data <- au[cv_group != i, ]
      test_data <- au[cv_group == i, ]
      results[j * 10 + i] <- RMSE(
         predict(model_process_vif(train_data), newdata = test_data),
         test_data$MedianIncome)
   }
}
cv_vif <- mean(results)

cv_vif_results <- data.frame(
   results = results,
   trial = rep(1:10, cv_repeat_num),
   cv_repeat = rep(1:cv_repeat_num, each = 10)
)


#===============reporting results===============
# combine the three cross-validation results together and combined with
# the bootstrap results from earlier
summary_results <- data.frame(rbind(
   simple, 
   enhanced,
   c(mean(cv_step$resample$RMSE), 
     cv_vif,
     mean(cv_full$resample$RMSE)
     )
   ), check.names = FALSE) %>%
   mutate(method = c("Simple bootstrap", "Enhanced bootstrap", 
                     paste(cv_repeat_num, "repeats 10-fold\ncross-validation"))) %>%
   gather(variable, value, -method)

# Draw a plot summarising the results
svg("../img/0043a-boot-v-cv.svg", 8, 5)
direct.label(
summary_results %>%
   mutate(variable = factor(variable, levels = c(
      "Use all variables", "AIC stepwise selection", "Remove collinear variables"
   ))) %>%
   ggplot(aes(y = method, x = value, colour = variable)) +
   geom_point(size = 3) +
   labs(x = "Estimated Root Mean Square Error (higher is worse)\n",
        colour = "Modelling\nstrategy",
        y = "Method of estimating model fit\n",
        caption = "Data from New Zealand Census 2013") +
   ggtitle("Three different validation methods of three different regression strategies",
           subtitle = "Predicting areas' median income based on census variables")
   
)
dev.off()   

