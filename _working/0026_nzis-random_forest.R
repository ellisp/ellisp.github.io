#------------------setup------------------------
library(showtext)
library(RODBC)
library(ggplot2)
library(scales)
library(MASS) # for stepAIC.  Needs to be before dplyr to avoid select namespace clash
library(dplyr)
library(tidyr)

library(rpart)
library(rpart.plot)   # for prp()
library(caret)        # for train()
library(randomForest)
library(partykit)     # for plot(as.party())
library(quantregForest) # for prediction intervals



PlayPen <- odbcConnect("PlayPen_prod")
font.add.google("Poppins", "myfont")
showtext.auto()
sqlQuery(PlayPen, "use nzis11")


#------------------transformation functions------------
# helper functions for transformations of skewed data that crosses zero.  See 
# http://ellisp.github.io/blog/2015/09/07/transforming-breaks-in-a-scale/
.mod_transform <- function(y, lambda){
   if(lambda != 0){
      yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
   } else {
      yt = sign(y) * (log(abs(y) + 1))
   }
   return(yt)
}


.mod_inverse <- function(yt, lambda){
   if(lambda != 0){
      y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
   } else {
      y <- (exp(abs(yt)) - 1) * sign(yt)
      
   }
   return(y)
}

# parameter for reshaping - equivalent to sqrt:
lambda <- 0.5

#---------------------------download and transform data--------------------------
# This query will include double counting of people with multiple ethnicities
sql <-
"SELECT sex, agegrp, occupation, qualification, region, hours, income, 
         a.survey_id, ethnicity FROM
   f_mainheader a                                               JOIN
   d_sex b           on a.sex_id = b.sex_id                     JOIN
   d_agegrp c        on a.agegrp_id = c.agegrp_id               JOIN
   d_occupation e    on a.occupation_id = e.occupation_id       JOIN
   d_qualification f on a.qualification_id = f.qualification_id JOIN
   d_region g        on a.region_id = g.region_id               JOIN
   f_ethnicity h     on h.survey_id = a.survey_id               JOIN
   d_ethnicity i     on h.ethnicity_id = i.ethnicity_id"

orig <- sqlQuery(PlayPen, sql, stringsAsFactors = TRUE)

# ...so we spread into wider format with one column per ethnicity
nzis <- orig %>%
   mutate(ind = TRUE) %>%
   spread(ethnicity, ind, fill = FALSE) %>%
   select(-survey_id) %>%
   mutate(income = .mod_transform(income, lambda = lambda))

for(col in unique(orig$ethnicity)){
   nzis[ , col] <- factor(ifelse(nzis[ , col], "Yes", "No"))
}


names(nzis)[11:14] <- c("MELAA", "Other", "Pacific", "Residual")

set.seed(123)
nzis$use <- ifelse(runif(nrow(nzis)) > 0.8, "Test", "Train")
trainData <- nzis %>% filter(use == "Train") %>% select(-use)
testData <- nzis %>% filter(use == "Test") %>% select(-use)

# traditional R modelling formula versions:
trainX <- trainData %>% select(-income) %>% data.frame()
trainY <- trainData$income
testX <- testData %>% select(-income) %>% data.frame()
testY <- testData$income

# sparse versions with a column for each factor-level combination:
trainX2 <- model.matrix(income ~ ., trainData)[ , -1]
testX2 <- model.matrix(income ~ ., testData)[ , -1]

#---------------------modelling with a single tree---------------
# single tree, with factors all grouped together
set.seed(234)
rpartTune <- train(trainX, trainY,
                     method = "rpart",
                     tuneLength = 10,
                     trControl = trainControl(method = "cv"))


rpartTree <- rpart(income ~ ., data = trainData, 
                   control = rpart.control(cp = rpartTune$bestTune),
                   method = "anova")


node.fun1 <- function(x, labs, digits, varlen){
   paste0("$", round(.mod_inverse(x$frame$yval, lambda = lambda), 0))
}

# exploratory plot only - not for dissemination:
plot(as.party(rpartTree))


svg("../img/0026-polished-tree.svg", 12, 10)
par(fg = "blue", family = "myfont")

prp(rpartTree, varlen = 5, faclen = 7, type = 4, extra = 1, 
    under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
    split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
    branch.col = "grey85", under.col = "lightblue",
    node.fun = node.fun1)

grid.text("New Zealanders' income in one week in 2011", 0.5, 0.89,
          gp = gpar(fontfamily = "myfont", fontface = "bold"))  

grid.text("Other factors considered: qualification, region, ethnicity.",
          0.8, 0.2, 
          gp = gpar(fontfamily = "myfont", cex = 0.8))

grid.text("$ numbers in blue are 'average' weekly income:\nsquared(mean(sign(sqrt(abs(x)))))\nwhich is a little less than the median.",
          0.8, 0.1, 
          gp = gpar(fontfamily = "myfont", cex = 0.8, col = "blue"))

dev.off()

#----------home made random forest--------------
# resample both rows and columns, as in a random forest,
# and draw a picture for each fitted tree.  Knit these
# into an animation.

variables <- c("sex", "agegrp", "occupation", "qualification",
               "region", "hours", "Maori")
n <- nrow(trainData)

home_made_rf <- list()
reps <- 50
set.seed(123)
for(i in 1:reps){
   
   these_variables <- sample(variables, 3, replace = FALSE)
   
   this_data <- trainData[
      sample(1:n, n, replace = TRUE),
      c(these_variables, "income")
   ]
   
   
   
   this_rpartTune <- train(this_data[,1:3], this_data[,4],
                      method = "rpart",
                      tuneLength = 10,
                      trControl = trainControl(method = "cv"))
   
   
   
   home_made_rf[[i]] <- rpart(income ~ ., data = this_data, 
                      control = rpart.control(cp = this_rpartTune$bestTune),
                      method = "anova")
 
   png(paste0("_output/0026_random_forest/", 1000 + i, ".png"), 1200, 1000, res = 100)  
      par(fg = "blue", family = "myfont")
      prp(home_made_rf[[i]], varlen = 5, faclen = 7, type = 4, extra = 1, 
          under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
          split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
          branch.col = "grey85", under.col = "lightblue",
          node.fun = node.fun1, mar = c(3, 1, 5, 1))
      
      grid.text(paste0("Variables available to this tree: ", 
                      paste(these_variables, collapse = ", "))
                , 0.5, 0.90,
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "darkblue"))
      
      grid.text("One tree in a random forest - three randomly chosen predictor variables for weekly income,
resampled observations from New Zealand Income Survey 2011", 0.5, 0.95,
                gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text(i, 0.05, 0.05, gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text("$ numbers in blue are 'average' weekly income:\nsquared(mean(sign(sqrt(abs(x)))))\nwhich is a little less than the median.",
                0.8, 0.1, 
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "blue"))
      
      dev.off()

}   

# knit into an actual animation
old_dir <- setwd("_output/0026_random_forest")
# combine images into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 400 *.png "rf.gif"')
# move the asset over to where needed for the blog
file.copy("rf.gif", "../../../img/0025-rf.gif", overwrite = TRUE)
setwd(old_dir)


#-----------------random forest----------

tuneRF(trainX, trainY, 3, stepFactor = 1)

# need to relabel the columns if going to use these

# with factors grouped together


# train() takes too long
# rf1Tune <- train(trainX, trainY,
#                    method = "rf",
#                    tuneLength = 10,
#                    trControl = trainControl(method = "cv"))


# so we do cross-validation tuning ourselves manually


# First we hold ntree constant and try different values of mtry
# values of m to try for mtry
m <- c(2, 3, 4, 5, 6, 8)

folds <- 3

cvData <- trainData %>%
   mutate(group = sample(1:folds, nrow(trainData), replace = TRUE))

results <- matrix(numeric(length(m) * folds), ncol = folds)

for(i in 1:length(m)){
   message(i)
   for(j in 1:folds){
      
      cv_train <- cvData %>% filter(group != j) %>% select(-group)
      cv_train_x <- cv_train %>% select(-income)
      cv_train_y <- cv_train$income
      cv_test <- cvData %>% filter(group == j) %>% select(-group)
      cv_test_x <- cv_test %>% select(-income)
      cv_test_y <- cv_test$income
      system.time(tmp <- randomForest(cv_train_x, cv_train_y, 
                                      ntree = 500, mtry = m[i])) # about 20 minutes per fit
      preds <- predict(tmp, newdata = cv_test_x)
      results[i, j] <- RMSE(preds, obs = cv_test_y)
      print(paste("mtry", m[i], j, round(results[i, j], 2), sep = " : "))
   }
}

results_df <- as.data.frame(results)
results_df$meanRMSE <- apply(results, 1, mean)
results_df$mtry <- m
results_df %>% arrange(meanRMSE)
results_df %>% ggplot() +
   aes(x = mtry, y = meanRMSE) +
   geom_point() +
   geom_line()
   

rf1 <- randomForest(trainX, trainY, ntree = 1000, mtry = 3)
rf2 <- randomForest(trainX2, trainY, ntree = 1000)

importance(rf1)
importance(rf2)


#-------xgboost------------
library(xgboost)
library(Matrix)
library(data.table)

sparse_matrix <- sparse.model.matrix(income ~ . -1, data = trainData)

# boosting.  After 16 rounds it starts to overfit:
xgb.cv(data = sparse_matrix, label = trainY, nrounds = 25, objective = "reg:linear", nfold = 5)

mod_xg <- xgboost(sparse_matrix, label = trainY, nrounds = 16, objective = "reg:linear")


#---------------------two stage approach-----------
# this is the only method that preserves the bimodal structure of the response
mod1 <- glm((income > 0) ~ ., family = binomial, data = trainData)

trainData2 <- subset(trainData, income != 0)
mod2 <- randomForest(income ~ ., data = trainData2, ntree = 1000, mtry = 3)

prob_pos <- predict(mod1, newdata = testData, type = "response")
pred_inc <- predict(mod2, newdata = testData)

pred_comb <- prob_pos  * pred_inc # specify what elements though


r <- predict(mod2) - trainData2$income
plot(predict(mod2), r)

plot(pred_comb, testY - pred_comb)



#---------------compare predictions on test set--------------------
# baseline linear models
lin_basic <- lm(income ~ ., data = trainData)          # first order only
lin_full  <- lm(income ~ . + . ^ 2, data = trainData)  # second order interactions and polynomials
lin_fullish <- lm(income ~ sex * (agegrp + occupation + qualification + region +
                     hours ^ 2 + Asian + European + Maori + MELAA + 
                     Other + Pacific + Residual),
                  data = trainData)

lin_step <- stepAIC(lin_fullish, k = log(nrow(trainData)))

tree_preds <- predict(rpartTree, newdata = testData)
rf1_preds <- predict(rf1, newdata = testX)
rf2_preds <- predict(rf2, newdata = testX2)
lin_basic_preds <- predict(lin_basic, newdata = testData)
lin_full_preds <- predict(lin_full, newdata = testData)
lin_step_preds <-  predict(lin_step, newdata = testData)
xgboost_pred <- predict(mod_xg, newdata = sparse.model.matrix(income ~ . -1, data = testData))

RMSE(lin_basic_preds, obs = testY) # 21.3
RMSE(lin_full_preds, obs = testY)  # 21.4
RMSE(lin_step_preds, obs = testY)  # 21.2
RMSE(tree_preds, obs = testY)      # 21.4
RMSE(rf1_preds, obs = testY)
RMSE(rf2_preds, obs = testY)
RMSE(xgboost_pred, obs = testY)    # 21.0
RMSE(pred_comb, obs = testY)       # 21.0

#-----------------quantile random forests-------------
# needs more memory than I've got

# 
# system.time(qrf <- quantregForest(trainX2, trainY, importance = TRUE, ntree = 1000))
# 
# p <- predict(qrf, newdata = testX2)
# R2(p, obs = testY)
# 
# dim(testX)
# dim(trainX2)
# b <- cbind(p, smallTrainY) %>% as.data.frame()
# names(b) <- c("low", "median", "high", "actual")
# b <- b %>%  mutate(correct = (actual > low & actual < high))
# summary(b)
# 
# 




#----------------shiny app-------------

d_sex <- sort(as.character(unique(nzis$sex)))
d_agegrp <- sort(as.character(unique(nzis$agegrp)))
d_occupation <- sort(as.character(unique(nzis$occupation)))
d_qualification <- sort(as.character(unique(nzis$qualification)))
d_region <- sort(as.character(unique(nzis$region)))



save(d_sex, d_agegrp, d_occupation, d_qualification, d_region,
     file = "_output/0026-shiny/dimensions.rda")

nzis_shiny <- nzis %>% 
   select(-use) %>%
   mutate(Other = factor(ifelse(Other == "Yes" | Residual == "Yes" | MELAA == "Yes",
                         "Yes", "No"))) %>%
   select(-MELAA, -Residual)
   

mod1_shiny <- glm((income > 0) ~ ., family = binomial, data = nzis_shiny)

nzis_pos <- subset(nzis_shiny, income != 0) 

mod2_shiny <- randomForest(income ~ ., data = nzis_pos, ntree = 1000, mtry = 3, 
                           nodesize = 15, importance = FALSE, replace = FALSE)

res <- predict(mod2_shiny) - nzis_pos$income
nzis_skeleton <- nzis_shiny[0, ]
all_income <- nzis$income

save(mod1_shiny, mod2_shiny, res, nzis_skeleton, all_income, nzis_shiny,
     file = "_output/0026-shiny/models.rda")

#------------------save expensive stuff---------------
save.image("0026-workspace.rdata")