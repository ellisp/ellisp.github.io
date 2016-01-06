#------------------setup------------------------
library(showtext)
library(RMySQL)
library(ggplot2)
library(scales)
library(MASS) # for stepAIC.  Needs to be before dplyr to avoid "select" namespace clash
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)

library(rpart)
library(rpart.plot)   # for prp()
library(caret)        # for train()
library(partykit)     # for plot(as.party())

library(doMC)         # for multicore processing with caret, on Linux

library(h2o)


library(xgboost)
library(Matrix)
library(data.table)


library(survey) # for rake()


font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(base_family = "myfont"))

PlayPen <- dbConnect(RMySQL::MySQL(), username = "analyst", dbname = "nzis11")


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

orig <- dbGetQuery(PlayPen, sql) 



# ...so we spread into wider format with one column per ethnicity
nzis <- orig %>%
   mutate(ind = TRUE) %>%
   spread(ethnicity, ind, fill = FALSE) %>%
   select(-survey_id) %>%
   mutate(income = .mod_transform(income, lambda = lambda))

for(col in unique(orig$ethnicity)){
   nzis[ , col] <- factor(ifelse(nzis[ , col], "Yes", "No"))
}

# in fact, we want all characters to be factors
for(i in 1:ncol(nzis)){
   if(class(nzis[ , i]) == "character"){
      nzis[ , i] <- factor(nzis[ , i])
   }
}



names(nzis)[11:14] <- c("MELAA", "Other", "Pacific", "Residual")

set.seed(123)
nzis$use <- ifelse(runif(nrow(nzis)) > 0.8, "Test", "Train")
trainData <- nzis %>% filter(use == "Train") %>% select(-use)
testData <- nzis %>% filter(use == "Test") %>% select(-use)

# sparse versions with a column for each factor-level combination:
trainX2 <- model.matrix(income ~ ., trainData)[ , -1]
testX2 <- model.matrix(income ~ ., testData)[ , -1]

#---------------------modelling with a single tree---------------
# single tree, with factors all grouped together
set.seed(234)

# set up parallel processing to make this faster, for this and future use of train()
registerDoMC(cores = 3)
rpartTune <- train(income ~., data = trainData,
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

commentary <- str_wrap(c(
   "This animation illustrates the use of an ensemble of regression trees to improve estimates of income based on a range of predictor variables.",
   "Each tree is fitted on a resample with replacement from the original data; and only three variables are available to the tree.",
   "The result is that each tree will have a different but still unbiased forecast for a new data point when a prediction is made.  Taken together, the average prediction is still unbiased and has less variance than the prediction of any single tree.",
   "This method is similar but not identical to a Random Forest (tm).  In a Random Forest, the choice of variables is made at each split in a tree rather than for the tree as a whole."
   ), 50)


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
      
      grid.text("One tree in a random spinney - three randomly chosen predictor variables for weekly income,
resampled observations from New Zealand Income Survey 2011", 0.5, 0.95,
                gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text(i, 0.05, 0.05, gp = gpar(fontfamily = "myfont", cex = 1))
      
      grid.text("$ numbers in blue are 'average' weekly income:\nsquared(mean(sign(sqrt(abs(x)))))\nwhich is a little less than the median.",
                0.8, 0.1, 
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "blue"))
      
      comment_i <- floor(i / 12.5) + 1
      
      grid.text(commentary[comment_i], 
                0.3, 0.1,
                gp = gpar(fontfamily = "myfont", cex = 1.2, col = "orange"))
      
      dev.off()

}   

# knit into an actual animation
old_dir <- setwd("_output/0026_random_forest")
# combine images into an animated GIF
system('convert -loop 0 -delay 400 *.png "rf.gif"') # linux only; in Windows need full file path to convert
# move the asset over to where needed for the blog
file.copy("rf.gif", "../../../img/0026-rf.gif", overwrite = TRUE)
setwd(old_dir)


#-----------------random forest----------
h2o.init(nthreads = 4, max_mem_size = "3G")

# Hold ntree constant and try different values of mtry
# values of m to try for mtry
m <- c(1, 2, 3, 4, 5, 6)

folds <- 10

cvData <- trainData %>%
   mutate(group = sample(1:folds, nrow(trainData), replace = TRUE))

results <- matrix(numeric(length(m) * folds), ncol = folds)

var_names <- names(trainData)[!names(trainData) == "income"]

# note - this next section isn't efficient use of h2o because I ported it from something
# much less powerful.  But it does the job fine, and I like to do the CV by hand sometimes.
for(i in 1:length(m)){
   message(i)
   for(j in 1:folds){
      
      cv_train <- cvData %>% filter(group != j) %>% select(-group) %>% as.h2o()
      cv_test <- cvData %>% filter(group == j) %>% select(-group) %>% as.h2o()
      
      system.time(tmp <- h2o.randomForest(x = var_names, y = "income", 
                                          training_frame = cv_train,
                                          validation_frame = cv_test, 
                                          ntrees = 100,
                                          mtries = m[i])) # about 26 seconds - compared to 20 minutes per fit for {randomForest}
      results[i, j] <- sqrt(tmp@model$validation_metrics@metrics$MSE)
      print(paste("mtry", m[i], j, round(results[i, j], 2), sep = " : "))
   }
}

results_df <- as.data.frame(results)
results_df$mtry <- m

svg("../img/0026-rf-cv.svg", 6, 4)
print(
   results_df %>% 
   gather(trial, RMSE, -mtry) %>% 
   ggplot() +
   aes(x = mtry, y = RMSE) +
   geom_point() +
   geom_smooth(se = FALSE) +
   ggtitle(paste0(folds, "-fold cross-validation for random forest;\ndiffering values of mtry"))
)
dev.off()   

trainData_h2o <- as.h2o(trainData)
rf <- h2o.randomForest(x = var_names, y = "income", 
                    training_frame = trainData_h2o, 
                    ntrees = 1000, 
                    mtries = 2)


# importances
p1 <- rf@model$variable_importances %>%
   arrange(percentage) %>%
   mutate(variable = factor(variable, levels = variable)) %>%
   ggplot(aes(x = percentage, y = variable)) + 
   geom_point() +
   labs(x = "Percentage importance contribution to estimating income", 
        title = "Variables in the random forest")


p2 <- rf@model$scoring_history %>%
   mutate(RMSE = sqrt(training_MSE)) %>%
   ggplot(aes(x = number_of_trees, y = RMSE)) +
   geom_line() +
   labs(x = "Number of trees", y = "Root Mean Square Error",
        title = "Training scoring history of random forest")

svg("../img/0026-rf.svg", 8, 5)
   grid.arrange(p1, p2, ncol = 2)   
dev.off()


#-------xgboost------------
sparse_matrix <- sparse.model.matrix(income ~ . -1, data = trainData)

# boosting.  After 16 rounds it starts to overfit:
xgb.cv(data = sparse_matrix, label = trainY, nrounds = 25, objective = "reg:linear", nfold = 5)

mod_xg <- xgboost(sparse_matrix, label = trainY, nrounds = 16, objective = "reg:linear")


#---------------------two stage approach-----------
# this is the only method that preserves the bimodal structure of the response
trainData2 <- trainData %>%
   mutate(income = factor(income != 0)) %>%
   as.h2o()

mod1 <- h2o.randomForest(x = var_names, y = "income",
                         training_frame = trainData2,
                         ntrees = 1000)

trainData3 <- trainData %>% filter(income != 0) %>% as.h2o()
mod2 <- h2o.randomForest(x = var_names, y = "income",
                         training_frame = trainData3, 
                         ntrees = 1000, 
                         mtries = 3)


# best point estimate for an individual is the probability of any income, multiplied by
# the expected income given that they have any income
prob_inc <- predict(mod1, newdata = as.h2o(select(testData, -income)), type = "response")[ , "TRUE"]
pred_inc <- predict(mod2, newdata = as.h2o(testData))
pred_comb <- as.vector(prob_inc  * pred_inc)

# Note that this is different to how we eventually use this 2 stage model for estimating an
# overall distribution.



#---------------compare predictions on test set--------------------
# baseline linear models
lin_basic <- lm(income ~ sex + agegrp + occupation + qualification + region +
                   sqrt(hours) + Asian + European + Maori + MELAA + Other + Pacific + Residual, 
                data = trainData)          # first order only
lin_full  <- lm(income ~ (sex + agegrp + occupation + qualification + region +
                   sqrt(hours) + Asian + European + Maori + MELAA + Other + Pacific + Residual) ^ 2, 
                data = trainData)  # second order interactions and polynomials
lin_fullish <- lm(income ~ (sex + Maori) * (agegrp + occupation + qualification + region +
                     sqrt(hours)) + Asian + European + MELAA + 
                     Other + Pacific + Residual,
                  data = trainData) # selected interactions only

lin_step <- stepAIC(lin_fullish, k = log(nrow(trainData))) # bigger penalisation for parameters given large dataset

tree_preds <- predict(rpartTree, newdata = testData)
# home made random decision tree preds go here
rf_preds <- as.vector(predict(rf, newdata = as.h2o(testData)))
lin_basic_preds <- predict(lin_basic, newdata = testData)
lin_full_preds <- predict(lin_full, newdata = testData)
lin_step_preds <-  predict(lin_step, newdata = testData)
xgboost_pred <- predict(mod_xg, newdata = sparse.model.matrix(income ~ . -1, data = testData))
# and we already worked out the combined model predictions

RMSE(lin_basic_preds, obs = testY) # 22.09
RMSE(lin_full_preds, obs = testY)  # 22.09
RMSE(lin_step_preds, obs = testY)  # 21.93
RMSE(tree_preds, obs = testY)      # 21.64
RMSE(rf_preds, obs = testY)        # 21.65 - something definitely wrong...
RMSE(xgboost_pred, obs = testY)    # 21.57
RMSE(pred_comb, obs = testY)       # 21.78


plot(density(.mod_inverse(pred_comb, lambda)), xlim = c(-1000, 4000), col = 2)
lines(density(.mod_inverse(nzis$income, lambda)), xlim = c(-1000, 4000), col = 1)
lines(density(.mod_inverse(rf_preds, lambda)), xlim = c(-1000, 4000), col = 3)
lines(density(.mod_inverse(xgboost_pred, lambda)), xlim = c(-1000, 4000), col = 4)
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
   
for(col in c("European", "Asian", "Maori", "Other", "Pacific")){
   nzis_shiny[ , col]   <- ifelse(nzis_shiny[ , col] == "Yes", 1, 0)
   }



# income a binomial response for first model
nzis_rf <- nzis_shiny %>%  mutate(income = factor(income !=0))
mod1_shiny <- randomForest(income ~ ., data = nzis_rf,
                           ntree = 500, importance = FALSE, mtry = 3, nodesize = 5)
save(mod1_shiny, file = "_output/0026-shiny/mod1.rda")

nzis_nonzero <- subset(nzis_shiny, income != 0) 

mod2_shiny <- randomForest(income ~ ., data = nzis_nonzero, ntree = 500, mtry = 3, 
                           nodesize = 10, importance = FALSE, replace = FALSE)

res <- predict(mod2_shiny) - nzis_pos$income
nzis_skeleton <- nzis_shiny[0, ]
all_income <- nzis$income

save(mod2_shiny, res, nzis_skeleton, all_income, nzis_shiny,
   file = "_output/0026-shiny/models.rda")


#---------------population--------

nzis_pop <- expand.grid(d_sex, d_agegrp, d_occupation, d_qualification, d_region,
                        c(1, 0), c(1, 0), c(1, 0), c(1, 0), c(1, 0))
names(nzis_pop) <-  c("sex", "agegrp", "occupation", "qualification", "region",
                      "European", "Maori", "Asian", "Pacific", "Other")
nzis_pop$count <- 0
for(col in c("European", "Asian", "Maori", "Other", "Pacific")){
 nzis_pop[ , col]   <- as.numeric(nzis_pop[ , col])
   
}

nzis_pop <- nzis_shiny %>%
   select(-hours, -income) %>%
   mutate(count = 1) %>%
   rbind(nzis_pop) %>%
   group_by(sex, agegrp, occupation, qualification, region, 
             European, Maori, Asian, Pacific, Other) %>%
   summarise(count = sum(count)) %>%
   ungroup() %>%
   mutate(Ethnicities = European + Maori + Asian + Pacific + Other) %>%
   filter(Ethnicities %in% 1:2) %>%
   select(-Ethnicities)

# this pushes my little 4GB of memory to its limits
 mod3 <- glm(count ~ (sex + Maori) * (agegrp + occupation + qualification) + region + 
                Maori:region + occupation:qualification + agegrp:occupation +
                agegrp:qualification, 
             data = nzis_pop, family = poisson)
 
 nzis_pop$pop <- predict(mod3, type = "response")

# total population should be (1787 + 1410) * 1000 = 319700.  But we also want
# the marginal totals (eg all men, or all women) to match the sum of weights
# in the NZIS (where wts = 319700 / 28900 = 1174).  So we use the raking method
# for iterative proportional fitting of survey weights

 
 
 h2o.shutdown(prompt = F) 

wt <- 1174

sex_pop <- nzis_shiny %>%
   group_by(sex) %>%
   summarise(freq = length(sex) * wt)

agegrp_pop <- nzis_shiny %>%
   group_by(agegrp) %>%
   summarise(freq = length(agegrp) * wt)

occupation_pop <- nzis_shiny %>%
   group_by(occupation) %>%
   summarise(freq = length(occupation) * wt)

qualification_pop <- nzis_shiny %>%
   group_by(qualification) %>%
   summarise(freq = length(qualification) * wt)

region_pop <- nzis_shiny %>%
   group_by(region) %>%
   summarise(freq = length(region) * wt)

European_pop <- nzis_shiny %>%
   group_by(European) %>%
   summarise(freq = length(European) * wt)

Asian_pop <- nzis_shiny %>%
   group_by(Asian) %>%
   summarise(freq = length(Asian) * wt)

Maori_pop <- nzis_shiny %>%
   group_by(Maori) %>%
   summarise(freq = length(Maori) * wt)

Pacific_pop <- nzis_shiny %>%
   group_by(Pacific) %>%
   summarise(freq = length(Pacific) * wt)

Other_pop <- nzis_shiny %>%
   group_by(Other) %>%
   summarise(freq = length(Other) * wt)

nzis_svy <- svydesign(~1, data = nzis_pop, weights = ~pop)

nzis_raked <- rake(nzis_svy,
                   sample = list(~sex, ~agegrp, ~occupation, 
                                 ~qualification, ~region, ~European,
                                 ~Maori, ~Pacific, ~Asian, ~Other),
                   population = list(sex_pop, agegrp_pop, occupation_pop,
                                     qualification_pop, region_pop, European_pop,
                                     Maori_pop, Pacific_pop, Asian_pop, Other_pop),
                   control = list(maxit = 20, verbose = FALSE))

nzis_pop$pop <- weights(nzis_raked)

save(nzis_pop, file = "_output/0026-shiny/nzis_pop.rda")
shinyapps::deployApp("_output/0026-shiny", "NZIS", account = "ellisp")

#------------------save expensive stuff---------------



save.image("0026-workspace.rdata")
