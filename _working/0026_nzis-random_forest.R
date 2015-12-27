library(showtext)
library(RODBC)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

PlayPen <- odbcConnect("PlayPen_prod")
font.add.google("Poppins", "myfont")
showtext.auto()
sqlQuery(PlayPen, "use nzis11")


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
   select(-survey_id)

for(col in unique(orig$ethnicity)){
   nzis[ , col] <- factor(ifelse(nzis[ , col], "Yes", "No"))
}

# names(nzis) <- make.names(names(nzis))

set.seed(123)
nzis$use <- ifelse(runif(nrow(nzis)) > 0.8, "Test", "Train")
trainData <- nzis %>% filter(use == "Train") %>% select(-use)
testData <- nzis %>% filter(use == "Test") %>% select(-use)

testX <- testData %>% select(-income) %>% data.frame()
testY <- testData$income

#---------------------modelling---------------
# single tree, with factors all grouped together
set.seed(234)
rpartTune <- train(testX, testY,
                     method = "rpart",
                     tuneLength = 10,
                     trControl = trainControl(method = "cv"))


rpartTree <- rpart(income ~ ., data = testData, 
                   control = rpart.control(cp = 0.00616),
                   method = "anova")



svg("_output/tree.svg", 12, 10)
par(fg = "blue", family = "myfont")

prp(rpartTree, varlen = 5, faclen = 7, type = 4, extra = 1, 
    under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
    split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
    branch.col = "grey85", under.col = "lightblue",
    prefix = "$")

grid.text("New Zealanders' income in one week in 2011", 0.5, 0.89,
          gp = gpar(fontfamily = "myfont", fontface = "bold"))  

grid.text("Other factors considered: sex, qualification.",
          0.2, 0.2, 
          gp = gpar(fontfamily = "myfont", cex = 0.8))

dev.off()

#----------home made random forest--------------

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
 
   png(paste0("_output/0025_random_forest/", 1000 + i, ".png"), 1200, 1000)  
      par(fg = "blue", family = "myfont")
      prp(home_made_rf[[i]], varlen = 5, faclen = 7, type = 4, extra = 1, 
          under = TRUE, tweak = 0.9, box.col = "grey95", border.col = "grey92",
          split.font = 1, split.cex = 0.8, eq = ": ", facsep = " ",
          branch.col = "grey85", under.col = "lightblue",
          prefix = "$", mar = c(2, 1, 6, 1))
      
      grid.text(paste0("Variables available to this tree: ", 
                      paste(these_variables, collapse = ", "))
                , 0.5, 0.90,
                gp = gpar(fontfamily = "myfont", cex = 0.8, col = "darkblue"))
      
      grid.text("One tree in a random forest - three randomly chosen predictor variables for weekly income,
resampled observations from New Zealand Income Survey 2011", 0.5, 0.95,
                gp = gpar(fontfamily = "myfont", cex = 1))
      
   dev.off()

}   




old_dir <- setwd("_output/0025_random_forest")
# combine images into an animated GIF
system('"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert" -loop 0 -delay 400 *.png "rf.gif"')
# move the asset over to where needed for the blog
file.copy("rf.gif", "../../../img/0025-rf.gif", overwrite = TRUE)
setwd(old_dir)


#-----------------random forest----------
X <- model.matrix(income ~ ., testData)[ , -1]
# need to relabel the columns if going to use these

rf1Tune <- train(testX, testY,
                   method = "rf",
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))

# with factors grouped together
rf1 <- randomForest(testX, testY, ntree = 1000)

# with factors as individual columns
rf2 <- randomForest(X, testY, ntree = 1000)

importance(rf1)
importance(rf2)

