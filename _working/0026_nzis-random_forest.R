library(showtext)
library(RODBC)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

library(rpart)
library(partykit)
library(rpart.plot)
library(caret)

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

orig <- sqlQuery(PlayPen, sql, stringsAsFactors = FALSE)

# ...so we spread into wider format with one column per ethnicity
nzis <- orig %>%
   mutate(ind = TRUE) %>%
   spread(ethnicity, ind, fill = FALSE) %>%
   select(-survey_id)

for(col in unique(orig$ethnicity)){
   nzis[ , col] <- ifelse(nzis[ , col], "Yes", "No")
}


set.seed(123)
nzis$use <- ifelse(runif(nrow(nzis)) > 0.8, "Test", "Train")
trainData <- nzis %>% filter(use == "Train")
testData <- nzis %>% filter(use == "Test")

testX <- testData %>% select(-income)
testY <- testData$income

#---------------------modelling---------------
set.seed(234)
rpartTune <- train(testX, testY,
                     method = "rpart",
                     tuneLength = 10,
                     trControl = trainControl(method = "cv"))


rpartTree <- rpart(income ~ ., data = testData, 
                   control = rpart.control(cp = 0.00616))



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


