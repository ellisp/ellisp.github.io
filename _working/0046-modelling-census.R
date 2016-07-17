

#===================setup=======================
library(ggplot2)
library(scales)
library(boot)
library(dplyr)
library(Hmisc) # for spearman2
library(mgcv)  # for gam
library(caret) # for RMSE
library(grid)
library(stringr)
library(ggrepel)

set.seed(123)

# devtools::install_github("ellisp/nzelect/pkg2")
library(nzcensus)

au <- AreaUnits2013 %>%
   # drop the columns with areas' code and name, and the redundant coordinate system
   select(-AU2014, -AU_NAM, -NZTM2000Easting, -NZTM2000Northing) %>%
   # drop some intrinsically collinear variables that would be exactly collinear
   # if it weren't for rounding error and confidentialisation:
   select(-PropWorked40_49hours2013, -Prop35to39_2013, -PropFemale2013)

# give meaningful rownames, helpful for some diagnostic plots later
row.names(au) <- AreaUnits2013$AU_NAM

# remove some repetition from the variable names
names(au) <- gsub("_2013", "", names(au))
names(au) <- gsub("2013", "", names(au))
names(au) <- gsub("Prop", "", names(au))

# restrict to areas with no missing data.  If this was any more complicated (eg
# imputation),it would need to be part of the validation resampling too; but
# just dropping them all at the beginning doesn't need to be resampled; the only
# implication would be sample size which would be small impact and complicating.
au <- au[complete.cases(au), ]



# ind <- sample(1:nrow(au), replace = TRUE)
the_data <- au

X <- the_data %>% select(-MedianIncome)
Y <- the_data$MedianIncome 



#===============scaling==================
# scale the training set so it is mean zero and standard deviation 1, preserving
# the original mean and sd for backtransformation and for applying later to the
# test set
means <- apply(X, 2, mean, na.rm = TRUE)
sds <- apply(X, 2, sd, na.rm = TRUE)
X_scaled <- data.frame(t((t(X) - means) / sds))


X_scaled$Chathams <- X$WGS84Longitude < 0
#=================collinearity================
# Some of the variables are intrinsically co-linear
library(cluster)
library(car) # for vif

X_d <- diana(t(X_scaled))
X_a <- agnes(t(X_scaled))
plot(X_d, which = 2)
plot(X_a, which = 2)

mod <- lm(the_data$MedianIncome ~ ., data = X_scaled , na.action = na.omit)
summary(mod)
vifs <- vif(mod)
sort(vifs)


# some variables naturally go together


reduce_pc <- function(orig, 
                      variables, 
                      pcs = floor(length(variables) / 2), 
                      newname = "PC"){
   tmp <- orig[ , names(orig) %in% variables] 
   new_data <- orig[ , !names(orig) %in% variables]    
   
   oldncol <- ncol(new_data)
   
   tmp_pc <- prcomp(tmp)
   new_data <- cbind(new_data, tmp_pc$x[ , 1:pcs])
   names(new_data)[(oldncol + 1) : ncol(new_data)] <- paste0(newname, 1:pcs)
   return(new_data)
   
}

X_scaled2 <- reduce_pc(X_scaled, c("Asian", "Maori", "Pacific",
                                   "European", "NZBorn"),
                       newname = "EthnicPC")

X_scaled2 <- reduce_pc(orig = X_scaled2, 
                       variables = c("AreChildren", "X20to24", "X25to29", "X30to34", "X40to44",
                                   "X45to49", "X50to54", "X55to59",
                                   "X60to64", "X65AndOlder"),
                       newname = "AgePC")

X_scaled2 <- reduce_pc(orig = X_scaled2,
                       variables = c("Worked10_19hours", "Worked20_29hours", 
                                     "Worked30_39hours", "Worked50_59hours", 
                                     "WorkedOver60hours", "Worked1_9hours"),
                       newname = "WorkingHoursPC")

X_scaled2 <- reduce_pc(orig = X_scaled2,
                       variables = c("SelfEmployed", "Employer", 
                                     "Employee", "SelfEmployedNoEmployees"),
                       newname = "EmploymentPC")

X_scaled2 <- reduce_pc(orig = X_scaled2,
                       variables = c("OwnResidence", "NotOwnedHH"),
                       newname = "ResidencePC")

X_scaled2 <- reduce_pc(orig = X_scaled2,
                       variables = c("UnemploymentBenefit", "Unemployed", "FullTimeEmployed",
                                     "PartTimeEmployed"),
                       newname = "UnemplPC")

X_scaled2 <- reduce_pc(orig = X_scaled2,
                       variables = c("StudentAllowance", "PTStudent", "FTStudent"),
                       newname = "StudentPC")


dim(X_scaled2)
dim(X_scaled)
mod <- lm(the_data$MedianIncome ~ ., data = X_scaled2 , na.action = na.omit)
vifs <- vif(mod)
sort(vifs)

X_cor <- cor(X_scaled2)
sort(abs(X_cor[ , "StudentPC1"]))  # variables most associated with OwnResidence








#==================allocate degrees of freedom===========
# we have about 25 spare degrees of freedom to allocate to curvature and
# interactions.  Lacking theoretical guidance there's too many interactions
# to consider, so we focus on allowing a bit of non-linearity for the variables
# with the highest rank/rank-squared correlation coefficient

data_pc <- cbind(MedianIncome = Y, X_scaled2)

sp <- spearman2(MedianIncome ~ ., data = data_pc)
plot(sp)
sp[order(sp[ ,6]), ]


# see http://stackoverflow.com/questions/30627642/issue-with-gam-function-in-r
the_formula1 <- terms(MedianIncome ~  
                        s(UnemplPC1, k = 6) + 
                        s(InternetHH, k = 6) +
                        s(NoQualification, k = 5) +
                        s(Smoker, k = 5) +
                        s(Partnered, k = 5)  +
                        s(Managers, k = 4) +
                        s(Bachelor, k = 4) +
                        s(NoMotorVehicle, k = 4) +
                        s(Separated, k = 3) +
                        s(Labourers, k = 3) +
                        s(EthnicPC2, k = 3) +
                        s(ProfServices, k = 3) +
                        s(WGS84Longitude, WGS84Latitude) +
                        .,
                     data = data_pc)

gam_model <- gam(the_formula, data = data_pc)
lm_model <- lm(MedianIncome ~ ., data = data_pc)

png("../img/0044-gam-relations.png", 1000, 900, res = 100)
par(bty = "l", family = "myfont", mar = c(5,4, 2, 1))
plot(gam_model, residuals = TRUE, pages = 1, shade = TRUE, 
     seWithMean = TRUE, ylab = "")
grid.text("Impact of regional variables on median income by area unit", 0.5, 0.99,
          gp = gpar(fontfamily = "myfont"))
dev.off()

# bootstrapping
fit_gam1 <- function(data, i){
   # data = data_pc; i = sample(1:nrow(data), nrow(data), replace = TRUE)
   mod1 <- gam(the_formula1, data = data[i, ])
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   RMSE(predict(mod1, newdata = data), data$MedianIncome)
   
}

(1:n)[!(1:n) %in% i]

rmses <- boot(data_pc, statistic = fit_gam, R = 99) # takes a few minutes
boot.ci(rmses, type = "perc")
rmses
plot(rmses)
mean(rmses$t)



#===================lasso=======================
library(glmnet)

foldid <- sample(1:10, nrow(the_data), replace = TRUE)

cv_results <- data_frame(lambda = numeric(), alpha = numeric(), mcve = numeric())
alphas <- seq(from = 0, to = 1, length.out = 9)

for(i in alphas){
   cvfit <- glmnet::cv.glmnet(as.matrix(X), Y, foldid = foldid, alpha = i)
   tmp <- data_frame(lambda = cvfit$lambda, alpha = i, mcve = cvfit$cvm)   
   cv_results <- rbind(cv_results, tmp)
}

arrange(cv_results, mcve)

ggplot(cv_results, aes(x = lambda, y = mcve, colour = as.factor(round(alpha, 3)))) +
   geom_line(size = 2) +
   scale_x_log10() +
   coord_cartesian(ylim = c(2 * 10 ^ 6, 10 ^ 7), xlim = c(1, 1000)) +
   scale_colour_brewer(palette = "Set1")




fit_elastic <- function(data, i){
   # i = sample(1:nrow(data), nrow(data), replace = TRUE)
   Y_orig <- data$MedianIncome
   X_orig <- as.matrix(select(data, -MedianIncome))
   data2 <- data[i, ]
   Y_new <- data2$MedianIncome
   X_new <- as.matrix(select(data2, -MedianIncome))
   lambda <- cv.glmnet (X_new, Y_new, alpha = 0.25)$lambda.min
   mod1 <- glmnet(X_new, Y_new, lambda = lambda, alpha = 0.25)
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   rmse <- RMSE(predict(mod1, newx = X_orig), Y)
   return(rmse)
}

rmses_elastic <- boot(the_data, statistic = fit_elastic, R = 499) # takes a few minutes
plot(rmses_elastic)
mean(rmses_elastic$t)



fit_lm <- function(data, i){
   # i = sample(1:nrow(data), nrow(data), replace = TRUE)
   mod1 <- lm(MedianIncome ~ ., data = data[i, ])
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   rmse <- RMSE(predict(mod1, newdata = data), Y)
   return(rmse)
}
rmses_lm <- boot(the_data, statistic = fit_lm, R = 499) # takes a few minutes
plot(rmses_lm)
mean(rmses_lm$t)


