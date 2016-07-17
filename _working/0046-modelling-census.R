

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

#=============inference about a particular parameter of interest=============
# create a data frame with one row, and each column is the trimmed mean of
# the variables in the original dataset
average_data <- data.frame(t(apply(au, 2, mean, tr = 0.2)))

HH_number_impact <- function(data, i){
   comparison_data <- average_data %>%
      mutate(NumberInHH = NumberInHH + 0.5)
   mod1 <- gam(the_formula, data = data[i, ])
   comparison <- predict(mod1, newdata = average_data) -
      predict(mod1, newdata = comparison_data)
   return(comparison)
}

HH_number_boot <- boot(au, HH_number_impact, R = 499) # takes 10+ minutes
boot.ci(HH_number_boot, type = "perc")

#==============identification of interesting outliers=================
mod_res <- data_frame(
   Fitted = fitted(original_model),
   Actual = au$MedianIncome,
   Residuals = residuals(original_model),
   Location = str_sub(row.names(au), start = 8)
) %>%
   mutate(
      Outlier = abs(Residuals) > 6500 | 
         Actual < quantile(Actual, 0.001) | 
         Actual > quantile(Actual, 0.999),
      Outlier_Lab = ifelse(Outlier, Location, "")
   )


d1 <- ggplot(mod_res, aes(x = Fitted, y = Residuals)) +
   geom_hline(yintercept = 0) +
   geom_smooth() +
   geom_point(alpha = 0.5, colour = "steelblue") +
   geom_text_repel(data = filter(mod_res, Outlier), aes(label = Outlier_Lab)) +
   scale_y_continuous("Difference between real and predicted median income", label = dollar) +
   scale_x_continuous("Predicted median income", label = dollar)

png("../img/0046-d1.png", 700, 600, res = 100)
print(d1)
dev.off()

d2 <- ggplot(mod_res, aes(x = Fitted, y = Actual)) +
   geom_abline(intercept = 0, slope = 1, colour = "skyblue") +
   geom_point(alpha = 0.5, colour = "steelblue") +
   geom_text_repel(data = filter(mod_res, Outlier), aes(label = Outlier_Lab),
                   colour = "chocolate4") +
   scale_y_continuous("Actual median individual income\n", label = dollar) +
   scale_x_continuous("\nPredicted median individual income, based on 53 census variables", label = dollar) +
   coord_equal() +
   ggtitle("Individual income by area unit in the 2013 New Zealand Census")
png("../img/0046-d2.png", 700, 600, res = 100)
print(d2)
dev.off()

d3 <- ggplot(mod_res, aes(x = Fitted, y = sqrt(abs(Residuals)))) +
   geom_point(alpha = 0.4, colour = "steelblue") +
   geom_text(aes(label = Outlier_Lab)) +
   geom_smooth()
png("../img/0046-d3.png", 700, 600, res = 100)
print(d3)
dev.off()


d4 <- ggplot(mod_res, aes(sample = scale(Residuals))) +
   geom_abline(slope = 1, intercept = 0) +
   stat_qq(color = "steelblue")
png("../img/0046-d4.png", 700, 600, res = 100)
print(d4)
dev.off()




