#===================setup=======================
library(showtext)
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

# Fonts:
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont") )

set.seed(123)

# install nzelect package that has census data
devtools::install_github("ellisp/nzelect/pkg")
library(nzelect)

# drop the columns with areas' code and name
au <- AreaUnits2013 %>%
   select(-AU2014, -Area_Code_and_Description)

# give meaningful rownames, helpful for some diagnostic plots later
row.names(au) <- AreaUnits2013$Area_Code_and_Description

# remove some repetition from the variable names
names(au) <- gsub("2013", "", names(au))

# restrict to areas with no missing data.  If this was any more complicated (eg
# imputation),it would need to be part of the validation resampling too; but
# just dropping them all at the beginning doesn't need to be resampled; the only
# implication would be sample size which would be small impact and complicating.
au <- au[complete.cases(au), ]

#==================allocate degrees of freedom===========
# we have about 25 spare degrees of freedom to allocate to curvature and
# interactions.  Lacking theoretical guidance there's too many interactions
# to consider, so we focus on allowing a bit of non-linearity for the variables
# with the highest rank/rank-squared correlation coefficient

sp <- spearman2(MedianIncome ~ ., data = au)
sp <- sp[order(sp[ ,1]), ]


# see http://stackoverflow.com/questions/30627642/issue-with-gam-function-in-r
the_formula <- terms(MedianIncome ~  
                        s(PropFullTimeEmployed, k = 6) + 
                        s(PropInternetHH, k = 6) +
                        s(PropNoQualification, k = 5) +
                        s(PropUnemploymentBenefit, k = 5) +
                        s(PropSmoker, k = 5) +
                        s(PropPartnered, k = 5)  +
                        s(PropManagers, k = 5) +
                        s(PropBachelor, k = 5) +
                        s(PropSelfEmployed, k = 5) +
                        s(PropNoMotorVehicle, k = 4) +
                        s(PropUnemployed, k = 4) +
                        s(PropLabourers, k = 4) +
                        s(PropSeparated, k = 4) +
                        s(PropMaori, k = 4) +
                        s(PropProfServices, k = 4) +
                        s(PropOld, k = 4) +
                        .,
                     data = au)

original_model <- gam(the_formula, data = au)

# bootstrapping
fit_gam <- function(data, i){
   mod1 <- gam(the_formula, data = data[i, ])
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   return(RMSE(predict(mod1, newdata = data), data$MedianIncome))
   
}

png("../img/0044-gam-relations.png", 1000, 900, res = 100)
par(bty = "l", family = "myfont", mar = c(5,4, 2, 1))
plot(original_model, residuals = TRUE, pages = 1, shade = TRUE, 
     seWithMean = TRUE, ylab = "")
grid.text("Impact of regional variables on median income by area unit", 0.5, 0.99,
          gp = gpar(fontfamily = "myfont"))
dev.off()

rmses <- boot(au, statistic = fit_gam, R = 499) # takes a few minutes
boot.ci(rmses, type = "perc")
rmses


#=============inference about a particular parameter of interest=============
# create a data frame with one row, and each column is the trimmed mean of
# the variables in the original dataset
average_data <- data.frame(t(apply(au, 2, mean, tr = 0.2)))

HH_number_impact <- function(data, i){
   compare_data <- average_data %>%
      mutate(NumberInHH = NumberInHH + 0.5)
   mod1 <- gam(the_formula, data = data[i, ])
   comparison <- predict(mod1, newdata = average_data) -
      predict(mod1, newdata = compare_data)
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

png("../img/0044-d1.png", 700, 600, res = 100)
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
png("../img/0044-d2.png", 700, 600, res = 100)
print(d2)
dev.off()

d3 <- ggplot(mod_res, aes(x = Fitted, y = sqrt(abs(Residuals)))) +
   geom_point(alpha = 0.4, colour = "steelblue") +
   geom_text(aes(label = Outlier_Lab)) +
   geom_smooth()
png("../img/0044-d3.png", 700, 600, res = 100)
print(d3)
dev.off()


d4 <- ggplot(mod_res, aes(sample = scale(Residuals))) +
   geom_abline(slope = 1, intercept = 0) +
   stat_qq(color = "steelblue")
png("../img/0044-d4.png", 700, 600, res = 100)
print(d4)
dev.off()




