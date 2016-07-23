
# repurpose this so it is comparing lm (all variables), glmnet, and gam.  
# Illustrates that there are two problems to deal with  - collinearity
# and hence interpreting coefficients; and curvature in relationships.


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
library(glmnet)
library(maps) # for country borders on the smoothing plot

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

# restrict to areas with no missing data.  An improvement for later
# is to use imputation and include this step within the validation.
au <- au[complete.cases(au), ]

# give the data a generic name for ease of copying and pasting
the_data <- au

# we need a dummy variable for the Chathams because it's extreme value of longitude
# makes any spatial variables otherwise highly problematic.
# the_data$chathams <- the_data$WGS84Longitude < 100
the_data <- the_data[the_data$WGS84Longitude > 100, ]

names(the_data) <- make.names(names(the_data))


#==========Method 1 - lm================
fit_lm <- function(data, i){
   mod1 <- lm(MedianIncome ~ ., data = data[i, ])
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   RMSE(predict(mod1, newdata = data), data$MedianIncome)
   
}

fit_lm(data = the_data, i = 1:nrow(the_data))

# use bootstrap validation for an unbiaased estimate of root mean square error
rmses_lm_boot <- boot(the_data, statistic = fit_lm, R = 99)
lm_rmse <- mean(rmses_lm_boot$t)

# save a single version of this model for later
mod_lm <- lm(MedianIncome ~ ., data = the_data)

# There's a bit of curvature in the residuals, and the
# residuals have heavy tails, so any inference should
# not rely on normality assumption
svg("../img/0046-lm-diagnostics.svg", 7, 6)
par(mfrow = c(2, 2), family = "myfont")
plot(mod_lm)
dev.off()

#===================elastic net=======================
# A lasso, ridge rigression, or elastic net (which combines the two) is a way
# of dealing with the collinearity by forcing some coefficients to shrink (possibly to zero)
# while doing minimal damage to the inferential qualities of the rest and to the overall model fit.

# First we need to decide between ridge regression and lasso or elastic net (between the two)
# define folds for cross validation so can check the impact of different values of alpha 
set.seed(124)
foldid <- sample(1:10, nrow(the_data), replace = TRUE)

# separate out the explanatory from response variable
# for when using lasso and ridge regression
X <- the_data %>% select(-MedianIncome)
Y <- the_data$MedianIncome 


cv_results <- data_frame(lambda = numeric(), alpha = numeric(), mcve = numeric())
alphas <- seq(from = 0, to = 1, length.out = 9)

for(i in alphas){
   cvfit <- cv.glmnet(as.matrix(X), Y, foldid = foldid, alpha = i)
   tmp <- data_frame(lambda = cvfit$lambda, alpha = i, mcve = cvfit$cvm)   
   cv_results <- rbind(cv_results, tmp)
}

arrange(cv_results, mcve) 
# best alpha with this see is 0.75 but right combination of alpha and lambda
# works pretty well for any alpha

# I take square root of mcve so it is back on same scale as RMSE used elsewhere in this post

svg("../img/0046-alpha-lambda.svg", 7, 5)
print(
   ggplot(cv_results, aes(x = lambda, y = sqrt(mcve), colour = as.factor(round(alpha, 3)))) +
   geom_line(size = 2) +
   geom_line(size = 0.2, colour = "grey50", aes(group = as.factor(round(alpha, 3)))) +
   scale_x_log10() +
   coord_cartesian(ylim = c(1800, 4000), xlim = c(5, 1000)) +
   scale_colour_brewer("alpha", palette = "Greens", guide = guide_legend(reverse = TRUE)) +
   ggtitle("Cross-validation to select hyper parameters\nin elastic net regression") +
   scale_y_continuous("Square root of mean cross validation error", label = comma) +
   theme(legend.position = "right")
)
dev.off()




fit_elastic <- function(data, i){
   # i = sample(1:nrow(data), nrow(data), replace = TRUE)
   Y_orig <- data$MedianIncome
   X_orig <- as.matrix(select(data, -MedianIncome))
   data2 <- data[i, ]
   Y_new <- data2$MedianIncome
   X_new <- as.matrix(select(data2, -MedianIncome))
   lambda <- cv.glmnet (X_new, Y_new, alpha = 0.85)$lambda.min
   mod1 <- glmnet(X_new, Y_new, lambda = lambda, alpha = 0.85)
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   rmse <- RMSE(predict(mod1, newx = X_orig), Y)
   return(rmse)
}

rmses_elastic_boot <- boot(data = the_data, statistic = fit_elastic, R = 99) # takes a few minutes
elastic_rmse <- mean(rmses_elastic_boot$t)

lambda <- cv.glmnet (as.matrix(X), Y, alpha = 0.25)$lambda.min
mod_elastic <- glmnet(as.matrix(X), Y, lambda = lambda, alpha = 0.25)
coefs <- data.frame(lm = coef(mod_lm), elastic = as.vector(coef(mod_elastic)))
coefs$variable <- row.names(coefs)

svg("../img/0046-compare-coefs.svg", 9, 7)
print(
   ggplot(coefs, aes(x = lm, y = elastic, label = variable)) + 
   geom_abline(slope = 1, intercept = 0) +
   geom_point() +
   geom_text_repel(colour = "steelblue") +
   labs(x = "Coefficient from ordinary least squares",
        y = "Coefficient after shrinkage from elastic net regularization") +
   coord_equal()
)
dev.off()

#==================gam===========
# Allocate degrees of freedom.
# We have about 20-60 spare degrees of freedom to allocate to curvature and
# interactions.  Lacking theoretical guidance there's too many interactions
# to consider, so we focus on allowing a bit of non-linearity for the variables
# with the highest rank/rank-squared correlation coefficient, and controlling for
# spatial correlation in the residuals.

sp <- spearman2(MedianIncome ~ ., data = the_data)
sp[order(-sp[ ,6])[1:15], ]


# see http://stackoverflow.com/questions/30627642/issue-with-gam-function-in-r
# for why the data needs to be specified in both terms() and in gam()
the_formula <- terms(MedianIncome ~  
                        s(FullTimeEmployed, k = 6) + 
                        s(InternetHH, k = 6) +
                        s(NoQualification, k = 5) +
                        s(UnemploymentBenefit, k = 5) +
                        s(Smoker, k = 5) +
                        s(Partnered, k = 5)  +
                        s(Managers, k = 4) +
                        s(Bachelor, k = 4) +
                        s(SelfEmployed, k = 4) +
                        s(NoMotorVehicle, k = 4) +
                        s(Unemployed, k = 3) +
                        s(Labourers, k = 3) +
                        s(Worked50_59hours, k = 3) +
                        s(Separated, k = 3) +
                        s(Maori, k = 3) +
                        s(WGS84Longitude, WGS84Latitude) +
                        .,
                     data = the_data)

gam_model <- gam(the_formula, data = the_data)

plot_gam <- function(){
   par(bty = "l", family = "myfont", mar = c(5,4, 2, 1))
   plot(gam_model, residuals = TRUE, pages = 1, shade = TRUE, 
        seWithMean = TRUE, ylab = "")
   grid.text("Impact of area average variables on median income by area unit (New Zealand census 2013)", 0.5, 0.99,
             gp = gpar(fontfamily = "myfont"))
}

png("../img/0046-gam-relations.png", 1000, 900, res = 100)
plot_gam()
dev.off()

svg("../img/0046-gam-relations.svg", 10, 9)
plot_gam()
dev.off()

png("../img/0046-gam-spatial-residuals.png", 600, 800, res = 100)
par(bty = "l", family = "myfont", fg = "white")
plot(gam_model, shade = TRUE, select = 16, rug = TRUE, se = FALSE, scheme = 2, col = topo.colors(100), 
     pch = 1, ylab = "Latitude", xlab = "Longitude", main = "Spatial pattern in regression of income\non demographic area variables")
map(add = TRUE, col = "grey75")
dev.off()


# bootstrapping
fit_gam <- function(data, i){
   mod1 <- gam(the_formula, data = data[i, ])
   # use the model based on resample on the original data to estimate how
   # good or not it is:
   RMSE(predict(mod1, newdata = data), data$MedianIncome)
   
}

rmses_gam_boot <- boot(data = the_data, statistic = fit_gam, R = 99) # takes a few minutes
plot(rmses_gam_boot)
gam_rmse <- mean(rmses_gam_boot$t)



#=======================summary===============
# the elastic approach has a slight cost in RMSE which pays for the better estimates
# of coefficients.  The GAM has much better RMSE because it fits the curvature of the 
# relationships better.  But it doesn't deal with the collinearity (and concurvity).
data.frame(lm_rmse, elastic_rmse, gam_rmse)

# further improvements: imputation; and gamsel to apply the elastic approach to the GAM


