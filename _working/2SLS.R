library(AER) # from Kleiber and Zeileis Applied Econometrics with R (2008)

# lots of datasets:
data(package = "AER")



## data.  This is US state aggregate observations of cigarette sales
data("CigarettesSW", package = "AER")



CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)


model1 <- lm(log(packs) ~ log(rprice) + log(rincome),
             data = CigarettesSW, subset = year == "1995")
summary(model1)


# the problem with this fit is that the price of cigarettes is impacted by the 
# number sold (the more sold the more prices go up).

# The solution is to replace rprice with a predicted value from a first stage model
# based on other variables that are (we hope) exogenous eg tax rates
# 
# rprice ~ tdiff + tax/cpi + rincome
# packs ~ expected(rprice) + rincome

# formula structure for ivreg() is
# y ~ x1 + x2 + ... + xp | z1 + z2 + ....
# x variables are the explanatory ones and z are instruments
# exogenous variables (rincome in the case below) need to be on both sides of the pipe


model2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi),
            data = CigarettesSW, subset = year == "1995")
summary(model2)

coef(model1)
coef(model2)
confint(model1)
confint(model2)




# do it by hand
the_data <- CigarettesSW %>% filter(year == "1995")

the_data$my_log_rprice <- predict(lm(log(rprice) ~ tdiff + I(tax/cpi) + log(rincome),
                            data = the_data))

model3 <- lm(log(packs) ~ my_log_rprice + log(rincome), 
             data = the_data) 
coef(model3)
coef(model2)
coef(model1)

summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)

## ANOVA
model3 <- ivreg(log(packs) ~ log(rprice) | tdiff, data = CigarettesSW, subset = year == "1995")
anova(fm, fm2)
