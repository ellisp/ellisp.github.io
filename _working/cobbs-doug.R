library(mbieDBmisc)
library(nlme)
library(dplyr)
library(forecast)
library(Cairo)

TRED <- odbcConnect("TRED_Prod")

hours <- ImportTS2(TRED, "Total Paid Hours by Industry (ANZSIC06) (Qrtly-Mar/Jun/Sep/Dec)") %>%
   # Change the name of two industries that we don't have GDP on:
   mutate(Industry = rename.levels(CV1, 
                 orig = c("Accommodation and Food Services", "Retail Trade"),
                 new = rep("Retail Trade and Accommodation", 2))) %>%
   # Convert quarterly dates to Year ending March:
   mutate(YEMar = YearEnd(TimePeriod, 3)) %>%
   # Aggregate up over the revised industries and the YE March:
   group_by(YEMar, Industry) %>%
   summarise(L = sum(Value)) %>%
   ungroup() %>%
   # drop the first year, which doesn't have a full four quarters:
   filter(YEMar != min(YEMar))


# Net Capital Stock, real
capital <- ImportTS2(TRED, "SNE - Series, Balance sheet items, Chain Volume, Actual, ANZSIC06 high-level industry (Annual-Mar)") %>%
   rename(Industry = CV2, K = Value) %>%
   mutate(YEMar = YearEnd(TimePeriod, 3)) %>%
   select(YEMar, Industry, K)

# GDP production measure
gdp <- ImportTS2(TRED, "SNE - Series, GDP(P), Chain volume, Actual, ANZSIC06 high-level industry groups (Annual-Mar)") %>%
   rename(Industry = CV2, Y = Value) %>%
   mutate(YEMar = YearEnd(TimePeriod, 3)) %>%
   select(YEMar, Industry, Y)


levels(hours$Industry) %in% levels(capital$Industry) # don't all match.  eg no Agriculture.
levels(gdp$Industry) %in% levels(capital$Industry)

good_inds <- levels(gdp$Industry)[levels(gdp$Industry) %in% levels(hours$Industry)]

nz_econ <- hours %>%
   left_join(capital) %>%
   left_join(gdp) %>%
   filter(Industry %in% good_inds) %>%
   # remove 2015 which is an NA:
   filter(!is.na(K)) %>%
   arrange(YEMar) 

#=========================analysis with the Cobbs Douglas production function========
# background - google...


# pick one industry to fit to.  For starters we'll use the whole economy:
this_data <- nz_econ %>% 
   filter(Industry == "Total All Industries") 

# note - you can index all the data to 1990 = 100 if you want, but get the same
# coefficients:
# this_data <- nz_econ %>% 
#    filter(Industry == "Total All Industries") %>%
#    mutate(Y = Index(Y),
#           K = Index(K),
#           L = Index(L))

pairs(this_data[ , c("Y", "K", "L")], panel = "lines")


#-----------Fitting the original Cobbs-Douglas model--------
# terminology a bit arbitrary, I happen to be using same as http://www.srabbani.com/cobb_douglas.pdf 
# the original Cobbs-Douglas model was:
# Y = A . K ^ alpha . L ^ (1 - alpha)
# This means (by assumption) there are constant returns to scale; and constant shares
# of the economy's income spent on capital and labour.  At the time (1930s - 50s)
# this was thought to be realistic, based on fairly sketchy analysis of sketchy US data


# which can be manipulated as follows
#          log(Y) = log(A . K ^ alpha . L ^ (1 - alpha))
#                 = log(A) + alpha(log(K)) + (1 - alpha)log(L)
#                 = log(A) + alpha(log(K)) + log(L) - alpha(log(L))
# log(Y) - log(L) = log(A) + alpha(log(K) - log(L))

this_data2 <- this_data %>%
   mutate(logYminuslogL = log(Y) - log(L),
          logKminuslogL = log(K) - log(L))

# this is how Cobbs and Douglas did it (I think) - using OLS:
model1 <- lm(logYminuslogL ~ logKminuslogL, data = this_data2)
summary(model1) # returns estimate of alpha > 1, not possible
# however, the residuals are very clearly autocorrelated, so OLS from lm()
# not the appropriate way to have fitted it (note however it is very common
# to do so!)
acf(residuals(model1), type = "partial")

# so we need to control for the autocorrelation.  As we learnt in the time series block,
# two main ways to do this
model2 <- gls(logYminuslogL ~ logKminuslogL, data = this_data2, correlation = corAR1())
model2

model2a <- auto.arima(this_data2$logYminuslogL, xreg = this_data2$logKminuslogL)
model2a

# various values of alpha, which is the share of capital
coef(model1) # 1.07, definitely wrong
coef(model2) # 0.47, more plausible
coef(model2a)# 0.34

# confidence intervals of the two models are wide, and overlap:
confint(model2a)
confint(model2)
# so we won't worry which model is "right"

#-------------Fitting a more flexible model with variable return to scale----------
# Eventually it was noticed that the shares of labour and capital changed,
# and though implausible that there were constant returns to scale.  So
# we have a more flexible version:
#          Y = A . K ^ alpha . L ^ beta
# If alpha + beta = 1, we have the original (constant returns to scale ie 
# doubling K and L means double the production).
# If alpha + beta > 1, we have increasing returns to scale
# If alpha + beta < 1, we have decreasing returns to scale
#
# note that increasing returns to scale can different to output elasticity to
# a change in the input - which is constant at alpha (for capital) and beta (for labour)
# as you see in the partial deriviatives the equation in line 118.  See
# http://economicpoint.com/production-function/cobb-douglas 

# we don't need as much manipulation of the data to fit this version:
model3 <- lm(log(Y) ~ log(K) + log(L), data = this_data)
acf(ts(residuals(model3)), type = "partial") # looks like AR1


model4 <- gls(log(Y) ~ log(K) + log(L), data = this_data, correlation = corAR1())
model4a <- auto.arima(log(this_data$Y), xreg = log(this_data[ , c("K", "L")]))

coef(model3) # notice how different the OLS estimates are from those that treat the time series properly
coef(model4)
coef(model4a) # very similar to 4
confint(model4) # confidence intervals very wide, as only 25 years of autocorrelated data.  

#---------------------generalising for all industries------------

# set up a data frame to hold the results
results <- data_frame(Industry = character(), A = numeric(), Alpha = numeric(), Beta = numeric())
all_industries <- as.character(unique(nz_econ$Industry))

for (i in 1:length(all_industries)){
   this_data <- nz_econ %>% 
      filter(Industry == all_industries[i]) 
   model <- gls(log(Y) ~ log(K) + log(L), data = this_data, correlation = corAR1())
   results[i, 1] <- all_industries[i]
   results[i, 3:4] <- coef(model)[2:3]
   results[i, "A"] <- exp(coef(model)[1]) # put total factor productivity on original scale
}

# Industries in order of total factor productivity multiplier (not sure what this means)
results %>% arrange(A)

# Industries in order of returns to increase capital with constant labour
results %>% arrange(Alpha)

# Industries in order of returns to increase labour with constant capital
results %>% arrange(Beta)

# Industries in order of total increasing returns to scale (results not very plausible):
results %>% arrange(Beta + Alpha)


# this chart nicely shows the industry level results.  Whether it is meaningful
# requires taking a view on the theory
results %>%
   ggplot(aes(x = Alpha, y = Beta, label = wrap(Industry, 23))) +
   geom_abline(intercept = 1, slop = -1, colour = "white") +
   geom_text(size = 3.5) +
   labs(x = "Responsive to change in capital inputs",
        y = "Responisve to change in labour inputs") +
   ylim(-0.1, 1.2) + xlim(-0.1, 1.2)+
   annotate("text", label = wrap(c("Increasing returns to scale of inputs", "Decreasing returns to scale of inputs"), 25),
            x = c(0.9, 0.1), y = c(0.8, 0.3), colour = "grey50", fontface = "italic") +
   coord_equal()

#---------------presenting relationship graphically---------------------
# if we thought this was a valid approach (which I'm unsure of), you could 
# easily turn this into a shiny app where the user selects industry and K 
# and sees the relationship of Y to L; or industry and L and sees the 
# relationship of Y to K.

CD <- function(A, Alpha, Beta, K, L){
   # returns production at given level of capital (K) and labour (L)
   A * K ^ Alpha * L ^ Beta
}


CairoPDF("labour-and-capital.pdf", 6, 5)
for(i in 1:length(all_industries)){

this_data <- nz_econ %>% 
   filter(Industry == all_industries[i]) 

these_coefs <- results[i, ]

Lrange <- mean(this_data$L) * c(0.3, 5) # ie range from 30% to 5 times the average value
Krange <- mean(this_data$K) * c(0.3, 5) # ie range from 30% to 5 times the average value

Lgrid <- seq(from = Lrange[1], to = Lrange[2], length = 500)
Kgrid <- seq(from = Krange[1], to = Krange[2], length = 500)
BothGrid <- expand.grid(Lgrid, Kgrid)
head(BothGrid)

ConstantK <- data.frame(
   Y = with(these_coefs, CD(A = A, Alpha = Alpha, Beta = Beta, K = mean(this_data$K), L = Lgrid)),
   L = Lgrid
   )

print(ggplot(ConstantK, aes(x = L, y = Y)) +
   geom_line() +
   scale_x_continuous("Hours worked", label = comma) +
   scale_y_continuous("Predicted production ($m)") +
   # maximum labour input so far shown by dotted line:
   geom_vline(xintercept = max(this_data$L), linetype = 2) + 
   ggtitle(paste0("Returns to labour at average capital levels in\n", all_industries[i])))

ConstantL <- data.frame(
   Y = with(these_coefs, CD(A = A, Alpha = Alpha, Beta = Beta, L = mean(this_data$L), K = Kgrid)),
   K = Kgrid
)

print(ggplot(ConstantL, aes(x = K, y = Y)) +
   geom_line() +
   scale_x_continuous("Capital stock", label = comma) +
   scale_y_continuous("Predicted production ($m)") +
   # maximum capital input so far shown by dotted line:
   geom_vline(xintercept = max(this_data$K), linetype = 2) + 
   ggtitle(paste0("Returns to capital at average labour levels in\n", all_industries[i])))

}
dev.off()
