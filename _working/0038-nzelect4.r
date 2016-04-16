#---------load up functionality and fonts------------
devtools::install_github("ellisp/nzelect/pkg")
devtools::install_github("hadley/ggplot2") # latest version needed for subtitles and captions

library(MASS) # for rlm().  Load before dplyr to avoid "select" conflicts
library(mice) # for multiple imputation
library(dplyr)    
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)
library(car)      # for vif()
library(ggthemes) # for theme_tufte
library(mgcv)     # for a version of gam with vcov() method
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont"))

# load in the data
library(nzelect)

#-----------general data prep------------
# Some voting places don't have a match to mesthblock and hence aren't any
# good for our purposes.  Note - Chatham Islands *should* be a match, but that
# an issue to fix in the nzelect package which we'll ignore for now
bad_vps <- c(
   "Chatham Islands Council Building, 9 Tuku Road, Waitangi",
   "Ordinary Votes BEFORE polling day",
   "Overseas Special Votes including Defence Force",
   "Special Votes BEFORE polling day", 
   "Special Votes On polling day", 
   "Votes Allowed for Party Only",
   "Voting places where less than 6 votes were taken")

GE2014a <- GE2014 %>%
   filter(!VotingPlace %in% bad_vps)


#------------Greens / (Greens + Labour)--------------
# make a dataset with the response variable we want (Greens / (Greens + Labour))
# and merged with the VotingPlace locations and the relevant meshblock data
greens <- GE2014a %>%
   filter(VotingType == "Party" &
             Party %in% c("Green Party", "Labour Party")) %>%
   group_by(VotingPlace) %>%
   summarise(PropGreens = sum(Votes[Party == "Green Party"]) / sum(Votes),
             TotalGLVotes = sum(Votes)) %>%
   ungroup() %>%
   left_join(Locations2014, by = "VotingPlace") %>%
   left_join(Meshblocks2013, by = c("MB2014" = "MB")) %>%
   select(PropGreens, TotalGLVotes, WGS84Latitude, WGS84Longitude,
          MeanBedrooms2013:PropStudentAllowance2013) 

# Tidy up names of variables.  All the census data is from 2013 so we don't
# need to say so in each variable:
names(greens) <- gsub("2013", "", names(greens))


# Identify and address collinearity in the explanatory variables
mod1 <- lm(PropGreens ~ . , data = greens)
sort(vif(mod1) )
# PropEuropean is 17 - can be predicted from maori, Pacific Asian.
# PropOwnResidence is 11 - can be predicted from PropNotOwnedHH
# so let's take those two un-productive variables out
greens <- greens[ , !names(greens) %in% c("PropEuropean", "PropOwnResidence")]


# Image of scatter plots of explanatory variables v response variable
png("../img/0038-green-labour.png", 1000, 1000, res = 100)
greens %>%
   gather(Variable, Value, -PropGreens) %>%
   mutate(Variable = gsub("2013", "", Variable),
          Variable = gsub("Prop", "Prop ", Variable)) %>%
   ggplot(aes(x = Value, y = PropGreens)) +
   facet_wrap(~Variable, scales = "free_x", ncol = 6) +
   geom_point(alpha = 0.2) +
   geom_smooth(method = "rlm", se = FALSE) +
   scale_y_continuous("Percentage of Labour and Green voters who voted Green Party in party vote", 
                      label = percent) +
   scale_x_continuous("", label = comma) +
   labs(caption = "Note: horizontal scales vary; and some proportions exceed 1.0 due to confidentialising.\nBlue lines are outlier-resistant robust regressions.") +
   ggtitle("Choosing Green over Labour in the 2014 New Zealand General Election",
      subtitle = "Each point represents an individual voting location (vertical axis) and the meshblock within which it is located (horizontal axis).") +    theme(panel.margin = unit(1.5, "lines"))
dev.off()


# More data munging prior to model fitting

# only 39% of cases are complete, due to lots of confidentialisation:
sum(complete.cases(greens)) / nrow(greens)
# so to avoid chucking out half the data we will need to impute.  Best way
# is multiple imputation, which gives

# to make it easier to compare the ultimate coefficients, we're going to
# scale the explanatory variables.  Easiest to do this before imputationL
greens_scaled <- greens
greens_scaled[ , -(1:2)] <- scale(greens_scaled[ , -(1:2)] )

# Now we do the multiple imputation.  
# Note that we ignore PropGreens for imputation purposes - don't want to impute
# the Xs based on the Y!  First we define the default predictor matrix:
predMat <- 1 - diag(1, ncol(greens_scaled))
# Each row corresponds to a target variable, columns to the variables to use in
# imputing them.  We say nothing should use PropGreens (first column).  Everything
# else is ok to use, including latitude and longitude and the total green and labour
# votes:
predMat[ , 1] <- 0
greens_mi <- mice(greens_scaled, m = 20, predictorMatrix = predMat)

# what are all the variable names?:
paste(names(greens_scaled)[-1], collapse = " + ")

# fit models to each of the imputed datasets:
mod2 <- with(greens_mi, 
             gam(PropGreens ~ MeanBedrooms + PropPrivateDwellings + 
                   PropSeparateHouse + NumberInHH + PropMultiPersonHH + 
                   PropInternetHH + PropNotOwnedHH + MedianRentHH + 
                   PropLandlordPublic + PropNoMotorVehicle + PropOld + 
                   PropEarly20s + PropAreChildren + PropSameResidence5YearsAgo + 
                   PropOverseas5YearsAgo + PropNZBorn + PropMaori + 
                   PropPacific + PropAsian + PropNoReligion + PropSmoker +
                   PropSeparated + PropPartnered + PropNoChildren + 
                   PropNoQualification + PropBachelor + PropDoctorate + 
                   PropFTStudent + PropPTStudent + MedianIncome + 
                   PropSelfEmployed + PropUnemploymentBenefit + 
                   PropStudentAllowance + 
                    s(WGS84Latitude) + s(WGS84Longitude) +
                    s(WGS84Latitude * WGS84Longitude),
                family = binomial, weights = TotalGLVotes)
)

   
# extract all the estimates of coefficients, except the uninteresting intercept:
coefs <- summary(pool(mod2))[-1, ] 
vars <- rownames(coefs)
coefs <- as.data.frame(coefs) %>%
   mutate(variable = vars) %>%
   arrange(est) %>%
   mutate(variable = gsub("Prop", "", variable)) %>%
   mutate(variable = factor(variable, levels = variable)) %>%
   # drop all the uninteresting estimates associated with the spatial splines:
   filter(!grepl("WGS84", variable))

# make names referrable:
names(coefs) <- gsub(" ", "_", names(coefs), fixed = TRUE)

# define plot of results:
p2 <- ggplot(coefs, aes(x = variable, ymin = lo_95, ymax = hi_95, y = est)) + 
   geom_hline(yintercept = 0, colour = "lightblue") +
   geom_linerange(colour = "grey20") +
   geom_text(aes(label = variable), size = 3, family = "myfont", vjust = 0, nudge_x = 0.15) +
   coord_flip() +
   labs(y = "\nHorizontal lines show 95% confidence interval of scaled impact on
proportion that voted Green out of Green and Labour voters.
The numbers show coefficients from a logistic regression and 
should be taken as indicative, not strictly interpretable.\n",
        x = "",
        caption = "Source: http://ellisp.github.io") +
   ggtitle("Census characteristics of voting places that party-voted Green over Labour",
      subtitle = "New Zealand General Election 2014") +
   theme_tufte(base_family = "myfont") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank()) +
   annotate("text", y = -0.2, x = 6, label = "More likely to\nvote Labour", 
            colour = "red", family = "myfont") +
   annotate("text", y = 0.18, x = 31, label = "More likely to\nvote Green", 
            colour = "darkgreen", family = "myfont")
svg("../img/0038-model-results.svg", 8, 8)
   print(p2)
dev.off()

png("../img/0038-model-results.png", 800, 800, res = 100)
   print(p2)
dev.off()


