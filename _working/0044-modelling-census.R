#===================setup=======================
library(showtext)
library(ggplot2)
library(scales)
library(MASS)
library(boot)
library(caret)
library(dplyr)
library(tidyr)
library(directlabels)
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
