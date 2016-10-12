
# train with rlm only works if factors are legal column names

?check.names
update.packages(oldPkgs="caret", ask=FALSE)
make.names
library(caret)

# 185KB download for reproducible example:
download.file("http://ellisp.github.io/data/nzis.rda", destfile = "nzis.rda")
load("nzis.rda")

nzis$region <- gsub(" / ", "_", nzis$region, fixed = TRUE)
nzis$region <- gsub("'", "", nzis$region, fixed = TRUE)
nzis$region <- gsub(" ", "", nzis$region, fixed = TRUE)

table(nzis$region)
cv_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# lm works fine:
mo_lm <- train(income ~ ., data = nzis, method = "lm", 
                 trControl = cv_control)
# works:
mod_rlm1 <- train(income ~ hours, data = nzis, method = "rlm", 
                 trControl = cv_control)

# works:
mod_rlm2 <- train(income ~ hours + sex, data = nzis, method = "rlm", 
                  trControl = cv_control)

# doesn't work:
mod_rlm3 <- train(income ~ region, data = nzis, method = "rlm", 
                  trControl = cv_control)


# doesn't work:
mod_rlm3 <- train(income ~ occupation, data = nzis, method = "rlm", 
                  trControl = cv_control)

# but the underlying model is ok.  Both these work:
rlm(income ~ occupation, data = nzis)
rlm(income ~ ., data = nzis)

nzis_small <- droplevels(head(nzis[, c("income", "region")], 20))
nzis_small$region_legal <- make.names(nzis_small$region)

# doesn't work
mod_rlm3 <- train(income ~ region_legal, data = nzis_small, method = "rlm", 
                  trControl = cv_control)

# works with
mod_rlm3 <- train(income ~ region, data = nzis_small, method = "rlm", 
                  trControl = cv_control)

# works ok:
rlm(income ~ region, data = nzis_small)

#=====MRE for reporting bug============
library(caret)
# make some test data including an explanatory variable with
# levels that would be illegal as column names:
n <- 100
testdata <- data.frame(
   x1 = rnorm(n),
   x2 = sample(c("dog", "cat's mother", "bear"), n, replace = TRUE)
)
testdata$y <- with(testdata,
                   0.5 + x1 * 0.2 + (x2 == "bear") * 0.3 - (x2 =="dog") * 0.2 +
                      rnorm(n)
                   )

# make a variant of x2 that *would* be legal as column names
testdata$x3 <- make.names(testdata$x2)

#---------------fit models-------------
cv_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# doesn't work, because x2 contains levels that are illegal as column names
mod_rlm <- train(y ~ x1 + x2, data = testdata, method = "rlm", trControl = cv_control)

# does work:
mod_rlm <- train(y ~ x1 + x3, data = testdata, method = "rlm", trControl = cv_control)

# so far I've only found this a problem with rlm.  eg this works ok
mod_lm <- train(y ~ x1 + x2, data = testdata, method = "lm", trControl = cv_control)

# problem doesn't happen with rlm used directly
rlm(y ~ x1 + x2, data = testdata)

sessionInfo()
