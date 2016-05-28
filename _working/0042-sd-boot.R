#===================setup=======================
library(showtext)
library(ggplot2)
library(scales)
library(boot)
library(dplyr)
library(moments)

nzis <- read.csv("http://www.stats.govt.nz/~/media/Statistics/services/microdata-access/nzis11-cart-surf/nzis11-cart-surf.csv")


test_boot_ci <- function(full_data, reps = 50, R = 499, title = match.call()){
   title <- paste("Estimating standard deviation from", title) 
   
   sampsd <- function(x, i, xbar = mean(x)){
      # function suitable for boot which returns estimated standard deviation
      # from a sample x that has been scrambled by the index i
      d <- x[i]
      n <- length(d)
      
      # this next formula is the best general approximation to a general  
      # unbiased estimator of standard deviation from a non-normal distribution
      # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
      unbiased_sd <- sqrt(
         sum((d - xbar) ^ 2) / 
            (n - 1.5 - (kurtosis(d) - 3) / 4)
         )
      return(unbiased_sd)
   }
   
   # create a data frame to hold the results, with "reps" rows for each
   # sample size from 50, 100, 200, ..., 12800
   ns <- rep(2 ^ (0:8) * 50, each = reps)
   results <- data.frame(
      n = ns,
      point_est = numeric(length(ns)),
      ci_basic_correct = logical(length(ns)),
      ci_perc_correct = logical(length(ns))
   )
      
   
   truevalue <- sd(full_data)
   
   for(i in 1:nrow(results)){
   
      this_data <- full_data[sample(1:length(full_data), results[i, "n"])]   
      res <- boot(this_data, statistic = sampsd, R = R)
      
      results[i, "point_est"] <- res$t0
      
      ciobj <- boot.ci(res, type = "basic")
      citest <- (truevalue > ciobj$basic[4] & truevalue < ciobj$basic[5])
      results[i, "ci_basic_correct"] <- citest
      
      ciobj <- boot.ci(res, type = "perc")
      citest <- (truevalue > ciobj$perc[4] & truevalue < ciobj$perc[5])
      results[i, "ci_perc_correct"] <- citest
      
   }
   
   
   tab <- results %>%
      group_by(n) %>%
      summarise(coverage_basic = round(sum(ci_basic_correct) / length(ci_basic_correct), 3),
                coverage_perc =  round(sum(ci_perc_correct) / length(ci_perc_correct), 3),
                bias = round((mean(point_est) - truevalue) / truevalue, 3))
   
   p <- results %>%
      mutate(n = paste("n =", n),
             n = factor(n, levels = paste("n =", ns) ) ) %>%
      ggplot(aes(x =point_est)) +
      geom_density(fill = "grey75", alpha = 0.5) +
      geom_rug() +
      facet_wrap(~n, scales = "free_y") +
      geom_vline(xintercept = truevalue, colour = "blue") +
      ggtitle(title) +
      labs(x = "Point estimates (blue line shows true value)")
   
   return(list(tab = tab, p = p))
   
}

reps_per_sample_size <- 200
reps_per_bootstrap <- 999

income_test <- test_boot_ci(full_data = nzis$income, 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "income data")

hours_test <- test_boot_ci(full_data = nzis$hours, 
                           reps = reps_per_sample_size, 
                           R = reps_per_bootstrap,
                           title = "hours worked data")

normal_test <- test_boot_ci(full_data = rnorm(30000), 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "simulated Normal data")

unif_test <- test_boot_ci(full_data = runif(30000), 
                          reps = reps_per_sample_size, 
                          R = reps_per_bootstrap,
                          title = "simulated uniform data")

lognormal_test <- test_boot_ci(full_data = exp(rnorm(30000)), 
                               reps = reps_per_sample_size, 
                               R = reps_per_bootstrap,
                               title = "simulated log-normal data")

income_test
hours_test
normal_test
unif_test
lognormal_test

