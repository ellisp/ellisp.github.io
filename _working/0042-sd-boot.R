#===================setup=======================
library(showtext)
library(ggplot2)
library(scales)
library(boot)
library(dplyr)
library(moments) # for kurtosis()
library(tidyr)
library(gridExtra) # for grid.arrange()

# Fonts:
font.add.google("Poppins", "myfont")
showtext.auto()
theme_set(theme_light(10, base_family = "myfont") )

theme_small <- theme_light(8, base_family = "myfont") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#==========Functions==================
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



test_boot_ci <- function(full_data, 
                         statistic, 
                         truevalue = statistic(full_data, i = 1:length(full_data)),
                         reps = 50, 
                         R = 499, 
                         title = match.call()){

   # Function to explore the coverage of basic and percentile bootstrap 
   # 95% confidence intervals.
   #' @full_data A full population of data from which samples of various sizes
   #           will be drawn without replacement, and each of those samples will
   #           be analysed with the bootstrap as though it were all that is
   #           available.
   #' @statistic a function suitable for passing to the statistic argument of boot
   #           ie first argument is data, second argument is an index for 
   #           scrambling the data
   #' @truevalue true value that the statistic is trying to esimate.
   #' @reps number of repeats of each sample size to analyse
   #' @R number of bootstrap resamples for each sample
   #' @title Title for plots
   
   
   # create a data frame to hold the results, with "reps" rows for each
   # sample size from 50, 100, 200, ..., 12800
   ns <- rep(2 ^ (0:8) * 50, each = reps)
   results <- data.frame(
      n = ns,
      point_est = numeric(length(ns)),
      ci_basic_correct = logical(length(ns)),
      ci_perc_correct = logical(length(ns))
   )
      
   
   # Create point estimates and two types of bootstrap confidence interval
   # for each repetition at each value of sample size:
   for(i in 1:nrow(results)){
   
      this_data <- full_data[sample(1:length(full_data), results[i, "n"])]   
      res <- boot(this_data, statistic = statistic, R = R)
      
      results[i, "point_est"] <- res$t0
      
      ciobj <- boot.ci(res, type = "basic")
      citest <- (truevalue > ciobj$basic[4] & truevalue < ciobj$basic[5])
      results[i, "ci_basic_correct"] <- citest
      
      ciobj <- boot.ci(res, type = "perc")
      citest <- (truevalue > ciobj$perc[4] & truevalue < ciobj$perc[5])
      results[i, "ci_perc_correct"] <- citest
      
   }
   
   # Prepare summary table and plots for output
   tab <- results %>%
      group_by(n) %>%
      summarise(coverage_basic = round(sum(ci_basic_correct) / length(ci_basic_correct), 3),
                coverage_perc =  round(sum(ci_perc_correct) / length(ci_perc_correct), 3),
                bias = round((mean(point_est) - truevalue) / truevalue, 3))
   
   p1 <- tab %>%
      dplyr::select(-bias) %>%
      gather(Method, value, -n) %>%
      mutate(Method = gsub("coverage_perc", "Percentile", Method),
             Method = gsub("coverage_basic", "Basic", Method),
             Method = factor(Method, levels = c("Percentile", "Basic"))) %>%
      ggplot(aes(x = n, y = value, color = Method)) +
      geom_hline(yintercept = 0.95, colour = "blue") +
      geom_point() +
      geom_line() +
      scale_y_continuous("Actual coverage of 95% confidence interval", 
                         label = percent) +
      scale_x_sqrt("Sample size", label = comma, breaks = unique(ns)) +
      labs(colour = "Bootstrap\nmethod") +
      ggtitle(title) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   p2 <- tab %>%
      ggplot(aes(x = n, y = bias)) +
      geom_hline(yintercept = 0, colour = "blue") +
      geom_point() +
      geom_line() +
      scale_y_continuous("Bias (as percentage of true value)", 
                         label = percent) +
      scale_x_sqrt("Sample size", label = comma, breaks = unique(ns)) +
      ggtitle(title) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   p3 <- results %>%
      mutate(n = paste("n =", n),
             n = factor(n, levels = paste("n =", unique(ns) ) ) ) %>%
      ggplot(aes(x =point_est)) +
      geom_density(fill = "grey75", alpha = 0.5) +
      geom_rug() +
      facet_wrap(~n, scales = "free_y") +
      geom_vline(xintercept = truevalue, colour = "blue") +
      ggtitle(title) +
      labs(x = "Point estimates (blue line shows true value)")
   
   return(list(tab = tab, p1 = p1, p2 = p2, p3 = p3))
   
}


#===============Application==================
if(!exists("nzis")){
   nzis <- read.csv("http://www.stats.govt.nz/~/media/Statistics/services/microdata-access/nzis11-cart-surf/nzis11-cart-surf.csv")   
}


reps_per_sample_size <- 200
reps_per_bootstrap <- 999

#------------standard deviation of income data------------
income_sd <- test_boot_ci(full_data = nzis$income, 
                            statistic = sampsd, 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "Estimating standard deviation from income data")



#---------standard deviation of simulated data--------------
normal_sd <- test_boot_ci(full_data = rnorm(300000), 
                          statistic = sampsd, 
                          reps = reps_per_sample_size, 
                          R = reps_per_bootstrap,
                          title = "Estimating standard deviation from simulated Normal data")

unif_sd <- test_boot_ci(full_data = runif(300000), 
                        statistic = sampsd, 
                        reps = reps_per_sample_size, 
                        R = reps_per_bootstrap,
                        title = "Estimating standard deviation from simulated uniform data")

lognormal_sd <- test_boot_ci(full_data = exp(rnorm(300000)), 
                             reps = reps_per_sample_size, 
                             statistic = sampsd, 
                             R = reps_per_bootstrap,
                             title = "Estimating standard deviation from simulated log-normal data")


mixture <- c(exp(rnorm(100000)), rnorm(100000), runif(100000))
mixture_sd <- test_boot_ci(full_data = mixture, 
                             reps = reps_per_sample_size, 
                             statistic = sampsd, 
                             R = reps_per_bootstrap,
                             title = "Estimating standard deviation from simulated mixture data")


#-------simpler statistics on income data-----------
income_mean <- test_boot_ci(full_data = nzis$income,
                            statistic = function(x, i){mean(x[i])}, 
                            reps = reps_per_sample_size, 
                            R = reps_per_bootstrap,
                            title = "Estimating mean from income data")

income_trmean <- test_boot_ci(full_data = nzis$income,
                              statistic = function(x, i){mean(x[i], tr = 0.2)}, 
                              reps = reps_per_sample_size, 
                              R = reps_per_bootstrap,
                              title = "Estimating trimmed mean from income data")

#==================exploratory graphics======
#------------Density of income data---------------
svg("../img/0042-full-data.svg", 6, 4)
print(
   ggplot(nzis, aes(x = income)) +
   geom_density(fill = "grey75", alpha = 0.5) +
   geom_rug() +
   scale_x_continuous("Weekly income", label = dollar) +
   ggtitle("New Zealand Income Survey 2011", subtitle = "Full data")
)
dev.off()

set.seed(123)
svg("../img/0042-sample-data.svg", 6, 4)
print(
ggplot(nzis[sample(1:nrow(nzis), 50), ], aes(x = income)) +
   geom_density(fill = "grey75", alpha = 0.5) +
   geom_rug() +
   scale_x_continuous("Weekly income", label = dollar) +
   ggtitle("New Zealand Income Survey 2011", subtitle = "Subsample of 50")
)
dev.off()

set.seed(123)
sd(nzis[sample(1:nrow(nzis), 50), "income"])



#================results graphics==================
#-----------standard deviation of income--------------
svg("../img/0042-sd-ci-coverage.svg", 8, 4)
print(income_sd$p1 + 
         labs(caption = "\nSource: New Zealand Income Survey 2011, Statistics New Zealand"))
dev.off()

png("../img/0042-sd-ci-coverage.png", 800, 400, res = 100)
print(income_sd$p1 + 
         labs(caption = "\nSource: New Zealand Income Survey 2011, Statistics New Zealand"))
dev.off()


svg("../img/0042-sd-bias-summary.svg", 8, 4)
print(income_sd$p2)
dev.off()


svg("../img/0042-sd-bias-full.svg", 8, 6)
print(income_sd$p3)
dev.off()


#----------------other distributions--------------
svg("../img/0042-other-dists.svg", 10, 8)
print(grid.arrange(
   normal_sd$p1 + theme_small,
   unif_sd$p1 + theme_small,
   lognormal_sd$p1 + theme_small,
   mixture_sd$p1 + theme_small
))
dev.off()



#----------------mean and trimmed mean-----------------
svg("../img/0042-mean-ci-coverage.svg", 8, 4)
print(
   grid.arrange(
      income_mean$p1 + theme_small,
      income_trmean$p1 + theme_small,
      ncol = 2
   )
)
dev.off()


svg("../img/0042-mean-bias.svg", 8, 4)
print(
   grid.arrange(
      income_mean$p2 + 
         theme_small +
         scale_y_continuous(limits = c(-0.05, 0.05), label = percent),
      income_trmean$p2 + 
         theme_small +
         scale_y_continuous(limits = c(-0.05, 0.05), label = percent),
      ncol = 2
   )
)
dev.off()
