library(mbie)

library(RODBC)
library(ineq)
library(dplyr)
library(ggplot2)
library(scales)
library(showtext) # for fonts
library(stringr)  # for str_wrap

font.add.google("Poppins", "myfont")
showtext.auto()

PlayPen <- odbcConnect("PlayPen_Prod")

inc <- sqlQuery(PlayPen, "select income from vw_mainheader") 
# inc <- f_mainheader$income

lorenz <- Lc(inc)
lorenz_df <- data.frame(prop_pop = lorenz$p, income = lorenz$L) %>%
   mutate(prop_equality = prop_pop)


plot_lc <- function(){
   p1 <- ggplot(lorenz_df, aes(x = prop_pop, y = income)) +
      geom_ribbon(aes(ymax = prop_equality, ymin = income), fill = "yellow") +
      geom_line() +
      geom_abline(slope = 1, xintercept = 0) +
      scale_x_continuous("\nCumulative proportion of population", label = percent) +
      scale_y_continuous("Cumulative proportion of income\n", label = percent) +
      theme_minimal(base_family = "myfont") +
      coord_equal() +
      annotate("text", 0.53, 0.32, label = "Inequality\ngap", family = "myfont") +
      annotate("text", 0.5, 0.6, label = "Complete equality line", angle = 45, family = "myfont") + 
      ggtitle (
         str_wrap("Cumulative distribution of New Zealand individual weekly income from all sources", 46))
   
   print(p1)
   
   grid.text("Source: Statistics New Zealand\nNational Income Survey 2011 SURF", 0.8, 0.23, 
          gp = gpar(fontfamily = "myfont", fontsize = 8))
   
}

svg("../img/0008-lorenz.svg", 5, 5)
   plot_lc()
dev.off()

png("../img/008-lorenz.png", 500, 500, res = 100)
   plot_lc()
dev.off()


#--------------weekly v annual income------------
Gini(inc)                   # 0.51

# if completely constant each week
Gini(inc * 52)              # 0.51

# create incomes that simulate each week being a random pull from the pool
random_incomes <- data_frame(
   income = sample(inc, 52 * length(inc), replace = TRUE),
   person = rep(1:length(inc), 52)) %>%  
   group_by(person) %>%
   summarise(income = sum(income))

Gini(random_incomes$income) # 0.09

#---------distribution of Gini coefficient from small smaple

sim_gini <- function(n, reps = 1000){
   results <- data.frame(
      trial = 1:reps,
      estimate = numeric(reps))
   
   set.seed(123) # for reproducibility
   for(i in 1:reps){
      results[i, "estimate"] <- 
         Gini(sample(inc, n, replace = TRUE))
   }
   
   print(results %>%
      ggplot(aes(x = estimate)) +
      geom_density() +
      geom_rug() +
      theme_minimal(base_family = "myfont") +
      ggtitle(paste("Distribution of estimated Gini coefficient, n =", n)))
   
   grid.text(paste0("Standard error: ", round(sd(results$estimate), 3)), 0.8, 0.6, 
             gp = gpar(fontfamily = "myfont", fontsize = 9))
   
   grid.text(paste0("95% of values between:\n", 
                    paste(round(quantile(results$estimate, c(0.025, 0.975)), 3), collapse = ", ")), 0.8, 0.5, 
             gp = gpar(fontfamily = "myfont", fontsize = 9))
   
}

svg("../img/0008-density-gini-n30.svg", 6, 3)
   sim_gini(30)
dev.off()

png("../img/0008-density-gini-n30.png", 500, 300, res = 100)
   sim_gini(30)
dev.off()

svg("../img/0008-density-gini-n1000.svg", 6, 3)
   sim_gini(1000)
dev.off()

svg("../img/0008-density-gini-n30000.svg", 6, 3)
   sim_gini(30000)
dev.off()




