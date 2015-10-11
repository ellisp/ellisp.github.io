library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(tidyr)
library(shiny)



#============main app begins============
shinyServer(function(input, output) {
   
   #========within-app helper============
   CD <- function(economy, i){
      K <- economy[i, "capital"] # for ease of use
      L <- economy[i, "labour"]
      Y <- 1.95 * K ^ input$capital_coef * L ^ input$labour_coef * exp(sum(log(economy[1:i, "productivity_growth"])))
      return(Y)
      
   }
   
   
      #==============the simulation========
   
      starter <- reactive({
         set.seed(input$CountMe)
         data.frame(
               year = 1:input$duration,
               capital = 1000,
               labour = 1000,
               production = 1000,
               saving_rate = input$start_savings / 100,
               growth = 1,
               labour_growth = 1 + input$start_labour / 100 + 
                  arima.sim(model = list(ar = 0.9), n = input$duration) / 200 * input$labour_random /5,
               productivity_growth = 1 + (input$prod_growth_underlying / 100) +
                  arima.sim(model = list(ar = 0.9), n = input$duration) / 300 * input$prod_growth_random / 5, # productivity
               randomness2 = arima.sim(model = list(ar = 0.9), n = input$duration) / 100 * input$savings_random / 5,     # savings
               war = arima.sim(model = list(ar = 0.93), n = input$duration)            
            )
      })
            
      war_threshhold <- reactive({quantile(starter()$war, input$war_threshhold)})
      
      simulation <- reactive({
      economy <- starter()
         
      for(i in 2:input$duration){
         economy[i, "saving_rate"] <- economy[1, "saving_rate"] +
            (economy[i - 1, "growth"] - (1 + input$growth_save_thresh / 100)) * 
               input$growth_save_strength +
            economy[i, "randomness2"]
         
         economy[i, "capital"] <- economy[i - 1, "capital"] + 
            economy[i, "saving_rate"] * economy[i - 1, "production"]
         economy[i, "labour"] <- economy[i - 1, "labour"] * 
            economy[i, "labour_growth"]
         
         if(economy[i, "war"] < war_threshhold()){
            economy[i, "capital"] <- economy[i, "capital"] * 
               runif(1, input$war_capital_impact[1], input$war_capital_impact[2])
            economy[i, "labour"] <- economy[i, "labour"] * 
               runif(1, input$war_labour_impact[1], input$war_labour_impact[2])
         }
         
         economy[i, "production"] <- CD(economy, i)
         economy[i, "growth"] <- economy[i, "production"] / economy[i - 1, "production"] 
         
      }
      
      # capital expressed as number of years of production:
      economy$CapIncRatio <- with(economy, capital / production)
      
      # delete first few years where there is sometimes a burn-in period while production comes up to approx 
      # correct level:
      return(economy[-(1:15), ])
      
   })
   
   
   #============draw plots============
   p1 <- reactive({
      simulation() %>%
         mutate(LogProduction = log10(production)) %>%
         select(year, CapIncRatio, LogProduction) %>%
         gather("variable", "value", -year) %>%
         mutate(variable = gsub("CapIncRatio", "Capital as a proportion of a year's production", variable),
                variable = gsub("LogProduction", "Production (logarithm)", variable)) %>%
         ggplot(aes(x = year, y = value)) +
         geom_vline(xintercept = simulation()$year[simulation()$war <  war_threshhold()], colour = "red") +
         facet_wrap(~variable, ncol = 1, scales = "free_y") +
         geom_line()
   })
   output$p1 <- renderPlot(print(p1()))
  
   
   p4 <- reactive({
      simulation() %>%
      mutate(prod_pp = production / labour,
             prod_pc = production / capital) %>%
      select(year, prod_pp, prod_pc) %>%
      gather("variable", "value", -year) %>%
      mutate(variable = gsub("prod_pp", "Production per person", variable),
             variable = gsub("prod_pc", "Production per capital", variable)) %>%
      ggplot(aes(x = year, y = value)) +
      facet_wrap(~variable, ncol = 1, scales = "free_y") +
        geom_vline(xintercept = simulation()$year[simulation()$war <  war_threshhold()], colour = "red") +
      geom_line()
   })
   output$p4 <- renderPlot(print(p4()))
  
   
   p2 <- reactive({
      ggplot(simulation(), aes(x = year, y = saving_rate)) +
      geom_line() +
      scale_y_continuous("Savings as a percent of last year's production", label = percent)
   })
   output$p2 <- renderPlot(print(p2()))
   
   p3 <- reactive({
      ggplot(simulation(), aes(x = year, y = (growth - 1))) +
      geom_line() +
      scale_y_continuous("Growth in production", label = percent)
   })
   output$p3 <- renderPlot(print(p3()))
   
   p5 <- reactive({
      simulation() %>%
         ggplot(aes(x = growth, y = saving_rate)) + 
            geom_path() + 
            geom_smooth(method = "lm")
   })
   output$p5 <- renderPlot(print(p5()))
  
   p6 <- reactive({
      simulation() %>%
         ggplot(aes(x = year, y = labour)) + 
         geom_line() +
         geom_vline(xintercept = simulation()$year[simulation()$war <  war_threshhold()], colour = "red") +
         ggtitle("Labour force size")
   })
   output$p6 <- renderPlot(print(p6()))
   
   
})