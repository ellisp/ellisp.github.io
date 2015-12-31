library(shiny)
library(randomForest)
library(ggvis)
library(dplyr)

load("models.rda")
load("mod1.rda")

lambda = 0.5

.mod_transform <- function(y, lambda){
   if(lambda != 0){
      yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
   } else {
      yt = sign(y) * (log(abs(y) + 1))
   }
   return(yt)
}


.mod_inverse <- function(yt, lambda){
   if(lambda != 0){
      y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
   } else {
      y <- (exp(abs(yt)) - 1) * sign(yt)
      
   }
   return(y)
}

FormatDollars <- function (x, endmark = "", ...) {
    x <- paste0("$", format(round(x, ...), big.mark = ","), endmark)
    x <- gsub(" ", "", x)
    return(x)
}

shinyServer(function(input, output) {
# input <- data.frame(sex = "female", agegrp = "40-44", occupation = "Community and Personal Service Workers", qualification = "Other", region = "Auckland", hours = 35)
  
  person <- reactive({
   tmp <-   nzis_skeleton
    tmp[1, "hours"] <- input$hours
    tmp[1, "sex"] <- input$sex
    tmp[1, "agegrp"] <- input$agegrp
    tmp[1, "occupation"] <- input$occupation
    tmp[1, "qualification"] <- input$qualification
    tmp[1, "region"] <- input$region
    tmp[1, "Asian"] <- "No" 
    tmp[1, "Maori"] <- "No" 
    tmp[1, "European"] <- "No" 
    tmp[1, "Pacific"]  <- "No" 
    tmp[1, "Other"]  <- "No" 
    
    for(eth in c("Asian", "Maori", "European", "Pacific", "Other")){
      if(eth %in% input$ethnicity) tmp[1, eth] <- "Yes"
     }
    
    
    return(tmp)
  })
  
  the_prob <- reactive({
   prob <- predict(mod1_shiny, newdata = person(), type = "prob")[ , "TRUE"]
   return(prob)
  })
  
  the_data <- reactive({
   
    point <- predict(mod2_shiny, newdata = person())
    
  
    n <- 10000
    positives <- round(the_prob() * n)
    
    set.seed(123)
    x <- c(rep(0, (n - positives)),
          point + sample(res, positives, replace = TRUE))
          
    tmp <- data.frame(income = .mod_inverse(c(x, all_income), lambda = lambda),
                     type = rep(c("Your selection", "All New Zealand"), c(n, length(all_income)))
                     ) %>%
         group_by(type)
          
     return(tmp)
    })  
    
    the_data %>% 
    filter(income > -2000 & income < 4000)  %>% 
         ggvis(x = ~income, fill = ~type, stroke = ~type) %>%
         layer_densities() %>%
         scale_numeric("x", domain = c(-2000, 4000), nice = TRUE) %>%
         set_options(width = "auto") %>%
         add_axis("x", title = "Income per week ($)") %>%
         add_axis("y", title = "Density", title_offset = 50) %>%
         bind_shiny("plot")
    
    
    the_table <- reactive({
      tmp <- the_data() %>%    summarise(Mean = FormatDollars(mean(income)), 
                         Median = FormatDollars(median(income))) %>%
                         t() %>%
                         data.frame()
       tmp$row <- row.names(tmp)
       tmp[1, "row"] <- ""
       
      return(tmp[, c("row", "X1", "X2")])                         
    })
    
    output$table <- renderTable(the_table(),
                        include.rownames = FALSE, include.colnames = FALSE)
    
    sample_size <- reactive({
      tmp <- person() %>%
      select(-hours, -income) %>%
      mutate(marker = 1) %>%
      right_join(nzis_shiny) %>%
      filter(!is.na(marker))
      
      n <- nrow(tmp) 
      txt <- paste("Sample size of ", n, " people in the NZIS with this combination of sex, age, occupation, qualification and region.")
      return(txt)
    })
     output$txt1 <- renderText(sample_size())
     
     no_income <- reactive({
      txt <- paste0(round((1 - the_prob()) * 100), "% of this category (taking hours worked into account) are estimated to have zero income from any source.")
      return(txt)
     })
     output$txt2 <- renderText(no_income())

})
