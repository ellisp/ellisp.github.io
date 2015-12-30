library(shiny)
library(randomForest)
library(ggvis)
library(dplyr)
load("models.rda")

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
# input <- data.frame(sex = "male", agegrp = "35-39", occupation = "Community and Personal Service Workers", qualification = "Other", region = "Southland", hours = 35)
  
  the_data <- reactive({
    person <-   nzis_skeleton
    person[1, "hours"] <- input$hours
    person[1, "sex"] <- input$sex
    person[1, "agegrp"] <- input$agegrp
    person[1, "occupation"] <- input$occupation
    person[1, "qualification"] <- input$qualification
    person[1, "region"] <- input$region
    person[1, "Asian"] <- "No" 
    person[1, "Maori"] <- "No" 
    person[1, "European"] <- "No" 
    person[1, "Pacific"]  <- "No" 
    person[1, "Other"]  <- "No" 
    
    for(eth in c("Asian", "Maori", "European", "Pacific", "Other")){
      if(eth %in% input$ethnicity) person[1, eth] <- "Yes"
      }
    
    prob <- predict(mod1_shiny, newdata = person, type = "response")
    point <- predict(mod2_shiny, newdata = person)
    
    set.seed(123)
    n <- 10000
    positives <- round(prob * n)
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
      tmp <- person %>%
      left_join(nzis_shiny)
      n <- nrow(tmp) * 1174
      txt <- paste("<p>An estimated", round(n, -2), "people in New Zealand in 2011 actually fitted this description.</p>")
      return(txt)
    })
   
   output$txt <- renderText(sample_size())

})
