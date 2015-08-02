library(shiny)
library(dygraphs)
library(tidyr)
library(dplyr)

load("benefits.rda")

shinyServer(function(input, output) {
    
   the_data_ts <- reactive({
   
      the_data <- benefits %>%
         filter(Category == input$cat) %>%
         select(-Category) %>%
         spread(Variable, Value) %>%
         select(-Period)
      
      tmp <- ts(the_data[ , 1], frequency = 4, start = c(2010, 2))
      
      for(i in 2:ncol(the_data)){
         tmp <- cbind(tmp, ts(the_data[ , i], frequency = 4, start = c(2010, 2)))
      }
      
      colnames(tmp) <- names(the_data)
      return(tmp)
   })
   
   the_graph <- reactive({
      tmp <- dygraph(the_data_ts()) %>%
      dyOptions(labelsKMB = "K", animatedZooms = FALSE) %>%
      dyAxis("y", label = "Recipients of main benefits") %>%
      dyLegend(labelsSeparateLines = TRUE, labelsDiv = "LegendDiv") %>%
      dyRangeSelector(height = 20)       
      
      if(input$stack == "Stacked"){
         tmp <- tmp %>%
            dyOptions(stackedGraph = TRUE, labelsKMB = "K", animatedZooms = FALSE)
      }
      
      return(tmp)
   })
   
   output$TimeSeries <- renderDygraph({the_graph()})

})