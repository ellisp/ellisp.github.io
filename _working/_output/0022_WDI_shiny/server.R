library(shiny)
library(ggplot2)
library(WDI)
library(RCurl)
library(dplyr)
library(showtext) # for fonts

font.add.google("Poppins", "myfont")
showtext.auto()

load("good_series.rda")

country_groups <- c("1A","4E","Z4","7E","Z7","XC","EU","F1","XD","XE","XJ",
                    "ZJ","XL","XM","XN","XO","ZQ", "DZ", "",
                    "XP","XQ","XU","XR","XS","OE","8S","ZF","ZG","XT","1W")

shinyServer(function(input, output, session) {
   output$mytable = renderDataTable({good_series})
  
   the_data <- reactive({
      series <- input$indicator
      x <- WDI(indicator = series, end = 2015, start = 1960)
      return(x)
   })
   
   the_plot <- reactive({
      x <- the_data()
      series <- input$indicator
      
      if(input$incl_groups == "Individual Countries"){
         x <- x %>%
            filter(!iso2c %in% country_groups)
      } 
      
      if(input$incl_groups == "Country Groups"){
         x <- x %>%
            filter(iso2c %in% country_groups)
      } 
      
      if(nrow(x) == 0){
         x <- the_data()
        updateSelectInput(session, "incl_groups", selected = "Both")
      }
      
      names(x)[names(x) == series] <- "value"
      
      selected_countries <- x %>%
         group_by(country) %>%
         summarize(n = sum(!is.na(value)),
                   ave = mean(value, na.rm = TRUE, tr = 0.2)) %>%
         filter(n > input$minimum) %>%
         arrange(ave)
      
      if(input$fixed_scale){
         the_facet <- facet_wrap(~country)
      } else {
         the_facet <- facet_wrap(~country, scales = "free_y")
      }
      
      if(input$smoother) {
         the_smooth <- geom_smooth(se = FALSE, span = input$span, method = "loess")
      } else {
         the_smooth <- NULL
      }
      
      if(input$sample_only & nrow(selected_countries) > 16){
         set.seed(input$samp_count)
         selected_countries <- selected_countries[sample(1:nrow(selected_countries), 16, replace = FALSE), ] %>%
            arrange(ave)
         
      }
      
      
      p1 <- x %>%
            filter(country %in% selected_countries$country) %>%
            filter(!is.na(value)) %>%
            mutate(country = factor(country, levels = selected_countries$country)) %>%
            ggplot(aes(x = year, y = value, colour = country)) +
            geom_point(size = 1) +
            the_facet +
            the_smooth +
            theme_grey(10, base_family = "myfont") +
            theme(legend.position = "none") +
            ggtitle(good_series[good_series$indicator == series, 2]) +
            labs(x = "", y = "")
      return(p1)
   })
   
   output$the_plot <- renderPlot(the_plot(), res = 100)
   
})
