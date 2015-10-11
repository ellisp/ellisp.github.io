library(shiny)

shinyUI(fluidPage(
   
   # Application title
   titlePanel("Toy simulation of an economy"),
   
   column(3,
         fixedRow(
          column(6,
         selectInput("duration",
                     "Number of years:",
                     choices = c(50, 100, 200, 500, 1000, 2000, 10000),
                     selected = 500),
         sliderInput("start_savings",
                     "Underlying saving rate",
                     min = 0, max = 30, value = 12, step = 0.1),
         sliderInput("start_labour",
                     "Underlying population growth",
                     min = -1, max = 5, value = 1, step = 0.1),
         sliderInput("capital_coef", "Capital coefficient in Cobbs-Douglas production function",
                     min = 0, max = 1, value = 0.3, step = 0.01),
         sliderInput("labour_coef", "Labour coefficient in Cobbs-Douglas production function",
                     min = 0, max = 1, value = 0.6, step = 0.01),
         actionButton("CountMe", "Get a new random seed"),
         hr(),
         submitButton(text = "Apply Changes", icon = NULL)
         ),
         column(6,
            sliderInput("growth_save_thresh", "Growth rate at which people feel encouraged to save",
                        min = 0, max = 10, value = 2, step = 0.1),
            sliderInput("growth_save_strength", "Impact of growth on savings",
                        min = -3, max = 6, value = 3, step = 0.1),
            sliderInput("prod_growth_underlying", "Underlying productivity growth (% per year)",
                        min = -3, max = 3, value = 0, step = 0.1),
            sliderInput("prod_growth_random", "Random element in productivity growth",
                        min = 0, max = 10, value = 5, step = 0.1),
            sliderInput("savings_random", "Random element in savings",
                        min = 0, max = 10, value = 5, step = 0.1),
            sliderInput("labour_random", "Random labour growth",
                        min = 0, max = 10, value = 5, step = 0.1)
            
            
                
         )
         ),
         
         fixedRow(
            hr(),
            column(6,
         sliderInput("war_threshhold", 
                     "Overall proportion of years that are in war",
                     min = 0, max = 0.05, value = 0.01)
         
         
          ),
         column(6,
                sliderInput("war_labour_impact", "Labour after a year of war",
                            min = 0, max = 1, value = c(0.85, 0.95)),
                sliderInput("war_capital_impact", "Capital after a year of war",
                            min = 0, max = 1, value = c(0.75, 0.95))
                )
         
         )
         
         
            
         
         
      ),
    
   
      column(3,
            plotOutput("p1"),
            plotOutput("p4")
            
         ),
         column(3,
                plotOutput("p2"),
                plotOutput("p3")
                ),
         column(3,
                plotOutput("p5"),
                plotOutput("p6")
         )
      )
)    

