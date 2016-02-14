library(shiny)
load("shocks.rda")

shinyUI(fluidPage(
   tags$style(HTML("
@import url('https://fonts.googleapis.com/css?family=Poppins');

body {

font-family: 'Poppins', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;
         color: rgb(0,0,0);

                      }
")),
   titlePanel("Dynamic Stochastic General Equilibrium Model demonstration"),

   column(3,
      column(6, 
          h3("Starting values"),
          actionButton("goButton", "Go!", width = "100%"),
          sliderInput("z", "z", 0.6, 1.4, step = 0.1, 1),
         sliderInput("z_f", "z_f", 0.6, 1.4, step = 0.1, 1),
         sliderInput("Q", "Q", 0.6, 1.4, step = 0.1, 1),
         sliderInput("Q_f", "Q_f", 0.6, 1.4, step = 0.1, 1),
         sliderInput("pi", "pi", 0.6, 1.4, step = 0.1, 1),
         sliderInput("pi_obj", "pi_obj", 0.6, 1.4, step = 0.1, 1)
      ),
      column(6,
             sliderInput("epsilon_b", "epsilon_b", 0.6, 1.4, step = 0.1, 1),
             sliderInput("epsilon_L", "epsilon_L", 0.6, 1.4, step = 0.1, 1),
             sliderInput("epsilon_I", "epsilon_I", 0.6, 1.4, step = 0.1, 1),
             sliderInput("epsilon_a", "epsilon_a", 0.6, 1.4, step = 0.1, 1),
             sliderInput("epsilon_G", "epsilon_G", 0.6, 1.4, step = 0.1, 1),
             sliderInput("r_k", "r_k", 0.001, 0.07, 0.01),
             sliderInput("r_k_f", "r_k_f", 0.001, 0.07, 0.01))
      
               
),
    # Show a plot of the generated distribution
    column(9,
       fixedRow(
           column(4,
           selectInput("shock_var", "Choose a variable to shock by",
                   choices = c("eta_b", "eta_L", "eta_I", "eta_a",
                               "eta_w", "eta_p", "eta_G", "eta_R", "eta_pi"))
           ),
       column(5,
       
          conditionalPanel("input.shock_var == 'eta_b'",
                           sliderInput("eta_b", "eta_b", 0, 0.75, shocks[1, "value"])),
          conditionalPanel("input.shock_var == 'eta_L'",
                           sliderInput("eta_L", "eta_L", 0, 7, step = 0.01, shocks[2, "value"])),
          conditionalPanel("input.shock_var == 'eta_I'",
                           sliderInput("eta_I", "eta_I", 0, 0.2, shocks[3, "value"])),
          conditionalPanel("input.shock_var == 'eta_a'",      
                           sliderInput("eta_a", "eta_a", 0, 1.2, shocks[4, "value"])),
          conditionalPanel("input.shock_var == 'eta_w'",      
                           sliderInput("eta_w", "eta_w", 0, 1.4, shocks[5, "value"])),
          conditionalPanel("input.shock_var == 'eta_p'",      
                           sliderInput("eta_p", "eta_p", 0, 1.6, shocks[6, "value"])),
          conditionalPanel("input.shock_var == 'eta_G'",      
                           sliderInput("eta_G", "eta_G", 0, 0.75, shocks[7, "value"])),
          conditionalPanel("input.shock_var == 'eta_R'",      
                           sliderInput("eta_R", "eta_R", 0, 0.2, shocks[8, "value"])),
          conditionalPanel("input.shock_var == 'eta_pi'",      
                           sliderInput("eta_pi", "eta_pi", 0, 0.05, shocks[9, "value"]))
       )),
       
       
      plotOutput("irf_plot", height = "600px")
      
      
    )
  
)
)

