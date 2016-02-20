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
          h3("Free parameters"),
          actionButton("goButton", "Go!", width = "100%"),
          h4("Consumer"),
          sliderInput("beta", "Discount factor", 0.9, 0.999, 0.99),
          sliderInput("tau", "Capital depreciation rate", 0, 0.1, 0.025),
          sliderInput("varphi", "Investment adjustment cost", 2, 13, 6.771),
          sliderInput("psi", "Capacity utilisation cost", 0.05, 0.3, 0.169),
          sliderInput("sigma_c", "Relative risk aversion", 0.9, 2, 1.353),
          sliderInput("h", "Habit formation intensity", 0.3, 1, 0.573),
          sliderInput("sigma_l_inv", "Labour elasticity wrt wage", 0.1, 0.9, 1 / 2.4),
          sliderInput("omega", "Labour disutility", 0.5, 1.5, 1)
         
      ),
      column(6,
             h4("Other"),
             sliderInput("alpha", "Capital share in output", 0, 0.9, step = 0.01, 0.3),
             sliderInput("gamma_w", "Indexation for non-optimising workers", 0, 1, step = 0.001, 0.763),
             sliderInput("lambda_w", "Wage markup", 0.3, 0.7, step = 0.001, 0.5),
             sliderInput("xi_w", "Probability of missing wage-change signal", 0.6, 1, step = 0.001, 0.737),
             sliderInput("gamma_p", "Indexation for non-optimising firms", 0, 1, step = 0.001, 0.469),
             sliderInput("xi_p", "Probability of missing price-change signal", 0.6, 1, step = 0.001, 0.908),
             sliderInput("r_pi", "Weight given by monetary authority to inflation", 0, 3, step = 0.001, 1.684)
             )
      
               
),
    # Show a plot of the generated distribution
    column(9,
       fixedRow(
           column(4,
           selectInput("shock_var", "Choose a variable to shock by",
                   choices = c("Preference", "Labour supply", "Investment", "Productivity",
                               "Wage markup", "Price markup", "Government spending", "Interest rates", 
                               "Inflation objective"))
           ),
       column(5,
       
          conditionalPanel("input.shock_var == 'Preference'",
                           sliderInput("eta_b", "Preference shock", 0, 0.75, shocks[1, "value"])),
          conditionalPanel("input.shock_var == 'Labour supply'",
                           sliderInput("eta_L", "Labour supply shock", 0, 7, step = 0.01, shocks[2, "value"])),
          conditionalPanel("input.shock_var == 'Investment'",
                           sliderInput("eta_I", "Investment shock", 0, 0.2, shocks[3, "value"])),
          conditionalPanel("input.shock_var == 'Productivity'",      
                           sliderInput("eta_a", "Productivity shock", 0, 1.2, shocks[4, "value"])),
          conditionalPanel("input.shock_var == 'Wage markup'",      
                           sliderInput("eta_w", "Wage markup shock", 0, 1.4, shocks[5, "value"])),
          conditionalPanel("input.shock_var == 'Price markup'",      
                           sliderInput("eta_p", "Price markup shock", 0, 1.6, shocks[6, "value"])),
          conditionalPanel("input.shock_var == 'Government spending'",      
                           sliderInput("eta_G", "Government spending shock", 0, 0.75, shocks[7, "value"])),
          conditionalPanel("input.shock_var == 'Interest rates'",      
                           sliderInput("eta_R", "Interest rates shock", 0, 0.2, shocks[8, "value"])),
          conditionalPanel("input.shock_var == 'Inflation objective'",      
                           sliderInput("eta_pi", "Inflation objective shock", 0, 0.05, shocks[9, "value"]))
       )),
       
       
      plotOutput("irf_plot", height = "600px"),
      HTML("<p>This is an implementation using R, gEcon and Shiny of the <i>Smets-Wouters '03</i> 
Dynamic Stochastic General Equilibrium model of the Euro zone.  See this
<a href = 'http://ellisp.github.io/blog/2016/02/20/DSGE/'>blog post by Peter Ellis</a> for
further information.</p>")
      
      
    )
  
)
)

