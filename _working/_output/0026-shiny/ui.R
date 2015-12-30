library(shiny)
library(ggvis)

load("dimensions.rda")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   tags$style(HTML("
@import url('https://fonts.googleapis.com/css?family=Poppins');
                      
body {
         
font-family: 'Poppins', 'Lucida Grande', Verdana, Lucida, Helvetica, Arial, Calibri, sans-serif;
         color: rgb(0,0,0);
         background-color: #d2d2d2;
                      }
")),
  # Application title
  titlePanel("Modelled weekly income based on the New Zealand Income Survey 2011"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("hours",
                  "Hours of work:",
                  min = 0,
                  max = 60,
                  value = 37),
    
    selectInput("sex", 
                "Sex",
                choices = d_sex,
                selected = sample(d_sex, 1)),
    selectInput("agegrp", 
                "Age group",
                choices = d_agegrp,
                selected = sample(d_agegrp, 1)),
    selectInput("occupation", 
                "Current or most recent occupation",
                choices = d_occupation,
                selected = sample(d_occupation, 1)),
    selectInput("qualification", 
                "Highest level of qualification",
                choices = d_qualification,
                selected = sample(d_qualification, 1)),
    selectInput("region", 
                "Region",
                choices = d_region,
                selected = sample(d_region, 1)),
checkboxGroupInput("ethnicity", "Choose one or more ethnic groups",
                  choices = c("Maori", "European", "Asian", "Pacific", "Other"),
                  selected = sample(c("Maori", "European", "Asian", "Pacific"), 1))                
),
    # Show a plot of the generated distribution
    mainPanel(
      ggvisOutput("plot"),
      tableOutput("table"),
      textOutput("txt"),
      p("This page shows an estimated modelled income distribution for 'what if' scenarios of combinations of variables including rare or impossible combinations."),
      p("All estimates should be treated with great caution!  Effective confidence intervals are in many cases around +/- 20% or higher.")
    )
  )
))
