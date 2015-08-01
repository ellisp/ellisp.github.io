library(shiny)
library(dygraphs)

load("benefits.rda")

shinyUI(fluidPage(
#    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://ellisp.github.io/css/bootstrap.min.css")),
#    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://ellisp.github.io/css/bootstrap-theme.min.css")),
#    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://ellisp.github.io/css/main.css")),
      
   titlePanel("Working-age recipients of main benefits - last 5 years"),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("cat", "Choose a type of classification of beneficiary", 
                     choices = unique(benefits$Category), selectize = FALSE),
         radioButtons("stack", "Type of chart", choices = c("Individual lines", "Stacked"),
                      selected = "Individual lines"),
         
         HTML("<p>Dates refer to quarters commencing in the month on the horizontal axis (note that this differs from the original publication, which has the month the quarter <i>completes</i>).</p>")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         dygraphOutput("TimeSeries"),
         HTML("<p>Source: 
   <i><a href = 'https://www.msd.govt.nz/about-msd-and-our-work/publications-resources/statistics/benefit/index.html#Datatables6).'>
              Ministry of Social Development</a></i>.  Graphic by <a href = 'http://ellisp.github.io'><i>Stats in the Wild<i></a>.</p>")
      )
   )
))
