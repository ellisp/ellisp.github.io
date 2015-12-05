library(shiny)

load("good_series.rda")

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
  titlePanel("World Development Indicators from the World Bank"),
  
  sidebarLayout(
    sidebarPanel(
h3("Introduction"),
HTML("<p>This is an exploratory web app to help navigation of the WDI.  
            The definitive, full set of data is on the 
            <a href = 'http://data.worldbank.org/data-catalog/world-development-indicators'>World Bank's website</a>.  
     This app makes use of the <a href = 'https://cran.r-project.org/web/packages/WDI/index.html'>WDI R package</a> by 
     Vincent Arel-Bundock <varel at umich.edu>.  This app was written by <a href = 'http://ellisp.github.io'>Peter Ellis</a> and you are encouraged to use it but
     I will not be held liable for any damages, misunderstandings, wasted time, etc. from its use."),
p("From the 'List of Series' tab, find a series you are interested in (the search function is helpful).  Copy the 
'indicator' field (eg 'AG.AGR.TRAC.NO') and paste it into this text field below:"),

textInput("indicator", "Paste or type an indicator here and press enter:", value = sample(good_series$indicator, 1)),

p("This only really works on a large monitor.  And it takes about 20 seconds to download the data
  and draw the plot.  So be warned; this is an exploration utility, not a polished end user experience."),

hr(),
checkboxInput("fixed_scale", "Same vertical axis for each facet?", value = TRUE),
checkboxInput("smoother", "Show a trend line (adds a few seconds drawing time)?", value = FALSE),
conditionalPanel("input.smoother",
                 sliderInput("span", "Span for loess smoothing function (smaller is wigglier)", min = 0.1, max = 5, value = 2)
                 ),
checkboxInput("sample_only", "Only show 16 randomly chosen countries/groups?", value = FALSE),
conditionalPanel("input.sample_only",
                 actionButton("samp_count", "A different 16 please!", icon = icon("refresh"))
                 
                 ),
sliderInput("minimum", "Select a minimum number of observations for a country to be included", min = 1, max = 10, step = 1, value = 1),
selectInput("incl_groups", "Show:", 
                   choices = c("Individual Countries", "Country Groups", "Both"),
                   selected = "Individual Countries")

),
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Plot", 
                 plotOutput("the_plot", height = "750px")
                 ), 
        tabPanel("List of series",
                 dataTableOutput("mytable")
                 )
      )
    )
  )
))
