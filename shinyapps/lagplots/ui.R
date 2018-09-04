# Define UI for random distribution application
shinyUI(fluidPage(

  # Application title
  titlePanel("Time Series Visualization"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dataset", "Dataset:",
                   c("Johnson & Johnson" = "jj",
                     "Speech" = "speech",
                     "Earthquake" = "EQ5",
                     "Explosion" = "EXP6",
                     "Temperatures" = "gtemp",
                     "Chocolate" = "chocolate",
                     "Gaussian noise" = "gn",
                     "MA(2)" = "ma2",
                     "AR(1)" = "ar1",
                     "Cosine + noise" = "cosine")),
      br(),
      sliderInput("k",
                  "Lagplot:",
                  value = 1,
                  min = 1,
                  max = 200),
      br(),
      sliderInput("lag",
                  "ACF maximum lag:",
                  value = 30,
                  min = 10,
                  max = 200)
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Traceplot", plotOutput("traceplot")),
                  tabPanel("Autocorrelogram", plotOutput("acf")),
                  tabPanel("Lagplot", plotOutput("lagplot")),
                  tabPanel("Histogram", plotOutput("histogram"))

      )
    )
  )
))
