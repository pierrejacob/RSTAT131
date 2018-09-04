# Define UI for random distribution application
shinyUI(fluidPage(

  # Application title
  titlePanel("ACF of AR(2) processes"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "obs",
                  label = "Observations:",
                  choices = c(10, 50, 100, 250, 500, 1000),
                  selected = 100),
      br(),
      sliderInput(inputId = "phi1", label = ("AR coef 1"),
                  min = -1.05, max = 1.05, value = 0, step = 0.01),
      br(),
      sliderInput(inputId = "phi2", label = ("AR coef 2"),
                  min = -1.05, max = 1.05, value = 0, step = 0.01),
      br(),
      sliderInput("lag",
                  "ACF maximum lag:",
                  value = 30,
                  min = 10,
                  max = 200),
      br(),
      actionButton("goButton", "Simulate")
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Traceplot", plotOutput("traceplot")),
                  tabPanel("Autocorrelogram", plotOutput("acf")),
                  tabPanel("Partial autoc.", plotOutput("pacf"))

      )
    )
  )
))
