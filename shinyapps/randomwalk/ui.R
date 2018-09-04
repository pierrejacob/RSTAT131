shinyUI(fluidPage(
  titlePanel("Random Walks Simulation"),
  withMathJax(),
  helpText("Model: \\(Y_t = \\delta +  Y_{t-1} + \\sigma \\mathcal{N}(0, 1) \\)."),

  sidebarLayout(position = "right",
                sidebarPanel(
                  selectInput(inputId = "obs",
                              label = "Observations:",
                              choices = c(100, 250, 500, 1000),
                              selected = 100),
                  br(),
                  sliderInput(inputId = "delta", label = ("Drift"), min = -1, max = 1, value = 0, step = 0.05),
                  br(),
                  sliderInput(inputId = "sigma", label = ("Noise std deviation"), min = 0, max = 10, value = 1, step = 0.1),
                  br(),
                  actionButton("goButton", "Simulate")
                ),
                mainPanel(
                  "",
                  plotOutput("plot")
                )
  )



))

