shinyUI(fluidPage(
  titlePanel("Periodic Variations Simulation"),
  withMathJax(),
  helpText("Model: \\(Y_t = A \\cos(2\\pi \\omega t +  \\phi) + \\sigma \\mathcal{N}(0, 1) \\)."),
  sidebarLayout(position = "right",
                sidebarPanel(
                  sliderInput(inputId = "A", label = ("Amplitude"), min = 0, max = 5, value = 2, step = 0.5),
                  br(),
                  sliderInput(inputId = "omega", label = ("Frequency of oscillation"), min = 0, max = 0.1, value = 1.0/50.0, step = 0.005),
                  br(),
                  sliderInput(inputId = "phi", label = ("Phase shift"), min = -5, max = 5, value = 0, step = 0.5),
                  br(),
                  sliderInput(inputId = "sigma", label = ("Noise std deviation"), min = 0, max = 5, value = 0, step = 0.1),
                  br(),
                  actionButton("goButton", "Simulate")
                ),
                mainPanel(
                  plotOutput("plot"),
                  br(),
                  br()
                )
  )



))

