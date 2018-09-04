
shinyUI(fluidPage(
  titlePanel("AR Simulation"),
  withMathJax(),
  helpText("Model: \\(Y_t = \\phi Y_{t-1} + \\sigma \\mathcal{N}(0, 1) \\)."),

  sidebarLayout(position = "right",
                sidebarPanel(
                  selectInput(inputId = "obs",
                              label = "Observations:",
                              choices = c(10, 50, 100, 250, 500, 1000),
                              selected = 100),
                  br(),
                  sliderInput(inputId = "phi", label = ("Autoregressive Parameter"), min = -1.05, max = 1.05, value = 0, step = 0.01),
                  br(),
                  sliderInput(inputId = "sigma", label = ("Noise std deviation"), min = 0, max = 1, value = 0.5, step = 0.05),
                  br(),
                  actionButton("goButton", "Simulate")
                ),
                mainPanel(
                  "",
                  plotOutput("plot"),
                  br(),
                  br(),
                  br(),
                  p("Based on ",
                    a("Andrew Q. Philips", href = "http://people.tamu.edu/~aphilips/"))
                )
  )



))

