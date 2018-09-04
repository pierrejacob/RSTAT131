
shinyUI(fluidPage(
  titlePanel("Stochastic Volatility Simulation"),
  withMathJax(),
  helpText("Model: \\(X_t = \\exp(X_t) \\mathcal{N}(0, 1) \\), and \\(X_t = \\phi X_{t-1} + \\sigma \\mathcal{N}(0, 1)\\)."),

  sidebarLayout(position = "right",
                sidebarPanel(
                  selectInput(inputId = "obs",
                              label = "Observations:",
                              choices = c(500, 1000, 5000),
                              selected = 1000),
                  br(),
                  sliderInput(inputId = "phi", label = ("Autoregressive Parameter"), min = -1, max = 1, value = 0.95, step = 0.01),
                  br(),
                  sliderInput(inputId = "sigma", label = ("Noise std deviation"), min = 0, max = 1, value = 0.5, step = 0.05),
                  br(),
                  actionButton("goButton", "Simulate")
                ),
                mainPanel(
                  "",
                  plotOutput("plot"),
                  br(),
                  br()
                )
  )



))

