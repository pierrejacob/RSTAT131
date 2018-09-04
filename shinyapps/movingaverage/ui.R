shinyUI(fluidPage(
  titlePanel("MA(2) Simulation"),
  withMathJax(),
  helpText("Model: \\(Y_t = W_t + \\theta_1 W_{t-1} + \\theta_2 W_{t-2}\\),"),
  helpText("where \\(W_t \\sim \\mathcal{N}(0,\\sigma^2)\\)."),
  sidebarLayout(position = "right",
                sidebarPanel(
                  selectInput(inputId = "obs",
                              label = "Observations:",
                              choices = c(10, 50, 100, 250, 500, 1000),
                              selected = 100),
                  br(),
                  sliderInput(inputId = "theta1", label = ("MA Parameter 1"), min = -2, max = 2, value = 0, step = 0.05),
                  br(),
                  sliderInput(inputId = "theta2", label = ("MA Parameter 2"), min = -2, max = 2, value = 0, step = 0.05),
                  br(),
                  sliderInput(inputId = "sigma", label = ("Noise std deviation"), min = 0, max = 1, value = 0.5, step = 0.05),
                  br(),
                  actionButton("goButton", "Simulate")
                ),
                mainPanel(
                  "",
                  plotOutput("plot"),
                  br()
                )
  )
))

