library(ggplot2)
set.seed(17)
source("helpers.R")
ndefault <- 100
generate_randomness(100)

input <- list(theta1=0.5, theta2 = 0.2, sigma = 1)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    makeplot <- function()	{
      load(filename)
      if (length(randomness) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(randomness)
      }
      nobs <- length(randomness)
      y <- rep(0, nobs)
      randomness <- input$sigma * randomness
      y[1] <- randomness[1]
      y[2] <- randomness[2] + input$theta1 * randomness[1]
      for (t in 3:nobs){
        y[t] <- randomness[t] + input$theta1 * randomness[t-1] + input$theta2 * randomness[t-2]
      }
      dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")   }
    if(input$goButton) {			# if user presses Re-Simulate
      makeplot()
    } else	{									# so graph appears on startup
      makeplot()
    }
  })
  observeEvent(input$goButton, {
    generate_randomness(input$obs)
  })
})

