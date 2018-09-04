library(ggplot2)
library(gridExtra)
set.seed(17)
source("helpers.R")
ndefault <- 1000
generate_randomness(ndefault)


shinyServer(function(input, output) {

  output$plot <- renderPlot({
    makeplot <- function()	{
      load(filename)
      if (length(xrandomness) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(xrandomness)
      }
      nobs <- length(xrandomness)
      x <- rep(0, nobs)
      x[1] <- xrandomness[1]
      for (t in 2:nobs){
        x[t] <- input$phi * x[t-1] + input$sigma * xrandomness[t]
      }
      y <- rep(0, nobs)
      for (t in 1:nobs){
        y[t] <- exp(x[t]) * yrandomness[t]
      }
      dataset <- data.frame(Time = seq(1:nobs), series = "x", x)
      gX <- ggplot(data=dataset, aes(x=Time,y=x, group = series)) + geom_line() + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
      dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      gY <- ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(panel.grid.major = element_line(colour = "black", linetype = "dotted"))
      grid.arrange(gY, gX, ncol = 1)
    }
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

