library(ggplot2)
set.seed(17)
source("helpers.R")
ndefault <- 100
generate_randomness(100)


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
      y[1] <- randomness[1]
      for (t in 2:nobs){
        y[t] <- input$delta + y[t-1] + input$sigma * randomness[t]
      }
      dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      g <- ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")
      g + geom_line(aes(y = input$delta * seq(0:(nobs-1))), linetype = 2, colour = "red")
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
