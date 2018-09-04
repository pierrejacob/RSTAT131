library(ggplot2)
set.seed(17)
source("helpers.R")
ndefault <- 500
generate_randomness(ndefault)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    makeplot <- function()	{
      load(filename)
      y <- rep(0, ndefault)
      # y[1] <- randomness[1]
      for (t in 1:ndefault){
        y[t] <- input$A * cos(2 * pi * input$omega * t + input$phi) + input$sigma * randomness[t]
      }
      dataset <- data.frame(Time = seq(1:ndefault), series = "y", y)
      ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")
      }
    if(input$goButton) {			# if user presses Re-Simulate
      makeplot()
    } else	{									# so graph appears on startup
      makeplot()
    }
  })
  observeEvent(input$goButton, {
    generate_randomness(ndefault)
  })
})
