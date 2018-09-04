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
        y[t] <- input$phi * y[t-1] + input$sigma * randomness[t]
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

# this is my first shiny app
# it has been done by trial and error from this simple app from Andy Philips
# # --------------------------------#
# library(ggplot2)
# shinyServer(function(input, output) {
#
#   output$plot <- renderPlot({
#     makeplot <- function()	{
#       y <- arima.sim(n = input$obs, list(ar = input$phi))
#       dataset <- data.frame(Time = seq(1:input$obs),series = "y",y)
#       ggplot(data=dataset, aes(x=Time,y=y, group = series, colour = series)) + geom_line() + theme_minimal() + scale_color_brewer(palette="Accent") + theme(legend.position="none")
#     }
#     if(input$goButton) {			# if user presses Re-Simulate
#       makeplot()
#     } else	{									# so graph appears on startup
#       makeplot()
#     }
#   })
#
# })
