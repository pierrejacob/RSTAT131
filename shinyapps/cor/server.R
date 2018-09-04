library(ggplot2)
set.seed(17)
source("helpers.R")
# ndefault <- 100
generate_randomness(100)

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    makeplot <- function()	{
      load(filename)
      if (length(x) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(x)
      }
      nobs <- length(x)
      # dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      if (input$checkstdized){
        g <- qplot(x = xtilde, y = ytilde, geom = "blank") + geom_point(colour = "blue", size = 5)
        g <- g + geom_abline(intercept = 0, slope = corxy, colour = "red", size = 2)
        g <- g + geom_abline(intercept = 0, slope = yonxtilde, colour = "black", linetype = 2, size = 2)
        # g <- g + geom_abline(intercept = 0, slope = xonytilde, colour = "green", linetype = 2, size = 2)
        print(g + xlim(-5,5) + ylim(-5,5)  + xlab("x standardized") + ylab("y standardized"))
      } else {
        if (input$centered){
          g <- qplot(x = x - mean(x), y = y - mean(y), geom = "blank") + geom_point(colour = "blue", size = 5)
          g <- g + geom_abline(intercept = 0, slope = corxy, colour = "red", size = 2)
          g <- g + geom_abline(intercept = 0, slope = yonx, colour = "black", linetype = 2, size = 2)
          # g <- g + geom_abline(intercept = 0, slope = xony, colour = "green", linetype = 2, size = 2)
          print(g + xlim(-10,10) + ylim(-10,10) + xlab("x centered") + ylab("y centered"))
        }
        else {
          g <- qplot(x = x, y = y, geom = "blank") + geom_point(colour = "blue", size = 5)
          g <- g + geom_abline(intercept = 0, slope = corxy, colour = "red", size = 2)
          g <- g + geom_abline(intercept = yonxintercept, slope = yonx, colour = "black", linetype = 2, size = 2)
          # g <- g + geom_abline(intercept = xonyinterceptxony, slope = 1/xony, colour = "green", linetype = 2, size = 2)
          print(g + xlim(-5, 10) + ylim(-5, 15) + xlab("x") + ylab("y"))
        }
      }
    }
      # ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")   }
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
