library(ggplot2)
library(astsa)
theme_set(theme_classic())
theme_update(axis.text.x = element_text(size = 20),
             axis.text.y = element_text(size = 20),
             axis.title.x = element_text(size = 25, margin=margin(20,0,0,0)),
             axis.title.y = element_text(size = 25, angle = 90, margin = margin(0,20,0,0)),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 25),
             title = element_text(size = 25),
             strip.text = element_text(size = 25),
             legend.position = "bottom")

set.seed(17)
source("helpers.R")
generate_randomness(100)

shinyServer(function(input, output) {
  # Generate a plot of the data.
  output$traceplot <- renderPlot({
    make_traceplot <- function() {
      load(filename)
      if (length(randomness) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(randomness)
      }
      nobs <- length(randomness)
      y <- rep(0, nobs)
      y[1] <- randomness[1]
      y[2] <- input$phi1 * y[1]  + randomness[2]
      for (t in 3:nobs){
        y[t] <- input$phi1 * y[t-1] +  input$phi2 * y[t-2] + randomness[t]
      }
      dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")
    }
    if(input$goButton) {			# if user presses Re-Simulate
      make_traceplot()
    } else	{									# so graph appears on startup
      make_traceplot()
    }
  })

  output$acf <- renderPlot({
    make_acfplot <- function() {
      load(filename)
      if (length(randomness) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(randomness)
      }
      nobs <- length(randomness)
      y <- rep(0, nobs)
      y[1] <- randomness[1]
      y[2] <- input$phi1 * y[1]  + randomness[2]
      for (t in 3:nobs){
        y[t] <- input$phi1 * y[t-1] +  input$phi2 * y[t-2] + randomness[t]
      }
      maxlag <- min(length(y) - 10, input$lag)
      lags <- 0:maxlag
      acfvalues <- acf(y, plot = FALSE, lag.max = maxlag)$acf
      g <- qplot(x = lags, y = acfvalues, geom = "blank")
      g <- g + geom_segment(aes(xend = lags, yend = 0))
      g <- g + ylab("ACF") + xlab("Lag")
      # dashed blue lines corresponding to confidence interval for the autocorrelation under white noise assumption
      g <- g + geom_hline(yintercept = +1.96/sqrt(nobs), linetype = 2, colour = "blue")
      g <- g + geom_hline(yintercept = -1.96/sqrt(nobs), linetype = 2, colour = "blue")
      pol <- polyroot(c(1, -input$phi1, -input$phi2))
      polpaste <- paste("root", 1:2, ":", round(pol, 2), collapse = "; ")
      g <- g + annotate("text", x = maxlag/2, y = 1, label = polpaste,
                   size = 8)
      return(g)
      # acf(y, lag.max = maxlag)
      # dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      # ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")
    }
    if(input$goButton) {			# if user presses Re-Simulate
      make_acfplot()
    } else	{									# so graph appears on startup
      make_acfplot()
    }
  })
  output$pacf <- renderPlot({
    make_pacfplot <- function() {
      load(filename)
      if (length(randomness) != input$obs){
        generate_randomness(input$obs)
        load(filename)
        nobs <- length(randomness)
      }
      nobs <- length(randomness)
      y <- rep(0, nobs)
      y[1] <- randomness[1]
      y[2] <- input$phi1 * y[1]  + randomness[2]
      for (t in 3:nobs){
        y[t] <- input$phi1 * y[t-1] +  input$phi2 * y[t-2] + randomness[t]
      }
      maxlag <- min(length(y) - 10, input$lag)
      lags <- 1:maxlag
      acfvalues <- pacf(y, plot = FALSE, lag.max = maxlag)$acf
      g <- qplot(x = lags, y = acfvalues, geom = "blank")
      g <- g + geom_segment(aes(xend = lags, yend = 0))
      g <- g + ylab("PACF") + xlab("Lag")
      # dashed blue lines corresponding to confidence interval for the autocorrelation under white noise assumption
      g <- g + geom_hline(yintercept = +1.96/sqrt(nobs), linetype = 2, colour = "blue")
      g <- g + geom_hline(yintercept = -1.96/sqrt(nobs), linetype = 2, colour = "blue")
      pol <- polyroot(c(1, -input$phi1, -input$phi2))
      polpaste <- paste("root", 1:2, ":", round(pol, 2), collapse = "; ")
      g <- g + annotate("text", x = maxlag/2, y = 1, label = polpaste,
                        size = 8)
      return(g)
      # acf(y, lag.max = maxlag)
      # dataset <- data.frame(Time = seq(1:nobs), series = "y", y)
      # ggplot(data=dataset, aes(x=Time,y=y, group = series)) + geom_line() + theme(legend.position="none")
    }
    if(input$goButton) {			# if user presses Re-Simulate
      make_pacfplot()
    } else	{									# so graph appears on startup
      make_pacfplot()
    }
  })

  observeEvent(input$goButton, {
    generate_randomness(input$obs)
  })

})
