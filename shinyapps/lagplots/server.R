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

chocolate <- read.csv("https://sites.google.com/site/pierrejacob/chocolate.csv")
dates <- as.Date(as.character(chocolate[,1]), format = "%m/%d/%y")
chocolate <- chocolate[,2]

cosine <- 2*cos(2*pi/50*(1:1000)) + rnorm(1000)

ar1 <- arima.sim(n = 1000, model = list(ar = c(0.95)))

gn <- rnorm(1000)

ma2 <- arima.sim(n = 1000, model = list(ma = c(0.2, -0.2)))

shinyServer(function(input, output) {
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    dataset <- switch(input$dataset,
                   gtemp = gtemp,
                   jj = jj,
                   speech = speech,
                   EXP6 = EXP6,
                   EQ5 = EQ5,
                   chocolate = chocolate,
                   cosine = cosine,
                   ma2 = ma2,
                   ar1 = ar1,
                   gn = gn)
    as.numeric(dataset)
  })

  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$traceplot <- renderPlot({
    dist <- input$dist
    dataset <- data()

    g <- qplot(x = 1:length(dataset), y = dataset, geom = "line") + geom_point()
    g <- g + xlab("Time") + ylab("y")
    g
  })

  output$histogram <- renderPlot({
    dist <- input$dist
    dataset <- data()

    g <- qplot(x = dataset, geom = "blank") + geom_histogram(aes(y = ..density..))
    g <- g + xlab("y")
    g
  })

  output$acf <- renderPlot({
    dist <- input$dist
    dataset <- data()
    maxlag <- min(length(dataset) - 10, input$lag)
    acf(dataset, lag.max = maxlag)
  })

  output$lagplot <- renderPlot({
    dist <- input$dist
    dataset <- data()
    # print(dataset)
    k <- input$k
    if (input$k >= length(dataset)){
      k <- 1
    }
    lagdataset_y <- dataset[(1+k):length(dataset)]
    lagdataset_x <- dataset[1:(length(dataset)-k)]
    g <- qplot(x = lagdataset_x, y = lagdataset_y, geom = "point")
    g <- g + xlab("y[t-lag]") + ylab("y[t]")
    g
  })

})
