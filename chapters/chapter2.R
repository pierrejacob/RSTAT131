rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

library(shiny)
## start with an app showing the relation between regression coefficients
## in a scatter plot and correlation coefficients.
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/cor")

## also don't forget to play at http://guessthecorrelation.com/

## let's generate noise
y <- rnorm(1000)
# traceplot
plot(y, xlab = "Time")
# lag-plot
lag.plot(y, 4)
# autocorrelogram
acf(y)

## let's do the same with an AR(1) process
nobs <- 1000
y <- arima.sim(n = nobs, list(ar = c(0.9)))
plot(y, xlab = "Time")
lag.plot(y, 4)
acf(y)
# ACF shows some correlations, and tails off to zero (it's hard to say when exactly it
# becomes negligible)

# let's do the same with an MA(3) process
y <- arima.sim(n = nobs, list(ma = c(1, 1, 1)))
plot(y, xlab = "Time")
lag.plot(y, 4)
acf(y)
# ACF also shows some correlations, but cuts off after 3 lags

## random walk with drift
input <- list(delta = 0.2, sigma = 1)
randomness <- rnorm(nobs)
y <- rep(0, nobs)
y[1] <- randomness[1]
for (t in 2:nobs){
  y[t] <- input$delta + y[t-1] + input$sigma * randomness[t]
}
plot(y, type = "l", xlab  = "Time")
lines(x = 1:nobs, y = (1:nobs) * input$delta, col = "red", lty = 3)
lag.plot(y, 4)
acf(y)

# this shiny app allows to see these graphs for various series,
# with both real data and synthetic time series
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/lagplots")

### Ridge plots to see the stationarity of some stochastic processes
R <- 250
T <- 1000
# start with cosine + trend
Y <- matrix(nrow = R, ncol = T)
for (t in 1:T){
  Y[,t] <- 2*cos(2*pi*t/200) + rnorm(R)
}
Y <- melt(Y)
tail(Y)
names(Y) <- c("rep", "Time", "value")
# one path
ggplot(Y %>% filter(rep == 1), aes(x = Time, y = value, group = rep)) + geom_line(alpha = 1) + ylab("y") + ylim(-6,6)
# many paths
ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.05) + ylab("y")+ ylim(-6,6)
# ridges
library(ggridges)
g <- ggplot(Y %>% filter(Time %% 4 == 1), aes(x = value, y = factor(Time))) + geom_density_ridges(scale = 10)
g <- g + scale_y_discrete(breaks = c(0,250,500,750,1000)) + xlab("y") + ylab("Time") + xlim(-6,6)
g <- g + coord_flip()
g
## same with AR(1)

R <- 250
T <- 1000
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = 0, sd = sqrt(1/(1-0.99^2)))
for (t in 2:T){
  Y[,t] <- 0.99*Y[,t-1] + rnorm(R, mean = 0, sd = 1)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

# one path
ggplot(Y %>% filter(rep == 2), aes(x = Time, y = value, group = rep)) + geom_line(alpha = 1) + ylab("y") + ylim(-30,30)
# many paths
ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.05) + ylab("y") + ylim(-30,30)
# ridges
g <- ggplot(Y %>% filter(Time %% 4 == 1), aes(x = value, y = factor(Time))) + geom_density_ridges(scale = 10)
g <- g + scale_y_discrete(breaks = c(0,250,500,750,1000)) + xlab("y") + ylab("Time") + xlim(-30,30)
g <- g + coord_flip()
g

## finally some prediction of the J&J time series, with ARMA techniques,
## to motivate the ARMA chapter of the course
plot(jj)
y <- as.numeric(log(jj))
times <- 1:length(y)
regression <- lm(y ~ times)
plot(x = times, y = y)
lines(x = times, y = predict(regression))
r <- residuals(regression)
acf(r)
pacf(r)
# it looks like MA(4) might be appropriate

arimamodel <- arima(r, order = c(4, 0, 0))
future <- 85:100
# prediction of the residuals
predicted_r <- predict(arimamodel, n.ahead = 16)$pred
plot(x = times, y = r, xlim = c(0, 100), type = "l")
lines(future, predicted_r, col = "red")
# on the original scale, prediction of line + residuals
plot(x = times, y = y, xlim = c(0, 100), ylim = c(-1, 4), type = "l")
predicted_l <- predict(regression, newdata = data.frame(times = future))
predicted_y <- predicted_l + predicted_r
var_y <- sum(regression$residuals^2)/regression$df.residual
var_y <- var_y + arimamodel$sigma2
plot(x = times, y = as.numeric(jj), xlim = c(0, 100), ylim = c(0, 35), type = "l", xlab = "Time", ylab = "jj")
lines(x = c(84,future), y = c(jj[84],exp(predicted_y+0.5*var_y)), col = "red")
# gives very plausible prediction, reproducing the seasonality
# but if we go too far in the future
farfuture <- 85:185
predicted_r <- predict(arimamodel, n.ahead = length(farfuture))$pred
plot(x = times, y = y, xlim = c(0, max(farfuture)), ylim = c(-1, 8), type = "l")
predicted_l <- predict(regression, newdata = data.frame(times = farfuture))
predicted_y <- predicted_l + predicted_r
lines(x = farfuture, y = predicted_y, col = "red")
var_y <- sum(regression$residuals^2)/regression$df.residual
var_y <- var_y + arimamodel$sigma2
plot(x = times, y = as.numeric(jj), xlim = c(0, max(farfuture)), ylim = c(0, 1200), type = "l", xlab = "Time", ylab = "jj")
lines(x = c(84,farfuture), y = c(jj[84],exp(predicted_y+0.5*var_y)), col = "red")
# then it does not make sense anymore
