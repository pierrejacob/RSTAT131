rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

## start with various stationary and nonstationary AR processes
# first, many repeats of a stationary AR(1) process
mu_0 <- 50
sigma_0 <- 10
rho <- 0.9
sigma <- 1
var_stationary <- (sigma^2)/(1-rho^2)
R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = 0, sd = sqrt(var_stationary))
for (t in 2:T){
  Y[,t] <- rho*Y[,t-1] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

# next, many repeats of the same dynamics but started outside of stationarity
R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = mu_0, sd = sigma_0)
for (t in 2:T){
  Y[,t] <- rho*Y[,t-1] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

# next, an explosive AR process
R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
sigma <- 1
Y[,1] <- rnorm(R, mean = 0, sd = 5)
rho <- 1.02
for (t in 2:T){
  Y[,t] <- rho*Y[,t-1] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")
g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

# and another explosive AR process (with negative coefficient)
R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
sigma <- 1
Y[,1] <- rnorm(R, mean = 0, sd = 5)
rho <- -1.02
for (t in 2:T){
  Y[,t] <- rho*Y[,t-1] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

# when the coefficient is exactly one, what happens?
R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
sigma <- 1
Y[,1] <- rnorm(R, mean = 0, sd = 5)
rho <- 1.0
for (t in 2:T){
  Y[,t] <- rho*Y[,t-1] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

## next we can play with AR(2) processes

phi1 <- 1.1
phi2 <- -0.2
sigma <- 1
polyroot(c(1,-phi1,-phi2))

R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = 0, sd = 1)
Y[,2] <- rnorm(R, mean = 0, sd = 1)
for (t in 3:T){
  Y[,t] <- phi1*Y[,t-1] + phi2*Y[,t-2] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g
# the above one looks "stable" (non-explosive)
# on the other hand, let's look at the following one...
phi1 <- 0.9
phi2 <- 0.2
sigma <- 1
polyroot(c(1,-phi1,-phi2))

R <- 100
T <- 100
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = 0, sd = 1)
Y[,2] <- rnorm(R, mean = 0, sd = 1)
for (t in 3:T){
  Y[,t] <- phi1*Y[,t-1] + phi2*Y[,t-2] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

# we start to see that the condition for stability might have
# to do with the roots of the AR polynomial
phi1 <- 1.1
phi2 <- -0.95
sigma <- 1
polyroot(c(1,-phi1,-phi2))

R <- 10
T <- 100
Y <- matrix(nrow = R, ncol = T)
Y[,1] <- rnorm(R, mean = 0, sd = 1)
Y[,2] <- rnorm(R, mean = 0, sd = 1)
for (t in 3:T){
  Y[,t] <- phi1*Y[,t-1] + phi2*Y[,t-2] + rnorm(R, mean = 0, sd = sigma)
}
Y <- melt(Y)
names(Y) <- c("rep", "Time", "value")

g <- ggplot(Y, aes(x = Time, y = value, group = rep)) + geom_line(alpha = 0.25) + ylab("y")
g

## shiny apps for AR and MA processes
library(shiny)
runGitHub(repo="RSTAT131", username="pierrejacob",subdir="shinyapps/acfautoregressive")
runGitHub(repo="RSTAT131", username="pierrejacob",subdir="shinyapps/acfma")

# we cannot make MA processes explode, no matter the values of the coefficients

## Forecast of cardiovascular mortality
plot(cmort)
y <- as.numeric(cmort)
x <- 1:length(y)
T <- length(y)
n.ahead <- 52
n <- T - n.ahead
ytrain <- y[1:n]
ytest <- y[(n+1):T]

times <- 1:n
times.ahead <- (n+1):T
regression <- lm(ytrain ~ times)
acf(regression$residuals, 60)
pacf(regression$residuals, 60)
plot(times, ytrain, type ="l", xlim = c(200, T), ylab = "cmort", xlab = "Time")
lines(times, predict(regression))
ar <- arima(regression$residuals, order = c(2, 0, 0))
predicted_r <- predict(ar, n.ahead = n.ahead)$pred
predicted_l <- predict(regression, newdata = data.frame(times = times.ahead))
predicted_y <- predicted_l + predicted_r
lines(x = times.ahead, y = predicted_l, col = "red")
lines(x = c(n, times.ahead), y = c(ytrain[n], predicted_y), col = "red")
lines(x = c(n, times.ahead), y = c(ytrain[n], ytest), col = "blue")
## AR(2) predictions do not look plausible

plot(times, ytrain, type ="l", xlim = c(200, T), ylab = "cmort", xlab = "Time")
lines(times, predict(regression))
ar <- arima(regression$residuals, order = c(2, 0, 0), seasonal = list(order = c(1,0,1), period = 52))
predicted_r <- predict(ar, n.ahead = n.ahead)$pred
predicted_l <- predict(regression, newdata = data.frame(times = times.ahead))
predicted_y <- predicted_l + predicted_r
lines(x = times.ahead, y = predicted_l, col = "red")
lines(x = c(n, times.ahead), y = c(ytrain[n], predicted_y), col = "red")
lines(x = c(n, times.ahead), y = c(ytrain[n], ytest), col = "blue")
# Seasonal ARIMA predictions look much more plausible
#
