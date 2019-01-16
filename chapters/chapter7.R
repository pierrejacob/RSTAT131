rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

## compute filtering means with Kalman filter on a hidden AR(1) model
## and then approximate it with particle filter, to see if it seems to work or not
## Model: X_0 ~ Normal(0,1), X_t|X_{t-1} ~ Normal(phi X_{t-1}, sigma_w^2),
## and Y_t|X_t ~ Normal(X_t,sigma_v^2)
m0 <- 0
C0 <- 1
#
phi <- 0.9
sigma_w <- 1
A <- 1
sigma_v <- 1
# generate data
T <- 100
Y <- rep(0, T)
X <- rnorm(1, m0, sqrt(C0))
for (t in 1:T){
  X <- rnorm(1, 0.9*X, sqrt(sigma_w))
  Y[t] <- rnorm(1, X, sqrt(sigma_v))
}
# synthetic time series generated from the model
plot(Y, type = "l")

# Kalman filter, coded manually
filter_means <- rep(0,T+1)
filter_variances <- rep(0,T+1)
filter_means[1] <- m0
filter_variances[1] <- C0
for (t in 1:T){
  # prediction step
  prediction_mean <- 0.9 * filter_means[t]
  prediction_var <- 0.9^2 * filter_variances[t] + sigma_w
  # update step
  K <- prediction_var / (prediction_var + sigma_v)
  filter_means[t+1] <- prediction_mean + K * (Y[t] - prediction_mean)
  filter_variances[t+1] <- (1 - K) * prediction_var
}

lines(x = 0:T, y = filter_means, col = "red")
## check against already-made implementation
library(astsa)
kfilter0run <- Kfilter0(T, Y, 1, m0, C0, 0.9, 1, 1)
lines(x = 1:T, y = kfilter0run$xf[1,1,], col = "orange", lty = 2, lwd = 2)
# seem to agree!

## particle filtering
# with N particles
N <- 50
X <- rnorm(N, m0, sqrt(C0))
hist(X, prob = TRUE)
curve(dnorm(x, m0, sqrt(C0)), add = TRUE)
pfilter_means <- rep(0, T)
#
for (t in 1:T){
  # prediction step
  X <- 0.9 * X + rnorm(N, 0, sqrt(sigma_w))
  # update step
  w <- dnorm(Y[t], mean = X, sqrt(sigma_v))
  normw <- w / sum(w)
  # resample
  indices <- sample(x = 1:N, N, replace = TRUE, prob = normw)
  X <- X[indices]
  pfilter_means[t] <- mean(X)
}

plot(Y, type = "l")
lines(x = 0:T, y = filter_means, col = "red")
lines(x = 1:T, y = pfilter_means, col = "blue", lwd = 3, lty = 2)
# seems to work too!
