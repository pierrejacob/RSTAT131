rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()


# we start by checking that we have understood what the Yule-Walker methods computes
y <- arima.sim(list(order = c(2, 0, 0), ar = c(0.7,
                                               -0.5)), n = 10000)
ar.yw(y, order.max = 2)
gamma <- acf(y,lag.max=2, type="covariance")$acf[,,1]
Gamma <- matrix(0, 2, 2)
Gamma[1,1] <- Gamma[2,2] <- gamma[1]
Gamma[2,1] <- Gamma[1,2] <- gamma[2]
solve(Gamma, gamma[2:3])

## asymptotic distribution of Yule-Walker estimates

library(doRNG)
library(doParallel)
library(foreach)
registerDoParallel(cores = 6)
nrep <- 1000
coefs <- foreach(i = 1:nrep, .combine = rbind) %dorng% {
  ysim <- arima.sim(list(order = c(2, 0, 0), ar = c(0.7, -0.5)), n = 10000)
  aryw <- ar.yw(ysim,order.max = 2)
  aryw$ar
}
g <- ggplot(data.frame(coefs), aes(x = X1, y = X2)) + geom_point() + xlab(expression(phi[1]))
g <- g + stat_density2d()
g <- g + ylab(expression(phi[2]))
g <- g + geom_point(data=NULL, aes(x = 0.7, y = -0.5), colour = "red", size = 5)
g

# and if the model is misspecified?
nrep <- 1000
coefs <- foreach(i = 1:nrep, .combine = rbind) %dorng% {
  ysim <- arima.sim(list(order = c(1, 0, 2), ar = c(0.3), ma = c(-0.4, 0.3)), n = 10000)
  aryw <- ar.yw(ysim,order.max = 2)
  aryw$ar
}
g <- ggplot(data.frame(coefs), aes(x = X1, y = X2)) + geom_point() + xlab(expression(phi[1]))
g <- g + stat_density2d()
g <- g + ylab(expression(phi[2]))
g
# the estimates still converge somewhere

## Likelihood in an AR(1) model
y <- arima.sim(list(order = c(1,0,0), ar = c(0.6)), n = 1000)
likelihood <- function(varphi, sigma2){
  term <- dnorm(y[1], mean = 0, sd = sqrt(sigma2/(1-varphi^2)), log = TRUE)
  for (t in 2:length(y)){
    term <- term + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
  }
  return(term)
}
z <- expand.grid(f = seq(from= -0.99, to = 0.99, length.out = 25),
                 s = seq(from = 0.2, to = 2, length.out = 25))
values <- sapply(1:nrow(z), function(index) likelihood(z[index,1], z[index,2]))
z$values <- values
theta_mle <- z[which.max(z[,3]),1:2]

gg <- ggplot(z, aes(x=f, y=s, fill=values))
gg <- gg + geom_raster() + geom_contour(aes(z = values), bins = 50, colour = "black")
gg <- gg + scale_fill_gradient2(low="red", mid = "yellow", high="blue" , midpoint = -5000)
gg <- gg + geom_point(data=NULL, aes(x = theta_mle$f, y = theta_mle$s), size = 5, colour = "red")
gg <- gg + xlab(expression(phi[1])) + ylab(expression(sigma^2))
gg <- gg + theme(legend.position = "none")
gg

## optimization
## via Newton Raphson
## on the AR(1) log-likelihood obtained after 10 observations
library(numDeriv)
ll <- function(varphi, sigma2){
  term <- dnorm(y[1], mean = 0, sd = sqrt(sigma2/(1-varphi^2)), log = TRUE)
  for (t in 2:10){
    term <- term + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
  }
  return(term)
}

f <- function(varphi) -ll(varphi, 1)
x <- seq(from = -0.99, to = 0.99, length.out = 1000)
g <- qplot(x = x, y = f(x), geom = "line")
g
x0 <- -0.7
l0 <- paste("x[0]")
g1 <-  g + geom_vline(xintercept = x0, linetype = 2) + annotate("text", x = -0.6, y = 13,
                                                                label = l0, parse = TRUE, size = 5)
g1

approx0 <- function(x) f(x0) + (x-x0)*grad(f, x0)
g2 <- g1 + geom_line(y = approx0(x), linetype = 1, colour = "red")
g2

approx1 <- function(v) return(f(x0) + (v-x0)*grad(f, x0) + 0.5*(v-x0)^2*hessian(f, x0)[1,1])
g2 <- g1 + geom_line(y = approx1(x), linetype = 1, colour = "red")
g2

x1 <- x0 - hessian(f, x0)^(-1) * grad(f, x0)
g2 <-g2 + geom_vline(xintercept = x1, linetype = 2, col = "red") +
  annotate("text", x = -0.15, y = 13, label = "x[1]", parse = TRUE, size = 5, col = "red")
g2

approx2 <- function(v) return(f(x1) + (v-x1)*grad(f, x1) + 0.5*(v-x1)^2*hessian(f, x1)[1,1])
g3 <- g2 + geom_line(y = approx2(x), linetype = 1, colour = "blue")
g3

x2 <- x1 - hessian(f, x1)[1,1]^(-1) * grad(f, x1)
g3 <- g3 + geom_vline(xintercept = x2, linetype = 2, col = "blue") +
  annotate("text", x = 0.22, y = 13.25, label = "x[2]", parse = TRUE, size = 5, col = "blue")
g3

# now with 1000 observations
ll <- function(varphi, sigma2){
  term <- dnorm(y[1], mean = 0, sd = sqrt(sigma2/(1-varphi^2)), log = TRUE)
  for (t in 2:1000){
    term <- term + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
  }
  return(term)
}

f <- function(varphi) -ll(varphi, 1)
x <- seq(from = -0.99, to = 0.99, length.out = 1000)
g <- qplot(x = x, y = f(x), geom = "line")
g

x0 <- -0.7
l0 <- paste("x[0]")
g1 <-  g + geom_vline(xintercept = x0, linetype = 2) + annotate("text", x = -0.6, y = 1500,
                                                                label = l0, parse = TRUE, size = 5)
g1

approx1 <- function(v) return(f(x0) + (v-x0)*grad(f, x0) + 0.5*(v-x0)^2*hessian(f, x0)[1,1])
g2 <- g1 + geom_line(y = approx1(x), linetype = 1, colour = "red")
g2
x1 <- x0 - hessian(f, x0)[1,1]^(-1) * grad(f, x0)
g2 <-g2 + geom_vline(xintercept = x1, linetype = 2, col = "red") +
  annotate("text", x = 0.55, y = 1500, label = "x[1]", parse = TRUE, size = 5, col = "red")
g2
# already near the minimizer after 1 step of Newton Raphson
# because the log-likelihood becomes quadratic as the numbers of observations increases


# computes log-likelihood in AR(1) model
# based on multivariate Normal density evaluation (cost of T^3)
T <- 1000
varphi <- 0.3
sigma2 <- 0.8
G <- matrix(0, T, T)
for (i in 1:T)
  for (j in 1:i)
    G[i,j] <- G[j,i] <- varphi^(abs(i-j))
G <- G * var_stat
library(mvtnorm)
dmvnorm(y[1:T], mean = rep(0, T), sigma = G, log = TRUE)
# or based on conditional decomposition (cost of T)
var_stat <- sigma2/(1-varphi^2)
ll <- dnorm(y[1], mean = 0, sd = sqrt(var_stat), log = TRUE)
for (t in 2:T){
  ll <- ll + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
}
ll
# same exact result, but cost is much lower

## model choice
## compute AIC, BIC and sum of squared prediction errors
## on sunspot data
plot(sunspot.year)

sunspot_test <- sunspot.year[280:289]
sunspot_train <- sunspot.year[1:279]
T <- length(sunspot_train)
n.ahead <- length(sunspot_test)

plot(sunspot_train, type = "l", ylab = "# sunspots", xlab = "year")
acf(sunspot_train, main = "ACF # sunspots")
pacf(sunspot_train, main = "PACF # sunspots")

df <- data.frame()
for (p in 0:10){
  cat("p = ", p, "/", 10, "\n")
  for (q in 0:10){
    fit <- arima(sunspot_train, order = c(p,0,q))
    bic <- AIC(fit, k = log(length(sunspot_train)))
    predarma <- predict(fit, n.ahead = n.ahead)
    sumsquares <- sum((predarma$pred - sunspot_test)^2)
    df <- rbind(df, data.frame(p = p, q = q, aic = fit$aic, bic = bic, sumsquares = sumsquares))
  }
}

library(ggthemes)
df$biccat <- cut(df$bic, breaks = quantile(df$bic, probs = c(0,0.2, 0.4, 0.6, 0.8,1)))
df$aiccat <- cut(df$aic, breaks = quantile(df$aic, probs = c(0,0.2, 0.4, 0.6, 0.8,1)))
df$sumcat <- cut(df$sumsquares, breaks = quantile(df$sumsquares, probs = c(0,0.2, 0.4, 0.6, 0.8,1)))

g <- ggplot(df, aes(x = p, y = q, fill = aiccat)) + geom_tile()
g <- g + scale_fill_pander()
g <- g + theme(legend.position = "none")
g <- g + geom_text(aes(label = round(aic)))
g <- g + scale_x_continuous(breaks = 0:10)
g <- g + scale_y_continuous(breaks = 0:10)
g

g <- ggplot(df, aes(x = p, y = q, fill = biccat)) + geom_tile()
g <- g + scale_fill_pander()
g <- g + theme(legend.position = "none")
g <- g + geom_text(aes(label = round(bic)))
g <- g + scale_x_continuous(breaks = 0:10)
g <- g + scale_y_continuous(breaks = 0:10)
g

g <- ggplot(df, aes(x = p, y = q, fill = sumcat)) + geom_tile()
g <- g + scale_fill_pander()
g <- g + theme(legend.position = "none")
g <- g + geom_text(aes(label = round(sumsquares)))
g <- g + scale_x_continuous(breaks = 0:10)
g <- g + scale_y_continuous(breaks = 0:10)
g
