rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

## First, try filtering and smoothing on the Nile data set
# and with premade functions from the package dlm
library(dlm)
plot(Nile, type="o", col = c("darkgrey"),  xlab = "", ylab = "Level", lwd = 2)

NilePoly <- dlmModPoly(order = 1, dV = 15100, dW = 1468)
unlist(NilePoly)
NileFilt <- dlmFilter(Nile, NilePoly)
lines(dropFirst(NileFilt$m), lty = "longdash", lwd = 2)

NileSmooth1 <- dlmSmooth(Nile, NilePoly)
lines(dropFirst(NileSmooth1$s), lty = 4, col = "darkred", lwd = 2)

# we see the filtering and smoothing means at all times, overlaid with the data

## For another motivation, let us look at structural models applied to Johnson & Johnson's time series
# we'll use the functions in astsa to perform filtering and smoothing
library(astsa)
num = length(jj); A = cbind(1, 1, 0, 0)
# Function to Calculate Likelihood
Linn=function(para){
  Phi = diag(0,4); Phi[1,1] = para[1]
  Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0, 1, 0, 0); Phi[4,]=c(0, 0, 1, 0)
  cQ1 = para[2]; cQ2 = para[3]; cR = para[4] # sqrt of q11, q22, r11
  cQ=diag(0,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2;
  kf = Kfilter0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)
  return(kf$like) }

# Initial Parameters
mu0 = c(.7, 0, 0, 0); Sigma0 = diag(.04, 4)
init.par = c(1.03, .1, .1, .5) # Phi[1,1], the 2 Qs and R
# Estimation
est = optim(init.par, Linn, NULL, method="BFGS", hessian=TRUE,
            control=list(trace=1,REPORT=1))
SE = sqrt(diag(solve(est$hessian)))
u = cbind(estimate=est$par,SE)
rownames(u)=c("Phi11","sigw1","sigw2","sigv")
u
# Smooth
Phi = diag(0,4); Phi[1,1] = est$par[1]
Phi[2,]=c(0,-1,-1,-1); Phi[3,]=c(0,1,0,0); Phi[4,]=c(0,0,1,0)
cQ1 = est$par[2]; cQ2 = est$par[3]; cR = est$par[4]
cQ = diag(1,4); cQ[1,1]=cQ1; cQ[2,2]=cQ2
ks = Ksmooth0(num, jj, A, mu0, Sigma0, Phi, cQ, cR)
# Filter plot
Tfm = ts(ks$xf[1,,], start=1960, freq=4)
Sfm = ts(ks$xf[2,,], start=1960, freq=4)
pf1 = 2*sqrt(ks$Pf[1,1,]); pf2 = 2*sqrt(ks$Pf[2,2,])

plot(Tfm, main="Trend Component", ylab="Trend")
lines(Tfm+pf1, lty=2, col=4); lines(Tfm-pf1,lty=2, col=4)

plot(Sfm, main="Seasonal Component", ylim=c(-5,4), ylab="Season")
lines(Sfm+pf2,lty=2, col=4); lines(Sfm-pf2,lty=2, col=4)

plot(jj, type="p", main="Data (points) and Trend+Season (line)")
lines(Tfm+Sfm)

# Smoothing plot
Tsm = ts(ks$xs[1,,], start=1960, freq=4)
Ssm = ts(ks$xs[2,,], start=1960, freq=4)
p1 = 2*sqrt(ks$Ps[1,1,]); p2 = 2*sqrt(ks$Ps[2,2,])
# par(mfrow=c(3,1))
plot(Tsm, main="Trend Component", ylab="Trend")
lines(Tsm+p1, lty=2, col=4); lines(Tsm-p1,lty=2, col=4)

plot(Ssm, main="Seasonal Component", ylim=c(-5,4), ylab="Season")
lines(Ssm+p2,lty=2, col=4); lines(Ssm-p2,lty=2, col=4)

plot(jj, type="p", main="Data (points) and Trend+Season (line)")
lines(Tsm+Ssm)

## prediction
n.ahead=12; y = ts(append(jj, rep(0,n.ahead)), start=1960, freq=4)
rmspe = rep(0,n.ahead); x00 = ks$xf[,,num]; P00 = ks$Pf[,,num]
Q=t(cQ)%*%cQ; R=t(cR)%*%(cR) # see footnote and discussion below
for (m in 1:n.ahead){
  xp = Phi%*%x00; Pp = Phi%*%P00%*%t(Phi)+Q
  sig = A%*%Pp%*%t(A)+R; K = Pp%*%t(A)%*%(1/sig)
  x00 = xp; P00 = Pp #-K%*%A%*%Pp
  y[num+m] = A%*%xp; rmspe[m] = sqrt(sig)
}
plot(y, type="o", main="", ylab="", ylim=c(5,35), xlim=c(1975,1984))
upp = ts(y[(num+1):(num+n.ahead)]+2*rmspe, start=1981, freq=4)
low = ts(y[(num+1):(num+n.ahead)]-2*rmspe, start=1981, freq=4)
lines(upp, lty=2);  lines(low, lty=2);  abline(v=1980.75, lty=3)

### Now implementation of the Kalman filter and smoother
### on a target tracking example
### model: the latent process contains the horizontal and vertical positions and momenta
## X_t = (x1_t, v_t, x1dot_t, x2dot_t)
## x1_t = x1_{t-1} + x1dot_{t-1} + 0
## x2_t = x2_{t-1} + x2dot_{t-1} + 0
## x1dot_t = phi1 * x1dot_{t-1} + x1noise_t
## x2dot_t = phi2 * x2dot_{t-1} + x2noise_t

## X_t = Phi X_{t-1} + W_t
phi1 <- phi2 <- 0.99
Phi <- matrix(c(1, 0, 1, 0,
                0, 1, 0, 1,
                0, 0, phi1, 0,
                0, 0, 0, phi2), byrow = TRUE, ncol = 4)
scale_w <- 1
Sigma_W <- scale_w * diag(c(0,0,1,1))

## And we observe noisy measurements of the positions
## y1_t = x1_t + y1noise_t
## y2_t = x2_t + y2noise_t
A <- matrix(c(1, 0, 0, 0,
              0, 1, 0, 0), byrow = TRUE, ncol = 4, nrow = 2)
scale_v <- 500
Sigma_V <- scale_v * diag(c(1, 1))
## initial position
m_0 <- rep(0, 4)
C_0 <- diag(rep(1, 4))
lineargaussianmodel <- list(m_0 = m_0, C_0 = C_0, A = A, Phi = Phi, Sigma_W = Sigma_W, Sigma_V = Sigma_V)
## Simulate data
simulate_data <- function(n, model){
  A <- model$A
  Phi <- model$Phi
  Sigma_W <- model$Sigma_W
  Sigma_V <- model$Sigma_V
  Xchain <- matrix(0, nrow = n+1, ncol = 4)
  Ychain <- matrix(0, nrow = n, ncol = 2)
  X <- t(mvtnorm::rmvnorm(1, model$m_0, model$C_0))
  Xchain[1,] <- t(X)
  for (time in 1:n){
    X <- t(mvtnorm::rmvnorm(1, Phi %*% X, Sigma_W))
    Y <- mvtnorm::rmvnorm(1, A %*% X, Sigma_V)
    Xchain[time+1,] <- t(X)
    Ychain[time,] <- Y
  }
  return(list(Xchain = Xchain, Ychain = Ychain))
}


## simulate trajectory
simulated_data <- simulate_data(500, lineargaussianmodel)

gpath <- qplot(x = simulated_data$Ychain[,1], y = simulated_data$Ychain[,2], geom = "blank") +
  geom_point(colour = "blue") + xlab(expression(X[1])) + ylab(expression(X[2])) +
  geom_path(aes(x = simulated_data$Xchain[,1], y = simulated_data$Xchain[,2]), linetype = 2)
gpath

gx1 <- qplot(x = 1:nrow(simulated_data$Ychain), y = simulated_data$Ychain[,1], geom = "blank") +
  geom_point(colour = "blue") + xlab("time") + ylab(expression(X[1]))
gx1 <- gx1 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = simulated_data$Xchain[,1]), linetype = 2)
gx1

gx2 <- qplot(x = 1:nrow(simulated_data$Ychain), y = simulated_data$Ychain[,2], geom = "blank") +
  geom_point(colour = "blue") + xlab("time") + ylab(expression(X[2]))
gx2 <- gx2 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = simulated_data$Xchain[,2]), linetype = 2)
gx2

kalman <- function(observations, model){
  A <- model$A
  Phi <- model$Phi
  Sigma_W <- model$Sigma_W
  Sigma_V <- model$Sigma_V
  T <- dim(observations)[1]
  # mcurrent is the filtering mean E[X_t | Y_1,...,Y_t, \theta]
  # and Vcurrent is the filtering covariance matrix V[X_t | Y_1,...,Y_t, \theta]
  mcurrent <- model$m_0
  Vcurrent <- model$C_0
  # we store the filtering means at all times
  filtering_means <- matrix(nrow = T+1, ncol = 4)
  filtering_means[1,] <- mcurrent
  filtering_variances <- array(0, dim = c(T+1, 4, 4))
  filtering_variances[1,,] <- Vcurrent
  prediction_means <- matrix(nrow = T, ncol = 4)
  prediction_variances <-  array(0, dim = c(T, 4, 4))
  # and we compute the log-likelihood recursively
  loglikelihood <- 0
  # for all times t=1,...,T
  for (next_time in 1:T){
    # prediction X_next given X_current
    mnext <- Phi %*% mcurrent
    Vnext <- Phi %*% Vcurrent %*% t(Phi) + Sigma_W
    ## update given Y_next
    if (is.na(observations[next_time,1])){
      mcurrent <- mnext
      Vcurrent <- Vnext
    } else {
      # observation at this time
      Y_next <- observations[next_time,]
      # likelihood update
      loglikelihood <- loglikelihood + mvtnorm::dmvnorm(Y_next, mean = A %*% mnext,
                                                        sigma = A %*% Vnext %*% t(A) + Sigma_V, log = TRUE)
      # computation of the new filtering mean and variance
      K <- Vnext %*% t(A) %*% solve(A %*% Vnext %*% t(A) + Sigma_V)
      mcurrent <- mnext + K %*% (Y_next - A %*% mnext)
      Vcurrent <- Vnext - K %*% A %*% Vnext
    }
    # store filtering means
    filtering_means[1+next_time,] <- mcurrent
    filtering_variances[1+next_time,,] <- Vcurrent
    prediction_means[next_time,] <- mnext
    prediction_variances[next_time,,] <- Vnext
  }
  ## Now smoothing
  smooth_means <- matrix(nrow = T+1, ncol = 4)
  smooth_means[T+1,] <- filtering_means[T+1,]
  smooth_variances <- array(0, dim = c(T+1, 4, 4))
  smooth_variances[T+1,,] <- filtering_variances[T+1,,]
  for (t in (T-1):0){
    L <- filtering_variances[t+1,,] %*% t(Phi) %*% solve(prediction_variances[t+1,,])
    m <- filtering_means[t+1,] + L %*% (smooth_means[t+2,] - prediction_means[t+1,])
    V <- filtering_variances[t+1,,] + L %*% (smooth_variances[t+2,,] - prediction_variances[t+1,,]) %*% t(L)
    smooth_means[t+1,] <- m
    smooth_variances[t+1,,] <- V
  }
  return(list(filtering_means = filtering_means, smooth_means = smooth_means,
              loglikelihood = loglikelihood))
}

kalman_results <- kalman(simulated_data$Ychain, lineargaussianmodel)


gpath <- qplot(x = simulated_data$Ychain[,1], y = simulated_data$Ychain[,2], geom = "blank") +
  xlab(expression(X[1])) + ylab(expression(X[2])) +
  geom_path(aes(x = simulated_data$Xchain[,1], y = simulated_data$Xchain[,2]), linetype = 2)
# path + filtering means
gpath + geom_path(aes(x = kalman_results$filtering_means[,1], y = kalman_results$filtering_means[,2]), linetype = 2, colour = "red")
# path + smoothing means
gpath + geom_path(aes(x = kalman_results$smooth_means[,1], y = kalman_results$smooth_means[,2]), linetype = 4, colour = "orange")

# marginal 1
gx1 <- qplot(x = 1:nrow(simulated_data$Ychain), y = simulated_data$Ychain[,1], geom = "blank") +
  xlab("time") + ylab(expression(X[1]))
gx1 <- gx1 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = simulated_data$Xchain[,1]), linetype = 2)
gx1 <- gx1 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = kalman_results$filtering_means[,1]), linetype = 2, colour = "red")
gx1 <- gx1 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = kalman_results$smooth_means[,1]), linetype = 4, colour = "orange")
gx1

# marginal 2
gx2 <- qplot(x = 1:nrow(simulated_data$Ychain), y = simulated_data$Ychain[,2], geom = "blank") +
  xlab("time") + ylab(expression(X[2]))
gx2 <- gx2 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = simulated_data$Xchain[,2]), linetype = 2)
gx2 <- gx2 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = kalman_results$filtering_means[,2]), linetype = 2, colour = "red")
gx2 <- gx2 + geom_line(aes(x = 0:nrow(simulated_data$Ychain), y = kalman_results$smooth_means[,2]), linetype = 4, colour = "orange")
gx2

# compute log-likelihood
loglikelihood_ <- function(sigma, tau){
  phi1 <- phi2 <- 0.99
  Phi <- matrix(c(1, 0, 1, 0,
                  0, 1, 0, 1,
                  0, 0, phi1, 0,
                  0, 0, 0, phi2), byrow = TRUE, ncol = 4)
  Sigma_W <- sigma^2 * diag(c(0,0,1,1))
  ## And we observe noisy measurements of the positions
  ## y1_t = x1_t + y1noise_t
  ## y2_t = x2_t + y2noise_t
  A <- matrix(c(1, 0, 0, 0,
                0, 1, 0, 0), byrow = TRUE, ncol = 4, nrow = 2)
  Sigma_V <- (tau^2) * diag(c(1, 1))
  ## initial position
  m_0 <- rep(0, 4)
  C_0 <- diag(rep(1, 4))
  lineargaussianmodel <- list(m_0 = m_0, C_0 = C_0, A = A, Phi = Phi, Sigma_W = Sigma_W, Sigma_V = Sigma_V)
  kalman_results <- kalman(simulated_data$Ychain, lineargaussianmodel)
  return(kalman_results$loglikelihood)
}

# compute log-likelihood on a grid of parameter values
z <- expand.grid(sigma = seq(from = 0.5, to = 2, length.out = 20),
                 tau = seq(from = 16, to = 30, length.out = 20))
values <- sapply(1:nrow(z), function(index) loglikelihood_(z[index,1], z[index,2]))
z$values <- values
theta_mle <- z[which.max(z[,3]),1:2]
theta_mle

gg <- ggplot(z, aes(x=sigma, y=tau, fill=values))
gg <- gg + geom_raster() + geom_contour(aes(z = values), bins = 10, colour = "black")
gg <- gg + viridis::scale_fill_viridis()
gg <- gg + geom_point(data=NULL, aes(x = theta_mle$sigma, y = theta_mle$tau), size = 5, colour = "red")
gg <- gg + geom_point(data=NULL, aes(x = 1, y = sqrt(500)), size = 5, colour = "blue")
gg <- gg + xlab(expression(sigma)) + ylab(expression(tau))
gg <- gg + theme(legend.position = "none")
gg

## sampling paths from smoothing distribution
kalman_pathsampling <- function(npaths, observations, model){
  A <- model$A
  Phi <- model$Phi
  Sigma_W <- model$Sigma_W
  Sigma_V <- model$Sigma_V
  T <- dim(observations)[1]
  # mcurrent is the filtering mean E[X_t | Y_1,...,Y_t, \theta]
  # and Vcurrent is the filtering covariance matrix V[X_t | Y_1,...,Y_t, \theta]
  mcurrent <- model$m_0
  Vcurrent <- model$C_0
  # we store the filtering means at all times
  filtering_means <- matrix(nrow = T+1, ncol = 4)
  filtering_means[1,] <- mcurrent
  filtering_variances <- array(0, dim = c(T+1, 4, 4))
  filtering_variances[1,,] <- Vcurrent
  prediction_means <- matrix(nrow = T, ncol = 4)
  prediction_variances <-  array(0, dim = c(T, 4, 4))
  # and we compute the log-likelihood recursively
  loglikelihood <- 0
  # for all times t=1,...,T
  for (next_time in 1:T){
    # prediction X_next given X_current
    mnext <- Phi %*% mcurrent
    Vnext <- Phi %*% Vcurrent %*% t(Phi) + Sigma_W
    ## update given Y_next
    if (is.na(observations[next_time,1])){
      mcurrent <- mnext
      Vcurrent <- Vnext
    } else {
      # observation at this time
      Y_next <- observations[next_time,]
      # likelihood update
      loglikelihood <- loglikelihood + mvtnorm::dmvnorm(Y_next, mean = A %*% mnext,
                                                        sigma = A %*% Vnext %*% t(A) + Sigma_V, log = TRUE)
      # computation of the new filtering mean and variance
      K <- Vnext %*% t(A) %*% solve(A %*% Vnext %*% t(A) + Sigma_V)
      mcurrent <- mnext + K %*% (Y_next - A %*% mnext)
      Vcurrent <- Vnext - K %*% A %*% Vnext
    }
    # store filtering means
    filtering_means[1+next_time,] <- mcurrent
    filtering_variances[1+next_time,,] <- Vcurrent
    prediction_means[next_time,] <- mnext
    prediction_variances[next_time,,] <- Vnext
  }
  ## Now sampling paths
  paths <- array(NA, dim = c(T+1, 4, npaths))
  for (ipath in 1:npaths){
    paths[T+1,,ipath] <- mvtnorm::rmvnorm(1, filtering_means[T+1,], filtering_variances[T+1,,])
  }
  for (t in (T-1):0){
    L <- filtering_variances[t+1,,] %*% t(Phi) %*% solve(prediction_variances[t+1,,])
    V_ <- filtering_variances[t+1,,] - L %*% prediction_variances[t+1,,] %*% t(L)
    for (ipath in 1:npaths){
      m_ <- filtering_means[t+1,] + L %*% matrix((paths[t+2,,ipath] - prediction_means[t+1,]), ncol = 1)
      paths[t+1,,ipath] <- mvtnorm::rmvnorm(1, m_, V_)
    }
  }
  return(list(filtering_means = filtering_means, paths = paths,
              loglikelihood = loglikelihood))
}

kalman_paths <- kalman_pathsampling(100, simulated_data$Ychain, lineargaussianmodel)

smoothing_means <- matrix(nrow = nrow(simulated_data$Ychain)+1, ncol = 4)
for (t in 1:(nrow(simulated_data$Ychain)+1)){
  smoothing_means[t,] <- rowMeans(kalman_paths$paths[t,,])
}

library(reshape2)
library(tidyr)
kalman.path.df <- reshape2::melt(kalman_paths$paths[,1:2,])
kalman.path.df <- kalman.path.df %>% tidyr::spread(Var2, value)
names(kalman.path.df) <- c("time", "path", "X1", "X2")

gpath <- qplot(x = simulated_data$Ychain[,1], y = simulated_data$Ychain[,2], geom = "blank") +
  xlab(expression(X[1])) + ylab(expression(X[2]))
gpath2 <- gpath + geom_path(aes(x = kalman_results$smooth_means[,1], y = kalman_results$smooth_means[,2]), linetype = 2, colour = "orange")

gpath2 + geom_path(data=kalman.path.df %>% filter(path < 100),
                            aes(x = X1, y = X2, group = path), alpha = 0.15)

