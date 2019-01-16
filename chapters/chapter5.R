rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

## Bayesian inference for AR(1) process
y <- arima.sim(list(order = c(1,0,0), ar = c(0.6)), n = 300)
plot(y)
# prior on variance
curve(dgamma(x, shape = 2, rate = 0.5), from = 0, to = 10, main = "Gamma(2,0.5)", y = "density",
      xlab = expression(sigma^{2}))
# likelihood
likelihood <- function(varphi, sigma2){
  term <- dnorm(y[1], mean = 0, sd = sqrt(sigma2/(1-varphi^2)), log = TRUE)
  for (t in 2:length(y)){
    term <- term + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
  }
  return(term)
}
# prior on both parameters
priordensity <- function(varphi, sigma2){
  as.numeric(varphi < 1 && varphi > -1) / (2) * dgamma(sigma2, shape = 2, rate = 0.5)
}

# plot prior posterior density
z <- expand.grid(f = seq(from= -0.99, to = 0.99, length.out = 25),
                 s = seq(from = 0., to = 10, length.out = 25))
priorvalues <- sapply(1:nrow(z), function(index) priordensity(z[index,1], z[index,2]))
likelihoodvalues <- sapply(1:nrow(z), function(index) likelihood(z[index,1], z[index,2]))
z$priorvalues <- priorvalues
z$likelihoodvalues <- likelihoodvalues
zpost <- z %>% mutate(logposterior = log(priorvalues) + likelihoodvalues) %>% mutate(posterior = exp(logposterior))
zpost <- zpost %>% mutate(normpost = posterior / sum(posterior))
# prior density
gg <- ggplot(z, aes(x=f, y=s, fill=priorvalues))
gg <- gg + geom_raster()
gg <- gg + scale_fill_gradient2(low="red", mid = "yellow", high="blue")
gg <- gg + xlab(expression(phi[1])) + ylab(expression(sigma^2))
gg <- gg + theme(legend.position = "none")
gg <- gg + xlim(-2, 2)
gg
# posterior density
gg <- ggplot(zpost, aes(x=f, y=s, fill=posterior))
gg <- gg + geom_raster()
gg <- gg + scale_fill_gradient2(low="red", mid = "yellow", high="blue")
gg <- gg + xlab(expression(phi[1])) + ylab(expression(sigma^2))
gg <- gg + theme(legend.position = "none")
gg <- gg + xlim(-2, 2)
gg

# next, Metropolis--Hastings algorithm to approximate posterior distribution

library(mvtnorm)
library(ggthemes)

likelihood <- function(y, varphi, sigma2){
  term <- dnorm(y[1], mean = 0, sd = sqrt(sigma2/(1-varphi^2)), log = TRUE)
  for (t in 2:length(y)){
    term <- term + dnorm(y[t] - varphi * y[t-1], mean = 0, sd = sqrt(sigma2), log = TRUE)
  }
  return(term)
}

metropolishastings <- function(y, target, tuning_parameters){
  niterations <- tuning_parameters$niterations
  nchains <- tuning_parameters$nchains
  cov_proposal <- tuning_parameters$cov_proposal
  # store whole chains
  chains <- rep(list(matrix(nrow = niterations, ncol = target$dimension)), nchains)
  # current states of the chains
  current_chains <- matrix(nrow = nchains, ncol = target$dimension)
  # initialization of the chains
  current_chains <- tuning_parameters$rinit(nchains)
  for (ichain in 1:nchains){
    chains[[ichain]][1,] <- current_chains[ichain,]
  }
  # log target density values associated with the current states of the chains
  current_dtarget <- target$density(current_chains, y)
  #
  naccepts <- 0
  # run the chains
  for (iteration in 2:niterations){
    if (iteration %% 100 == 1){
      cat("iteration ", iteration, "average acceptance:", naccepts / (iteration*nchains) * 100, "%\n")
    }
    if (iteration > 50 && tuning_parameters$adaptation > 0  && (iteration %% tuning_parameters$adaptation) == 0){
      # adapt the proposal covariance matrix based on the last < 50,000 samples of all chains
      mcmc_samples <- foreach(ichain = 1:nchains, .combine = rbind) %do% {
        matrix(chains[[ichain]][max(1, iteration - 50000):(iteration-1),], ncol = target$dimension)
      }
      cov_proposal <- cov(mcmc_samples) / target$dimension
    }
    # proposals
    proposals <- current_chains + rmvnorm(nchains, rep(0, target$dimension), cov_proposal)
    # proposals' target density
    proposal_dtarget <- target$density(proposals, y)
    # log Metropolis Hastings ratio
    acceptance_ratios <- (proposal_dtarget - current_dtarget)
    # uniforms for the acceptance decisions
    uniforms <- runif(n = nchains)
    # acceptance decisions
    accepts <- (log(uniforms) < acceptance_ratios)
    naccepts <- naccepts + sum(accepts)
    # make the appropriate replacements
    current_chains[accepts,] <- proposals[accepts,]
    if (is.null(dim(current_chains))) current_chains <- matrix(current_chains, ncol = target$dimension)
    current_dtarget[accepts] <- proposal_dtarget[accepts]
    # book keeping
    for (ichain in 1:nchains){
      chains[[ichain]][iteration,] <- current_chains[ichain,]
    }
  }
  cat("average acceptance:", naccepts / (niterations*nchains) * 100, "%\n")
  return(list(chains = chains, naccepts = naccepts, cov_proposal = cov_proposal))
}

# tuning parameters of Metropolis--Hastings algorithm
tuning_parameters <- list(nchains = 2, niterations = 5000, cov_proposal = diag(c(0.1,0.1)),
                          rinit = function(nchains){
                            x <- matrix(0, nrow = nchains, ncol = 2)
                            x[,1] <- 0
                            x[,2] <- 1
                            return(x)
                          }, adaptation = 250)
# target log-density function
target <- list(dimension = 2, density = function(x, y){
  d <- sapply(1:nrow(x), function(i) log(priordensity(x[i,1], x[i,2])))
  potential <- which(is.finite(d))
  if (length(potential) > 0){
    d[potential] <- sapply(potential, function(i) likelihood(y, x[i,1], x[i,2]))
  }
  return(d)
})

# generate data
y <- arima.sim(list(order = c(1,0,0), ar = c(0.6)), n = 1000)
# try multiple numbers of observations
datalengths <- c(10, 100, 1000)
chains.df <- data.frame()
for (datalength in datalengths){
  mh <- metropolishastings(y[1:datalength], target, tuning_parameters)
  chains.df_ <- foreach(ichain = 1:tuning_parameters$nchains, .combine = rbind) %do% {
    df <- melt(mh$chains[[ichain]])
    names(df) <- c("iteration", "component", "value")
    df$chain = ichain
    df
  }
  chains.df_$datalength = datalength
  chains.df <- rbind(chains.df, chains.df_)
}
#
chains.df %>% tail
fitarima <- arima(y, order = c(1,0,0))
fitarima$coef[1]
burnin <- 500
# show posterior density for both parameters, overlaid
# to see that the posterior concentrates somewhere
g <- ggplot(chains.df %>% filter(iteration > burnin, component == 1), aes(x = value, group = datalength,
                                                                          fill = factor(datalength)))
g <- g + geom_histogram(aes(y = ..density..), binwidth = 0.02)
g <- g + theme(legend.position = "none")
g <- g + scale_fill_gdocs() + xlab(expression(phi[1]))
g <- g + geom_vline(xintercept = fitarima$coef[1])
g

g <- ggplot(chains.df %>% filter(iteration > burnin, component == 2), aes(x = value, group = datalength,
                                                                          fill = factor(datalength)))
g <- g + geom_histogram(aes(y = ..density..), binwidth = 0.05)
g <- g + theme(legend.position = "none")
g <- g + scale_fill_gdocs() + xlab(expression(sigma^2))
g <- g + geom_vline(xintercept = fitarima$sigma2)
g <- g + xlim(0, 3)
g

# in 2d
posterior.df <- chains.df %>% filter(iteration > burnin)
library(tidyr)
posterior.df <- posterior.df %>% spread(component, value)
names(posterior.df)[4:5] <- c("varphi", "sigma2")
g <- ggplot(posterior.df, aes(x = varphi, y = sigma2, group = datalength, colour = factor(datalength)))
g <- g + geom_point()
g <- g + scale_colour_colorblind(name = "datalength")
g <- g + xlab(expression(phi[1])) + ylab(expression(sigma^2))
g

# posterior seems to concentrate around the data-generating values
