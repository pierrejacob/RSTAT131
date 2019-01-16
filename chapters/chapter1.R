rm(list = ls())
set.seed(17)
library(RSTAT131)
setmytheme()

# Plot examples of time series
# following first chapter of  http://www.stat.pitt.edu/stoffer/tsa4/tsaEZ.pdf
?jj
plot(jj, type="o", ylab="Quarterly Earnings per Share")

?gtemp
plot(gtemp, type="o", ylab="Global Temperature Deviations")

?speech
plot(speech, ylab="Speech")

?EQ5
plot(EQ5, ylab="Earthquake")
# and histogram
hist(EQ5, prob = TRUE, nclass = 100)

?EXP6
plot(EXP6, ylab="Explosion")

### the following is based on the series of search volumes
### corresponding to the term "chocolate" downloaded from Google Trends
# chocolate <- read.csv("chocolate.csv", header = FALSE)
# dates <- as.Date(as.character(chocolate[,1]), format = "%m/%d/%y")
# volumes <- chocolate[,2]
# plot(dates, volumes, type = "l", ylim = c(0,100), xlab = "Time", ylab = "Chocolate search volume")


## double counts of red kangaroos in New South Wales
## taken from Caughley, G., N. Shepherd, and J. Short (1987)
y = matrix(c(267, 333, 159, 145, 340, 463, 305, 329, 575, 227, 532, 769, 526, 565, 466, 494, 440, 858, 599, 298, 529, 912, 703, 402,
             669, 796, 483, 700, 418, 979, 757, 755, 517, 710, 240, 490, 497, 250, 271, 303, 386, 326, 144, 145, 138, 413, 531, 331, 329, 529, 318, 449,
             852, 332, 742, 479, 620, 531, 751, 442, 824, 660, 834, 955, 453, 953, 808, 975, 627, 851, 721, 1112, 731, 748, 675, 272, 292, 389, 323,
             272, 248, 290, 1973.497, 1973.75, 1974.163, 1974.413, 1974.665, 1975.002, 1975.245, 1975.497, 1975.75, 1976.078, 1976.33, 1976.582, 1976.917,
             1977.245, 1977.497, 1977.665, 1978.002, 1978.33, 1978.582, 1978.832, 1979.078, 1979.582, 1979.832, 1980.163, 1980.497, 1980.75, 1980.917,
             1981.163, 1981.497, 1981.665, 1981.917, 1982.163, 1982.413, 1982.665, 1982.917, 1983.163, 1983.413, 1983.665, 1983.917, 1984.163, 1984.413),
           c(3, 41), byrow = T)

matplot(x = y[3,], y = t(y[1:2,]), type = "l", ylim = c(0, 1100), xlab = "Time", ylab = "Kangaroo counts")

## First attempts at prediction on the Johnson & Johnson's time series
plot(jj, type="o", ylab="Quarterly Earnings per Share")
length(jj)
ntrain <- 70
Tpresent_indices <- 1:ntrain
Tpresent <- as.numeric(time(jj))[1:ntrain]
Tfuture_indices <- (ntrain+1):84
Tfuture <- as.numeric(time(jj))[(ntrain+1):84]

jjtrain <- jj[Tpresent_indices]
jjtest <- jj[Tfuture_indices]
plot(x = Tpresent, jjtrain, type="o", ylab="Quarterly Earnings per Share", xlim = c(min(Tpresent), max(Tfuture)), ylim = c(0, 18))
points(x = Tfuture, y = jjtest, col = "red")
lines(x = Tfuture, y = jjtest, col = "red")

## try a quadratic regression for prediction
times <- Tpresent
timessq <- times*times
train.df <- data.frame(Y = jjtrain, X1 = times, X2 = timessq)
reg <- lm(Y ~ X1 + X2, data = train.df)
timestest <- Tfuture
timessqtest <- timestest*timestest
test.df <- data.frame(X1 = timestest, X2 = timessqtest)
predictreg <- predict(reg, newdata = test.df)
sigmahat <- sqrt(1/reg$df.residual*sum(reg$residuals^2))
plot(x = Tpresent, jjtrain, type="o", ylab="Quarterly Earnings per Share",
     xlim = c(min(Tpresent), max(Tfuture)), ylim = c(0, 18), xlab = "Time")
points(x = Tfuture, y = jjtest, col = "red")
lines(x = Tfuture, y = predictreg, col = "blue", lty = 2)
xupper <- qnorm(p = 0.975, mean = predictreg, sd = sigmahat)
xlower <- qnorm(p = 0.025, mean = predictreg, sd = sigmahat)
lines(x = Tfuture, y = xupper, col = "blue")
lines(x = Tfuture, y = xlower, col = "blue")
## remarkably all observations are outside of the prediction intervals!

## try to take logarithm instead
train.df$logY <- log(train.df$Y)
reglog <- lm(logY ~ X1, data = train.df)
plot(x = times, y = log(jjtrain), type = "o")
lines(x = times, y = fitted(reglog))
plot(fitted(reglog), residuals(reglog))

predictreglog <- predict(reglog, newdata = test.df)
sigmahat_log <- sqrt(1/reglog$df.residual*sum(reglog$residuals^2))
plot(x = Tpresent, log(jjtrain), type="o", ylab="Quarterly Earnings per Share",
     xlim = c(min(Tpresent), max(Tfuture)), ylim = c(-2, 5), xlab = "Time")
points(x = Tfuture, y = log(jjtest), col = "red")
lines(x = Tfuture, y = predictreglog, col = "blue", lty = 2)
xupper_log <- qnorm(p = 0.975, mean = predictreglog, sd = sigmahat_log)
xlower_log <- qnorm(p = 0.025, mean = predictreglog, sd = sigmahat_log)
lines(x = Tfuture, y = xupper_log, col = "blue")
lines(x = Tfuture, y = xlower_log, col = "blue")

## if we go back to the original scale
plot(x = Tpresent, jjtrain, type="o", ylab="Quarterly Earnings per Share",
     xlim = c(min(Tpresent), max(Tfuture)), ylim = c(0, 18), xlab = "Time")
points(x = Tfuture, y = jjtest, col = "red")
lines(x = Tfuture, y = exp(predictreglog+.5*sigmahat_log^2), col = "blue", lty = 2)
lines(x = Tfuture, y = qlnorm(p = 0.975, meanlog = predictreglog, sdlog = sigmahat_log), col = "blue")
lines(x = Tfuture, y = qlnorm(p = 0.025, meanlog = predictreglog, sdlog = sigmahat_log), col = "blue")
# it's better but our predictions clearly overshoot

### run Shiny apps to generate some realizations of stochastic processes
library(shiny)
## cosine trend + noise
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/cosine")
## random walk
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/randomwalk")
## auto-regressive
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/autoregressive")
## moving average
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/movingaverage")
## stochastic volatility
runGitHub(repo="RSTAT131",username="pierrejacob",subdir="shinyapps/stochasticvolatility")
