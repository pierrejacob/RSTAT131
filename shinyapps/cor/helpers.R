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

filename <- tempfile(fileext=".RData")

generate_randomness <- function(nobs){
  x <- rnorm(nobs, mean = 1, sd = 2)
  y <- rnorm(1) * x + rgamma(nobs, 3, 3)^2 + rnorm(nobs)
  yonx <- lm(y ~ x)
  yonxintercept <- coef(yonx)[1]
  yonx <- coef(yonx)[2]

  xony <- lm(x ~ y)
  xonyintercept <- coef(xony)[1]
  xony <- coef(xony)[2]

  xtilde <- (x - mean(x)) / sd(x)
  ytilde <- (y - mean(y)) / sd(y)
  yonxtilde <- lm(ytilde ~ xtilde)
  yonxtilde <- coef(yonxtilde)[2]
  xonytilde <- lm(xtilde ~ ytilde)
  xonytilde <- coef(xonytilde)[2]
  corxy <- cor(x,y)
  save(x, y, xtilde, ytilde, yonx, xony, yonxintercept, xonyintercept, xonytilde, yonxtilde, corxy, file = filename)
}

# generate_randomness(100)
