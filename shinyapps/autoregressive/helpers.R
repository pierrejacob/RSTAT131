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
  randomness <- rnorm(nobs)
  save(randomness, file = filename)
}
