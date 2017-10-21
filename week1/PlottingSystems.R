

slide4 <- function() {
  library(datasets)
  data(cars)

  with(cars, plot(speed, dist))
}

slide7 <- function() {
  library(lattice)

  state <- data.frame(state.x77, region = state.region) 
  xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))
}

slide9 <- function() {
  library(ggplot2)
  data(mpg)
  
  qplot(displ, hwy, data = mpg)
}