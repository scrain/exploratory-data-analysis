demo1 <- function() {
  x <- rnorm(100)
  
  # Some defaults
  # main = "Histogram of " xname
  # xlab = "xname
  # ylab = "Frequency"
  hist(x)
}

demo2 <- function() {
  x <- rnorm(100)
  y <- rnorm(100)
  
  # some defaults
  # ylab = yname
  # xlab = xname
  # mar = c(5,4,2,1)
  plot(x,y)
  
  # example("points") to get example of point plotting
}

demo3 <- function() {
  x <- rnorm(1000)
  y <- rnorm(1000)

  plot(x, y, pch = 20)
  
  title('Scatterplot!!!')
  text(-2,-2, "label @ -2,-2")
  legend("topleft", legend="Data", pch=20)
  
  # lm linear model
  fit <- lm(y ~ x)
  abline(fit, lwd=3, col='blue')
}

demo4 <- function() {
  x <- rnorm(1000)
  y <- rnorm(1000)
  
  plot(x, y, pch = 20, xlab='Weight', ylab="Height", main ="Scatterplot")
  legend("topright", legend="Data", pch=20)
  fit <- lm(y~x)
  abline(fit, lwd=3, col="red")
}

demo5 <- function() {
  x <- rnorm(100)
  y <- x + rnorm(100)
  g <- gl(2,50, labels= c('Male', 'Female'))
  
  plot(x,y, type="n")  # type='n' keeps points from rendering
  points(x[g=='Male'], y[g=='Male'], col='green')
  points(x[g=='Female'], y[g=='Female'], col='blue')
}

demo5()