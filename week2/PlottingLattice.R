library(lattice)
library(datasets)

slide5 <- function() {
  ## Simple scatterplot
  xyplot(Ozone ~ Wind, data = airquality)
}

slide6 <- function() {
  ## Convert 'Month' to a factor variable
  airquality <- transform(airquality, Month = factor(Month)) 
  xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))
}

slide6b <- function() {
  ## Convert 'Temp' to a factor variable (rounded to nearest 10 degrees)
  airquality <- transform(airquality, Temp = factor(round(Temp,-1))) 
  xyplot(Ozone ~ Wind | Temp, data = airquality, layout = c(5, 1))
}

slide8 <- function() {
  # xypot returns a lattice class that auto-displays from the console
  # but can be assigned to variable and then displayed later.
  #
  p <- xyplot(Ozone ~ Wind, data = airquality) ## Nothing happens! 
  print(p) ## Plot appears
}

slide10 <- function() {
  # create multiple plot groupings and plot them each using a factor f
  set.seed(10)
  x <- rnorm(1000)
  f <- rep(0:3, each = 250) 
  y<-x+f-f*x+rnorm(100,sd=0.5)
  f <- factor(f, labels = c("Group 1", "Group 2", "Group 3", "Group 4"))
  xyplot(y ~ x | f, layout = c(2, 2)) ## 4 plots with 2x2 panels
}

# Lattice panel functions: median line
slide11 <- function() {
  set.seed(10)
  x <- rnorm(1000) # 1000 total values split across...
  f <- rep(0:3, each = 250) # 4 groups (0,1,2,3)
  y<-x+f-f*x+rnorm(100,sd=0.5)
  f <- factor(f, labels = c("Group 1", "Group 2", "Group 3", "Group 4"))
  
  ## Custom panel function
  xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## First call default panel function 
    panel.abline(h = median(y), lty = 2) ## Add a horizontal line at the median
  })
}

# Lattice panel functions: Regression line
slide12 <- function() {
  set.seed(10)
  x <- rnorm(1000) # 1000 total values split across...
  f <- rep(0:3, each = 250) # 4 groups (0,1,2,3)
  y<-x+f-f*x+rnorm(100,sd=0.5)
  f <- factor(f, labels = c("Group 1", "Group 2", "Group 3", "Group 4"))
  
  ## Custom panel function
  xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...) ## First call default panel function 
    panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
  })
}
