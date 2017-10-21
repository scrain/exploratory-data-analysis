slide6 <- function() {
  library(datasets)
  hist(airquality$Ozone) ## Draw a new plot
}

slide7 <- function() {
  library(datasets)
  with(airquality, plot(Wind, Ozone))
}

slide8 <- function() {
  library(datasets)
  
  # turns month into factor (enumeration)... boxplot seems to work fine without this
  # airquality <- transform(airquality, Month = factor(Month)) 
  
  # xlab = x axis label
  # ylab = y axis label
  boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")  
}


slide11 <- function() {
  print('See defaults for par: ')
  print(paste("line type:               par('lty') = ", par('lty')))
  print(paste("line width:              par('lwd') = ", par('lwd')))
  print(paste("color:                   par('col') = ", par('col')))
  print(paste("plotting symbol:         par('pch') = ", par('pch')))
  print(paste("axis label orientation:  par('las') = ", par('las')))
  print(paste("background color:        par('bg') =  ", par('bg')))
}

slide14 <- function() {
  library(datasets)
  with(airquality, plot(Wind, Ozone))
  title(main = "Ozone and Wind in New York City") ## Add a title
}