pollution <- read.csv(
  url('https://raw.githubusercontent.com/DataScienceSpecialization/courses/master/04_ExploratoryAnalysis/exploratoryGraphs/data/avgpm25.csv'),
  colClasses = c('numeric', 'character', 'factor', 'numeric', 'numeric')
)


print(summary(pollution$pm25))

slide9 <- function() {
  boxplot(pollution$pm25, col='blue')
}

slide10 <- function() {
  hist(pollution$pm25, col='green')
}

slide11 <- function() {
  hist(pollution$pm25, col='green')
  rug(pollution$pm25)
}

slide12 <- function() {
  hist(pollution$pm25, col='green', breaks=50)
  rug(pollution$pm25)
}

slide13 <- function() {
  boxplot(pollution$pm25, col = "blue") 
  
  # horizontal line at y=12
  abline(h = 12)
}

slide14 <- function() {
  hist(pollution$pm25, col = "green")
  
  # vertical line at x=12, line width = 2
  abline(v = 12, lwd = 2)
  
  # vertical line at median value of pm25, line width = 4
  abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
}

slide15 <- function() {
  barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")
}

slide17 <- function() {
  boxplot(pm25 ~ region, data = pollution, col = "red")
}

slide18 <- function() {
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) 
  hist(subset(pollution, region == "east")$pm25, col = "green") 
  hist(subset(pollution, region == "west")$pm25, col = "green")
}

slide19 <- function() {
  with(pollution, plot(latitude, pm25)) 
  
  # equivalent of
  plot(pollution$latitude, pollution$pm25)
  
  # horizontal line at y-12, line width=2, line type = dashed
  abline(h=12,lwd=2,lty=2)
}

slide20 <- function() {
  with(pollution, plot(latitude, pm25, col = region)) 
  abline(h=12,lwd=2,lty=2)
}

slide21 <- function() {
  par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
  with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West")) 
  with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))
}

slide22 <- function() {
  
}














