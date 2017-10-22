# Matrix data
slide2 <- function() {
  set.seed(12345)
  par(mar=rep(0.2, 4))
  dataMatrix <- matrix(rnorm(400), nrow=40)
  image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
}

# Cluster the data
slide3 <- function() {
  set.seed(12345)
  par(mar=rep(0.2, 4))
  dataMatrix <- matrix(rnorm(400), nrow=40)
  
  heatmap(dataMatrix)
}

# what if we add a pattern?
slide4 <-function() {
  set.seed(12345)
  dataMatrix <- matrix(rnorm(400), nrow=40)

  set.seed(678910)
  for (i in 1:40) {
    coinFlip <- rbinom(1, size=1, prob=0.5)
    
    # if coin is heads, add a common pattern to that row
    if (coinFlip) {
      dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each=5)  # bump 5 right columns by +3
    }
  }
  image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
}

# what if we add a pattern?  the clustered data
slide6 <-function() {
  set.seed(12345)
  dataMatrix <- matrix(rnorm(400), nrow=40)
  
  set.seed(678910)
  for (i in 1:40) {
    coinFlip <- rbinom(1, size=1, prob=0.5)
    
    # if coin is heads, add a common pattern to that row
    if (coinFlip) {
      dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each=5)  # bump 5 right columns by +3
    }
  }
  
  par(mar = rep(0.2, 4)) 
  heatmap(dataMatrix)
}

# Patterns in rows and columns
slide7 <- function() {
  set.seed(12345)
  dataMatrix <- matrix(rnorm(400), nrow=40)
  
  set.seed(678910)
  for (i in 1:40) {
    coinFlip <- rbinom(1, size=1, prob=0.5)
    
    # if coin is heads, add a common pattern to that row
    if (coinFlip) {
      dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each=5)
    }
  }
  
  hh <- hclust(dist(dataMatrix))
  dataMatrixOrdered <- dataMatrix[hh$order, ]
  par(mfrow = c(1, 3), mar=c(5,4,1,1))
  image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1]) 
  plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab = "Row", pch = 19) 
  plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)
}