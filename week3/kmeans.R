
slide4 <- function() {
  set.seed(1234)
  par(mar=c(0,0,0,0))
  x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
  y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2) 
  
  plot(x,y,col="blue",pch=19,cex=2) 
  text(x+0.05,y+0.05,labels=as.character(1:12))
}

# kmeans()
slide11 <- function() {
  set.seed(1234)
  par(mar=c(0,0,0,0))
  x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
  y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2) 

  dataFrame <- data.frame(x,y)
  # x         y
  # 1  0.7585869 0.8447492
  # 2  1.0554858 1.0128918
  # 3  1.2168882 1.1918988
  # 4  0.5308605 0.9779429
  # 5  2.0858249 1.8977981
  # 6  2.1012112 1.8177609
  # 7  1.8850520 1.8325657
  # 8  1.8906736 2.4831670
  # 9  2.8871096 1.0268176
  # 10 2.8219924 0.9018628
  # 11 2.9045615 0.9118904
  # 12 2.8003227 1.0919179  
  
  kmeansObj <- kmeans(dataFrame, centers=3)

  # K-means clustering with 3 clusters of sizes 4, 4, 4
  # 
  # Cluster means:
  #   x         y
  # 1 1.9906904 2.0078229
  # 2 2.8534966 0.9831222
  # 3 0.8904553 1.0068707
  # 
  # Clustering vector:
  #   [1] 3 3 3 3 1 1 1 1 2 2 2 2
  # 
  # Within cluster sum of squares by cluster:
  #   [1] 0.34732441 0.03298027 0.34188313
  # (between_SS / total_SS =  93.6 %)
  
  names(kmeansObj)
  # [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"         "iter"        
  # [9] "ifault"   
  
  kmeansObj$cluster
  # [1] 3 3 3 3 1 1 1 1 2 2 2 2
}

# plot kmeans cluster
slide13 <- function() {
  set.seed(1234)
  par(mar=c(0,0,0,0))
  x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
  y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2) 
  dataFrame <- data.frame(x,y)
  kmeansObj <- kmeans(dataFrame, centers=3)
  
  par(mar=rep(0.2,4)) 
  plot(x, y, col=kmeansObj$cluster, pch=19, cex=2) 
  points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)
}

# Heatmaps
slide14 <- function() {
  set.seed(1234)
  par(mar=c(0,0,0,0))
  x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
  y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2) 
  dataFrame <- data.frame(x,y)
  
  set.seed(1234)
  dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
  kmeansObj  <- kmeans(dataMatrix, centers=3) 
  par(mfrow=c(1,2), mar = c(2, 4, 0.1, 0.1)) 
  image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt="n") 
  image(t(dataMatrix)[,order(kmeansObj$cluster)], yaxt="n")
}

# Determining the number of clusters in a data set
# https://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set

# Statistics for Genomcs: Distances and Clustering
# https://www.youtube.com/watch?v=wQhVWUcXM0A

# The Elements of Statistical Learning
# https://web.stanford.edu/~hastie/ElemStatLearn/


