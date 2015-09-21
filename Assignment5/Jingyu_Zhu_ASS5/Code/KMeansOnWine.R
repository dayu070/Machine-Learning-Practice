rm(list = ls())
data <- read.table("wine.data", header = FALSE, sep=",")
data <- as.matrix(data[, 2:14])
randomNum <- round(runif(3, min=1, max=178))

#Randomly select three examples as initial clusters
mean1 <- data[randomNum[1], ]
mean2 <- data[randomNum[2], ]
mean3 <- data[randomNum[3], ]

#Use flag to represent whether it has converged
flag <- 0
while (flag == 0){
  indices1 <- vector()
  indices2 <- vector()
  indices3 <- vector()
  for (i in 1:length(data[,1]))
  {
    distanceTo1 <- sum((data[i,] - mean1)^2)
    distanceTo2 <- sum((data[i,] - mean2)^2)
    distanceTo3 <- sum((data[i,] - mean3)^2)
    if (min(distanceTo1, distanceTo2, distanceTo3) == distanceTo1)
    {
      indices1 <- c(indices1, i)
    }else if (min(distanceTo1, distanceTo2, distanceTo3) == distanceTo2)
    {
      indices2 <- c(indices2, i)
    }else
    {
      indices3 <- c(indices3, i)
    }
  }
  cluster1 <- as.matrix(data[indices1, ])
  cluster2 <- as.matrix(data[indices2, ])
  cluster3 <- as.matrix(data[indices3, ])
  if ((sum(mean1 - colMeans(cluster1)) == 0) && (sum(mean2 - colMeans(cluster2)) == 0) && (sum(mean3 - colMeans(cluster3)) == 0)) 
  {
    flag <- 1
  }
  mean1 <- colMeans(cluster1)
  mean2 <- colMeans(cluster2)
  mean3 <- colMeans(cluster3)
}

plot(10:15, 0:5, type = "n", main='Wine by K-means', ylab='Acid', xlab='Ash')  
points(cluster1[,1], cluster1[,2], col = "black")
points(cluster2[,1], cluster2[,2], col = "red")
points(cluster3[,1], cluster3[,2], col = "blue")
points(mean1[1], mean1[2], col = "black", pch = 8)
points(mean2[1], mean2[2], col = "red", pch = 8)
points(mean3[1], mean3[2], col = "blue", pch = 8)