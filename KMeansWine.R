rm(list = ls())
data <- read.table("wine.data", header = FALSE, sep=",")
data <- as.matrix(data[, 2:14])

result <- vector()
for (k in 2:10){
  randomNum <- round(runif(k, min=1, max=178))
  mean <- list()
  for (j in 1:k){
    mean[[j]] <- data[randomNum[j], ]
  }
  
  flag <- 0
  while (flag == 0){
    indices <- list()
    for (j in 1:k){
      indices[[j]] <- vector()
    }
    for (i in 1:length(data[,1])){
      minDistance <- 999999
      for (j in 1:k){
        if (sum((data[i,] - mean[[j]])^2) < minDistance){
          minDistance <- sum((data[i,] - mean[[j]])^2)
          clusterNum <- j
        }
      }
      indices[[clusterNum]] <- c(indices[[clusterNum]], i)
    }
    
    cluster <- list()
    for (j in 1:k){
      cluster[[j]] <- as.matrix(data[indices[[j]], ])
    }
    
    sum <- 0
    for (j in 1:k){
      sum <- sum + sum((mean[[j]]-colMeans(cluster[[j]]))^2)
      mean[[j]] <- colMeans(cluster[[j]])
    }
    if (sum == 0)
    {
      flag <- 1
    }
  }
  
  sumOfDistance <- 0
  for (j in 1:k){
    for (i in 1:length(cluster[[j]][,1])){
      sumOfDistance <- sumOfDistance + sqrt(sum((mean[[j]]-cluster[[j]][i,])^2))
    }
  }
  
  result <- c(result, k, sumOfDistance)
}


result <- matrix(result, 9, 2, byrow = T)
colnames(result) <- c("k", "Performance")
result