rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data <- as.matrix(data[, 1:4])

library(MASS)

calProbOfX <- function(x, miu, sigma){
  temp <- exp(-0.5 * t(as.matrix(x - miu)) %*% ginv(sigma) %*% as.matrix(x - miu))
  temp <- temp/((2*pi)^2 * sqrt(det(sigma))) #Four features in the data set
  return (temp)
}

calProbOfLGivenX <- function(x, miu, sigma, alpha, l, k1){
  sum1 <- 0
  for (m1 in 1:k1){
    sum1 <- sum1 + alpha[[m1]] * calProbOfX(x, miu[[m1]], sigma[[m1]])
  }
  return (alpha[[l]] * calProbOfX(x, miu[[l]], sigma[[l]]) / sum1)
}

calMahalanobisDistance <- function(x, miu, sigma){
  return(sqrt(t(as.matrix(x-miu)) %*% ginv(sigma) %*% as.matrix(x-miu)))
}

result <- vector()

for (k in 2:8){
  
  miu <- list()
  sigma <- list()
  alpha <- list()
  for (j in 1:k){
    miu[[j]] <- c(j,j,j,j)
    sigma[[j]] <- diag(1, 4, 4)  #Create a covariance matrix for four features
    alpha[[j]] <- 1/k
  }
  
  flag <- 0
  while (flag == 0){
    alphaNew <- list()
    miuNew <- list()
    for (j in 1:k)
    {
      alphaNew[[j]] <- 0
    }
    
    for (i in 1:length(data[,1])){
      for (j in 1:k){
        alphaNew[[j]] <- alphaNew[[j]] + calProbOfLGivenX(data[i,], miu, sigma, alpha, j, k)
      }
    }
    
    for(j in 1:k)
    {
      alphaNew[[j]] <- alphaNew[[j]] / length(data[,1])
    }
    
    for(j in 1:k)
    {
      sum <- c(0,0,0,0) #Four features
      for (i in 1:length(data[,1])){
        sum <- sum + data[i,] * calProbOfLGivenX(data[i,], miu, sigma, alpha, j, k)
      }
      miuNew[[j]] <- sum / (length(data[,1]) * alphaNew[[j]])
    }
    
    sigmaNew <- list()
    for(j in 1:k){
      sumMatrix <- matrix(0, length(data[1,]), length(data[1,]))
      for (i in 1:length(data[,1])){
        sumMatrix <- sumMatrix + as.matrix(calProbOfLGivenX(data[i,], miu, sigma, alpha, j, k) * (data[i,] - miuNew[[j]])) %*% t(as.matrix((data[i,] - miuNew[[j]])))
      }
      sigmaNew[[j]] <- sumMatrix / (matrix(length(data[,1]) * alphaNew[[j]], 4, 4))
    }
    
    flag1 <- 0
    for (j in 1:k){
      if (det(sigmaNew[[j]] - sigma[[j]]) >= 0.001){
        flag1 <- 1
      }
    }
    
    if (flag1 == 0){
      flag <- 1
    }
    
    for (j in 1:k)
    {
      alpha[[j]] <- alphaNew[[j]]
      sigma[[j]] <- sigmaNew[[j]]
      miu[[j]] <- miuNew[[j]]
    }
  }
  
  sumOfDistance <- 0
  for (i in 1:length(data[,1])){
    maxProb <- -99999
    clusterIndex <- -1
    for (j in 1:k){
      if (calProbOfLGivenX(data[i,], miu, sigma, alpha, j, k) > maxProb){
        maxProb <- calProbOfLGivenX(data[i,], miu, sigma, alpha, j, k)
        clusterIndex <- j
      }
    }
    sumOfDistance <- sumOfDistance + calMahalanobisDistance(data[i,], miu[[clusterIndex]], sigma[[clusterIndex]])
  }
  
  result <- c(result, k, sumOfDistance)
}

result <- matrix(result, 7, 2, byrow = T)
colnames(result) <- c("k", "Mahalanobis Distance")
result