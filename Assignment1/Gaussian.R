rm(list = ls())
data <- read.table("mvar-set1.dat")
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)

gaussian <- function(x,y,sigma){
  temp <- x - y
  sum <- 0
  for (k in 1:length(temp)){
    sum <- sum + temp[k]^2
  }
  return (exp(sum)/(2*sigma^2))
}

constructG <- function(x){
  temp <- 0
  g <- matrix(1, length(x[,1]), length(x[,1]))
  for (i in 1:length(x[,1])){
    for (j in 1:length(x[,1])){
      temp <- gaussian(x[i], x[j], 1)
      g[i,j] <- temp
    }
      
  }
  return (g)
}

v <- vector()
for (i in 1:1){
  indexes <- which(folds==i, arr.ind=TRUE)
  testing <- data[indexes, ]
  training <- data[-indexes, ]
  testingXMatrix <- cbind(matrix(data[indexes,][,1], 250, 1), matrix(data[indexes,][,2], 250, 1))
  trainingXMatrix <- cbind(matrix(data[-indexes,][,1], 2250, 1), matrix(data[-indexes,][,2], 2250, 1))
  testingYMatrix <- matrix(data[indexes,][,3], 250, 1)
  trainingYMatrix <-matrix(data[-indexes,][,3], 2250, 1)
  matrixG <- constructG(trainingXMatrix)
  alpha <- solve(matrixG) %*% trainingYMatrix
  yHat <- matrix(1,testingYMatrix[,1],1)
  for (m in 1:length(testingYMatrix[,1])){
    temp1 <- matrix(1,trainingXMatrix[,1],1)
    for (n in 1:length(trainingXMatrix[,1])){
      temp1[n,1] <- gaussian(testingXMatrix[m,], trainingXMatrix[n,],1)
    }
    yHat[m,1] <- alpha %*% temp1
  }
}

  theta <- solve(t(trainingZMatrix) %*% trainingZMatrix) %*% t(trainingZMatrix) %*% trainingYMatrix
  trainingAmount <- length(trainingZMatrix[,1])
  yTrainingHat <- t(theta) %*% t(trainingZMatrix)
  trainingMSE <- sum((t(yTrainingHat)-trainingYMatrix) ^ 2)/trainingAmount
  trainingRSE <- sum((t(yTrainingHat)-trainingYMatrix)^2/trainingYMatrix^2)/trainingAmount
  
  testingAmount <- length(testingZMatrix[,1])
  yTestingHat <- t(theta) %*% t(testingZMatrix)
  testingMSE <- sum((t(yTestingHat)-testingYMatrix) ^ 2)/testingAmount
  testingRSE <- sum((t(yTestingHat)-testingYMatrix)^2/testingYMatrix^2)/testingAmount
  
  if (i == 1){
    v <- c(trainingMSE, testingMSE)
  }else{
    v <- c(v, c(trainingMSE, testingMSE))
  }
}
finalMatrix <- matrix(v, 10, 2, byrow = T)
colnames(finalMatrix) <- c("trainingMSE", "testingMSE")