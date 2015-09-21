rm(list = ls())
data <- read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set4.dat")
dataMatrix <- cbind(matrix(rep(1, 100000), 100000, 1), data[ ,1], data[ ,2], data[ ,3], data[ ,4], data[ ,5], data[,1]*data[,2], data[,3]*data[,4], data[,1]*data[,5])
dataMatrix1 <- dataMatrix
colnames(dataMatrix1) <- c("1", "x1", "x2", "x3", "x4", "x5", "x1*x2", "x3*x4", "x1*x5")
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
v <- vector()
for (i in 1:10){
  indexes <- which(folds==i, arr.ind=TRUE)
  testingZMatrix <- dataMatrix[indexes, ]
  trainingZMatrix <- dataMatrix[-indexes, ]
  testingYMatrix <- matrix(data[indexes,][,6], 10000, 1)
  trainingYMatrix <-matrix(data[-indexes,][,6], 90000, 1)
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


