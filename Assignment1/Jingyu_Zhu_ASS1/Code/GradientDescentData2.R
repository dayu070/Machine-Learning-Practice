rm(list = ls())
data <- read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/mvar-set2.dat")
dataMatrix <- cbind(matrix(rep(1, 2500), 2500, 1), data[ ,1], data[,1]^2, data[ ,2], data[,2]^2, data[,1]*data[,2])
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)

gradient <- function(x, y, theta) {
  m<-length(y)
  gradient <- (1/m)* (t(x) %*% ((x %*% theta) - y))
  return(gradient)
}

gradientDescent <- function(x, y,maxNum){
  theta <- matrix(1, 6, 1)               #Start with guess theta0 = (1,1,1)
  yita = 0.05                            #Set the learning rate .05
  for (i in 1:maxNum) {
    theta <- theta - yita  * gradient(x, y, theta)   
  }
  return(theta)
}

v <- vector()
for (i in 1:10){
  indexes <- which(folds==i, arr.ind=TRUE)
  testingZMatrix <- dataMatrix[indexes, ]
  trainingZMatrix <- dataMatrix[-indexes, ]
  testingYMatrix <- matrix(data[indexes,][,3], 250, 1)
  trainingYMatrix <-matrix(data[-indexes,][,3], 2250, 1)
  finalTheta <- gradientDescent(trainingZMatrix, trainingYMatrix, 1000)  #Iterate 1000 times
  
  
  trainingAmount <- length(trainingZMatrix[,1])
  yTrainingHat <- t(finalTheta) %*% t(trainingZMatrix)
  trainingMSE <- sum((t(yTrainingHat)-trainingYMatrix) ^ 2)/trainingAmount
  trainingRSE <- sum((t(yTrainingHat)-trainingYMatrix)^2/trainingYMatrix^2)/trainingAmount
  
  testingAmount <- length(testingZMatrix[,1])
  yTestingHat <- t(finalTheta) %*% t(testingZMatrix)
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



