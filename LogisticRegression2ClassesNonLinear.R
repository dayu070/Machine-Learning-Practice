rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data <- data[1:100,]
dataClass0 <- as.matrix(cbind(data[1:50,1:4], data[1:50,1]*data[1:50,2], data[1:50,3]*data[1:50,4], matrix(0,50,1)))
dataClass1 <- as.matrix(cbind(data[51:100,1:4], data[51:100,1]*data[51:100,2], data[51:100,3]*data[51:100,4], matrix(1,50,1)))
folds <- cut(seq(1, nrow(dataClass0)), breaks=10, labels=FALSE)

gradient <- function(x, y, theta) {
  n<-length(x[1,])
  tempX <- cbind(matrix(1, length(x[,1]), 1), x)
  sum <- matrix(0, length(x[1,])+1, 1)
  for (i in 1:length(x[,1])){
    sum <- sum + as.vector(1/(1+exp(-t(theta)%*%as.matrix(tempX[i,]))) - y[i,1]) * as.matrix(tempX[i,])
  }
  return(sum)
}

gradientDescent <- function(x, y,maxNum){
  theta <- matrix(1, length(x[1,])+1, 1)               #Start with guess theta0 = (1,1,1)
  yita = 0.05                            #Set the learning rate .05
  for (i in 1:maxNum) {
    theta <- theta - yita  * gradient(x, y, theta)   
  }
  return(theta)
}

resultMatrix <- matrix(0,2,2)
for (m in 1:10){
  indexes <- which(folds==m, arr.ind=TRUE)
  training <- rbind(dataClass0[-indexes,],dataClass1[-indexes,])
  testing <- rbind(dataClass0[indexes,],dataClass1[indexes,])

  finalTheta <- gradientDescent(as.matrix(training[,1:(length(training[1,])-1)]), as.matrix(training[,length(training[1,])]), 200) #Iterate 200 times
  
  testingFeature <- cbind(matrix(1, length(testing[,1]), 1), testing[,1:(length(testing[1,])-1)])
  yHat <- 1/(1+exp(-testingFeature %*% finalTheta))
  for (i in 1:length(yHat[,1])){
    if(i <= length(yHat[,1])/2){
      if(yHat[i,1] <= 0.5){
        resultMatrix[1,1] <- resultMatrix[1,1] + 1
      }else{
        resultMatrix[2,1] <- resultMatrix[2,1] + 1
      }      
    }else{
      if(yHat[i,1] <= 0.5){
        resultMatrix[1,2] <- resultMatrix[1,2] + 1
      }else{
        resultMatrix[2,2] <- resultMatrix[2,2] + 1
      }     
    }
  }
}

precisionOfClass0 <- resultMatrix[1,1]/sum(resultMatrix[1,])
precisionOfClass1 <- resultMatrix[2,2]/sum(resultMatrix[2,])
recallOfClass0 <- resultMatrix[1,1]/sum(resultMatrix[,1])
recallOfClass1 <- resultMatrix[2,2]/sum(resultMatrix[,2])
FMeasureOfClass0 <- 2*precisionOfClass0*recallOfClass0/(precisionOfClass0+recallOfClass0)
FMeasureOfClass1 <- 2*precisionOfClass1*recallOfClass1/(precisionOfClass1+recallOfClass1)
accuracy <- (resultMatrix[2,2] + resultMatrix[1,1])/sum(resultMatrix)
resultMatrix
precisionOfClass0
precisionOfClass1
recallOfClass0
recallOfClass1
FMeasureOfClass0
FMeasureOfClass1
accuracy