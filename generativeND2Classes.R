require(ROCR)
rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data <- data[1:100,]
dataClass0 <- as.matrix(cbind(data[1:50,1:4], matrix(0,50,1)))
dataClass1 <- as.matrix(cbind(data[51:100,1:4], matrix(1,50,1)))
folds <- cut(seq(1, nrow(dataClass0)), breaks=10, labels=FALSE)
resultMatrix <- matrix(0, 2, 2)
predictionMatrix <- matrix(0,100,2)
counter <- 1
library("MASS")

for(m in 1:10){
  indexes <- which(folds==m, arr.ind=TRUE)
  training <- rbind(dataClass0[-indexes,],dataClass1[-indexes,])
  testing <- rbind(dataClass0[indexes,],dataClass1[indexes,])
  
  muClass0 <- c(mean(training[1:45,1]))
  muClass0[2] <- mean(training[1:45,2])
  muClass0[3] <- mean(training[1:45,3])
  muClass0[4] <- mean(training[1:45,4])
  muClass0 <- matrix(muClass0,4,1)
  muClass1 <- mean(training[46:90,1])
  muClass1[2] <- mean(training[46:90,2])
  muClass1[3] <- mean(training[46:90,3])
  muClass1[4] <- mean(training[46:90,4])
  muClass1 <- matrix(muClass1,4,1)

  sigmaClass0 <- cov(training[1:45, 1:4])
  sigmaClass1 <- cov(training[46:90, 1:4])

  for (i in 1:length(testing[,1])){
    tempX <- matrix(0, 4, 1)
    for (j in 1:4){
      tempX[j,1] <- testing[i,j]
    }
    result <- (-log(abs(det(sigmaClass0)))-0.5*t(tempX-muClass0)%*%ginv(sigmaClass0)%*%(tempX-muClass0)) - (-log(abs(det(sigmaClass1)))-0.5*t(tempX-muClass1)%*%ginv(sigmaClass1)%*%(tempX-muClass1))
    if (i <= 5){
      if(result >= 0){
        resultMatrix[1,1] <- resultMatrix[1,1] + 1
        counter <- counter + 1
      }else{
        resultMatrix[2,1] <- resultMatrix[2,1] + 1
        predictionMatrix[counter,1] <- 1
        counter <- counter + 1
      }
    }else{
      if(result >= 0){
        resultMatrix[1,2] <- resultMatrix[1,2] + 1
        predictionMatrix[counter,2] <- 1
        counter <- counter + 1
      }else{
        resultMatrix[2,2] <- resultMatrix[2,2] + 1
        predictionMatrix[counter,1] <- 1
        predictionMatrix[counter,2] <- 1
        counter <- counter + 1
      }
    }
  }

}
# precision-recall curves
pred <- prediction(predictionMatrix[,1]-2*runif(nrow(predictionMatrix)),predictionMatrix[,2])
perf <- performance(pred, "prec","rec")
plot(perf, xlim = c(0,1), ylim = c(0,1))

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
