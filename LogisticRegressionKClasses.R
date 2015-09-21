rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
dataClass0 <- as.matrix(cbind(data[1:50,1:4], matrix(0,50,1)))
dataClass1 <- as.matrix(cbind(data[51:100,1:4], matrix(1,50,1)))
dataClass2 <- as.matrix(cbind(data[101:150,1:4], matrix(2,50,1)))
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

resultMatrix <- matrix(0,3,3)
for (m in 1:10){
  indexes <- which(folds==m, arr.ind=TRUE)
  training0vs1 <- rbind(dataClass0[-indexes,],dataClass1[-indexes,])
  testing0vs1 <- rbind(dataClass0[indexes,],dataClass1[indexes,])
  training0vs2 <- rbind(dataClass0[-indexes,],dataClass2[-indexes,])
  testing0vs2 <- rbind(dataClass0[indexes,],dataClass2[indexes,])
  training1vs2 <- rbind(dataClass1[-indexes,],dataClass2[-indexes,])
  testing1vs2 <- rbind(dataClass1[indexes,],dataClass2[indexes,])
  
  finalTheta0vs1 <- gradientDescent(as.matrix(training0vs1[,1:(length(training0vs1[1,])-1)]), as.matrix(training0vs1[,length(training0vs1[1,])]), 200) #Iterate 200 times
  finalTheta0vs2 <- gradientDescent(as.matrix(training0vs2[,1:(length(training0vs2[1,])-1)]), rbind(matrix(0,45,1),matrix(1,45,1)), 200) #Iterate 200 times
  finalTheta1vs2 <- gradientDescent(as.matrix(training1vs2[,1:(length(training1vs2[1,])-1)]), rbind(matrix(0,45,1),matrix(1,45,1)), 200) #Iterate 200 times
  
  testingFeature0vs1 <- cbind(matrix(1, length(testing0vs1[,1]), 1), testing0vs1[,1:(length(testing0vs1[1,])-1)])
  testingFeature0vs2 <- cbind(matrix(1, length(testing0vs2[,1]), 1), testing0vs2[,1:(length(testing0vs2[1,])-1)])
  testingFeature1vs2 <- cbind(matrix(1, length(testing1vs2[,1]), 1), testing1vs2[,1:(length(testing1vs2[1,])-1)])
  yHat01 <- 1/(1+exp(-testingFeature0vs1 %*% finalTheta0vs1))
  yHat02 <- 1/(1+exp(-testingFeature0vs2 %*% finalTheta0vs2))
  yHat12 <- 1/(1+exp(-testingFeature1vs2 %*% finalTheta1vs2))


  for (i in 1:length(yHat01[,1])){
    if(i <= length(yHat01[,1])/2){
      if(yHat01[i,1] < 0.5){
        resultMatrix[1,1] <- resultMatrix[1,1] + 1
      }
      else{
        resultMatrix[2,1] <- resultMatrix[2,1] + 1
      }
      if(yHat12[i,1] < 0.5){
        resultMatrix[2,2] <- resultMatrix[2,2] + 1
      }else{
        resultMatrix[3,2] <- resultMatrix[3,2] + 1
      }
    }else{
      if(yHat02[i,1]>=0.5){
        resultMatrix[3,3] <- resultMatrix[3,3] + 1
      }else
      {
        resultMatrix[1,3] <- resultMatrix[1,3] + 1
      }
    }
  }
}

precisionOfClass0 <- resultMatrix[1,1]/sum(resultMatrix[1,])
precisionOfClass1 <- resultMatrix[2,2]/sum(resultMatrix[2,])
precisionOfClass2 <- resultMatrix[3,3]/sum(resultMatrix[3,])
recallOfClass0 <- resultMatrix[1,1]/sum(resultMatrix[,1])
recallOfClass1 <- resultMatrix[2,2]/sum(resultMatrix[,2])
recallOfClass2 <- resultMatrix[3,3]/sum(resultMatrix[,3])
FMeasureOfClass0 <- 2*precisionOfClass0*recallOfClass0/(precisionOfClass0+recallOfClass0)
FMeasureOfClass1 <- 2*precisionOfClass1*recallOfClass1/(precisionOfClass1+recallOfClass1)
FMeasureOfClass2 <- 2*precisionOfClass2*recallOfClass2/(precisionOfClass2+recallOfClass2)
accuracy <- (resultMatrix[2,2] + resultMatrix[1,1] + resultMatrix[3,3])/sum(resultMatrix)
resultMatrix
precisionOfClass0
precisionOfClass1
precisionOfClass2
recallOfClass0
recallOfClass1
recallOfClass2
FMeasureOfClass0
FMeasureOfClass1
FMeasureOfClass2
accuracy