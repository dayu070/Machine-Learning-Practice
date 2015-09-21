rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data <- data[1:100,]
dataClass0 <- cbind(data[1:50,1], matrix(0,50,1))
dataClass1 <- cbind(data[51:100,1], matrix(1,50,1))
folds <- cut(seq(1, nrow(dataClass0)), breaks=10, labels=FALSE)
resultMatrix <- matrix(0, 2, 2)
for (m in 1:10){
indexes <- which(folds==m, arr.ind=TRUE)
training <- rbind(dataClass0[-indexes,],dataClass1[-indexes,])
testing <- rbind(dataClass0[indexes,],dataClass1[indexes,])

mu0 <- mean(training[1:45,1])
mu1 <- mean(training[46:90,1])
sigma0 <- var(training[1:45,1])
sigma1 <- var(training[46:90,1])

  for(i in 1:length(testing[,1])){
    if (i <= 5){
      if((-0.5*(testing[i,1]-mu0)^2/sigma0^2) >= (-0.5*(testing[i,1]-mu1)^2/sigma1^2) ){
        resultMatrix[1,1] <- resultMatrix[1,1] + 1
      }else
      {
        resultMatrix[2,1] <- resultMatrix[2,1] + 1
      }
    }else{
      if((-0.5*(testing[i,1]-mu0)^2/sigma0^2) >= (-0.5*(testing[i,1]-mu1)^2/sigma1^2) ){
        resultMatrix[1,2] <- resultMatrix[1,2] + 1
      }else
      {
        resultMatrix[2,2] <- resultMatrix[2,2] + 1
      }
    }
  }

}

precisionOfClass0 <- resultMatrix[1,1]/(resultMatrix[1,1] + resultMatrix[1,2])
precisionOfClass1 <- resultMatrix[2,2]/(resultMatrix[2,1] + resultMatrix[2,2])
recallOfClass0 <- resultMatrix[1,1]/(resultMatrix[1,1] + resultMatrix[2,1])
recallOfClass1 <- resultMatrix[2,2]/(resultMatrix[2,2] + resultMatrix[1,2])
FMeasureOfClass0 <- 2*precisionOfClass0*recallOfClass0/(precisionOfClass0+recallOfClass0)
FMeasureOfClass1 <- 2*precisionOfClass1*recallOfClass1/(precisionOfClass1+recallOfClass1)
accuracy <- (resultMatrix[2,2] + resultMatrix[1,1])/(resultMatrix[2,2] + resultMatrix[1,1]+ resultMatrix[1,2] + resultMatrix[2,1])
resultMatrix
precisionOfClass0
precisionOfClass1
recallOfClass0
recallOfClass1
FMeasureOfClass0
FMeasureOfClass1
accuracy
