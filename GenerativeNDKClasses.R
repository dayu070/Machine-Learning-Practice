rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
dataClass0 <- as.matrix(cbind(data[1:50,1:4], matrix(0,50,1)))
dataClass1 <- as.matrix(cbind(data[51:100,1:4], matrix(1,50,1)))
dataClass2 <- as.matrix(cbind(data[101:150,1:4], matrix(2,50,1)))

folds <- cut(seq(1, nrow(dataClass0)), breaks=10, labels=FALSE)
resultMatrix <- matrix(0, 2, 2)
library("MASS")
resultMatrix <- matrix(0,3,3)
for(m in 1:10){
  indexes <- which(folds==m, arr.ind=TRUE)
  training <- rbind(dataClass0[-indexes,],dataClass1[-indexes,],dataClass2[-indexes,])
  testing <- rbind(dataClass0[indexes,],dataClass1[indexes,],dataClass2[indexes,])
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
  muClass2 <- c(mean(training[91:135,1]))
  muClass2[2] <- mean(training[91:135,2])
  muClass2[3] <- mean(training[91:135,3])
  muClass2[4] <- mean(training[91:135,4])
  muClass2 <- matrix(muClass2,4,1)

  sigmaClass0 <- cov(training[1:45, 1:4])
  sigmaClass1 <- cov(training[46:90, 1:4])
  sigmaClass2 <- cov(training[91:135, 1:4])


discriminativeClass0And1 <- function(x){
  temp0 <- -log(abs(det(sigmaClass0)))-0.5*t(x-muClass0)%*%ginv(sigmaClass0)%*%(x-muClass0)
  temp1 <- -log(abs(det(sigmaClass1)))-0.5*t(x-muClass1)%*%ginv(sigmaClass1)%*%(x-muClass1)
  return(temp0-temp1)
}

discriminativeClass0And2 <- function(x){
  temp0 <- -log(abs(det(sigmaClass0)))-0.5*t(x-muClass0)%*%ginv(sigmaClass0)%*%(x-muClass0)
  temp2 <- -log(abs(det(sigmaClass2)))-0.5*t(x-muClass2)%*%ginv(sigmaClass2)%*%(x-muClass2)
  return(temp0-temp2)
}

discriminativeClass1And2 <- function(x){
  temp1 <- -log(abs(det(sigmaClass1)))-0.5*t(x-muClass1)%*%ginv(sigmaClass1)%*%(x-muClass1)
  temp2 <- -log(abs(det(sigmaClass2)))-0.5*t(x-muClass2)%*%ginv(sigmaClass2)%*%(x-muClass2)
  return(temp1-temp2)
}

for (i in 1:length(testing[,1])){
  tempX <- matrix(0, 4, 1)
  for (j in 1:4){
    tempX[j,1] <- testing[i,j]
  }
  if(i<=5 && i >=1){
    if (discriminativeClass0And1(tempX) >= 0 && discriminativeClass0And2(tempX) >= 0){
      resultMatrix[1,1] <- resultMatrix[1,1]+1
    }else if(discriminativeClass1And2(tempX) >= 0){
      resultMatrix[1,2] <- resultMatrix[1,2]+1
    }else{
      resultMatrix[1,3] <- resultMatrix[1,3]+1
    }
  }else if(i<=10 && i >=6){
    if (discriminativeClass0And1(tempX) < 0 && discriminativeClass1And2(tempX) >= 0){
      resultMatrix[2,2] <- resultMatrix[2,2]+1
    }else if(discriminativeClass0And2(tempX) >= 0){
      resultMatrix[2,1] <- resultMatrix[2,1]+1
    }else{
      resultMatrix[2,3] <- resultMatrix[2,3]+1
    }
  }else{
    if (discriminativeClass0And2(tempX) < 0 && discriminativeClass1And2(tempX) < 0){
      resultMatrix[3,3] <- resultMatrix[3,3]+1
    }else if(discriminativeClass0And1(tempX) >= 0){
      resultMatrix[3,1] <- resultMatrix[3,1]+1
    }else{
      resultMatrix[3,2] <- resultMatrix[3,2]+1
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

