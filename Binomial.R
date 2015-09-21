rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data <- data[1:100,]
tempData <- matrix(0, 100, length(data[1,])-1)
for (i in 1:(length(data[1,])-1)){  
  for (j in 1:length(data[,1])){
      tempData[j,i] <- round(data[j,i])    
  }
}
dataClass0 <- as.matrix(cbind(tempData[1:50,1:4], matrix(0,50,1)))
dataClass1 <- as.matrix(cbind(tempData[51:100,1:4], matrix(1,50,1)))
folds <- cut(seq(1, nrow(dataClass0)), breaks=10, labels=FALSE)
resultMatrix <- matrix(0, 2, 2)

for(m in 1:10){
  indexes <- which(folds==m, arr.ind=TRUE)
  training <- rbind(dataClass0[-indexes,],dataClass1[-indexes,])
  testing <- rbind(dataClass0[indexes,],dataClass1[indexes,])
  alpha<- matrix(0,2,4)
for (i in 1:length(alpha[,1])){
  for (j in 1:length(alpha[1,])){
    if (i == 1){
      tempAlpha <- (sum(training[1:45,j])+0.1)/(sum(training[1:45,1:4])+2*0.1) #Apply Laplase Smoothing, where Epsilon is equal to 0.1
    }else{
      tempAlpha <- (sum(training[46:90,j])+0.1)/(sum(training[46:90,1:4])+2*0.1)
    }
    alpha[i,j] <- tempAlpha
  }
}

yHat <- matrix(0, 10, 1)
for (i in 1:length(testing[,1])){
  tempProduct1 <- 1
  tempProduct2 <- 1
  for (j in 1:(length(testing[1,])-1)){
      tempProduct1 <- tempProduct1 * choose(sum(testing[i,1:4]),testing[i,j])*(alpha[1,j]^testing[i,j])*((1-alpha[1,j])^(sum(testing[i,1:4])-testing[i,j]))
      tempProduct2 <- tempProduct2 * choose(sum(testing[i,1:4]),testing[i,j])*(alpha[2,j]^testing[i,j])*((1-alpha[2,j])^(sum(testing[i,1:4])-testing[i,j]))
  }
  if(tempProduct1 >= tempProduct2){
    yHat[i,1] <- 0
  }else{
    yHat[i,1] <- 1
  }
}

for (i in 1:length(yHat[,1])){
  if (i<=length(yHat[,1])/2){
    if (yHat[i,1] == 0){
      resultMatrix[1,1] <- resultMatrix[1,1] + 1
    }else{
      resultMatrix[2,1] <- resultMatrix[2,1] + 1
    }
  }else{
    if (yHat[i,1] == 0){
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
