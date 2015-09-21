rm(list = ls())

require(cvTools) 

Iris <- read.table('bezdekIris.data',sep=',')
data <- Iris[,1:4]
numberOfLabel1 <- nrow(Iris[which(Iris$V5=='Iris-setosa'),])
numberOfLabel2 <- nrow(Iris[which(Iris$V5=='Iris-versicolor'),])
numberOfLabel3 <- nrow(Iris[which(Iris$V5=='Iris-virginica'),])
data1 <- cbind(c(rep(0,numberOfLabel1),rep(1,numberOfLabel2+numberOfLabel3)),data[])
data2 <- cbind(c(rep(0,numberOfLabel2),rep(1,numberOfLabel3+numberOfLabel1)),data[])
data3 <- cbind(c(rep(0,numberOfLabel3),rep(1,numberOfLabel1+numberOfLabel2)),data[])
names(data1)[1]<-paste("V0")
names(data2)[1]<-paste("V0")
names(data3)[1]<-paste("V0")

#cross-validation 
k <- 10
folds <- cvFolds(nrow(data1), K = k, type = "interleaved")
confusionMatrix1 <- matrix(0,2,2) # confusion matrix computing by implemented function
confusionMatrix2 <- matrix(0,2,2)
confusionMatrix3 <- matrix(0,2,2)

sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

for(i in 1:k){
  testdata  <- subset(data1, folds$which == i)
  traindata <- subset(data1, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:5]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:5]))
  Y <- as.matrix(traindata$V0)

  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    C <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(C)
  }
  theta1 <- rep(0,ncol(Xtrain))
  cost(theta1)
  theta2 <- optim(par=theta1,fn=cost)
  theta <- theta2$par
  theta2$value
  db <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(db)){
    if(db[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix1 <- confusionMatrix1 + table(yhat,testdata[,1])
}


for(i in 1:k){
  testdata  <- subset(data2, folds$which == i)
  traindata <- subset(data2, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:5]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:5]))
  Y <- as.matrix(traindata$V0)

  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    C <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(C)
  }
  theta1 <- rep(0,ncol(Xtrain))
  cost(theta1)
  theta2 <- optim(par=theta1,fn=cost)
  theta <- theta2$par
  theta2$value
  db <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(db)){
    if(db[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix2 <- confusionMatrix2 + table(yhat,testdata[,1])
}


for(i in 1:k){
  testdata  <- subset(data3, folds$which == i)
  traindata <- subset(data3, folds$which != i)
  Xtrain <- as.matrix(cbind(rep(1,nrow(traindata)),traindata[2:5]))
  Xtest <- as.matrix(cbind(rep(1,nrow(testdata)),testdata[2:5]))
  Y <- as.matrix(traindata$V0)

  cost <- function(theta)
  {
    m <- nrow(Xtrain)
    g <- sigmoid(Xtrain%*%theta)
    C <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
    return(C)
  }
  theta1 <- rep(0,ncol(Xtrain))
  cost(theta1)
  theta2 <- optim(par=theta1,fn=cost)
  theta <- theta2$par
  theta2$value
  db <- sigmoid(Xtest%*%theta)
  yhat <- vector()
  for (i in 1:length(db)){
    if(db[i]>0.5)
      yhat <- c(yhat,1)
    else
      yhat <- c(yhat,0)
  }
  confusionMatrix3 <- confusionMatrix3 + table(yhat,testdata[,1])
}



#evaluate the performance of the algorithm
accuracy <- c((confusionMatrix1[2,2] + confusionMatrix1[1,1])/(confusionMatrix1[2,2] + confusionMatrix1[1,1]+ confusionMatrix1[1,2] + confusionMatrix1[2,1]),
              (confusionMatrix2[2,2] + confusionMatrix2[1,1])/(confusionMatrix2[2,2] + confusionMatrix2[1,1]+ confusionMatrix2[1,2] + confusionMatrix2[2,1]),
              (confusionMatrix3[2,2] + confusionMatrix3[1,1])/(confusionMatrix3[2,2] + confusionMatrix3[1,1]+ confusionMatrix3[1,2] + confusionMatrix3[2,1]))
precision <- as.matrix(c(confusionMatrix1[1,1]/(confusionMatrix1[1,1] + confusionMatrix1[1,2]),confusionMatrix2[1,1]/(confusionMatrix2[1,1] + confusionMatrix2[1,2]),confusionMatrix3[1,1]/(confusionMatrix3[1,1] + confusionMatrix3[1,2])))
rownames(precision) <- c("C1","C2","C3")
recall <- as.matrix(c(confusionMatrix1[1,1]/(confusionMatrix1[1,1] + confusionMatrix1[2,1]),confusionMatrix2[1,1]/(confusionMatrix2[1,1] + confusionMatrix2[2,1]),confusionMatrix3[1,1]/(confusionMatrix3[1,1] + confusionMatrix3[2,1])))
rownames(recall) <- c("C1","C2","C3")
Fmeasure <- c(2*precision[1]*recall[1]/(precision[1]+recall[1]),
              2*precision[2]*recall[2]/(precision[2]+recall[2]),
              2*precision[3]*recall[3]/(precision[3]+recall[3]))



confusionMatrix1
confusionMatrix2
confusionMatrix3
accuracy
precision
recall
Fmeasure

