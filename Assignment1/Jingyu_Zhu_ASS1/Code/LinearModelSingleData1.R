rm(list = ls())
data <- read.table("http://www.cs.iit.edu/~agam/cs584/data/regression/svar-set1.dat")
plot(data[,1], data[,2], xlab = "x", ylab = "y")

  folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
  v <- vector()
  for (i in 1:10){
    indexes <- which(folds==i, arr.ind=TRUE)
    testing <- data[indexes, ]
    training <- data[-indexes, ]
    zmatrix <- matrix(rep(1, length(training[,1])*length(training[1,])), length(training[,1]), length(training[1,]))
    testzmatrix <- matrix(rep(1, length(testing[,1])*length(testing[1,])), length(testing[,1]), length(testing[1,]))
    for (j in 2:length(training[1,])){
      zmatrix[ ,j] <-training[ ,j-1]
    }
    for (k in 2:length(testing[1,])){
      testzmatrix[ ,k] <-testing[ ,k-1]
    }
    ymatrix <- matrix(training[ ,length(training[1,])])
    testymatrix <- matrix(testing[ ,length(testing[1,])])
    theta <- solve(t(zmatrix) %*% zmatrix) %*% t(zmatrix) %*% ymatrix
    amount <- length(training[,1])
    yHat <- t(theta) %*% t(zmatrix)
    trainingMSE <- sum((t(yHat)-ymatrix) ^ 2)/amount
    trainingRSE <- sum((t(yHat)-ymatrix)^2/ymatrix^2)/amount
    testingAmount <- length(testing[,1])
    yHattest <- t(theta) %*% t(testzmatrix)
    testingMSE <- sum((t(yHattest)-testymatrix)^2)/testingAmount
    testingRSE <- sum((t(yHattest)-testymatrix)^2/testymatrix^2)/testingAmount
  
    xTraining <- training[, 1]
    yTraining <- training[, 2]
    xTesting <- testing[, 1]
    yTesting <- testing[, 2]
    linearModel <- lm(yTraining~xTraining)
    theta0 <- linearModel$coeff[1]
    theta1 <- linearModel$coeff[2]
    yHatlmTrain <- xTraining*theta1 + theta0
    yHatlmTest <- xTesting*theta1 + theta0
    lmTestingMSE <- sum((yHatlmTest-yTesting)^2)/testingAmount
    lmTestingRSE <- sum((yHatlmTest-yTesting)^2/yTesting^2)/testingAmount
    lmTrainingMSE <- sum((yHatlmTrain-yTraining)^2)/amount
    lmTrainingRSE <- sum((yHatlmTrain-yTraining)^2/yTraining^2)/amount
    if (i == 1){
      plot(xTesting, yTesting)
      abline(theta0, theta1)
      v <- c(trainingMSE, trainingRSE, testingMSE, testingRSE, lmTrainingMSE, lmTrainingRSE, lmTestingMSE, lmTestingRSE)
    }else{
      v <- c(v, c(trainingMSE, trainingRSE, testingMSE, testingRSE, lmTrainingMSE, lmTrainingRSE, lmTestingMSE, lmTestingRSE))
    }
  }
finalMatrix <- matrix(v, 10, 8, byrow = T)
colnames(finalMatrix) <- c("trainingMSE", "trainingRSE", "testingMSE", "testingRSE", "lmTrainingMSE", "lmTrainingRSE", "lmTestingMSE", "lmTestingRSE")

