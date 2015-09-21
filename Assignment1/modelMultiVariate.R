data <- read.table("mvar-set1.dat")
dataMatrix <- cbind(matrix(rep(1, 2500), 2500, 1), data[ ,1], data[,1]^2, data[ ,2], data[,2]^2, data[,1]*data[,2])
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
v <- vector()
for (i in 1:10){
  indexes <- which(folds==i, arr.ind=TRUE)
  testing <- dataMatrix[indexes, ]
  training <- dataMatrix[-indexes, ]

  yTraining <- training[, 3]

  yTesting <- testing[, 3]
  amount <- length(yTesting)
  trainAmount <- length(yTraining)
  multiModel <- lm(yTraining~.,data=data[,1:2])
}
  theta0 <- linearModel$coeff[1]
  theta1 <- linearModel$coeff[2]
  theta2 <- linearModel$coeff[3]
  theta3 <- linearModel$coeff[4]
  yHat <- theta0 + x1Testing*theta1 + x2Testing*theta2 + x1Testing*x2Testing*theta3
  yHatTrain <- theta0 + x1Training*theta1 + x2Training*theta2 + x1Training*x2Training*theta3
  testingMSE <- sum((yHat-yTesting)^2)/amount
  testingRSE <- sum((yHat-yTesting)^2/yTesting^2)/amount
  trainingMSE <- sum((yHatTrain-yTraining)^2)/trainAmount
  trainingRSE <- sum((yHatTrain-yTraining)^2/yTraining^2)/trainAmount
  if (i == 1){
    v <- c(testingMSE, testingRSE, trainingMSE, trainingRSE)
  }else{
    v <- c(v, c(testingMSE, testingRSE, trainingMSE, trainingRSE))
  }
}
finalMatrix <- matrix(v, 10, 4, byrow = T)
colnames(finalMatrix) <- c("testingMSE", "testingRSE", "trainingMSE", "trainingRSE")
