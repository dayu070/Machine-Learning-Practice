data <- read.table("svar-set1.dat")
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
v <- vector()
for (i in 1:10){
  indexes <- which(folds==i, arr.ind=TRUE)
  testing <- data[indexes, ]
  training <- data[-indexes, ]
  xTraining <- training[, 1]
  yTraining <- training[, 2]
  xTesting <- testing[, 1]
  yTesting <- testing[, 2]
  amount <- length(xTesting)
  linearModel <- lm(yTraining~xTraining)
  theta0 <- linearModel$coeff[1]
  theta1 <- linearModel$coeff[2]
  yHat <- xTesting*theta1 + theta0
  MSE <- sum((yHat-yTesting)^2)/amount
  RSE <- sum((yHat-yTesting)^2/yTesting^2)/amount
  if (i == 1){
    v <- c(theta0, theta1, MSE, RSE)
  }else{
    v <- c(v, c(theta0, theta1, MSE, RSE))
  }
}
finalMatrix <- matrix(v, 10, 4, byrow = T)
colnames(finalMatrix) <- c("theta0", "theta1", "MSE", "RSE")