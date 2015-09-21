rm(list = ls())
set.seed(100) # Set a seed to fix the generated random numbers
#Generate Datasets
dataset1 <- cbind(rnorm(50, 0, 1), rnorm(50, 1, 1), matrix(-1, 50, 1))
dataset2 <- cbind(rnorm(250, 3, 1), rnorm(250, 4, 1), matrix(1, 250, 1))
separableData <- rbind(dataset1, dataset2)

#Apply Linear SVM with Hard Margins (plotting with the first fold)
folds1 <- cut(seq(1, nrow(dataset1)), breaks=10, labels=FALSE)
indexes1 <- which(folds1==1, arr.ind=TRUE)
folds2 <- cut(seq(1, nrow(dataset2)), breaks=10, labels=FALSE)
indexes2 <- which(folds2==1, arr.ind=TRUE)
trainingLinear <- rbind(dataset1[-indexes1,],dataset2[-indexes2,])
testingLinear <- rbind(dataset1[indexes1,],dataset2[indexes2,])
trainingLinearX <- trainingLinear[,-3]
trainingLinearY <- as.matrix(trainingLinear[,3])
testingLinearX <- testingLinear[,-3]
testingLinearY <- as.matrix(testingLinear[,3])

H = (trainingLinearY %*% t(trainingLinearY)) * (trainingLinearX %*% t(trainingLinearX))
c = matrix(rep(-1, nrow(trainingLinearX)))
A = t(trainingLinearY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainingLinearX)))
u = matrix(rep(100, nrow(trainingLinearX)))

require(kernlab)
linearLDResult = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(linearLDResult))

w = rep(0, ncol(trainingLinearX))
for(i in 1:nrow(trainingLinearX))
{
  w = w + alpha[i] * trainingLinearY[i] * trainingLinearX[i, ]
}

sv = trainingLinear[alpha > 0.01, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#Show the plots
plot(-5:10, -5:10, type = "n", main='Before Replication', ylab='y', xlab='x')  
points(dataset1[,1], dataset1[,2], col = "black")
points(dataset2[,1], dataset2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "black")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "blue")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "black")  

#Replicate the smaller dataset 5 times to solve the problem
dataset1 <- rbind(dataset1, dataset1, dataset1, dataset1, dataset1)
trainingLinear <- rbind(dataset1[-indexes2,],dataset2[-indexes2,])
testingLinear <- rbind(dataset1[indexes2,],dataset2[indexes2,])
trainingLinearX <- trainingLinear[,-3]
trainingLinearY <- as.matrix(trainingLinear[,3])
testingLinearX <- testingLinear[,-3]
testingLinearY <- as.matrix(testingLinear[,3])
H = (trainingLinearY %*% t(trainingLinearY)) * (trainingLinearX %*% t(trainingLinearX))
c = matrix(rep(-1, nrow(trainingLinearX)))
A = t(trainingLinearY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainingLinearX)))
u = matrix(rep(100, nrow(trainingLinearX)))

require(kernlab)
linearLDResult = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(linearLDResult))

w = rep(0, ncol(trainingLinearX))
for(i in 1:nrow(trainingLinearX))
{
  w = w + alpha[i] * trainingLinearY[i] * trainingLinearX[i, ]
}

sv = trainingLinear[alpha > 0.01, ] #Support vectors
w0 = (1/nrow(sv)) * sum(as.matrix(sv[, 3]) - sv[, -3] %*% w)

#Show the plots
plot(-5:10, -5:10, type = "n", main='After Replication', ylab='y', xlab='x')  
points(dataset1[,1], dataset1[,2], col = "black")
points(dataset2[,1], dataset2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "black")
curve(-w[1]/w[2] * x - w0/w[2], add = TRUE, col = "blue")  
curve(-w[1]/w[2] * x - (w0+1)/w[2], add = TRUE, col = "red")  
curve(-w[1]/w[2] * x - (w0-1)/w[2], add = TRUE, col = "black")  