rm(list = ls())
set.seed(100) # Set a seed to fix the generated random numbers
#Generate Datasets
dataset1 <- cbind(rnorm(50, 0, 1), rnorm(50, 1, 1), matrix(-1, 50, 1))
dataset2 <- cbind(rnorm(50, 7, 1), rnorm(50, 8, 1), matrix(1, 50, 1))
separableData <- rbind(dataset1, dataset2)
dataset3 <- cbind(rnorm(50, 0, 2), rnorm(50, 1, 2), matrix(-1, 50, 1))
dataset4 <- cbind(rnorm(50, 2, 2), rnorm(50, 3, 2), matrix(1, 50, 1))
nonSeparableData <- rbind(dataset3, dataset4)

#Apply Linear SVM with Gaussian Kernel
folds <- cut(seq(1, nrow(dataset1)), breaks=10, labels=FALSE)
indexes <- which(folds==1, arr.ind=TRUE)
trainingLinear <- rbind(dataset1[-indexes,],dataset2[-indexes,])
testingLinear <- rbind(dataset1[indexes,],dataset2[indexes,])
trainingLinearX <- trainingLinear[,-3]
trainingLinearY <- as.matrix(trainingLinear[,3])
testingLinearX <- testingLinear[,-3]
testingLinearY <- as.matrix(testingLinear[,3])

trainingLinearNotSeparable <- rbind(dataset3[-indexes,],dataset4[-indexes,])
testingLinearNotSeparable <- rbind(dataset3[indexes,],dataset4[indexes,])
testingLinearNotSeparableX <- testingLinearNotSeparable[,-3]
testingLinearNotSeparableY <- as.matrix(testingLinearNotSeparable[,3])
trainingLinearNotSeparableX <- trainingLinearNotSeparable[,-3]
trainingLinearNotSeparableY <- as.matrix(trainingLinearNotSeparable[,3])

require(KRLS)
H = (trainingLinearY %*% t(trainingLinearY)) * (gausskernel(X=trainingLinearX, sigma=2))
c = matrix(rep(-1, nrow(trainingLinearX)))
A = t(trainingLinearY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainingLinearX)))
u = matrix(rep(1000000, nrow(trainingLinearX)))

require(kernlab)
linearLDResult = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(linearLDResult))

w = rep(0, ncol(trainingLinearX))
for(i in 1:nrow(trainingLinearX))
{
  w = w + alpha[i] * trainingLinearY[i] * trainingLinearX[i, ]
}

sv = trainingLinear[alpha > 0.1, ] #Support vectors
w0 = 0 
for(m in 1:nrow(sv))
{
  wtx = 0
  for(n in 1:nrow(trainingLinearX))
  {
    wtx = wtx + + alpha[n] * trainingLinearY[n] * gausskernel(X = rbind(trainingLinearX[n, ], sv[m, -3]), sigma=2)[1,2]
  }
  w0 = w0 + sv[m, 3] - wtx
}
w0 = (1/nrow(sv)) * w0

#Show the plots
plot(-5:10, -5:10, type = "n", main='Linearly Separable Gaussian', ylab='y', xlab='x')  
points(dataset1[,1], dataset1[,2], col = "black")
points(dataset2[,1], dataset2[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "black")

confusionMatrixLinear <- matrix(0, 2, 2)
for (j in 1:nrow(testingLinear))
{
  temp <- 0
  for (t in 1:nrow(trainingLinearX))
  {
    temp = temp + + alpha[t]*trainingLinearY[t] * gausskernel(X = rbind(trainingLinearX[t, ], testingLinearX[j, ]), sigma=2)[1,2]
  }
  temp <- temp + w0
  if (temp > 0)
  {
    if (j <= 5)
    {
      confusionMatrixLinear[2,1] <- confusionMatrixLinear[2,1] + 1
    }else
    {
      confusionMatrixLinear[2,2] <- confusionMatrixLinear[2,2] + 1
    }
  }else{
    if (j <= 5)
    {
      confusionMatrixLinear[1,1] <- confusionMatrixLinear[1,1] + 1
    }else
    {
      confusionMatrixLinear[1,2] <- confusionMatrixLinear[1,2] + 1
    }
  }
}

H = (trainingLinearNotSeparableY %*% t(trainingLinearNotSeparableY)) * (gausskernel(X=trainingLinearNotSeparableX, sigma=2))
c = matrix(rep(-1, nrow(trainingLinearNotSeparableX)))
A = t(trainingLinearNotSeparableY)
b = 0
r = 0
l = matrix(rep(0, nrow(trainingLinearNotSeparableX)))
u = matrix(rep(100000, nrow(trainingLinearNotSeparableX)))

require(kernlab)
linearNotSeparableLDResult = ipop(c, H, A, b, l, u, r)
alpha = matrix(primal(linearNotSeparableLDResult))

w = rep(0, ncol(trainingLinearNotSeparableX))
for(i in 1:nrow(trainingLinearNotSeparableX))
{
  w = w + alpha[i] * trainingLinearNotSeparableY[i] * trainingLinearNotSeparableX[i, ]
}

sv = trainingLinearNotSeparable[alpha > 10000, ] #Support vectors
w0 = 0 
for(m in 1:nrow(sv))
{
  wtx = 0
  for(n in 1:nrow(trainingLinearNotSeparableX))
  {
    wtx = wtx + + alpha[n] * trainingLinearNotSeparableY[n] * gausskernel(X = rbind(trainingLinearNotSeparableX[n, ], sv[m, -3]), sigma=6)[1,2]
  }
  w0 = w0 + sv[m, 3] - wtx
}
w0 = (1/nrow(sv)) * w0

#Show the plots
plot(-5:10, -5:10, type = "n", main='Not Separable Gaussian', ylab='y', xlab='x')  
points(dataset3[,1], dataset3[,2], col = "black")
points(dataset4[,1], dataset4[,2], col = "red")
points(matrix(sv[sv[,3]==1,],ncol = 3)[,1], matrix(sv[sv[,3]==1,],ncol = 3)[,2], pch = 19, col = "red")
points(matrix(sv[sv[,3]==-1,],ncol = 3)[,1], matrix(sv[sv[,3]==-1,],ncol = 3)[,2], pch = 19, col = "black")

confusionMatrixLinearNotSeparable <- matrix(0, 2, 2)
for (j in 1:nrow(testingLinearNotSeparable))
{
  temp <- 0
  for (t in 1:nrow(trainingLinearNotSeparableX))
  {
    temp = temp + + alpha[t]*trainingLinearNotSeparableY[t] * gausskernel(X = rbind(trainingLinearNotSeparableX[t, ], testingLinearNotSeparableX[j, ]), sigma=6)[1,2]
  }
  temp <- temp + w0
  if (temp > 0)
  {
    if (j <= 5)
    {
      confusionMatrixLinearNotSeparable[2,1] <- confusionMatrixLinearNotSeparable[2,1] + 1
    }else
    {
      confusionMatrixLinearNotSeparable[2,2] <- confusionMatrixLinearNotSeparable[2,2] + 1
    }
  }else{
    if (j <= 5)
    {
      confusionMatrixLinearNotSeparable[1,1] <- confusionMatrixLinearNotSeparable[1,1] + 1
    }else
    {
      confusionMatrixLinearNotSeparable[1,2] <- confusionMatrixLinearNotSeparable[1,2] + 1
    }
  }
}

for (k in 2:10) #10 cross validation
{
  indexes <- which(folds==k, arr.ind=TRUE)
  trainingLinear <- rbind(dataset1[-indexes,],dataset2[-indexes,])
  testingLinear <- rbind(dataset1[indexes,],dataset2[indexes,])
  trainingLinearX <- trainingLinear[,-3]
  trainingLinearY <- as.matrix(trainingLinear[,3])
  testingLinearX <- testingLinear[,-3]
  testingLinearY <- as.matrix(testingLinear[,3])
  
  trainingLinearNotSeparable <- rbind(dataset3[-indexes,],dataset4[-indexes,])
  testingLinearNotSeparable <- rbind(dataset3[indexes,],dataset4[indexes,])
  testingLinearNotSeparableX <- testingLinearNotSeparable[,-3]
  testingLinearNotSeparableY <- as.matrix(testingLinearNotSeparable[,3])
  trainingLinearNotSeparableX <- trainingLinearNotSeparable[,-3]
  trainingLinearNotSeparableY <- as.matrix(trainingLinearNotSeparable[,3])
  
  require(KRLS)
  H = (trainingLinearY %*% t(trainingLinearY)) * (gausskernel(X=trainingLinearX, sigma=2))
  c = matrix(rep(-1, nrow(trainingLinearX)))
  A = t(trainingLinearY)
  b = 0
  r = 0
  l = matrix(rep(0, nrow(trainingLinearX)))
  u = matrix(rep(1000000, nrow(trainingLinearX)))
  
  require(kernlab)
  linearLDResult = ipop(c, H, A, b, l, u, r)
  alpha = matrix(primal(linearLDResult))
  
  w = rep(0, ncol(trainingLinearX))
  for(i in 1:nrow(trainingLinearX))
  {
    w = w + alpha[i] * trainingLinearY[i] * trainingLinearX[i, ]
  }
  
  sv = trainingLinear[alpha > 0.1, ] #Support vectors
  w0 = 0 
  for(m in 1:nrow(sv))
  {
    wtx = 0
    for(n in 1:nrow(trainingLinearX))
    {
      wtx = wtx + + alpha[n] * trainingLinearY[n] * gausskernel(X = rbind(trainingLinearX[n, ], sv[m, -3]), sigma=2)[1,2]
    }
    w0 = w0 + sv[m, 3] - wtx
  }
  w0 = (1/nrow(sv)) * w0
  
  for (j in 1:nrow(testingLinear))
  {
    temp <- 0
    for (t in 1:nrow(trainingLinearX))
    {
      temp = temp + + alpha[t]*trainingLinearY[t] * gausskernel(X = rbind(trainingLinearX[t, ], testingLinearX[j, ]), sigma=2)[1,2]
    }
    temp <- temp + w0
    if (temp > 0)
    {
      if (j <= 5)
      {
        confusionMatrixLinear[2,1] <- confusionMatrixLinear[2,1] + 1
      }else
      {
        confusionMatrixLinear[2,2] <- confusionMatrixLinear[2,2] + 1
      }
    }else{
      if (j <= 5)
      {
        confusionMatrixLinear[1,1] <- confusionMatrixLinear[1,1] + 1
      }else
      {
        confusionMatrixLinear[1,2] <- confusionMatrixLinear[1,2] + 1
      }
    }
  }
  
  H = (trainingLinearNotSeparableY %*% t(trainingLinearNotSeparableY)) * (gausskernel(X=trainingLinearNotSeparableX, sigma=2))
  c = matrix(rep(-1, nrow(trainingLinearNotSeparableX)))
  A = t(trainingLinearNotSeparableY)
  b = 0
  r = 0
  l = matrix(rep(0, nrow(trainingLinearNotSeparableX)))
  u = matrix(rep(100000, nrow(trainingLinearNotSeparableX)))
  
  require(kernlab)
  linearNotSeparableLDResult = ipop(c, H, A, b, l, u, r)
  alpha = matrix(primal(linearNotSeparableLDResult))
  
  w = rep(0, ncol(trainingLinearNotSeparableX))
  for(i in 1:nrow(trainingLinearNotSeparableX))
  {
    w = w + alpha[i] * trainingLinearNotSeparableY[i] * trainingLinearNotSeparableX[i, ]
  }
  
  sv = trainingLinearNotSeparable[alpha > 10000, ] #Support vectors
  w0 = 0 
  for(m in 1:nrow(sv))
  {
    wtx = 0
    for(n in 1:nrow(trainingLinearNotSeparableX))
    {
      wtx = wtx + + alpha[n] * trainingLinearNotSeparableY[n] * gausskernel(X = rbind(trainingLinearNotSeparableX[n, ], sv[m, -3]), sigma=6)[1,2]
    }
    w0 = w0 + sv[m, 3] - wtx
  }
  w0 = (1/nrow(sv)) * w0
  
  for (j in 1:nrow(testingLinearNotSeparable))
  {
    temp <- 0
    for (t in 1:nrow(trainingLinearNotSeparableX))
    {
      temp = temp + + alpha[t]*trainingLinearNotSeparableY[t] * gausskernel(X = rbind(trainingLinearNotSeparableX[t, ], testingLinearNotSeparableX[j, ]), sigma=6)[1,2]
    }
    temp <- temp + w0
    if (temp > 0)
    {
      if (j <= 5)
      {
        confusionMatrixLinearNotSeparable[2,1] <- confusionMatrixLinearNotSeparable[2,1] + 1
      }else
      {
        confusionMatrixLinearNotSeparable[2,2] <- confusionMatrixLinearNotSeparable[2,2] + 1
      }
    }else{
      if (j <= 5)
      {
        confusionMatrixLinearNotSeparable[1,1] <- confusionMatrixLinearNotSeparable[1,1] + 1
      }else
      {
        confusionMatrixLinearNotSeparable[1,2] <- confusionMatrixLinearNotSeparable[1,2] + 1
      }
    }
  }
}

confusionMatrixLinear
confusionMatrixLinearNotSeparable
