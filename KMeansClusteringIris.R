rm(list = ls())
data <- read.table("bezdekIris.data", header = FALSE, sep=",")
data1 <- data[, 1:4]
result <- kmeans(data1, 3)
result
result$cluster
table(data[,5], result$cluster)
data2 <- data1[, 1:2]
colnames(data2) <- c("Sepal.Length", "Sepal.Width")
plot(data2, col = result$cluster)
points(result$centers[,1:2], col = 1:3, pch = 8, cex=2)
data3 <- data1[, 3:4]
colnames(data3) <- c("Petal.Length", "Petal.Width")
plot(data3, col = result$cluster)
points(result$centers[,3:4], col = 1:3, pch = 8, cex=2)
