rm(list = ls())
data <- read.table("wine.data", header = FALSE, sep=",")
data1 <- data[, 2:14]
result <- kmeans(data1, 3)
result
result$cluster
table(data[,1], result$cluster)
data2 <- data1[, 1:2]
colnames(data2) <- c("Alcohol", "Malic acid")
plot(data2, col = result$cluster)
points(result$centers[,1:2], col = 1:3, pch = 8, cex=2)
data3 <- data1[, 3:4]
colnames(data3) <- c("Ash", "Alcalinity of ash")
plot(data3, col = result$cluster)
points(result$centers[,3:4], col = 1:3, pch = 8, cex=2)