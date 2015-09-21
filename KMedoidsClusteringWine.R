rm(list = ls())
library(cluster)
data <- read.table("wine.data", header = FALSE, sep=",")
data1 <- data[, 2:14]
result <- pam(data1, 3)
result$cluster
table(data[,1], result$cluster)
data2 <- data1[, 1:2]
colnames(data2) <- c("Alcohol", "Malic acid")
plot(data2, col = result$cluster)
points(result$medoids[,1:2], col = 1:3, pch = 8, cex=2)
data3 <- data1[, 3:4]
colnames(data3) <- c("Ash", "Alcalinity of ash")
plot(data3, col = result$cluster)
points(result$medoids[,3:4], col = 1:3, pch = 8, cex=2)