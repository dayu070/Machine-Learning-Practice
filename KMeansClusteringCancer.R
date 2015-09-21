rm(list = ls())
data <- read.table("breast-cancer-wisconsin.data", header = FALSE, sep=",")
data <- rbind(data[1:23,],data[25:40,], data[42:139,], data[141:145,], data[147:158,], data[160:164,], data[166:206,])
data <- data[order(data[,11]),]
data1 <- data[, 1:10] 
result <- kmeans(data1, 2)
result
result$cluster
table(data[,11], result$cluster)
data2 <- data1[, 1:2]
colnames(data2) <- c("Radius", "Texture")
plot(data2, col = result$cluster)
points(result$centers[,1:2], col = 1:2, pch = 8, cex=2)
data3 <- data1[, 3:4]
colnames(data3) <- c("Perimeter", "Area")
plot(data3, col = result$cluster)
points(result$centers[,3:4], col = 1:2, pch = 8, cex=2)