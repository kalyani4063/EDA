library(datasets)
install.packages("ggplot2")
library(ggplot2)
library(cluster)
data(iris)
iris_data <- iris[, -5]
set.seed(123)
wss <- sapply(1:10, function(k) {
 kmeans(iris_data, centers = k, nstart = 20)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
 xlab = "Number of Clusters (k)",
 ylab = "Total Within-Cluster Sum of Squares",
 main = "Elbow Method for Determining Optimal Clusters")
set.seed(123)
S Kalyani 22cseb11
kmeans_result <- kmeans(iris_data, centers = 3, nstart = 20)
iris$Cluster <- as.factor(kmeans_result$cluster)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Cluster, shape = Species)) +
 geom_point(size = 3) +
 labs(title = "K-Means Clustering of Iris Data",
 x = "Petal Length",
 y = "Petal Width") +
 theme_minimal()
clusplot(iris_data, kmeans_result$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
print(kmeans_result$centers)
