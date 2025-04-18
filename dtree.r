library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(datasets)
data(iris)
set.seed(123)
train_index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]
model <- rpart(Species ~ ., data = train_data, method = "class")
rpart.plot(model, type = 3, extra = 104, fallen.leaves = TRUE, main = "Decision Tree for Iris Dataset")
predictions <- predict(model, test_data, type = "class")
conf_matrix <- table(test_data$Species, predictions)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 2)))
