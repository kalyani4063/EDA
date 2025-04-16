#2
iris_data <- read.csv('Iris.csv')
head(iris_data)
tail(iris_data)
summary(iris_data)
mean_petal_width <- function(data, species_name) {
 species_data <- subset(data, data[, 5] == species_name)
 return(mean(species_data[, 4]))
}
mean_versicolor_petal_width <- mean_petal_width(iris_data, "versicolor")
print(mean_versicolor_petal_width)
hist(iris_data[, 4],
 main = "Histogram of Petal Width",
 xlab = "Petal Width",
 col = "lightblue",
 border = "black")
calculate_statistics <- function(data) {
 mean_val <- mean(data[, 1])
 median_val <- median(data[, 1])
 variance_val <- var(data[, 1])
 sd_val <- sd(data[, 1])
 return(c(mean = mean_val, median = median_val, variance = variance_val, sd = sd_val))
}
get_species_statistics <- function(data, species_name) {
22CSEB40 HARSHA VARTHANAN S
 species_data <- subset(data, data[, 5] == species_name)
 return(calculate_statistics(species_data))
}
setosa_stats <- get_species_statistics(iris_data, "setosa")
versicolor_stats <- get_species_statistics(iris_data, "versicolor")
virginica_stats <- get_species_statistics(iris_data, "virginica")
results <- data.frame(
 Setosa = setosa_stats,
 Versicolor = versicolor_stats,
 Virginica = virginica_stats
)
print(results)
#1
age <- c(35, 65, 49, 30, 20, 40, 90, 54, 78, 45)
systolic_pressure <- c(122, 120, 120, 115, 130, 131, 118, 122, 120, 115)
diastolic_pressure <- c(83, 79, 78, 72, 90, 90, 82, 80, 82, 75)
calculate_stats <- function(data) {
 n <- length(data)
 min_val <- min(data)
 max_val <- max(data)
 median_val <- median(data)
 mean_val <- mean(data)
 variance_val <- var(data)
 sd_val <- sd(data)

 return(c(n, min_val, max_val, median_val, mean_val, variance_val, sd_val))
}
age_stats <- calculate_stats(age)
systolic_pressure_stats <- calculate_stats(systolic_pressure)
diastolic_pressure_stats <- calculate_stats(diastolic_pressure)
stats_table <- data.frame(
22CSEB40 HARSHA VARTHANAN S
 Statistic = c("Number of samples", "Minimum value", "Maximum value", "Median", "Mean",
"Variance", "Standard deviation"),
 Age = age_stats,
 Systolic_Pressure = systolic_pressure_stats,
 Diastolic_Pressure = diastolic_pressure_stats
)
print(stats_table)
