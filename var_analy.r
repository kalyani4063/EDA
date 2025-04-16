summary(mtcars)
hist(mtcars$mpg,
Name:Kalyani.S Rollno:22CSEB11 PgNo:
 main="Histogram of Miles Per Gallon",
 xlab="Miles Per Gallon",
 col="grey",
 border="black")
cylinder_counts <- table(mtcars$cyl)
print(cylinder_counts)
barplot(cylinder_counts,
 main="Frequency of Cars with Different Numbers of Cylinders",
 xlab="Number of Cylinders",
 ylab="Frequency",
 border="black",
 )
plot(mtcars$mpg, mtcars$hp,
 main="Scatter Plot of mpg vs hp",
 xlab="Miles Per Gallon (mpg)",
 ylab="Horsepower (hp)",
 pch=19,
 col="blue")
# linear trend line
abline(lm(hp ~ mpg, data=mtcars), col="red")
boxplot(mpg ~ cyl, data=mtcars,
 main="Box Plot of mpg by Number of Cylinders",
 xlab="Number of Cylinders",
 ylab="Miles Per Gallon (mpg)",
 col="lightblue",
 border="black")
# Create a contingency table of transmission type and number of gears
contingency_table <- table(mtcars$am, mtcars$gear)
print(contingency_table)
# Stacked bar plot
barplot(contingency_table,
 main="Stacked Bar Plot of Transmission Type by Number of Gears",
 xlab="Number of Gears",
 ylab="Count",
 col=c("lightblue", "grey"),
 legend.text=c("Automatic", "Manual"),
 args.legend=list(x="topright", bty="n"))
color_gradient <- colorRampPalette(c("blue", "red"))
colors <- color_gradient(100) # Generate 100 colors from blue to red
color_index <- cut(mtcars$wt, breaks = 100, labels = FALSE) # Divide wt into 100 intervals
point_colors <- colors[color_index]
plot(mtcars$hp, mtcars$mpg,
 col = point_colors,
 pch = 19, # Solid circles
 xlab = "Horsepower (hp)",
 ylab = "Miles Per Gallon (mpg)",
 main = "Scatter Plot of mpg vs hp with wt as Color Gradient")
# Add a color legend
legend("topright", legend = c("Low Weight", "High Weight"),
 fill = c(colors[1], colors[100]),
 title = "Weight (wt)")
Fit a linear regression model
model <- lm(mpg ~ hp + wt, data = mtcars)
# Display the model summary
summary(model)
correlation_matrix <- cor(mtcars)
Name:Kalyani.S Rollno:22CSEB11 PgNo:
# Print the correlation matrix
print(correlation_matrix)
correlation_matrix <- cor(mtcars)
# Create a heatmap of the correlation matrix
heatmap(correlation_matrix,
 main = "Correlation Matrix Heatmap",
 col = colorRampPalette(c("blue", "white", "red"))(25),
 scale = "none", # Do not scale the rows or columns
 margins = c(10, 10), # Adjust margins for labels
 xlab = "Variables",
 ylab = "Variables")
