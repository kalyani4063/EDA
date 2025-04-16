df <- read.csv("netflixstock.csv")
str(df)
1. How can Netflix’s Closing Price be predicted using its Opening Price and Volume?
model <- lm(Close ~ Open + Volume, data = df)
summary(model)
df$predicted_close <- predict(model, newdata = df)
rmse <- sqrt(mean((df$Close - df$predicted_close)^2))
rmse
2. Are there frequent patterns between Volume and stock price ranges?
install.packages("arules")
library(arules)
df$Open_range <- discretize(df$Open, method = "interval", breaks = 4, labels = c("Low",
"Medium", "High", "Very High"))
df$Close_range <- discretize(df$Close, method = "interval", breaks = 4, labels = c("Low",
"Medium", "High", "Very High"))
df$Volume_range <- discretize(df$Volume, method = "interval", breaks = 4, labels = c("Low",
"Medium", "High", "Very High"))
transactions <- as(df[, c("Open_range", "Close_range", "Volume_range")], "transactions")
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.8))
inspect(rules)
3. How are the Open, High, Low, and Close prices related for Netflix stock?
install.packages("corrplot")
library(corrplot)
Name: Kalyani S Roll No: 22CSEB11 Page No:
price_data <- df[, c("Open", "High", "Low", "Close")]
cor_matrix <- cor(price_data)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
print(cor_matrix)
4. Does higher trading Volume relate to larger changes in Netflix’s stock price?
# Investigate whether days with higher trading volumes lead to bigger price changes by plotting
a scatter plot of Volume vs. Price Change (difference between Open and Close). Also, run a
linear regression to see if there is a significant relationship.
df$Price_Change <- df$Close - df$Open
ggplot(df, aes(x = Volume, y = Price_Change)) +
 geom_point() +
 geom_smooth(method = "lm", col = "blue") +
 labs(title = "Volume vs Price Change", x = "Volume", y = "Price Change")
volume_change_model <- lm(Price_Change ~ Volume, data = df)
summary(volume_change_model)
