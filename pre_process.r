S0 <- read.csv("SalesData.csv")
str(S0)
summary(S0)
S1 <- na.omit(S0)
S0$Sales[is.na(S0$Sales)] <- mean(S0$Sales, na.rm = TRUE)
S0$Profit[is.na(S0$Profit)] <- mean(S0$Profit, na.rm = TRUE)
S0$Unit.Price[is.na(S0$Unit.Price)] <- mean(S0$Unit.Price, na.rm = TRUE)
S0 <- read.csv("SalesData.csv")
S0$Sales[is.na(S0$Sales)] <- runif(n = sum(is.na(S0$Sales)),min = min(S0$Sales, na.rm =
TRUE),
 max = max(S0$Sales, na.rm = TRUE))
S0$Profit[is.na(S0$Profit)] <- runif(n = sum(is.na(S0$Profit)),min = min(S0$Profit, na.rm =
TRUE),
 max = max(S0$Profit, na.rm = TRUE))
22CSEB11 KALYANI S Pg No:
S0$Unit.Price[is.na(S0$Unit.Price)] <- runif(n = sum(is.na(S0$Unit.Price)),min =
min(S0$Unit.Price, na.rm = TRUE),
 max = max(S0$Unit.Price, na.rm = TRUE))
S0$Order.Priority <- as.factor(S0$Order.Priority)
S0$Ship.Mode <- as.factor(S0$Ship.Mode)
S0$Customer.Name <- as.factor(S0$Customer.Name)
S0$Order.Priority[is.na(S0$Order.Priority)] <- sample(levels(S0$Order.Priority),
 size = sum(is.na(S0$Order.Priority)),
 replace = TRUE)
S0$Ship.Mode[is.na(S0$Ship.Mode)] <- sample(levels(S0$Ship.Mode),
 size = sum(is.na(S0$Ship.Mode)),
 replace = TRUE)
S0$Customer.Name[is.na(S0$Customer.Name)] <- sample(levels(S0$Customer.Name),
 size = sum(is.na(S0$Customer.Name)),
 replace = TRUE)
par(mfrow = c(1, 2))
boxplot(S1$Order.Quantity, main = "Order Quantity")
boxplot(S1$Profit, main = "Profit")
order_priority_summary <- summary(S1$Order.Priority)
order_priority_summary <- summary(S1$Order.Priority)
barplot(as.numeric(order_priority_summary),
 names.arg = names(order_priority_summary),
 main = "Order Priority Distribution",
 col = "lightblue",
 ylab = "Frequency")
S1$Order.Quantity <- scale(S1$Order.Quantity, center = TRUE, scale = TRUE)
S1$Sales <- scale(S1$Sales, center = TRUE, scale = TRUE)
S1$Profit <- scale(S1$Profit, center = TRUE, scale = TRUE)
S1$Unit.Price <- scale(S1$Unit.Price, center = TRUE, scale = TRUE)
S1$Shipping.Cost <- scale(S1$Shipping.Cost, center = TRUE, scale = TRUE)
22CSEB11 KALYANI S Pg No:
S1$Order.Quantity <- (S1$Order.Quantity - min(S1$Order.Quantity)) /
 (max(S1$Order.Quantity) - min(S1$Order.Quantity))
S1$Sales <- (S1$Sales - min(S1$Sales)) /
 (max(S1$Sales) - min(S1$Sales))
S1$Profit <- (S1$Profit - min(S1$Profit)) /
 (max(S1$Profit) - min(S1$Profit))
S1$Unit.Price <- (S1$Unit.Price - min(S1$Unit.Price)) /
 (max(S1$Unit.Price) - min(S1$Unit.Price))
S1$Shipping.Cost <- (S1$Shipping.Cost - min(S1$Shipping.Cost)) /
 (max(S1$Shipping.Cost) - min(S1$Shipping.Cost))
# Construct the distribution table
distribution_table <- table(S1$Order.Priority, S1$Product.Category)
# Display the table
print(distribution_table)
hist(S1$Profit, xlab = "Profit", main = "Profit", col = "lightblue")
S1$Profit.flag <- S1$Profit > 0
table(S1$Profit.flag)
S1$T.cost <- S1$Unit.Price * S1$Order.Quantity + S1$Shipping.Cost
hist(S1$T.cost, main = "Histogram of T.cost", xlab = "T.cost")
