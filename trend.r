df <- read.csv("sales_data.csv")
str(df)
library(dplyr)
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%m")
df$Day <- format(df$Date, "%d")
2. Identify missing or duplicate values.
missing_values <- colSums(is.na(df))
duplicate_rows <- sum(duplicated(df))
3. Calculate the mean, median, and standard deviation of 'Order_Quantity' and 'Profit'.
mean_order_qty <- mean(df$Order_Quantity, na.rm = TRUE)
median_order_qty <- median(df$Order_Quantity, na.rm = TRUE)
sd_order_qty <- sd(df$Order_Quantity, na.rm = TRUE)
mean_profit <- mean(df$Profit, na.rm = TRUE)
median_profit <- median(df$Profit, na.rm = TRUE)
sd_profit <- sd(df$Profit, na.rm = TRUE)
4. Identify the top 3 most frequent products based on 'Order_Quantity'.
top_products <- df %>%
 group_by(Product) %>%
 summarise(Total_Order_Quantity = sum(Order_Quantity)) %>%
 arrange(desc(Total_Order_Quantity)) %>%
 head(3)
5. Group the data by 'Month-Year' to calculate total sales and profit, then plot the monthly sales
trend.
df$Month_Year <- format(df$Date, "%Y-%m")
monthly_sales <- df %>%
 group_by(Month_Year) %>%
 summarise(Total_Revenue = sum(Revenue), Total_Profit = sum(Profit))
ggplot(monthly_sales, aes(x = as.Date(Month_Year), y = Total_Revenue, group = 1)) +
 geom_line() +
 labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Total Sales") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
6. Decompose the sales data to identify trends and seasonality, and plot a smoothed sales trend
using a 12-month moving average.
monthly_sales$Month_Year <- as.Date(paste0(monthly_sales$Month_Year, "-01"))
sales_ts <- ts(monthly_sales$Total_Revenue, frequency = 12)
sales_decomposed <- decompose(sales_ts)
plot(sales_decomposed)
monthly_sales$Smoothed_Sales <- zoo::rollmean(monthly_sales$Total_Revenue, k = 12, fill =
NA)
ggplot(monthly_sales, aes(x = Month_Year, y = Smoothed_Sales)) +
 geom_line() +
 labs(title = "Smoothed Sales Trend (12-Month Moving Average)", x = "Month-Year", y =
"Smoothed Sales")
7. Analyze sales by 'Age_Group' and create a plot to display sales for the "Adults (35-64)" age
group.
adults_sales <- df %>%
 filter(Age_Group == "Adults (35-64)") %>%
 group_by(Month_Year) %>%
 summarise(Total_Sales = sum(Revenue))
ggplot(adults_sales, aes(x = Month_Year, y = Total_Sales, group = 1)) +
 geom_line() +
 labs(title = "Sales for Adults (35-64)", x = "Month-Year", y = "Total Sales") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
8. Calculate the highest order quantity for each 'Product_Category' and 'Sub-Category', and
visualize it in a plot.
category_summary <- df %>%
 group_by(Product_Category, Sub_Category) %>%
 summarise(Max_Order_Quantity = max(Order_Quantity)) %>%
 arrange(desc(Max_Order_Quantity))
ggplot(category_summary, aes(x = reorder(Sub_Category, Max_Order_Quantity), y =
Max_Order_Quantity, fill = Product_Category)) +
 geom_bar(stat = "identity") +
 coord_flip() +
 labs(title = "Highest Order Quantities by Product Category and Sub-Category", x = "SubCategory", y = "Max Order Quantity")
9. Perform correlation analysis between 'Unit_Cost', 'Order_Quantity', and 'Profit', and plot a
scatter plot with a trend line showing the relationship between 'Unit_Cost' and 'Profit'.
correlation_matrix <- cor(df[, c("Unit_Cost", "Order_Quantity", "Profit")], use = "complete.obs")
ggplot(df, aes(x = Unit_Cost, y = Profit)) +
 geom_point() +
Name: Kalyani S Roll No: 22CSEB11 Page No:
 geom_smooth(method = "lm", co
