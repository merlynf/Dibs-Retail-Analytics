#Import data and installing packages
library(tidyverse)
library(readr)
library(rmarkdown)
library(dplyr)
library(lubridate)

jan=read.csv(file="01_Sales_Jan.csv")
feb=read.csv(file="02_Sales_Feb.csv")
mar=read.csv(file="03_Sales_Mar.csv")
apr=read.csv(file="04_Sales_Apr.csv")
may=read.csv(file="05_Sales_May.csv")
jun=read.csv(file="06_Sales_Jun.csv")
jul=read.csv(file="07_Sales_Jul.csv")
aug=read.csv(file="08_Sales_Aug.csv")
sep=read.csv(file="09_Sales_Sep.csv")
oct=read.csv(file="10_Sales_Oct.csv")
nov=read.csv(file="11_Sales_Nov.csv")
dec=read.csv(file="12_Sales_Dec.csv")

#Part 1

#Separate date and time
jan <- jan %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
feb <- feb %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
mar <- mar %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
apr <- apr %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
may <- may %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
jun <- jun %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
jul <- jul %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
aug <- aug %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
sep <- sep %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
oct <- oct %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
nov <- nov %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")
dec <- dec %>%
  separate(Order.Date, into = c("date", "time"), sep = " ")

#Separate Purchase Address
Jan <- jan %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Feb <- feb %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Mar <- mar %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Apr <- apr %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
May <- may %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Jun <- jun %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Jul <- jul %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Aug <- aug %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Sep <- sep %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Oct <- oct %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Nov <- nov %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
Dec <- dec %>%
  separate(Purchase.Address, into = c("Street", "City", "StateZip"), sep = ", ", remove = FALSE) %>%
  separate(StateZip, into = c("State", "Zip"), sep = " ")
 
Jan1 <- Jan %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Feb1 <- Feb %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Mar1 <- Mar %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Apr1 <- Apr %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

May1 <- May %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Jun1 <- Jun %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Jul1 <- Jul %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Aug1 <- Aug %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Sep1 <- Sep %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Oct1 <- Oct %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Nov1 <- Nov %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

Dec1 <- Dec %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "dmy")),
         date = format(date, "%m/%d/%Y"))

sales_combination <- bind_rows(Jan1,Feb1,Mar1,Apr1,May1,Jun1,Jul1,Aug1,Sep1,Oct1,Nov1,Dec1)

sales_combination1 <- sales_combination %>%
  mutate(month_name = month.name[month(as.Date(date, format = "%m/%d/%Y"))])

sales_combination2 <- sales_combination1 %>%
  filter(!is.na(Order.ID) & Order.ID != "",  # Remove NA and empty values in Order.ID
         Quantity.Ordered != 0)              # Remove rows where Quantity.Ordered is zero

unique(sales_combination2$Product) 

sales_combination3<-sales_combination2 %>%
  mutate(Product = replace(Product,Product == "iPhone","IPhone")) %>%
  mutate(Product = replace(Product,Product == "Wired Headphoness","Wired Headphones")) %>%
  mutate(Product = replace(Product,Product == "Goo0gle Phone","Google Phone")) %>%
  mutate(Product = replace(Product,Product == "AAA Batteries (4pack)","AAA Batteries (4-pack)")) %>%
  mutate(Product = replace(Product,Product == "USBC Charging Cable","USB-C Charging Cable"))%>% 
  mutate(Product = replace(Product,Product == "USBC Charging Cable","USB-C Charging Cable"))%>% 
  mutate(Product = replace(Product,Product == "LG Dryer","LG Washing Machine")) 

unique(sales_combination3$Product) 

sales_combination3 <- sales_combination3 %>%
  filter(!Product %in% c("Product", "### syste error###", "Fault error", "##system error##"))

unique(sales_combination3$Product)
unique(sales_combination3$Price.Each)

sales_combination3<-sales_combination3 %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "$149.99","149.99")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "$11.95","11.95")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "1700.0","1700")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "400.0","400")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "300.0","300")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "600.0","600")) %>%
  mutate(Price.Each = replace(Price.Each,Price.Each == "700.0","700")) 

unique(sales_combination3$Price.Each)

sales_combination_Fix <-  sales_combination3%>%
  mutate(Quantity.Ordered = as.numeric(Quantity.Ordered),
         Price.Each = as.numeric(sub("\\$", "", Price.Each)),  # Menghapus tanda dolar jika ada dan mengkonversi ke numerik
         TotalSpend = Quantity.Ordered * Price.Each)

unique(sales_combination_Fix$City)

sales_combination_Fix<-sales_combination_Fix %>%
  mutate(City = replace(City,City == "Las Angeles","Los Angeles")) %>%
  mutate(City = replace(City,City == "SanFrancisco","San Francisco"))

sales_combination_Fix$date <- as.Date(sales_combination_Fix$date, format = "%m/%d/%Y")

# Replace the year for specific rows to 2019
specific_rows <- c(295123, 278809, 236680, 236678, 236681, 236675, 236670, 236677, 236673, 236672, 236676, 236679, 236674, 236671, 319986, 319987, 319988, 319989, 319990, 319991, 319992, 319993, 319994, 319995, 319996, 319997, 319998, 319999)  # The IDs you want to modify
sales_combination_Fix$date[sales_combination_Fix$Order.ID %in% specific_rows] <- as.Date(format(sales_combination_Fix$date[sales_combination_Fix$Order.ID %in% specific_rows], "2019-%m-%d"))

str(sales_combination_Fix)

# Convert 'time' from character to POSIX time object and extract only the hours and minutes as a character
sales_combination_Fix$time <- format(strptime(sales_combination_Fix$time, format = "%H:%M"), format = "%H:%M")

# Ensure the column is treated purely as character data, not as a date or time type
sales_combination_Fix$time <- as.character(sales_combination_Fix$time)

str(sales_combination_Fix$time)

Final_Data <-  sales_combination_Fix
write.csv(Final_Data,"FD.csv")

#Part 2
#Dibs management team are looking for answers to the following questions, answer the below questions:

#a.        What is the worst year of sales and how much sales was earned?
#b.        How much was earned in the best Year of sales?

# Aggregate sales by year
yearly_sales <- Final_Data %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarize(TotalSales = sum(TotalSpend, na.rm = TRUE), .groups = 'drop')  # Replace 'TotalSpend' if your column name differs

# Find the worst and best year of sales
worst_year <- yearly_sales[which.min(yearly_sales$TotalSales), ]
best_year <- yearly_sales[which.max(yearly_sales$TotalSales), ]

# Print the results
print(paste("Worst year of sales:", worst_year$year, "with total sales of $", worst_year$TotalSales))
print(paste("Best year of sales:", best_year$year, "with total sales of $", best_year$TotalSales))

#c.        In the best year of sales which was the best month for sales?
#d.        In the best year of sales how much was earned in the best month?

monthly_sales_best_year <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  mutate(month = format(date, "%m")) %>%
  group_by(month) %>%
  summarize(TotalSales = sum(TotalSpend, na.rm = TRUE), .groups = 'drop')

# Find the best month in the best year of sales
best_month <- monthly_sales_best_year[which.max(monthly_sales_best_year$TotalSales), ]

# Print the results
print(paste("Best year of sales:", best_year$year))
print(paste("Best month in the best year of sales:", best_month$month, "with total sales of $", best_month$TotalSales))

#e.        Which City had the most sales in the best year of sales?

# Filter data for the best year and aggregate sales by city
city_sales_best_year <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  group_by(City) %>%
  summarize(TotalSales = sum(TotalSpend, na.rm = TRUE), .groups = 'drop')

# Find the city with the most sales in the best year of sales
best_city <- city_sales_best_year[which.max(city_sales_best_year$TotalSales), ]

# Print the results
print(paste("In the best year of sales, ", best_year$year, ", the city with the most sales was:", best_city$City, "with total sales of $", best_city$TotalSales))

#f.        To maximise the likelihood of customers buying a product, what time should Dibs business be displaying advertisements in the best year of sales?

# Filter data for the best year
data_best_year <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year)

# Analyze sales by hour within the best year
hourly_sales_best_year <- data_best_year %>%
  mutate(hour = format(as.POSIXct(time, format="%H:%M"), "%H")) %>%
  group_by(hour) %>%
  summarize(TotalSales = sum(TotalSpend, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(TotalSales))

# Identify the best hour for advertising
best_hour <- hourly_sales_best_year %>%
  slice(1) %>%
  pull(hour)

# Print the result
print(paste("In the best year of sales, ", best_year$year, ", the best hour for displaying advertisements is around:", best_hour, ":00"))

# g. Find products most often sold together
# First, we group by order ID and list products in each order
products_together <- Final_Data %>%
  group_by(Order.ID) %>%
  summarize(Products = toString(unique(Product)), .groups = 'drop') %>%
  count(Products) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Print top combinations
print("Products most often sold together:")
print(head(products_together, n = 5))

# h. Find the overall best-selling product
product_sales <- Final_Data %>%
  group_by(Product) %>%
  summarize(TotalQuantity = sum(Quantity.Ordered, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(TotalQuantity))

best_selling_product <- product_sales[1, ]

# Print results
print(paste("Best selling product overall:", best_selling_product$Product, "with total quantity:", best_selling_product$TotalQuantity))

# i. Find the least sold product in the best year of sales
least_sold_product_best_year <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  group_by(Product) %>%
  summarize(TotalQuantity = sum(Quantity.Ordered, na.rm = TRUE), .groups = 'drop') %>%
  arrange(TotalQuantity)

least_sold_product <- least_sold_product_best_year[1, ]

# Print results
print(paste("Least sold product in the best year of sales:", least_sold_product$Product, "with total quantity:", least_sold_product$TotalQuantity))



#Part 3
#The visualizations that the management team would like to see are only for the best year of sales and include:
library(ggplot2)  

#a.       Monthly sales trend vs monthly average sales

# Calculate monthly sales and the average for the best year
monthly_sales <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  mutate(month = format(date, "%m")) %>%
  group_by(month) %>%
  summarize(
    TotalSales = sum(TotalSpend, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate monthly average sales
average_monthly_sales <- mean(monthly_sales$TotalSales)

# Add the average to the dataframe for plotting
monthly_sales$AverageSales = average_monthly_sales

# Create a line plot for monthly sales trend vs. average
ggplot(monthly_sales, aes(x = month)) +
  geom_line(aes(y = TotalSales, group = 1), color = "blue", size = 1) +
  geom_line(aes(y = AverageSales, group = 1), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Monthly Sales Trend vs. Average Sales in", best_year$year),
    x = "Month",
    y = "Sales Amount ($)",
    caption = "Blue line represents monthly sales; Red dashed line represents average monthly sales."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the plot title


#b.       Sales by state

# Aggregate sales by state for the best year
state_sales <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  group_by(State) %>%
  summarize(TotalSales = sum(TotalSpend, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(TotalSales))  # Optional: arrange in descending order of sales

# Prepare the data for plotting
state_sales <- mutate(state_sales, State = reorder(State, TotalSales))

# Plot the data
ggplot(state_sales, aes(x = State, y = TotalSales, fill = TotalSales)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = paste("Sales by State in", best_year$year),
    x = "State",
    y = "Total Sales ($)",
    fill = "Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Optionally save the plot
ggsave("sales_by_state.png", width = 10, height = 8)

#c.       Top 10 products sold in the best year of sales

# Calculate sales for each product in the best year
product_sales_best_year <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  group_by(Product) %>%
  summarize(TotalSales = sum(Quantity.Ordered, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(TotalSales))

# Select the top 10 products
top_10_products <- head(product_sales_best_year, 10)

# Plot the data
ggplot(top_10_products, aes(x = reorder(Product, TotalSales), y = TotalSales, fill = TotalSales)) +
  geom_col() +  # geom_col() is used for bar charts when you already have the data summarized
  scale_fill_gradient(low = "skyblue", high = "midnightblue") +
  labs(
    title = paste("Top 10 Products Sold in", best_year$year),
    x = "Product",
    y = "Total Quantity Sold",
    fill = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Optionally save the plot
ggsave("top_10_products.png", width = 12, height = 8)

#d.       Monthly order trend vs monthly average order

# Calculate the total number of orders per month in the best year
monthly_orders <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  mutate(month = format(date, "%m")) %>%
  group_by(month) %>%
  summarize(NumberOfOrders = n(), .groups = 'drop')  # 'n()' counts the number of rows (orders) per month

# Calculate the average number of orders per month
average_monthly_orders <- mean(monthly_orders$NumberOfOrders)

# Add the average to the dataframe for plotting
monthly_orders$AverageOrders = average_monthly_orders

# Create a line plot for monthly order trend vs. average
ggplot(monthly_orders, aes(x = as.numeric(month))) +
  geom_line(aes(y = NumberOfOrders, group = 1), color = "blue", size = 1) +
  geom_line(aes(y = AverageOrders, group = 1), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Monthly Order Trend vs. Average Orders in", best_year$year),
    x = "Month",
    y = "Number of Orders",
    caption = "Blue line represents monthly orders; Red dashed line represents average monthly orders."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the plot title

# Optionally save the plot
ggsave("monthly_order_trends.png", width = 10, height = 8)

#e.       Daily order trend vs daily average

# Calculate the total number of orders per day in the best year
daily_orders <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  group_by(date) %>%
  summarize(NumberOfOrders = n(), .groups = 'drop')  # 'n()' counts the number of rows (orders) per day

# Calculate the average number of orders per day
average_daily_orders <- mean(daily_orders$NumberOfOrders)

# Add the average to the dataframe for plotting
daily_orders$AverageOrders = average_daily_orders

# Create a line plot for daily order trend vs. average
ggplot(daily_orders, aes(x = date)) +
  geom_line(aes(y = NumberOfOrders, group = 1), color = "blue", size = 1) +
  geom_line(aes(y = AverageOrders, group = 1), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Daily Order Trend vs. Average Orders in", best_year$year),
    x = "Date",
    y = "Number of Orders",
    caption = "Blue line represents daily orders; Red dashed line represents average daily orders."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Optionally save the plot
ggsave("daily_order_trends.png", width = 12, height = 8)

#f.       Hourly order trend vs hourly average order

# Calculate the total number of orders per hour in the best year
hourly_orders <- Final_Data %>%
  filter(format(date, "%Y") == best_year$year) %>%
  mutate(hour = format(as.POSIXct(time, format="%H:%M"), "%H")) %>%
  group_by(hour) %>%
  summarize(NumberOfOrders = n(), .groups = 'drop')  # 'n()' counts the number of rows (orders) per hour

# Calculate the average number of orders per hour
average_hourly_orders <- mean(hourly_orders$NumberOfOrders)

# Add the average to the dataframe for plotting
hourly_orders$AverageOrders = average_hourly_orders

# Create a line plot for hourly order trend vs. average
ggplot(hourly_orders, aes(x = as.numeric(hour))) +
  geom_line(aes(y = NumberOfOrders, group = 1), color = "blue", size = 1) +
  geom_line(aes(y = AverageOrders, group = 1), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = paste("Hourly Order Trend vs. Average Orders in", best_year$year),
    x = "Hour of the Day",
    y = "Number of Orders",
    caption = "Blue line represents hourly orders; Red dashed line represents average hourly orders."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Optionally save the plot
ggsave("hourly_order_trends.png", width = 12, height = 8)

#Part 4
#Build a predictive model for Dibs organisation to be able to predict future sales. In this model you are required to.
install.packages("caret")
install.packages("lattice")

library(ggplot2)
library(lattice)
library(caret)
library(rpart);

set.seed(100) # Setting a random seed for reproducibility


#a.      Spit your data into training/test

# Splitting the data into training and test sets
split <- createDataPartition(Final_Data$TotalSpend, p = 0.80, list = FALSE)
training_set <- Final_Data[split,]
testing_set <- Final_Data[-split,]

# Inspect the split
dim(training_set)
str(training_set)

dim(testing_set)
str(testing_set)

#Use the RentalCount column to check the quality of the prediction against actual values
actual_counts <- testing_set$TotalSpend;

#b.      Choose at least two methods that you will use for prediction
#c       Apply your data to the models and recommend the most appropriate model

# Model 1: Linear Regression
model_lm <- lm(TotalSpend ~ Quantity.Ordered + Price.Each + month_name + City + State, data = training_set)

# Model 2: Decision Tree
model_rpart <- rpart(TotalSpend ~ Quantity.Ordered + Price.Each + month_name + City + State, data = training_set)


#d.      Provide results and visualization of your two models
# Linear Regression Predictions
predict_lm <- predict(model_lm, testing_set)
predict_lm_df <- data.frame(TotalSpend_Pred = predict_lm, TotalSpend = testing_set$TotalSpend, 
                            City = testing_set$City, State = testing_set$State, 
                            month_name = testing_set$month_name, 
                            Quantity.Ordered = testing_set$Quantity.Ordered,
                            Price.Each = testing_set$Price.Each)

# Decision Tree Predictions
predict_rpart <- predict(model_rpart, testing_set)
predict_rpart_df <- data.frame(TotalSpend_Pred = predict_rpart, TotalSpend = testing_set$TotalSpend, 
                               City = testing_set$City, State = testing_set$State, 
                               month_name = testing_set$month_name, 
                               Quantity.Ordered = testing_set$Quantity.Ordered,
                               Price.Each = testing_set$Price.Each)

# View top rows of the predictions
head(predict_lm_df)
head(predict_rpart_df)

# Plotting the differences
par(mfrow = c(1, 1))
plot(predict_lm_df$TotalSpend_Pred - predict_lm_df$TotalSpend, 
     main = "Difference between actual and predicted. lm", ylab = "Difference", xlab = "Index")
plot(predict_rpart_df$TotalSpend_Pred - predict_rpart_df$TotalSpend, 
     main = "Difference between actual and predicted. rpart", ylab = "Difference", xlab = "Index")

# Scatter plot for linear regression model
plot(predict_lm_df$TotalSpend, predict_lm_df$TotalSpend_Pred,
     main = "Actual vs. Predicted: Linear Regression",
     xlab = "Actual TotalSpend", ylab = "Predicted TotalSpend",
     col = "blue", pch = 20)
abline(0, 1, col = "red")  # Add 45-degree line

# Scatter plot for decision tree model
plot(predict_rpart_df$TotalSpend, predict_rpart_df$TotalSpend_Pred,
     main = "Actual vs. Predicted: Decision Tree",
     xlab = "Actual TotalSpend", ylab = "Predicted TotalSpend",
     col = "green", pch = 20)
abline(0, 1, col = "red")  # Add 45-degree line


#Provide justification and reason for the model you have recommended
