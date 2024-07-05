library(dplyr)
library(readxl)
library(ggplot2)


df <- read_excel("Nike_UK_2023-09-01.xlsx")
str(df)


summary(df)
null_value <- sapply(df, function(x) any(is.na(x)))
names(df)[null_value]

df$PRODUCT_SIZE[is.na(df$PRODUCT_SIZE)] <- 0
df$BRAND[is.na(df$BRAND)] <- 0
null_value1 <- sapply(df, function(x) any(is.na(x)))
names(df)[null_value1]


nike_data <- df[df$BRAND == "Nike", ]

department_wise <- nike_data %>%
  group_by(DEPARTMENT) %>%
  summarise(total_current_price = sum(PRICE_CURRENT, na.rm = TRUE))

ggplot(department_wise, aes(x = DEPARTMENT, y = total_current_price)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Total Sum of Current Price by Department",
       x = "Department",
       y = "Total Current Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

category_wise <- nike_data %>%
  group_by(CATEGORY) %>%
  summarise(total_current_price = sum(PRICE_CURRENT, na.rm = TRUE))

ggplot(category_wise, aes(x = "", y = total_current_price, fill = CATEGORY)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Category-wise Total Current Price",
       fill = "Category") +
  theme_void() +
  theme(legend.position = "right")

product_type_wise <- nike_data %>%
  group_by(PRODUCT_TYPE) %>%
  summarise(total_price = sum(PRICE_CURRENT, na.rm = TRUE))

ggplot(product_type_wise, aes(x = PRODUCT_TYPE, y = total_price, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(title = "Product Type Wise Total Current Price",
       x = "Product Type",
       y = "Total Current Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

label_data_count <- table(nike_data$LABEL)
ggplot(data = as.data.frame(label_data_count), aes(x = "", y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Count of Label Types",
       fill = "Label Type") +
  theme_void() +
  theme(legend.position = "right")

category_wise_bestseller <- nike_data %>%
  group_by(CATEGORY, IS_BESTSELLER) %>%
  summarise(count = n())
ggplot(category_wise_bestseller, aes(x = CATEGORY, y = TRUE, fill = factor(TRUE))) +
  geom_bar(stat = "identity") +
  geom_bar(aes(y = FALSE, fill = factor(FALSE)), stat = "identity") +
  labs(title = "Distribution of Bestsellers by Product Category",
       x = "Product Category",
       y = "Count",
       fill = "Is Bestseller") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"), labels = c("Bestseller", "Non-bestseller")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip()


sales_value <- df %>%
  group_by(PRODUCT_TYPE) %>%
  summarise(total_sales = sum(PRICE_CURRENT))

brand_value <- df %>%
  group_by(BRAND) %>%
  summarise(total_sales = sum(PRICE_CURRENT))

ggplot(brand_value, aes(x = BRAND, y = total_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Brand",
       x = "Brand",
       y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wrangling1 <- df[df$BRAND == "Nike", ]
avg_price_brand <- aggregate(df$PRICE_CURRENT, by=list(Brand=df$BRAND), FUN=mean)
hist(df$PRICE_CURRENT, main="Histogram Plot of Product Prices", xlab="Price", ylab="Frequency")


wrangling2 <- df[df$PRICE_CURRENT > 50, ]
df$ON_SALE <- ifelse(df$PRICE_CURRENT < df$PRICE_RETAIL, "Yes", "No")
avg_price_category <- aggregate(df$PRICE_CURRENT, by=list(Category=df$CATEGORY), FUN=mean)
hist(avg_price_category$x, 
     main="Histogram of Average Prices by Category",
     xlab="Average Price", 
     ylab="Frequency",
     col="green")
