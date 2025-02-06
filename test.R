# Install and load necessary packages
install.packages(c("plyr", "data.table", "dplyr", "ggplot2", "stringr", "DT", "scales", "knitr", "randomForest", "e1071", "caret", "cluster"))
install.packages("factoextra")
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(scales)
library(knitr)
library(randomForest)
library(caret)
library(e1071)
library(cluster)  # Added for cluster analysis
library(factoextra)  # Added for visualization of clusters

# Set working directory
getwd()
setwd("C:/Users/lakhw/OneDrive/Desktop/Sem - 4/R project")
getwd()

# Read all CSV files / datasets
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")
departments <- read.csv("departments.csv")
aisles <- read.csv("aisles.csv")
opp <- read.csv("order_products__prior.csv")
opt <- read.csv("order_products__train.csv")

# Convert to factor variable 
orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)
orders$order_dow <- as.factor(orders$order_dow)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)
departments$department <- as.factor(departments$department)
aisles$aisle <-  as.factor(aisles$aisle)

# Merge product and aisles table and save as CSV file
products_aisles <- merge(products, aisles, by="aisle_id")     
write.csv(products_aisles, "merge_product_aisles.csv", row.names = FALSE)
list.files()

# Merge product_aisles and department table and save as CSV file
products_aisles_dept <- merge(products_aisles, departments, by="department_id")
write.csv(products_aisles_dept, "merge_pro_ais_dep.csv", row.names = FALSE)
list.files()

# Top 50 highest order product
temp <- orders %>%
  inner_join(opp, by="order_id") %>%
  inner_join(products, by="product_id")
summary(temp)  
kable(head(temp, 15))

highest_ordered_product <- sort(table(temp$product_name), decreasing = TRUE)[1:50]
kable(head(highest_ordered_product, 15))

hop <- as.data.frame(highest_ordered_product)
hop %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "black"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(50000, 480000, 25000)) +
  labs(title = "Highest Ordered Products - Top 50", y = "No of Orders")

# Top 50 highest reordered aisles
temp6 <- opp %>% 
  left_join(products, by = "product_id") %>%
  left_join(aisles, by = "aisle_id") %>%
  filter(reordered == 1)
kable(head(temp6, 10))
dim(temp6)

hroa <- sort(table(temp6$aisle), decreasing = TRUE)[1:50]
kable(head(hroa, 10))

hroa <- as.data.frame(hroa)
hroa %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(100000, 2700000, 500000)) +
  labs(title = "Highest RE-Ordered Aisles - Top 50", y = "No of Orders")

# Order hour of day vs number of orders of customers
p <- orders %>% 
  group_by(order_hour_of_day, order_number) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
kable(head(p, 10))

p %>% ggplot(aes(x = order_hour_of_day, y = order_number, fill = order_number)) +
  geom_tile(aes(fill = count), colour = "white") + 
  scale_fill_gradient(low = "aquamarine", high = "red") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(colour = "lightgrey")) +
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(title = "Order Hour of Day", x = "Order Hour of Day", y = "Count of order numbers")

# Order of day of week vs order hour of day
temp4 <- orders %>%
  mutate(order_dow = as.factor(order_dow)) %>%
  mutate(order_hour_of_day = as.factor(order_hour_of_day)) %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
kable(head(temp4, 10))

temp4 %>% ggplot(aes(x = order_dow, y = order_hour_of_day)) +
  geom_tile(aes(fill = count), colour = "white") + 
  scale_fill_gradient(low = "aquamarine", high = "red") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(colour = "grey")) +
  labs(title = "Which Day and what time do people order the most?", x = "Day of the Week", y = "Order Hour of the day")

p <- orders 
p %>% ggplot(aes(x = days_since_prior_order, fill = order_dow)) +
  geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(colour = "grey")) +
  scale_x_continuous(breaks = seq(0, 30, 1)) +
  labs(x = "No. of Days since last Order", y = "Count of Orders")

# Finding out total percentage of products which are reordered
temp2 <- opp %>%
  group_by(reordered) %>%
  summarise(count = n()) %>%
  mutate(reordered = as.factor(reordered)) %>%
  mutate(percentage = count / sum(count))
kable(head(temp2, 10))

temp2 %>% ggplot(aes(x = desc(reordered), y = percentage, fill = reordered)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_line(colour = "grey")) +
  scale_x_continuous(breaks = seq(0, 1, 1)) +
  scale_y_continuous(breaks = seq(0, 1.0, .15)) +
  labs(title = "%ge of products Reordered", x = "Reordered or Not")

# Maximum product varieties in each department
temp3 <- products %>%
  group_by(department_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(departments, by = "department_id") %>%
  top_n(20)
kable(head(temp3, 10))

temp3 %>% ggplot(aes(x = reorder(department, -count), y = count, fill = department)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey")) +
  labs(title = "Max number of product variety in which Department? (Top 20)", y = "Count of Products")

# Products from which departments are ordered the most?
temp6 <- opp %>% 
  left_join(products, by = "product_id") %>%
  left_join(departments, by = "department_id") %>%
  select(order_id, department)
kable(head(temp6, 10))
dim(temp6)

hod <- sort(table(temp6$department), decreasing = TRUE)[1:21]
kable(hod)

hod <- as.data.frame(hod)
hod %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(30000, 9500000, 500000)) +
  labs(title = "Highest Ordered Department", y = "No of Orders")

# Products from which departments are reordered the most?
temp6 <- opp %>% 
  left_join(products, by = "product_id") %>%
  left_join(departments, by = "department_id") %>%
  filter(reordered == 1) %>%
  select(order_id, department)
kable(head(temp6, 10))
dim(temp6)

hrod <- sort(table(temp6$department), decreasing = TRUE)[1:21]
kable(hrod)

hrod <- as.data.frame(hrod)
hrod %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(30000, 9500000, 500000)) +
  labs(title = "Highest Re-Ordered Department", y = "No of Orders")

# Top 50 highest ordered aisles
temp6 <- opp %>% 
  left_join(products, by = "product_id") %>%
  left_join(aisles, by = "aisle_id") %>%
  select(order_id, aisle)
kable(head(temp6, 10))
dim(temp6)

hoa <- sort(table(temp6$aisle), decreasing = TRUE)[1:50]
kable(head(hoa, 10))

hoa <- as.data.frame(hoa)
hoa %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(300000, 3700000, 500000)) +
  labs(title = "Highest Ordered Aisles - Top 50", y = "No of Orders")

# Looking into customers - highest ordered products in the first order
temp <- orders %>%
  inner_join(opp, by = "order_id") %>%
  inner_join(products, by = "product_id") %>%
  filter(order_number < 2) %>%
  select(user_id, order_number, product_id, product_name)
summary(temp)  
kable(head(temp, 15))

hopfo <- sort(table(temp$product_name), decreasing = TRUE)[1:50]
kable(head(hopfo, 15))
dim(hopfo)

hopfo <- as.data.frame(hopfo)
hopfo %>% ggplot(aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(5000, 30000, 5000)) +
  labs(title = "Highest Ordered Products in First Order - Top 50", y = "No of Orders")

# Aggregate data by customer
customer_data <- orders %>%
  inner_join(opt, by = "order_id") %>%
  group_by(user_id) %>%
  summarise(total_orders = n(),
            avg_days_between_orders = mean(days_since_prior_order, na.rm = TRUE),
            total_products = n_distinct(product_id))

# Normalize the data
customer_data_scaled <- scale(customer_data[, -1])

# Apply K-means clustering
set.seed(123)
kmeans_result <- kmeans(customer_data_scaled, centers = 5)
customer_data$cluster <- kmeans_result$cluster

# Visualize the clusters
fviz_cluster(kmeans_result, data = customer_data_scaled)
