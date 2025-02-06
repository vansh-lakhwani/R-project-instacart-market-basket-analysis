
# INSTALL R PACKAGES AND IMPORT LIBRARIES
install.packages(c("plyr", "data.table", "dplyr", "ggplot2", "stringr", "DT", "scales", "knitr"))
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(scales)
library(knitr)
library(caret)
library(e1071)

#SET WORING DIRECTORY
getwd()
setwd("C:/Users/lakhw/OneDrive/Desktop/Sem - 4/R project")
getwd()

#READ ALL CSV FILES / DATASETS
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")
departments <- read.csv("departments.csv")
aisles <- read.csv("aisles.csv")
opp <- read.csv("order_products__prior.csv")
opt <- read.csv("order_products__train.csv")

#CONVERT TO FACTOR VARIABLE 
orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)
orders$order_dow <- as.factor(orders$order_dow)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)
departments$department <- as.factor(departments$department)
aisles$aisle <-  as.factor(aisles$aisle)

#MERGE PRODUCT AND AISLES TABLE WHICH IS THEN CONVERTED IN CSV FILE
products_aisles <- merge(products,aisles,by="aisle_id")     
data <- data.frame(products_aisles)
write.csv(data, "merge_product_aisles.csv", row.names = FALSE)
list.files()
# MERGE PRODUCT_AISLES AND DEPARTMENT TABLE AND GIVE OUTPUT IN FORM OF CSV FILE
products_aisles_dept <- merge(products_aisles,departments,by="department_id")
data <- data.frame(products_aisles_dept)
write.csv(data, "merge pro_ais_dep.csv", row.names = FALSE)
list.files()


# TOP 50 HIGHEST OREDER PRODUCT
temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")
summary(temp)  
kable(head(temp,15))
highest_ordered_product <- sort(table(temp$product_name),decreasing = TRUE)
highest_ordered_product <- highest_ordered_product[1:50]
kable(head(highest_ordered_product,15))
hop <-as.data.frame(highest_ordered_product)
hop %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "black"),legend.position="none" )+
  scale_y_continuous(breaks=seq(50000, 480000, 25000))+
  labs(title="Highest Ordered Products - Top 50", y= "No of Orders")


#TOP 50 HIGHEST RE-ORDER AISLES
temp6 <- opp %>% 
  left_join(products,by="product_id")%>%
  left_join(aisles,by="aisle_id")%>%
  filter(reordered == 1)
kable(head(temp6,10))
dim(temp6)
hroa <-  sort(table(temp6$aisle),decreasing = TRUE)
dim(hroa)
hroa <- hroa[1:50]
kable(hroa)
hroa <-as.data.frame(hroa)
hroa %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "black"),legend.position="none" )+
  scale_y_continuous(breaks=seq(100000, 2700000, 500000))+
  labs(title="Highest RE-Ordered Aisles - Top 50", y= "No of Orders")


#ORDER HOUR OF DAY vs NUMBER OF ORDERS OF CUSTOMERS
p<-orders %>% 
  group_by(order_hour_of_day,order_number)%>%
  summarise(count = n()) %>%
  arrange(desc(count))
kable(head(p,10))
p %>% ggplot(aes(x=order_hour_of_day,y=order_number,fill=order_number))+
  geom_tile(aes(fill=count),colour = "white") + 
  scale_fill_gradient(low = "aquamarine",high = "red")+
  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "lightgrey"))+
  scale_x_continuous(breaks=seq(0, 24, 1))+
  scale_y_continuous(breaks=seq(0, 100, 5))+
  labs(title="Order Hour of Day", x="Order Hour of Day", y="Count of order numbers")


#ORDER OF DAY OF WEEK VS ORDER HOUR OF DAY
temp4 <- orders %>%
  mutate(order_dow = as.factor(order_dow)) %>%
  mutate(order_hour_of_day = as.factor(order_hour_of_day)) %>%
  group_by(order_dow,order_hour_of_day) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
kable(head(temp4,10))
temp4 %>% ggplot(aes(x=order_dow,y=order_hour_of_day))+
  geom_tile(aes(fill=count),colour = "white") + 
  scale_fill_gradient(low = "aquamarine",high = "red")+
  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
  labs(title="Which Day and what time do people order the most?",x="Day of the Week", y="Order Hour of the day")
p<-orders 
p %>% ggplot(aes(x=days_since_prior_order,fill=order_dow))+
  geom_histogram(stat = "count")+
  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
  scale_x_continuous(breaks=seq(0, 30, 1))+
  labs(x="No. of Days since last Order",y="Count of Orders")


#Finding out total %AGE of products which are reordered
#As you can see about 60% products are generally reordered & 40% of Products are not ordered again<br/> 
  temp2 <- opp %>%
  group_by(reordered) %>%
  summarise(count = n()) %>%
  mutate(reordered = as.factor(reordered)) %>%
  mutate(percentage = count/sum(count))
kable(head(temp2,10))
temp2 %>% ggplot(aes(x=desc(reordered),y=percentage, fill=reordered))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
  scale_x_continuous(breaks=seq(0, 1, 1))+
  scale_y_continuous(breaks=seq(0, 1.0, .15))+
  labs(title="%ge of products Reordered", x="Reordered or Not")

# Maximum Product varieties in each Department
#'Personal Care' and 'Snacks' has the maximum product varieties<br/>
temp3 <- products %>%
  group_by(department_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  left_join(departments,by="department_id")%>%
  top_n(20)
kable(head(temp3,10))
temp3 %>% ggplot(aes(x=reorder(department,-count),y=count, fill=department))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"))+
  labs(title="Max number of product variety in which Department? (Top 20)",y="Count of Products")


#Products from which Departments are ordered the most ?
temp6 <- opp %>% 
  left_join(products,by="product_id")%>%
  left_join(departments,by="department_id")%>%
  select(order_id,department)
kable(head(temp6,10))
dim(temp6)
hod <-  sort(table(temp6$department),decreasing = TRUE)
dim(hod)
hod <- hod[1:21]
kable(hod)
hod <-as.data.frame(hod)
hod %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
  scale_y_continuous(breaks=seq(30000, 9500000, 500000))+
  labs(title="Highest Ordered Department", y= "No of Orders")


#Products from which from which Departments are reordered the most?
temp6 <- opp %>% 
  left_join(products,by="product_id")%>%
  left_join(departments,by="department_id")%>%
  filter(reordered == 1) %>%
  select(order_id,department)
kable(head(temp6,10))
dim(temp6)
hrod <-  sort(table(temp6$department),decreasing = TRUE)
dim(hrod)
hrod <- hrod[1:21]
kable(hrod)
hrod <-as.data.frame(hrod)
hrod %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
  scale_y_continuous(breaks=seq(30000, 9500000, 500000))+
  labs(title="Highest Re-Ordered Department", y= "No of Orders")


#Top 50  highest ordered Aisles?
temp6 <- opp %>% 
  left_join(products,by="product_id")%>%
  left_join(aisles,by="aisle_id")%>%
  select(order_id,aisle)
kable(head(temp6,10))
dim(temp6)
hoa <-  sort(table(temp6$aisle),decreasing = TRUE)
dim(hoa)
hoa <- hoa[1:50]
kable(head(hoa,10))
hoa <-as.data.frame(hoa)
hoa %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
  scale_y_continuous(breaks=seq(300000, 3700000, 500000))+
  labs(title="Highest Ordered Aisles - Top 50", y= "No of Orders")


#Looking into Customers - Highest Ordered products in the First Order 
temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")%>%
  filter(order_number<2)%>%
  select(user_id,order_number,product_id,product_name)
summary(temp)  
kable(head(temp,15))
hopfo <- sort(table(temp$product_name),decreasing = TRUE)
hopfo <- hopfo[1:50]
kable(head(hopfo,15))
dim(hopfo)
hopfo <-as.data.frame(hopfo)
hopfo %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none")+
  scale_y_continuous(breaks=seq(5000, 30000, 5000))+
  labs(title="Highest Ordered Products in First Order - Top 50", y="No of Orders")


# Top 50 highest reordered Aisles?
temp6 <- opp %>% 
  left_join(products,by="product_id")%>%
  left_join(aisles,by="aisle_id")%>%
  filter(reordered == 1)
kable(head(temp6,10))
dim(temp6)
hroa <-  sort(table(temp6$aisle),decreasing = TRUE)
dim(hroa)
hroa <- hroa[1:50]
kable(hroa)
hroa <-as.data.frame(hroa)
hroa %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
  scale_y_continuous(breaks=seq(100000, 2700000, 500000))+
  labs(title="Highest RE-Ordered Aisles - Top 50", y= "No of Orders")

# Avg no of products in the first order of the customer
#Mean : 10.08  means, on an average customer purchases 10 products in the first order<br/>
temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")%>%
  filter(order_number<2)%>%
  group_by(user_id,order_number)%>%
  summarise(cnt = n())
summary(temp)  
kable(head(temp,15))


# Maximum Product varieties in each Aisles
# 'Personal Care' and 'Snacks' has the maximum product varieties<br/>
temp3 <- products %>%
group_by(aisle_id) %>%
summarise(count = n()) %>%
arrange(desc(count)) %>% 
left_join(aisles,by="aisle_id")%>%
mutate(aisle = if_else(as.character(aisle) %in% c('missing','other'), 'G-Others',as.character(aisle))) %>% 
top_n(50)
kable(head(temp3,10))
temp3 %>% ggplot(aes(x=reorder(aisle,-count),y=count, fill=aisle))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none")+
  labs(title="Max number of product variety in which Aisles? (Top 50)",y="Count of Products")


# Change in the Department rank during the reorder
uniq_dept <- hod %>%
  left_join(hrod, by="Var1")%>%
  arrange(desc(Freq.x))
uniq_dept

# Install and load necessary packages
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)

# Merge data for training
order_products <- rbind(opp, opt)
data_merged <- orders %>%
  inner_join(order_products, by = "order_id") %>%
  inner_join(products, by = "product_id")

# Prepare the dataset for modeling
model_data <- data_merged %>%
  select(order_dow, order_hour_of_day, days_since_prior_order, product_id, reordered) %>%
  filter(!is.na(reordered))

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(model_data$reordered, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- model_data[ trainIndex,]
test_data <- model_data[-trainIndex,]

# Train a Random Forest model
rf_model <- randomForest(reordered ~ ., data = train_data, ntree = 100)
print(rf_model)

# Predict on test data
predictions <- predict(rf_model, newdata = test_data)
confusionMatrix(predictions, test_data$reordered)
