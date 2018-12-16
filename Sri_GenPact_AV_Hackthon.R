# Sri - GenPact AV Hackthon

# set the workin directory
setwd("D:/Sri_DataScience_Practice/Sri_GenPack_Analytics Vidhya_Hackthon")

# Load packages
install.packages("ggplot2")
library("ggplot2")
install.packages("gdata")
library("gdata")
install.packages("plyr")
library("plyr")

# Load the datasets
train = read.csv("train.csv")
test = read.csv("test_QoiMO9B.csv")
meal_info = read.csv("meal_info.csv")
FF_Centre_info = read.csv("fulfilment_center_info.csv")

# Merge meal_info & FF info to form full data sets

train_fin_1 = merge(train, meal_info, by = "meal_id")
train_fin_2 = merge(train_fin_1, FF_Centre_info, by = "center_id")
test_fin_1 = merge(test, meal_info, by = "meal_id")
test_fin_2 = merge(test_fin_1, FF_Centre_info, by = "center_id")

View(train_fin_2)

# Week, Center_id, meal_id make a unique combination equivalen to id
# So these 3 column can be removed

train_fin_3 = train_fin_2[, !colnames(train_fin_2) %in%  c("center_id","meal_id","week")]
test_fin_3 = test_fin_2[,!colnames(test_fin_2) %in%  c("center_id","meal_id","week")]


# Perform EDA
str(train_fin_3)

hist(train_fin_3$checkout_price, labels = TRUE)
boxplot(train_fin_3$checkout_price, horizontal = TRUE)

hist(train_fin_3$base_price, labels = TRUE)
boxplot(train_fin_3$base_price, horizontal = TRUE)

hist(train_fin_3$emailer_for_promotion, labels = TRUE)
hist(train_fin_3$homepage_featured, labels = TRUE)

# No missing values in any of the columns
# Convert emailer & homepage as factor variables

train_fin_3$emailer_for_promotion = as.factor(train_fin_3$emailer_for_promotion)
train_fin_3$homepage_featured = as.factor(train_fin_3$homepage_featured)
train_fin_3$city_code = as.factor(train_fin_3$city_code)
train_fin_3$region_code = as.factor(train_fin_3$region_code)


test_fin_3$emailer_for_promotion = as.factor(test_fin_3$emailer_for_promotion)
test_fin_3$homepage_featured = as.factor(test_fin_3$homepage_featured)
test_fin_3$city_code = as.factor(test_fin_3$city_code)
test_fin_3$region_code = as.factor(test_fin_3$region_code)

# Remove id column & create new data set
train_fin_4 = train_fin_3[,!colnames(train_fin_3) %in% c("id","num_orders")]
test_fin_4 = test_fin_3[,!colnames(test_fin_3) %in% c("id","num_orders")]
str(train_fin_4)

# Perform one hot encoding for all factor variables
library("dummies")

train_fin_5 = dummy.data.frame(train_fin_4, names = c("emailer_for_promotion","homepage_featured",
                                                      "category","cuisine","city_code","region_code","center_type"))
train_fin_5$num_orders = train_fin_3$num_orders

test_fin_5 = dummy.data.frame(test_fin_4, names = c("emailer_for_promotion","homepage_featured",
                                                    "category","cuisine","city_code","region_code","center_type"))
View(test_fin_5)

# Plot correlation
str(train_fin_5)
cor(train_fin_5$checkout_price, train_fin_5$num_orders)
cor_matrix = as.data.frame(cor(train_fin_5))
View(cor_matrix)

write.csv(cor_matrix, file = "cor_matrix.csv")

# Prepare final data set 
train_fin_6 = train_fin_5[,colnames(train_fin_5) %in% c("checkout_price","base_price","emailer_for_promotion0","emailer_for_promotion1","homepage_featured0","homepage_featured1","categoryBiryani","categoryDesert","categoryPasta","categoryRice Bowl","categorySandwich","cuisineContinental","cuisineItalian","region_code56","op_area")]
train_fin_6$num_orders = train_fin_5$num_orders
View(train_fin_6)

# Build model

Sri_LR = lm(formula = train_fin_5$num_orders ~ .,data = train_fin_5)
summary(Sri_LR)

train_fin_7 = train_fin_5[,colnames(train_fin_5) %in% c("checkout_price","base_price","emailer_for_promotion0","homepage_featured0","categoryBeverages","categoryBiryani","categoryDesert","categoryExtras","categoryFish","categoryOther","categoryPizza","categoryRice","categorySalad","categorySandwich","categorySeafood","categorySoup","cuisineContinental","cuisineIndian","cuisineItalian","city_code456","city_code461","city_code473","city_code478","city_code485","city_code515","city_code517","city_code522","city_code526","city_code541","city_code553","city_code556","city_code561","city_code562","city_code576","city_code577","city_code579","city_code590","city_code593","city_code599","city_code602","city_code604","city_code609","city_code615","city_code620","city_code628","city_code632","city_code638","city_code648","city_code649","city_code651","city_code658","city_code659","city_code675","city_code676","city_code679","city_code680","city_code683","city_code693","city_code695","city_code699","city_code700","city_code702","city_code703","center_typeTYPE_A","center_typeTYPE_B","op_area")]
train_fin_7$num_orders = train_fin_5$num_orders

test_fin_7 = test_fin_5[,colnames(test_fin_5) %in% c("checkout_price","base_price","emailer_for_promotion0","homepage_featured0","categoryBeverages","categoryBiryani","categoryDesert","categoryExtras","categoryFish","categoryOther","categoryPizza","categoryRice","categorySalad","categorySandwich","categorySeafood","categorySoup","cuisineContinental","cuisineIndian","cuisineItalian","city_code456","city_code461","city_code473","city_code478","city_code485","city_code515","city_code517","city_code522","city_code526","city_code541","city_code553","city_code556","city_code561","city_code562","city_code576","city_code577","city_code579","city_code590","city_code593","city_code599","city_code602","city_code604","city_code609","city_code615","city_code620","city_code628","city_code632","city_code638","city_code648","city_code649","city_code651","city_code658","city_code659","city_code675","city_code676","city_code679","city_code680","city_code683","city_code693","city_code695","city_code699","city_code700","city_code702","city_code703","center_typeTYPE_A","center_typeTYPE_B","op_area")]

Sri_LR_Fin = lm(formula = train_fin_5$num_orders ~ ., data = train_fin_5)
summary(Sri_LR_Fin)

# Apply on test data

test_fin_5$num_orders = predict(Sri_LR_Fin, newdata = test_fin_5)
View(test_fin_5)

test_fin_5$num_orders = abs(round(test_fin_5$num_orders))
View(test)

test$num_orders = test_fin_7$num_orders

# Final test data
test_fin_sub = test[,colnames(test) %in% c("id","num_orders")]
View(test_fin_sub)

write.csv(test_fin_sub, file = "test_fin_sub.csv")
