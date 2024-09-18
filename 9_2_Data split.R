##################################################
### PROG8430                                    ##
### Simple Linear Regression - Demo             ## 
##################################################
# Written by Peiyuan
# ID: 123456
##################################################
### Basic Set Up                                ##
##################################################
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################
#If the library is not already downloaded, download it
if(!require(tidyverse)){install.packages("tidyverse")}
#library("tidyverse")
if(!require(caret)){install.packages("caret")}
#library("caret")
if(!require(datarium)){install.packages("datarium")}
#library("datarium")

############################### 
# Two way Hold out validation #
###############################
# loading the dataset
data("marketing", package = "datarium")
head(marketing)

# m is used to get number of rows instead of one feature we can use nrows of dataset to find
# the number of samples avaiable in the dataset
m=length(marketing$youtube)
m

set.seed(456) 
train_index=sample(m,m*0.8)
train_set <- marketing[train_index, ]
test_set <-marketing[-train_index,]

train_index=sample(m,m*0.7)

valid_index=sample(m,m*0.3)
train_set <- marketing[train_index, ]
test_set <-marketing[-train_index,]


wilcox.test(train_set$youtube, test_set$youtube)
wilcox.test(train_set$facebook, test_set$facebook)
wilcox.test(train_set$newspaper, test_set$newspaper)
wilcox.test(train_set$sales, test_set$sales)

#build model
model <- lm(sales ~., data = train_set)
# predicting the target variable
predictions <- predict(model, test_set)

# computing model performance metrics
data.frame( RMSE = RMSE(predictions, test_set$sales),
            R2 = R2(predictions, test_set$sales),
            MAE = MAE(predictions, test_set$sales)) # Mean absolute error

#rmse <- sqrt(mean((predictions - true_values)^2))
#r_squared <- 1-(sum(true-predictions)^2/sum(true-mean)^2)
#higher R-squared value (close to 1) indicates a better fit of the model to the data
#mae <- mean(abs(predictions - true_values))

##########################
# K-fold Cross-Validation#
##########################
set.seed(456)
# defining training control as cross-validation and K=10 
train_control <- trainControl(method = "cv", number = 10)

# training the model by assigning sales column
# as target variable and rest other column as independent variable
model_cv <- train(sales ~., data = marketing,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model_cv)

####################################
# 10-times-10-fold Cross-Validation#
####################################
set.seed(456)
# defining training control as cross-validation and K=10 
train_control <- trainControl(method = "repeatedcv", number = 10, repeats=10)

# training the model by assigning sales column
# as target variable and rest other column as independent variable
model_cv2 <- train(sales ~., data = marketing,
                  method = "lm",
                  trControl = train_control)

# printing model performance metrics
# along with other details
print(model_cv2)


















