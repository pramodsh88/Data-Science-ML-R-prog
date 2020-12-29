#activate the installed libraries
library(dplyr)    # This library is to convert into character
library(Amelia)   # To handle missing data
library(ggplot2)  # For Plotting
library(caTools)  # For splitting the data
library(ROCR)
library(class)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)  #For confusion matrix

#Reading the data
breastcancer <- read.csv('bc_data.csv',stringsAsFactors = T)
str(breastcancer)
summary(breastcancer)
missmap(breastcancer)
pairs(breastcancer)
breastcancer$class <- as.factor(breastcancer$class)

set.seed(101)
#Splitting the dataset
sample <- sample.split(breastcancer, SplitRatio = 0.7)
train <- subset(breastcancer, sample == T)
test <- subset (breastcancer, sample == F)

#changing the complete dataset into factor
chrs<- sapply (breastcancer, is.numeric)
chrscols <- names (breastcancer[,chrs])
breastcancer[,chrscols] = lapply(breastcancer[,chrscols], factor)
str(breastcancer)

#Model building for logistic regression
logistic_model <- glm(class~., family = binomial(), data = train)
print(breastcancer$class)
breastcancer$class <- as.factor(breastcancer$class)

#Prediction for logistic regression
bc_data_prob <- predict(logistic_model, test, type = 'response')
bc_data_prob

#checking the probability belong to which class 
test$predictions <- ifelse(bc_data_prob >=0.5 , '4', '2')
test$predictions <- as.factor(test$predictions)

#Confusion matrix
confusionMatrix(test$class, test$predictions, positive = "4")

#Model for Decision tree
DT_model <- rpart(class~. , data= train, method = 'class')

#Prediction for Decision tree
DT_pred <- predict(DT_model, test, type = "class")

# confusion matrix for DT
confusionMatrix(test$class, DT_pred, positive = "4")
