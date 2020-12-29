install.packages("knitr")
install.packages("mlr")
install.packages("corrplot")

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
library(mlr)
library(knitr)
library(corrplot)

set.seed(2281)

#Read the dataset
Attrition <- read.csv("Attrition.csv")
summary(Attrition)
str(Attrition)

#changing the Y variable in categorical data
Attrition$Attrition = factor(Attrition$Attrition, levels=c("Yes", "No"))

#Removal of unwanted variable columns from dataset
Attrition$Over18 = NULL
Attrition$EmployeeCount = NULL
Attrition$StandardHours = NULL

#Exploratry data analysis EDA
ggplot(Attrition, aes(x = Attrition, fill = Attrition)) + geom_bar() + ylab("Count") + ggtitle("Class distribution")

ind= sapply(Attrition, is.factor)
ind = which(ind == TRUE)
corr_mat <- cor(Attrition[,-c(2, 3, 5, 8, 11, 15, 17, 21)])
corrplot(corr_mat)

#Spliting the data
sample = sample.split(Attrition, SplitRatio = 0.8)
train= subset(Attrition, sample== T)
test = subset(Attrition, sample == F)

str(Attrition)
Attrition$BusinessTravel = as.factor(Attrition$BusinessTravel)
Attrition$Department = as.factor (Attrition$Department)
Attrition$EducationField = as.factor(Attrition$EducationField)
Attrition$Gender = as.factor(Attrition$Gender)
Attrition$JobRole = as.factor(Attrition$JobRole)
Attrition$MaritalStatus = as.factor(Attrition$MaritalStatus)
Attrition$OverTime = as.factor(Attrition$OverTime)

#Building the logistic regrission model
logit_model = glm(Attrition~. , data = train, family= binomial())

#Predicition by logistic regression model
pred = predict(logit_model, test, type = 'response')

#checking the probability belong to which class 
test$predictions <- ifelse(pred >=0.5 , 'No', 'Yes')
test$predictions <- as.factor(test$predictions)

#confusion matrix

confusionMatrix(test$Attrition, test$predictions, positive = "No")

#Model for Decision tree
DT_model <- rpart(Attrition~. , data= train, method = 'class')

#Prediction for Decision tree
DT_pred <- predict(DT_model, test, type = "class")

# confusion matrix for DT
confusionMatrix(test$Attrition, DT_pred, positive = "No")
