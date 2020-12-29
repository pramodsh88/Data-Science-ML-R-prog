demographics <- read.csv('demographics.csv', stringsAsFactors = T)

library(dplyr)    # This library is to convert into character, Data manipulation
library(Amelia)   # To handle missing data
library(ggplot2)  # For Plotting, Data visualization
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
library(tidyverse)

missmap(demographics)
head(demographics)
demographics <- select(demographics,-gender)
group <- function(income)
demographics$income<- as.factor(demographics$income)
#demographics <- na.omit(demographics)
#missmap(demographics, y.at = c(1), y.labels = c(''), col = c('Yellow', 'Black'))


ggplot(demographics, aes(age)) + geom_histogram(aes(fill = income), color = 'black', binwidth = 1) + theme_bw()

ggplot(demographics, aes(carpr)) + geom_histogram(aes(fill = income),  color= "blue" ,binwidth=1)+theme_bw()
demographics$income <-as.numeric (demographics$income)
hist(demographics$income)
plot(demographics$age,demographics$income)
set.seed(101)

sample <- sample.split(demographics, SplitRatio = 0.7)
train <- subset(demographics, sample == T)
test <- subset(demographics, sample == F)
demographics$retired <- as.factor(demographics$retired)
view(demographics)
str(demographics)
logistic <- glm(retired ~ . , family = binomial(), data  = train)
str(demographics)


str(test)
test$predictions <- ifelse(logit_probs>=0.5,'Yes','No')
test$predictions <- as.factor(test$predictions)

test$predictions <- as.factor(test$predictions)

logit_probs <- predict(logistic, test, type = 'response')

confusionMatrix(test$retired, test$predictions, positive = "Yes")
