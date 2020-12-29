dataset <- read.csv(file.choose()) #Get the data
summary(dataset) #summary of data
str (dataset)
dataset$State = factor(dataset$State, 
                          levels = c('New York', 'California', 'Florida'),
                          labels= c(1, 2, 3))
dataset$State = as.factor (dataset$State) #EDA of data
plot(dataset$R.D.Spend, dataset$Profit)
plot(dataset$Administration, dataset$Profit)
plot(dataset$Marketing.Spend, dataset$Profit)
plot(dataset$State, dataset$Profit)
pairs(dataset)
dataset$State = as.numeric(dataset$State)
cor(dataset)

#split the data into train & test set
library("caTools")
set.seed(1048)

sample = sample.split(dataset, SplitRatio = 0.8)
train <- subset(dataset, sample==T)
test <- subset(dataset, sample==F)

#Build your algorithm or model with train data
model_linear = lm(Profit~ R.D.Spend, train)

#Predict results with test data
preds = predict(model_linear, test)
test$prediction = preds
test$error = test$Profit - test$prediction
test$abs = abs(test$error)
test$sq_error = test$error^2
test$abs_perc_error = (test$abs/test$Profit)*100
