# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

dataset$State <- as.character(dataset$State)

str(dataset)
summary(dataset)

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

dataset$State <- as.factor(dataset$State)
dataset$State <- as.numeric(dataset$State)
summary(dataset)
plot(dataset$R.D.Spend, dataset$Profit)
plot(dataset$Administration, dataset$Profit)
str(dataset)
cor(dataset)
pairs(dataset)

plot(dataset$R.D.Spend, dataset$State)



# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library("caTools")
set.seed(1048)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)
sample <- sample.split(dataset, SplitRatio = 0.8)
train <-subset(dataset, sample == T)
test <- subset (dataset, sample == F)

lm(y ~ ., data) 

model_1 = lm(Profit ~ R.D.Spend,train)
preds = predict(model_1, test)
test$predictions = preds
test$error = test$Profit - test$predictions
test$abs_error = abs(test$error)
test$sq_error = test$error^2
test$abs_perc_error = (abs(test$Profit-test$preds)/test$Profit)*100

view(test)
lm()

# Fitting Multiple Linear Regression to the Training set
all_var_model = lm(Profit ~ ., train)


summary(regressor)


stepwise_model = step(regressor, direction ='both')
summary(stepwise_model)



rnd_mrktg_model = lm(Profit ~ R.D.Spend + Marketing.Spend, training_set)

# Predicting the Test set results

#predict(model_name, test_data)


y_pred = predict(stepwise_model, test_set)

y_pred

test_set$preds = y_pred
test_set$error = test_set$Profit - test_set$preds
test_set$abs_error = abs(test_set$error)
test_set$sq_errpr = test_set$error^2
test_set$abs_perc_error = (abs(test_set$Profit-test_set$preds)/test_set$Profit)*100


MAE = mean(test_set$abs_error)
MAPE = mean(test_set$abs_perc_error)
MSE = mean(test_set$sq_errpr)
RMSE = sqrt(MSE)

plot(stepwise_model)

plot(regressor)
augment(regressor)

