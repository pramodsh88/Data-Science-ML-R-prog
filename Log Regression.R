

install.packages('Amelia')
install.packages('ROCR')
install.packages("rpart.plot")

library(dplyr)    # This library is to convert into character
library(Amelia)   # To handle missing data
library(ggplot2)  # For Plotting
library(caTools)  # For splitting the data
library(ROCR)
library(class)
library(randomForest)
library(rpart)
library(rpart.plot)



adult <- read.csv('adult_sal.csv')
adult <- select(adult, -X, -fnlwgt, -capital_gain, -capital_loss)
View(adult)

adult <- select(adult, -education)

str(adult)
summary(adult)

adult$type_employer =as.factor(adult$type_employer)
adult$marital = as.factor(adult$marital)
adult$occupation = as.factor(adult$occupation)
adult$relationship = as.factor(adult$relationship)
adult$race = as.factor(adult$race)
adult$sex = as.factor(adult$sex)
adult$country = as.factor(adult$country)
adult$income = as.factor(adult$income)
str(adult)
summary(adult)
# Data Cleansing


groupemp <- function(job)
{
  job <- as.character(job)
  
  if(job == 'Never-worked' | job == 'Without-pay')
    return('Unemployed')
  else
    
    if(job == 'Local-gov' | job == 'State-gov' | job == 'Federal-gov')
      return('SL-gov')
  else
    
    if(job == 'Self-emp-inc' | job == 'Self-emp-not-inc')
      return('Self-emp')
    
  else
    
    return(job)
}


groupmarried <- function(mar)
{
  mar <- as.character(mar)
  
  if(mar == 'Divorced' | mar == 'Separated' | mar == 'Widowed')
    return('Not married')
  else
    
    if(mar == 'Married-AF-spouse' | mar == 'Married-civ-spouse' | mar == 'Married-spouse-absent')
      return('Married')
  else
    
    return(mar)
}



Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')




group_country <- function(ctry)
{
  
  if(ctry %in% Asia)
    return('Asia')
  
  else
  
    if(ctry %in% North.America)
        return('North America')

  else
  
    if(ctry %in% Europe)
      return('Europe')

  else
    
    if(ctry %in% Latin.and.South.America)
      return('Latin.and.South.America')
    
  else
    
    return('Other')
    
}


adult$type_employer <- sapply(adult$type_employer, groupemp)
adult$marital <- sapply(adult$marital, groupmarried)
adult$country <- sapply(adult$country, group_country)
adult <- rename(adult, Region = country)


                # Handling missing data


adult[adult == '?'] <- NA
missmap(adult)
missmap(adult, y.at = c(1), y.labels = c(''), col = c('Yellow', 'Black'))


adult$type_employer <- as.factor(adult$type_employer)
adult$marital <- sapply(adult$marital, factor)   # this is another way of coverting to factor
adult$Region <- as.factor(adult$Region)


str(adult)
                # Drop missing data


adult <- na.omit(adult)
missmap(adult, y.at = c(1), y.labels = c(''), col = c('Yellow', 'Black'))


ggplot(adult, aes(age)) + geom_histogram(aes(fill = income), color = 'black', binwidth = 1) + theme_bw()

ggplot(adult, aes(hr_per_week)) + geom_histogram(aes(fill = sex)) + theme_bw()

hist(adult$hr_per_week)

ggplot(adult, aes(hr_per_week)) + geom_histogram(data=adult, aes(x=sex), fill="green", color="black")

ggplot()+
  geom_histogram(data=adult, aes(x=education_num, y=..density..), fill="red", color="black")
              
ggplot()+
  geom_histogram(data=adult, aes(x=age, y=..density..), fill="orange", color="black")+
  facet_grid(sex~marital)

# Splitting the data


set.seed(101)

sample <- sample.split(adult, SplitRatio = 0.7)
train <- subset(adult, sample == T)
test <- subset(adult, sample == F)


             # Logistic Regression

logit_model <- glm(income ~ . , family = binomial(), data = train)
logit_model


#logit_predictions <- predict(logit_model, test, type = 'response')
logit_probs <- predict(logit_model, test, type = 'response')
logit_probs

# Lots of ways to do this

joiner <- function(x){
  if (x>=0.5){
    return('>50K')
  }else{
    return("<=50K")
  }
}

str(test)
test$predictions <- ifelse(logit_probs>=0.5,'>50K','<=50K')
test$predictions <- as.factor(test$predictions)


logit_predictions <- sapply(logit_probs, joiner)
logit_predictions <- as.factor(logit_predictions)



        # Confusion Matrix Table

library(caret)
confusionMatrix(test$income, test$predictions, positive = ">50K")






# Odds Ratio

exp(coef(logit_model))

# ROC Curve and Area under the curve - My method
install.packages("pROC")
library("pROC") #for ROC curve
ROC_lr = roc(test$income, logit_probs)

ROC_lr_auc = auc (ROC_lr)

plot(ROC_lr, col = "red", main = "Logistic Regression ROC plot")

paste("Accuracy % of logistic regression: ", mean(test$income) == round(logit_probs, digits = 0))
paste("Area under curve of logistic regression: ", ROC_lr_auc)

# ROC Curve and Area under the curve


logit_predictTest <- data.frame("Probability" = predict(logit_model, test))

logit_RCRTest <- prediction(logit_predictTest$Probability, test$income)

logit_ROCRTestperformance <- performance(logit_RCRTest, "tpr", "fpr")

plot(logit_ROCRTestperformance,main="Logistic Regression ROC Curve")

logit_auc <- paste(c("Logistic Regression AUC ="),round(as.numeric(performance(logit_RCRTest,"auc")@y.values),digits=2),sep="")

legend("topleft",logit_auc, bty="n")


# Decision Tree

decision_tree_model <- rpart(income ~ ., method = 'class', data = train)


decision_tree_predictions <- predict(decision_tree_model, test, type = "class")
confusionMatrix(decision_tree_predictions, test$income, positive = ">50K")