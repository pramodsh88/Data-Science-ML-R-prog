options(warn=-1)

library(data.table)
library(entropy)
library(ggplot2)
library(caTools)
library(ROCR)
library(rpart)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)
library(pROC)


attrition_dat = fread("Attrition.csv")

# Checking the dimension of data
dim(attrition_dat)

head(attrition_dat)

## check null values 
sum(is.na(attrition_dat))

############## Data exploration ##############


# Checking the classes of columns in overall dataset
# unlist(lapply(attrition_dat, class))
str(attrition_dat)


## converting categorical columns to factors
cat_cols = c("Attrition","BusinessTravel","Department","Education","EducationField","EnvironmentSatisfaction","Gender",
             "JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus","Over18","OverTime",
             "PerformanceRating","RelationshipSatisfaction","StandardHours","StockOptionLevel","WorkLifeBalance")

attrition_dat[,cat_cols] <-  lapply(attrition_dat[,cat_cols,with=FALSE], as.factor)


# Checking summary of data to know about the distribution of data
summary(attrition_dat)


# List of continuous variables in data
cont_vars <- c("Age", "DailyRate","DistanceFromHome","HourlyRate","MonthlyIncome","MonthlyRate",
               "NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear",
               "YearsAtCompany", "YearsInCurrentRole",  "YearsSinceLastPromotion","YearsWithCurrManager",
               "EmployeeCount")

# Distribution of continious variables across attrition
melt_attrition_dat = melt(attrition_dat[,c("Attrition", cont_vars),with=FALSE], id.var = "Attrition")
p <- ggplot(data = melt_attrition_dat , aes(x=variable, y=value)) + geom_boxplot(aes(fill= Attrition))
p <- p + facet_wrap( ~ variable, scales="free")
p


### % attrition across categorical variables
freq_tbl <-  apply(attrition_dat[,cat_cols,with=FALSE],2, function(x) table(attrition_dat$Attrition,x))
freq_tbl <- lapply(freq_tbl,function(x) as.data.frame.matrix(x))

perc_attrition_plot <- list()
i =0
for(name in names(freq_tbl)[-1]){
  i <- i +1
  var_data <- data.frame(apply(freq_tbl[name][[1]],2, function(x) x[2]/sum(x)))
  colnames(var_data) <- name
  my_plot <- ggplot(data=var_data, aes(x=row.names(var_data), y=var_data[,name])) +  geom_bar(stat="identity",fill='red') +
    ylim(0.0,1.0) + ylab("%attrition") + xlab(name) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  plot(my_plot)
  remove(my_plot)
}


## From the above plots we see that percentage churn is more for employees who are single, do over time, travel frequently and have less worklife balance satisfaction.
##Also churn percentage is high in employees who belong to the Human resource, Sales and Laboratory Technician job roles.


### % Hike vs salary color by attrition
ggplot(data = attrition_dat,aes(x = MonthlyIncome,y = YearsSinceLastPromotion,color=attrition_dat$Attrition )) + geom_point()


# Making correlation matrix for numeric variables
corrmat_cont_vars <- cor(attrition_dat[,setdiff(cont_vars,"EmployeeCount"),with=FALSE])


# Plotting correlation plot
corrplot(corrmat_cont_vars)

##############  Feature Selection  ##############

## Pre-modeling feature selection (variance and entropy based)

normalised_entropy_cat_vars = unlist(lapply(attrition_dat[,cat_cols,with=FALSE],function(x) entropy(table(x)/length(unique(x[!is.na(x)])))))/unlist(lapply(attrition_dat[,cat_cols,with=FALSE], function(x) log2(length(x[!is.na(x)]))))
low_entropy_variables = names(normalised_entropy_cat_vars[normalised_entropy_cat_vars ==0])


# normalised_attrition_data <- attrition_dat[,cont_vars,with=FALSE]
variance_cont_vars <- apply(attrition_dat[,cont_vars,with=FALSE],2, function(x) (x-min(x))/(max(x)-min(x)))
variance_cont_vars <- as.data.frame.matrix(variance_cont_vars)
variance_cont_vars <- apply(variance_cont_vars,2,var)
low_variance_vars <- names(variance_cont_vars[is.na(variance_cont_vars)==TRUE ])

print(paste(c("Variables with low entropy: ",low_entropy_variables),collapse = " "))
print(paste(c("Variables with low variance: ",low_variance_vars),collapse = " "))


# Removing variables with low variance and low entropy
attrition_dat1 <- attrition_dat[,-c(low_entropy_variables,low_variance_vars,"EmployeeNumber"),with=FALSE]
dim(attrition_dat1)


# Normalising contionious variables
attrition_dat1[,cont_vars[!(cont_vars %in% low_variance_vars)]] <- as.data.frame.matrix(apply(attrition_dat[,cont_vars[!(cont_vars %in% low_variance_vars)],with=FALSE],2, function(x) (x-min(x))/(max(x)-min(x))))
dim(attrition_dat1)

"Attrition"
table(attrition_dat1$Attrition)


# Spliting data into training and testing using Stratified sampling
# We will split data in the ratio of of 75:25 for training and testing. sample.split randomly splits data in ratio mention but it also makes sure that the target variable is well balanced in each piece.

set.seed(90)
split = sample.split(attrition_dat1$Attrition,SplitRatio = 0.75)
attrition_train <- subset(attrition_dat1,split == TRUE)
attrition_test <- subset(attrition_dat1,split == FALSE)

print(c("Row in Train",nrow(attrition_train)))
print(c("Row in Test", nrow(attrition_test)))


# Distribution of churn in Train
table(attrition_train$Attrition)


# Distribution of churn in test
table(attrition_test$Attrition)


attrition_train$Attrition <- as.factor(attrition_train$Attrition)
attrition_test$Attrition <- as.factor(attrition_test$Attrition)



                      ##############  Model building  ##############  
####################################################################################  

##############  Logistic Regression   ##############  

attr_log <- glm(Attrition ~ ., data = attrition_train,family = 'binomial')
summary(attr_log)


### Predicting for test data
predict_test = predict(attr_log,newdata = attrition_test,type = 'response')



## Threshold - 0.5
print("Confusion matrix for threshold 0.5")

thershold= 0.5

confusion_mat <- table(attrition_test$Attrition, predict_test > thershold)
confusion_mat
# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp <- confusion_mat[4]
tp_plus_fn <- confusion_mat[4] + confusion_mat[2]

sensitivity <- tp/tp_plus_fn
print(c("sensitivity",sensitivity))

# specificity tnr--> specificity = tn/(tn+FP)
tn <- confusion_mat[1]
tn_plus_fp <- confusion_mat[1] + confusion_mat[3]

specificity <- tn/tn_plus_fp
print(c("specificity",specificity))




# Threshold - 0.7
print("Confusion matrix for threshold 0.7")

thershold= 0.7

confusion_mat <- table(attrition_test$Attrition, predict_test > thershold)
confusion_mat
# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp <- confusion_mat[4]
tp_plus_fn <- confusion_mat[4] + confusion_mat[2]

sensitivity <- tp/tp_plus_fn
print(c("sensitivity",sensitivity))

# specificity tnr--> specificity = tn/(tn+FP)
tn <- confusion_mat[1]
tn_plus_fp <- confusion_mat[1] + confusion_mat[3]

specificity <- tn/tn_plus_fp
print(c("specificity",specificity))


# Threshold - 0.1
print("Confusion matrix for threshold 0.1")

thershold= 0.1
confusion_mat <- table(attrition_test$Attrition, predict_test > thershold)
confusion_mat

# sensitivity tpr --> sensitivity = tp/(tp+FN)
tp <- confusion_mat[4]
tp_plus_fn <- confusion_mat[4] + confusion_mat[2]

sensitivity <- tp/tp_plus_fn
print(c("sensitivity",sensitivity))

# specificity tnr--> specificity = tn/(tn+FP)
tn <- confusion_mat[1]
tnplusFP <- confusion_mat[1] + confusion_mat[3]

specificity <- tn/tnplusFP
print(c("specificity",specificity))


## Plotting Receiver operator characteristics curve to decide better on threshold
rocr_pred_logistic_best_treshold = prediction(predict_test ,attrition_test$Attrition)
rocr_perf_logistic_best_treshold = performance(rocr_pred_logistic_best_treshold,'tpr','fpr')
plot(rocr_perf_logistic_best_treshold,colorize=TRUE,print.cutoffs.at = seq(0,1,.1),text.adj =c(-0.2,1.7))
