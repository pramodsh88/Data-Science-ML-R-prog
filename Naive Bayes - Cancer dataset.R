library(e1071)

library(caret)
library(gmodels) #used for model fitting

bc_data  <- read.csv(file.choose())

df <- bc_data[-1]

str(df)
summary(df)




df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))


set.seed(1234)

train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]


nb_model <- naiveBayes(class ~., data=df.train)



nb_preds <- predict(nb_model, df.validate, type = 'class')



table(nb_preds, df.validate$class,dnn=c("Prediction","Actual"))


confusionMatrix(df.validate$class, nb_preds)
CrossTable(df.validate$class, nb_preds)

