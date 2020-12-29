bc_data  <- read.csv(file.choose())

df <- bc_data[-1]

str(df)
summary(df)

df$class <- as.factor(df$class)


df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))


set.seed(1234)

train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]



table(df.train$class)
table(df.validate$class)


prop.table(table(df.train$class))
prop.table(table(df.validate$class))


# Decision tree


library(rpart)

dtree <- rpart(class ~ ., data=df.train, method="class",
               parms=list(split="information"))

prp(dtree)


dtree_preds <- predict(dtree, df.validate, type = "class")

dtree_preds

confusionMatrix(df.validate$class, dtree_preds)



dtree$cptable
plotcp(dtree)


dtree.pruned <- prune(dtree, cp=.0125)
#install.packages("rpart.plot")
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104,
    fallen.leaves = TRUE, main="Decision Tree")




dtree.pred <- predict(dtree.pruned, df.validate, type="class")
dtree.perf <- table(df.validate$class, dtree.pred,
                    dnn=c("Actual", "Predicted"))
dtree.perf



dtree.pred


library(caret)
library(gmodels)


confusionMatrix(df.validate$class, dtree.pred)
CrossTable(df.validate$class, dtree.pred)



performance(dtree.perf)
#**********************************************************
  
  # Random Forest
  library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~., data=df.train, na.action=na.roughfix,importance=TRUE)
fit.forest
importance(fit.forest, type=2)
forest.pred <- predict(fit.forest, df.validate)
forest.perf <- table(df.validate$class, forest.pred, dnn=c("Actual", "Predicted"))
forest.perf
performance(forest.perf)

CrossTable(df.validate$class, forest.pred)
confusionMatrix(df.validate$class, forest.pred)

