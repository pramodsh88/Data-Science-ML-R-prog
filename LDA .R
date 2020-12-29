# Linear Discriminant Analysis (LDA)
install.packages("MASS")
library(MASS)
?iris
head(iris)


str(df)
df = iris



set.seed(1234)

train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]


lda_model <- lda(Species ~ ., data=df)

preds
preds <- predict(lda_model, df)
pc <- preds$class


summary(preds$class)
plot(iris[,3], iris[,4], col=iris[,5])
plot(iris[,3], iris[,4], col=pc)
df$preds = pc


library(caret)
confusionMatrix(df.validate$Species, df.validate$preds)
