# Initialize packages
library(mlbench)
library(caret)

# load dataset
data(Satellite)

# build needed dataset 

df <- Satellite[, c(17:20, 37)]

# split train and test sets
set.seed(9)
index <- createDataPartition(df$classes, p=0.80, list = FALSE) 

df_train <- df[index,]
df_test <- df[-index,]

# train and predict for random forest

rf <- train(classes~., data = df_train, method = "rf")
save(rf, file = "rf.RData")
predictions_rf <- predict(rf, df_test)
confusionMatrix(predictions_rf, df_test$classes)


