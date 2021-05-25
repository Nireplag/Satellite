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

rf <- train(classes~., data = df_train, method = "rf") # train model
saveRDS(rf, "rf.rds") # save trained model
predictions_rf <- predict(rf, df_test)
confusionMatrix(predictions_rf, df_test$classes)

# train and predict for SVM

svm <- train(classes~., data = df_train, method = "svmRadial")
saveRDS(svm, "svm.rds") #save trained model
predictions_svm <-predict(svm, df_test)
confusionMatrix(predictions_svm, df_test$classes)

# train and predict for RNA(nnet)

rna <- train(classes~., data = df_train, method = "nnet")
saveRDS(rna, "rna.rds") #save trained model
predictions_rna <- predict(rna, df_test)
confusionMatrix(predictions_rna, df_test$classes)


# create output file with all the confusion matrix information and analysis of the values
sink("confusion_matrix.txt")
print("--------------------------------------- Random Forest ----------------------------------------------")
print(confusionMatrix(predictions_rf, df_test$classes))
print("")
print("")
print("")
print("")
print("-------------------------------------- Support Vector Machine ---------------------------------------")
confusionMatrix(predictions_svm, df_test$classes)
print("")
print("")
print("")
print("")
print("-------------------------------------- RNA ---------------------------------------")
confusionMatrix(predictions_rna, df_test$classes)
sink()

#Training all a new svm with all data and checking confusion matrix

svm_final <- train(classes~., data = df, method = "svmRadial")
saveRDS(svm_final, "svm_final.rds") 
predictions_svm_final <- predict(svm_final, df)
confusionMatrix(predictions_svm_final, df$classes)

