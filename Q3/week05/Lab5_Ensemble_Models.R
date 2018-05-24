data_url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data'
data <- read.csv(data_url, header=FALSE, sep=",")
colnames(data) <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                    "marital_status", "occupation", "relationship", "race", "sex", 
                    "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

# Print the dimension of the data
print(dim(data))

# Print the summary of the data
print(summary(data))

num_obs = nrow(data)

set.seed(98004) 

index_shuffled <- sample(num_obs)
training_ratio <- 0.8
training_size <- round(num_obs * training_ratio)
training_index <- index_shuffled[1:training_size]
testing_index <- index_shuffled[(training_size+1):num_obs]
training_data <- data[training_index,]
testing_data <- data[testing_index,]

##################################
# Random Forest
##################################
library(randomForest)
library(tictoc)
tic()
print("Start training RF model with 200 trees using 1 core")
rf_model <- randomForest(income ~ ., data = training_data, ntree = 200, mtry = 5, 
                         nodesize=3, importance=TRUE)
print("Training 200 trees completed.")
toc()
## Show "importance" of variables: higher value mean more important (MeanDecreaseAccuracy): 
print(round(importance(rf_model), 2))

# Check the accuracy on testing data
pred_test <- predict(rf_model, testing_data, type='response')
accuracy <- sum(pred_test==testing_data[,"income"])/nrow(testing_data)*100
print(paste("The accuracy on testing data is ", round(accuracy, 2), "%", sep=""))

# Check the ROC on testing data
pred_test <- predict(rf_model, testing_data, type='prob')
library(ROCR)
pred <- prediction(pred_test[,2], testing_data[,"income"])
perf <- performance(pred,"tpr","fpr")

auc <- performance(pred,"auc")
plot(perf,col="black",lty=3, lwd=3, main=paste("AUC=", round(unlist(slot(auc, "y.values")),4)))

###################################
# Speed up RF Training by Parallelization
###################################
library(doSNOW)
library(foreach)
library(parallel)


# Setting number of cores in your machine. 
num_cores <- detectCores()
cl <- makeCluster(num_cores-1)
registerDoSNOW(cl)
ntrees <- 200

tic()
print(paste("Start training RF model with 200 trees using ", num_cores-1, " core", sep=""))

rf <- foreach(ntree = rep(round(ntrees/(num_cores-1)), num_cores-1), .combine = combine, .packages = "randomForest") %dopar% 
  randomForest(income~., data=training_data, ntree=ntree, mtry=3, 
                         importance=TRUE, nodesize=5, do.trace = TRUE)

print("Training 200 trees completed.")
toc()
stopCluster(cl)

#####################################
# Gradient Boosted Decision Trees
#####################################
library(gbm)
training_data[,15] <- as.character(training_data[,15])
testing_data[,15] <- as.character(testing_data[,15])
training_data[training_data[,15]==' <=50K', 15] <- 0
training_data[training_data[,15]==' >50K', 15] <- 1
testing_data[testing_data[,15]==' <=50K', 15] <- 0
testing_data[testing_data[,15]==' >50K', 15] <- 1
training_data[,15] <- as.numeric(training_data[,15])
testing_data[,15] <- as.numeric(testing_data[,15])
ntrees <- 250
gbm_model <- gbm(formula = income ~ ., distribution =  "bernoulli", data = training_data, 
                 n.trees = ntrees, interaction.depth = 4, n.minobsinnode = 5, shrinkage = 0.01)

pred_test <- predict(gbm_model, testing_data, n.trees = ntrees, type='response')
pred_test[pred_test > 0.5] <- 1
pred_test[pred_test <= 0.5] <- 0
accuracy <- sum(pred_test==testing_data[,"income"])/nrow(testing_data)*100
print(paste("The accuracy on testing data is ", round(accuracy, 2), "%", sep=""))

# Check the ROC on testing data
pred_test <- predict(gbm_model, testing_data, n.trees = ntrees, type='response')
library(ROCR)
pred <- prediction(pred_test, testing_data[,"income"])
perf <- performance(pred,"tpr","fpr")

auc <- performance(pred,"auc")
plot(perf,col="black",lty=3, lwd=3, main=paste("AUC=", round(unlist(slot(auc, "y.values")),4)))