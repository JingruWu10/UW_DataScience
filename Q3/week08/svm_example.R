file <- "titanic.csv"
data <- read.csv(file, sep=",", header=TRUE)
summary(data)

data$Name <- NULL

index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/3))
testset <- data[testindex,]
trainset <- data[-testindex,]
testset <- na.omit(testset)
library(e1071)
library(ROCR)
gamma = .1
kernel = 'sigmoid'
svm.model <- svm(Survived ~ ., data = trainset, cost = 100, gamma = gamma)#, kernel=kernel) 
svm.pred <- predict(svm.model, testset[,-2])

pred <- prediction(svm.pred, testset[,2])
auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))


# Linear kernel
svm.model <- svm(Survived ~ ., data = trainset, cost = 1, kernel='linear') 
svm.pred <- predict(svm.model, testset[,-2])

pred <- prediction(svm.pred, testset[,2])
auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))