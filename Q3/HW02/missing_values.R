library(datasets)
head(iris)
nrows <- nrow(iris)
ncols <- ncol(iris)
print(paste("There are ", nrows, " rows and ", ncols, " columns.", sep=""))

# Simulate the data quality
iris1 <- iris
iris1[c(1:10, 51:60, 101:110), 1:3] <- NA
na_index <- is.na(iris1)
print(paste("Missing data rate = ", round(sum(na_index)/(nrows*ncols)*100,2), "%", sep=""))

# Correlation matrix with missing values
library(corrgram)
library(corrplot)
par(mfrow=c(1,1))
c <- cor(iris1[,1:4], method = 'pearson')
corrplot(c, insig = 'p-value',  sig.level=-1)

# Correlation matrix by skipping observations with missing values
library(corrgram)
library(corrplot)
par(mfrow=c(1,1))
c <- cor(iris1[,1:4], method = 'pearson', use = "complete.obs")
corrplot(c, insig = 'p-value',  sig.level=-1)

# Column means of data frame with missing values
col_means <- colMeans(iris1[,1:4])
print(col_means)

# Column means of data frame with missing values skipped
col_means <- colMeans(iris1[,1:4], na.rm=TRUE)
print(col_means)

# Get rid of all observations with missing values
iris_nomissing <- na.omit(iris1)

# Let's see an example of imputting missing values with column mean before modeling
train_index <- c(1:40, 51:90, 101:140)
valid_index <- c(41:50, 91:100, 141:150)

library(randomForest)

# Replace missing values by its column mean
iris_imputted <- iris1
for (i in 1:3){
  iris_imputted[is.na(iris1[,i]), i] <- col_means[i]
}
rf_model <- randomForest(Species ~ ., data = iris_imputted[train_index,], ntree = 1, mtry = 4, nodesize=5, importance=TRUE, do.trace=TRUE)
# Check the accuracy on testing data
pred_test <- predict(rf_model, iris_imputted[valid_index,], type='response')
accuracy <- sum(pred_test==iris[valid_index,"Species"])/length(valid_index)*100
print(paste("The accuracy on testing data is ", round(accuracy, 2), "%", sep=""))
