file <- "./tennis.csv"
data <- read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE)

# If the target column, say column class, is numerical, you need to convert it to factors in R for classification problem
# data$class <- factor(data$class)
head(data)

# In your homework, you need to split the data into training and testing sets.
# smp_size <- floor(0.7 * nrow(data))
# set_seed(123)
# train_index <- sample(seq_len(nrow(data)), size = smp_size)
# train <- data[train_index,]
# test <- data[-train_index,]

library(naivebayes)
nbc <- naive_bayes(play ~ ., data = data)
plot(nbc)

# In your homework, you need to test your model performance on testing data
# test_pred <- predict(nbc, test)
# accuracy <- sum(test_pred == test$class)/nrow(test)*100
# print(paste("Your testing accuracy is ", round(accuracy, 2), "%", sep=""))

# Test the NBC model on a real case
test_data <- data.frame(outlook="sunny", temp="cool", humidity="high", windy="TRUE", stringsAsFactors=FALSE)
predict(nbc, test_data)
head(predict(nbc, test_data, type = "prob"))

# NBC with numerical features
iris_file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris_data <- read.csv(iris_file, header=FALSE, sep=",", stringsAsFactors=FALSE)
colnames(iris_data) <- c("sepal.length", "sepal.width", "petal.length", 
                         "petal.width", "class")
head(iris_data)
nrows <- nrow(iris_data)
smp_size <- floor(0.75 * nrows)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrows), size = smp_size)

train <- iris_data[train_ind, ]
test <- iris_data[-train_ind, ]

iris_nbc <- naive_bayes(class ~ ., data = train)
plot(iris_nbc)

iris_test_pred <- predict(iris_nbc, test)
accuracy <- sum(iris_test_pred==test[,'class'])/length(iris_test_pred)
print(paste("Test accuracy =", round(accuracy*100,2), "%", sep=""))