library(random)
file <- "C:\\UW\\data\\RedWhiteWine.csv"
data <- read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE)

# If the target column, say column class, is numerical, you need to convert it to factors in R for classification problem
data$Class <- factor(data$Class)
head(data)

# In your homework, you need to split the data into training and testing sets.
smp_size <- floor(0.7 * nrow(data))
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_index,]
test <- data[-train_index,]

library(naivebayes)
nbc <- naive_bayes(Class ~ ., data = train)
plot(nbc)

# In your homework, you need to test your model performance on testing data
test_pred <- predict(nbc, test)
accuracy <- sum(test_pred == test$Class)/nrow(test)*100
print(paste("Your testing accuracy is ", round(accuracy, 2), "%", sep=""))

# Simpler Model
nbc_simpler <- naive_bayes(Class ~ total.sulfur.dioxide, 
                           data = train)

test_pred <- predict(nbc_simpler, test)
accuracy <- sum(test_pred == test$Class)/nrow(test)*100
print(paste("Your testing accuracy is ", round(accuracy, 2), "%", sep=""))