library(stats)
library(neuralnet)
head(infert)

summary(infert)

# split data into training and validation
set.seed(98051)
train_index <- sample.int(n=nrow(infert), size=floor(0.75*nrow(infert)), replace=F)
train_data <- infert[train_index, ]
valid_data <- infert[-train_index, ]

# scale training and validation data
train_data_scaled <- train_data
train_data_scaled[, c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")] <-
  scale(train_data[,c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")])
valid_data_scaled <- valid_data

valid_data_scaled[, c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")] <-
  scale(valid_data[, c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")], 
        center=colMeans(train_data[, c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")]), 
        scale=apply(train_data[, c("age", "parity", "induced", "spontaneous", "stratum", "pooled.stratum")], 2, sd))

# hypper parameters
hidden_nodes <- 3
iterations <- 10 # number of iterations, default = 1
stepmax <- 10000
lr <- 0.05 # learning rate. 
error_function <- 'ce' # Cross entropy for classification, SSE for regression

# Train a Neural network with two hidden neurons
nn <- neuralnet(case~age+parity+induced+spontaneous, 
                data=train_data_scaled, hidden=hidden_nodes, rep=iterations, stepmax = stepmax,
                err.fct=error_function, learningrate=lr, linear.output=FALSE)

# Plot the learned weight matrix of the neural network
plot(nn, rep="best") # if rep="best" is not specified, all repetitions will be plotted. 

nn$result.matrix

covaresult <- cbind(nn$covariate, nn$response, nn$net.result[[1]])
dimnames(covaresult) <- list(NULL, c("age", "parity", "induced", "spontaneous", "case", "nn-output"))
print(covaresult)

# predict the validation data
valid_pred <- compute(nn, as.matrix(valid_data_scaled[,c("age", "parity", "induced", "spontaneous")]))$net.result
threshold <- 0.5
valid_label <- rep(0, nrow(valid_pred))
valid_label[valid_pred>threshold] <- 1

accuracy <- sum(valid_label == valid_data_scaled[,"case"])/nrow(valid_data_scaled)*100
print(paste("Accuracy=", round(accuracy,2), "%", sep=""))
