file <- "./tennis-1.csv"
data <- read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE)
head(data)

formula <- as.formula('play ~ .')

# Classification Tree with rpart
library(rpart)

# grow tree 
tree_model <- rpart(formula,
             method="class", data=data, control=rpart.control(minsplit=1, cp=0.001))

# plot tree 
plot(tree_model, uniform=TRUE, 
     main="Classification Tree for Play Tennis")
text(tree_model, use.n=TRUE, all=TRUE, cex=.8)

# Add a noise row to the data
data1 <- rbind(data, c("sunny", "hot", "normal", "TRUE", "no"))
# grow tree 
tree_model1 <- rpart(formula,
                    method="class", data=data1, control=rpart.control(minsplit=1, cp=0.001))

# plot tree 
plot(tree_model1, uniform=TRUE, 
     main="Classification Tree for Play Tennis")
text(tree_model1, use.n=TRUE, all=TRUE, cex=.8)

# Add some way to prevent overfitting, make minsplit=5 in the previous tree_model1, and see how the tree changes
