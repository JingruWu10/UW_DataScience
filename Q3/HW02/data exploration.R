library(datasets)
head(iris)
nrows <- nrow(iris)
ncols <- ncol(iris)
print(paste("There are ", nrows, " rows and ", ncols, " columns.", sep=""))

# Simulate the data quality
iris1 <- iris
iris1[20:40, 1:3] <- NA
na_index <- is.na(iris1)
print(paste("Missing data rate = ", round(sum(na_index)/(nrows*ncols)*100,2), "%", sep=""))

# Summary statistics of a data frame iris
summary(iris)

# Summary statistics of a data frame with missing values iris1
summary(iris1)

# Visualizing categorical variable
par(mfrow=c(1, 2)) 
barplot(table(iris[["Species"]]), main = 'Bar Plot of Species')
pie(table(iris[["Species"]]), main='Pie Chart of Species')

# Visualizing numerical variable
par(mfrow=c(2,2)) 
variable_index <- 2
col_names <- colnames(iris)
hist(iris[,variable_index], main = paste('Histogram of Target', col_names[variable_index]), xlab = col_names[variable_index])
# Kernel Density Plot
d <- density(iris[,variable_index]) # returns the density data 
plot(d, main = paste('Density Plot of', col_names[variable_index])) 
polygon(d, col='grey', border='blue') 
qqnorm(iris[,variable_index], main = paste('QQ Plot of ', col_names[variable_index]))
qqline(iris[,variable_index])
boxplot(iris[,variable_index], main = paste('Boxplot of Target', col_names[variable_index]))

# Rank variables
library(parallel)
library(doSNOW)
par(mfrow=c(1,1)) 
no_cores <- max(detectCores() - 1, 1)
cluster <- makeCluster(no_cores)
registerDoSNOW(cluster)
aov_v <- foreach(i=1:4,.export=c('iris'),.packages=c('DescTools'),.combine='c') %dopar%
{
  get('iris')
  if (max(iris[,i]) != min(iris[,i]))
  {
    col1 <- iris[,i]
    fit <- aov(col1 ~ iris[,5])
    tryCatch(EtaSq(fit)[1], error=function(e) 0)   
      
  } else{
    0
  }
}
names(aov_v) <- col_names[1:4]
aov_v <- subset(aov_v, names(aov_v)!=col_names[5])
aov_v <- sort(aov_v, decreasing = TRUE)
barplot(head(aov_v, 4), xlab = 'Eta-squared value', beside=TRUE, main = paste('Top', length(head(aov_v, 4)), 'Associated Numerical Variables'), las=2, cex.axis = 0.7, space=1)
stopCluster(cluster)

# Correlation structure of numerical variables
library(corrgram)
library(corrplot)
par(mfrow=c(1,1))
c <- cor(iris[,1:4], method = 'pearson')
corrplot(c, insig = 'p-value',  sig.level=-1)