X <- seq(-1, 3, 0.1)
Y <- X^2 - 2*X + 1
print(paste("Standard Deviation of Y=", sd(Y), sep=""))

noise_var <- 1
noise <- rnorm(length(X)) * noise_var

Y <- Y + noise
plot(X,Y)

print(paste("Correlation=",cor(X,Y), sep=""))

data <- data.frame(X=X,Y=Y)
library(infotheo)
dat <- discretize(data)
mi <- mutinformation(dat)
print(paste("Mutual information=", mi[1,2], sep=""))
