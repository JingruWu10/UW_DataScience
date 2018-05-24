file <- "bank-data.csv"

data <- read.csv(file, sep=",", header=TRUE)
summary(data)

cuts <- cut(data[,1], breaks=seq(15, 70, 5), labels=0:10)
print(seq(15, 70, 5))

print(paste("Original value=", data[1,1], ", bin label=", cuts[1], sep=""))
print(paste("Original value=", data[2,1], ", bin label=", cuts[2], sep=""))

data[,"sex"] <- tolower(data[,"sex"])
data[,"sex"] <- as.factor(data[,"sex"])
summary(data)

# z-transformation
data_z <- data
data_z[,c("age", "income", "children")] <- scale(data_z[,c("age", "income", "children")])
summary(data_z)

print(paste("Mean of income=", mean(data_z[,"income"]), ", sd of income=", sd(data_z[,"income"]), sep=""))

# scale to between 0 and 1
scale_0_1 <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_0_1 <- data
data_0_1[,c("age","income", "children")] <- lapply(data_0_1[,c("age", "income", "children")], scale_0_1)
summary(data_0_1)