file <-"/home/vagrant/git/UW_DataScience/Q3/HW03/Bank Data.csv"

data <- read.csv(file, sep=",", header=TRUE)
summary(data)

nrows <- nrow(data)
ntrain <- round(nrows*0.8)
index_shuffle <- sample(nrows)
train_index <- index_shuffle[1:ntrain]
test_index <- index_shuffle[(ntrain+1):nrows]

train_data <- data[train_index,]
test_data <- data[test_index,]

#########################
#One-hot encoding
#########################
library(onehot)
encoder <- onehot(train_data, stringsAsFactors=TRUE, max_levels=5)
train_data_encoded <- predict(encoder, train_data)
test_data_encoded <- predict(encoder, test_data)
summary(train_data)
summary(train_data_encoded)

###########################
# Risk values
###########################
unique_regions <- unique(train_data[,3])

region_risks <- list()
region_risks$region <- unique_regions
region_risks$risk <- rep(0,4)
count <- 1
for (region in unique_regions){
  n0 <- sum(train_data[,3]==region)
  n1 <- sum(train_data[,3]==region & train_data[,"pep"]=="YES")
  region_risks$risk[count] <- log((n1/n0)/((n0-n1)/n0))
  count <- count+1
}

# Usually, we use two smooth parameters to smooth the risk values
s1 = 2
s2 = 4
region_risks_smoothed <- list()
region_risks_smoothed$region <- unique_regions
region_risks_smoothed$risk <- rep(0,4)
count <- 1
for (region in unique_regions){
  n0 <- sum(train_data[,3]==region)
  n1 <- sum(train_data[,3]==region & train_data[,"pep"]=="YES")
  region_risks_smoothed$risk[count] <- log((n1+s1)/(n0+s2)/((n0+s2-n1-s1)/(n0+s2)))
  count <- count+1
}


