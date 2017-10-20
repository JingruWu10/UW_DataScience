# CollegeStudentsDataset.R
# Copyright 2017 by Ernst Henle; All rights reserved

# Read the data.  Do not change the following block
url <- "https://www.dropbox.com/s/3uuh9nk57votrnc/SudentPlans.csv?dl=1"
Students <- read.csv(url, header=T)
Students$CollegePlans <- as.numeric(Students$CollegePlans == "Plans to attend")
formula <- CollegePlans ~ Gender + ParentIncome + IQ + ParentEncouragement

# Set repeatable random seed. Do not change the following line
set.seed(4)

PartitionWrong <- function(dataSet, fractionOfTest = 0.3)
{
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows #1:numOfTestRows get True; the rest get False
  
  # print("here comes test flags")
  # print(head(testFlag))
  # >-----------<These lines are common to all Partition Functions>-----------
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is a list of 2 items
  return(dataSetSplit)
}

PartitionExact <- function(dataSet, fractionOfTest = 0.3)
{
  # generate vector of randoum numbers
  numberOfRows <- nrow(dataSet)
  # generate a random number for each row. Use runif() = random uniform
  x <- runif(numberOfRows)
  
  # sort the rows (based on the random numbers, I guess)
  sortedX <- sort(x)
  
  # look at R's quantile function
  # quantileX <- quantile(dataSet, x)
  exactThreshold <- quantile(x, probs=fractionOfTest)
  # Grab the value that matchs the "fractionOfTest" distance into table
  testFlag <- x < exactThreshold
  
  # print("here's the sorted vector")
  # print(sortedX)
  
  # print("here coes test flags")
  # print(testFlag)
  # -------<stuff I haven't figured out yet >-------
  
  # ValueAtFracton <- something <= something
  # x<-runif(numberOfRows)
  # TestFlag <- x < ValueAtFraction
  # >-----------<These lines are common to all Partition Functions>-----------
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is alist of 2 items
  return(dataSetSplit)
}

PartitionFast <- function(dataSet, fractionOfTest = 0.3)
{
  numberOfRows <- nrow(dataSet)
  # generate a random number for each row. Use runif() = random uniform
  x <- runif(numberOfRows)
  testFlag <- x < fractionOfTest
  # print("here's fast partition")
  # print(head(x))
  
  # print("here's the test flags")
  # print(head(testFlag))

  # >-----------<These lines are common to all Partition Functions>-----------
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is alist of 2 items
  return(dataSetSplit)
}
