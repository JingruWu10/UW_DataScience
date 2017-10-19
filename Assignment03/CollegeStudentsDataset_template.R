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
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is alist of 2 items
  return(dataSetSplit)
}

PartitionExact <- function(dataSet, fractionOfTest = 0.3)
{
  # generate vector of randoum numbers
  # sortedX <- Sort(x)
  # ValueAtFracton <- something <= something
  # x<-runif(numberOfRows)
  # sort the rows (based on the random numbers, I guess)
  # look at R's quantil function
  # Grab the value that matchs the "fractionOfTest" distance into table
  # TestFlag <- x < ValueAtFraction
  # ********** Add code here
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is alist of 2 items
  return(dataSetSplit)
}

PartitionFast <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  # Specify the test fraction
  # generate a random number for each row. Use runif() = random uniform
  # x<- runif(nrow(dataset))
  # Create flag to partition, if number is > 0.4, test set.
  # testFlag <- x <  fractionOfTest # Might need to use less-than instead.
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData) # this is alist of 2 items
  return(dataSetSplit)
}
