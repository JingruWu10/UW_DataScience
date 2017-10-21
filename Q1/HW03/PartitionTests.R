# PartitionTests.R
# Copyright 2016 by Ernst Henle

# To use this script:
# 1 Place CollegeStudentsDataset.R in your working directory
# 2 Source this script

# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")

source("CollegeStudentsDataset.R")

getDataToPartition <- function(numberOfRows = NA)
{
  if (is.na(numberOfRows))
  {
    numberOfRows <- 500 + round(runif(1),2)*1000
  }
  DataToPartition = data.frame(c1=runif(numberOfRows), c2=1:numberOfRows, c3=sample(c(0:9, letters, LETTERS), numberOfRows, replace=TRUE))
  return(DataToPartition)
} # getDataToPartition

getFractionOfTest <- function()
{
  fractionOfTest = 0.2 + round(runif(1)*0.6,1)
  return(fractionOfTest)
} # getFractionOfTest

TestBasic <- function(Partition)
{
  success <- "Test could not complete"
  tryCatch({
    testsCompleted <- 0
    numberOfTries <- 100
    
    for (testNo in 1:numberOfTries)
    {
      dataSet <- getDataToPartition()
      fractionOfTest <- getFractionOfTest()
      basicTest <- Partition(dataSet, fractionOfTest)
      
      # Test correct names in list
      expectedNames <- sort(c("trainingData", "testingData"))
      if(!identical(sort(names(basicTest)), expectedNames))
      {
        success <- paste(c("Error, partition should return: ", expectedNames, "; not: ", sort(names(basicTest))), collapse=" ")
        break;
      } # if else
      
      # Test total rows
      if (nrow(basicTest$trainingData) + nrow(basicTest$testingData) != nrow(dataSet))
      {
        success <- paste("Error:", nrow(basicTest$trainingData), " + ", nrow(basicTest$testingData), "!=", nrow(dataSet))
        break;
      } # if else
      
      # Test Exclusive
      dataSetRecombined <- rbind(basicTest$trainingData, basicTest$testingData)
      dataSetRecombined <- dataSetRecombined[order(dataSetRecombined$c2), ]
      row.names(dataSetRecombined) <- NULL
      dataSetOrdered <- dataSet[order(dataSet$c2), ]
      if(!identical(dataSetOrdered, dataSetRecombined))
      {
        success <- paste("Error, recombined test and training observations are not same as original")
        break
      }
      
      testsCompleted <- testsCompleted + 1
    } # for
    if ((testsCompleted == numberOfTries) && (success == "Test could not complete"))
    {
      success <- "Tests have correct results"
    } else if ((testsCompleted < numberOfTries) && (success != "Test could not complete"))
    {
      success <- paste("Test found problems: ", success)
    } else
    {
      success <- paste("Test failed: ", success)
    } # 2X if else
  },
  warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
  error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch
  return(success)
} # TestBasic

TestApprox <- function(Partition, numberOfTries = 200)
{
  success <- "Test could not complete"
  tryCatch({
    testsCompleted <- 0
    differences <- rep(NA, numberOfTries)
    
    for (testNo in 1:numberOfTries)
    {
      dataSet <- getDataToPartition()
      fractionOfTest <- getFractionOfTest()
      basicTest <- Partition(dataSet, fractionOfTest)
      actualNumberOfTest <- nrow(basicTest$testingData)
      expectedNumberOfTest <- nrow(dataSet)*fractionOfTest
      difference <- round(expectedNumberOfTest) - actualNumberOfTest
      differences[testNo] <- difference
      testsCompleted <- testsCompleted + 1
    } # for
    
    expectedMean <- -0.2
    expectedSD <- 14.8
    actualMean <- mean(differences)
    actualSd <- sd(differences)
    if ((abs(actualMean - expectedMean) > 3) || (abs(actualSd - expectedSD) > 3))
    {
      success <- paste("Error, too much difference between actualMean(", actualMean,") and expectedMean(", expectedMean, ") or actualSd(", actualSd,") and expectedSD(", expectedSD, ")")
    } # if
    
    if ((testsCompleted == numberOfTries) && (success == "Test could not complete"))
    {
      success <- "Tests have correct results"
    } else if ((testsCompleted == numberOfTries) && (success != "Test could not complete"))
    {
      success <- paste("Test found problems: ", success)
    } else
    {
      success <- paste("Test failed: ", success)
    } # 2X if else
  },
  warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
  error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch
  return(success)
} # TestApprox

TestRandom <- function(Partition = PartitionFast, numberOfTries = 500)
{
  success <- "Test could not complete"
  tryCatch({
    testsCompleted <- 0
    means <- rep(NA, numberOfTries)
    sds <- rep(NA, numberOfTries)
    
    for (testNo in 1:numberOfTries)
    {
      numberOfRows <- 100
      dataSet <- getDataToPartition(numberOfRows)
      fractionOfTest <- 0.15
      basicTest <- Partition(dataSet, fractionOfTest)
      means[testNo] <- mean(basicTest$testingData$c2)
      sds[testNo] <- sd(basicTest$testingData$c2)
      testsCompleted <- testsCompleted + 1
    } # for
    
    expectedMeanOfMeans <- 50.5 # 49 52 (numberOfRows + 1)/2
    expectedSdOfMeans <- 7.3 # 6 to 8.5 sd(1:numberOfRows)/sqrt(numberOfRows*fractionOfTest)
    expectedMeanOfSds <- 28.7 # 28 to 29.5 sd(1:numberOfRows)
    expectedSdOfSds <- 3.5 # 3 to 4 sqrt((1-fractionOfTest)*fractionOfTest/(numberOfRows*fractionOfTest))*expectedMeanOfSds*1.27
    MeanOfMeans <- mean(means)
    SdOfMeans <- sd(means)
    MeanOfSds <- mean(sds)
    SdOfSds <- sd(sds)
    
    if ((abs(MeanOfMeans - expectedMeanOfMeans) > 1.5) || (abs(SdOfMeans - expectedSdOfMeans) > 1.3) || (abs(MeanOfSds - expectedMeanOfSds) > 0.7) || (abs(SdOfSds - expectedSdOfSds) > 0.5))
    {
      success <- "Error"
      if (abs(MeanOfMeans - expectedMeanOfMeans) > 1.5)
      {
        success <- paste(success, "difference between MeanOfMeans(", MeanOfMeans,") and expectedMeanOfMeans (",expectedMeanOfMeans,")")
      } # if
      if (abs(SdOfMeans - expectedSdOfMeans) > 1.3)
      {
        success <- paste(success, "difference between SdOfMeans(", SdOfMeans,") and expectedSdOfMeans (",expectedSdOfMeans,")")
      } # if
      if (abs(MeanOfSds - expectedMeanOfSds) > 0.7)
      {
        success <- paste(success, "difference between MeanOfSds(", MeanOfSds,") and expectedMeanOfSds (",expectedMeanOfSds,")")
      } # if
      if (abs(SdOfSds - expectedSdOfSds) > 0.5)
      {
        success <- paste(success, "difference between SdOfSds(", SdOfSds,") and expectedSdOfSds (",expectedSdOfSds,")")
      } # if
    } # if
    
    if ((testsCompleted == numberOfTries) && (success == "Test could not complete"))
    {
      success <- "Tests have correct results"
    } else if ((testsCompleted == numberOfTries) && (success != "Test could not complete"))
    {
      success <- paste("Test found problems: ", success)
    } else
    {
      success <- paste("Test failed: ", success)
    } # 2X if else
  },
  warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
  error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch
  return(success)
} # TestRandom

TestWrong <- function(Partition, numberOfTries = 200)
{
  success <- "Test could not complete"
  tryCatch({
    testsCompleted <- 0
    
    for (testNo in 1:numberOfTries)
    {
      dataSet <- getDataToPartition()
      fractionOfTest <- getFractionOfTest()
      basicTest <- Partition(dataSet, fractionOfTest)
      if (min(basicTest$trainingData$c2) > max(basicTest$testingData$c2))
      {
        recombinedData <- rbind(basicTest$testingData, basicTest$trainingData)
      } else
      {
        recombinedData <- rbind(basicTest$trainingData, basicTest$testingData)
      } # if else
      
      if(!identical(recombinedData, dataSet))
      {
        success <- paste("Test 2: !identical(recombinedData, dataSet)")
        break;
      } # if else
      
      testsCompleted <- testsCompleted + 1
    } # for
    
    if ((testsCompleted == numberOfTries) && (success == "Test could not complete"))
    {
      success <- "Tests have correct results"
    } else if ((testsCompleted < numberOfTries) && (success != "Test could not complete"))
    {
      success <- paste("Test found problems: ", success)
    } else
    {
      success <- paste("Test failed: ", success)
    } # 2X if else
  },
  warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
  error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch
  return(success)
}

# The following test results are necessary but not sufficient for the assignment
print(TestBasic(PartitionWrong)) # "Tests have correct results"
print(TestBasic(PartitionFast)) # "Tests have correct results"
print(TestBasic(PartitionExact)) # "Tests have correct results"
print(TestWrong(PartitionWrong)) # "Tests have correct results"
print(TestApprox(PartitionFast)) # "Tests have correct results"
print(TestRandom(PartitionFast)) # "Tests have correct results"
