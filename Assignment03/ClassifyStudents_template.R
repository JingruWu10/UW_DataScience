# ClassifyStudents.R
# Copyright © 2017 by Ernst Henle.  All rights reserved

rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

source("CollegeStudentsDataset_template.R") # Change CollegeStudentsDataset_template to CollegeStudentsDataset

# Set repeatable random seed
set.seed(4)

###################################################

# Partition data between training and testing sets

# Replace the following line with a function that partitions the data correctly
StudentsSplit <- PartitionWrong(Students, fractionOfTest=0.4) # ********** Change here
TestStudents <- StudentsSplit$testingData
TrainStudents <-StudentsSplit$trainingData

###################################################

# Logistic Regression (glm, binomial)

# http://data.princeton.edu/R/glms.html
# http://www.statmethods.net/advstats/glm.html
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html
# http://www.stat.umn.edu/geyer/5931/mle/glm.pdf

# Create logistic regression
# ********** add code here
# Predict the outcomes for the test data. (predict type="response")
# ********** add code here
###################################################

# Naive Bayes
# http://cran.r-project.org/web/packages/e1071/index.html
# http://cran.r-project.org/web/packages/e1071/e1071.pdf

# Get the algorithm
reposURL <- "http://cran.rstudio.com/"
# install package with naive bayes if not alreay installed
if (!require("e1071")) {install.packages("e1071", dep=TRUE, repos=reposURL)} else {" e1071 is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(e1071)

# Create Naive Bayes model
# ********** add code here
# Predict the outcomes for the test data. (predict type="raw")
# ********** add code here
###################################################

# Confusion Matrices

actual <- ifelse(TestStudents$CollegePlans, "Attend", "NotAttend")
threshold <- 0.5

#Confusion Matrix for Logistic Regression
# convert the predicted probabilities to predictions using a threshold
# ********** add code here
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for Logistic Regression")
# create a table to compare predicted values to actual values
# ********** add code here

#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
# ********** add code here
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix Naive Bayes")
# create a table to compare predicted values to actual values
# ********** add code here
###################################################
# Wrong Partition; fractionOfTest=0.4; threshold = 0.5
# 
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#              Actual
# Predicted    Attend  NotAttend
# Attend        934        116
# NotAttend     759       1071
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (934 + 1071)/(934 + 759 + 116 + 1071) = 70%
# 
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      1154       228
# NotAttend    539       959
# Accuracy defined as fraction of predictions that are correct
# Accuracy:  (1154 + 959)/(1154 + 539 + 228 + 959) = 73%
###################################################

# Fill in the rest:

###################################################
# Assignment Item 8a
# Fast Partition; fractionOfTest=0.4; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here 
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here

###################################################
# Assignment Item 8b
# Exact Partition; fractionOfTest=0.4; threshold = 0.5
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here

###################################################
# Assignment Item 8c
# Exact Partition;  fractionOfTest=0.4; threshold = 0.8
#
# --------------------------------
# "Confusion Matrix for Logistic Regression"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here
#
# --------------------------------
# "Confusion Matrix Naive Bayes"
#            Actual
# Predicted   Attend NotAttend
# Attend      *****    *****   add results here
# NotAttend   *****    *****   add results here
# Accuracy defined as fraction of predictions that are correct
# Accuracy:   **************   add results here
###################################################
