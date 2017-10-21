# UW PCE Data Science Autumn 2017 Assignment 2
# Leo Salemann 10/13/17
# Based on KMeansNormTest.R, Copyright 2017 by Ernst Henle
###########################################################

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

source("KMeansNorm.R")
# source("KMeansNorm_skeleton.R")

TestObservations <- as.matrix(read.csv("TestObservations.csv", header=FALSE))
TestCenters <- matrix(c(1, 1, -2, -2, 2, -2), byrow=T, ncol=2)

# TestObservations Distribution in second dimension
hist(TestObservations[,2], col=rgb(0.2,0.5,0.5,1))
# TestObservations Distribution in first dimension
hist(TestObservations[,1], col=rgb(0.5,0.2,0.2,0.75), add=T)
# hist(TestObservations[,1], col=rgb(0.5,0.2,0.2,0.75))
# What is the single most obvious difference between these two distributions?

# For each of the next 4 tests determine if clustering occurs along one or two dimensions?  Which dimensions?  Why?
# Test 1
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=F)
# Test 2
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=F)
# Test 3
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=T)
# Test 4
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=T)
# Put the answers to the above questions at the bottom of KMeansNorm.R!

