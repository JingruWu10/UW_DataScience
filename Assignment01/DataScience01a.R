# DataScience01a.R
# Copyright 2017 by Ernst Henle
####################################################

# Introduction to R
# This is a rapid introduction for Data Scientists

# What is R?
# From Wikipedia:
# R is an open source programming language and software environment for statistical
# computing and graphics. The R language is widely used among statisticians and data
# miners for developing statistical software and data analysis.
# (http://en.wikipedia.org/wiki/R_%28programming_language%29)

# Basic R resources:
# http://www.r-project.org/
# http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf

# Download R from:
# http://cran.r-project.org/bin/

# Rstudio
# Rstudio integrates components into an IDE:
#    File Editor
#	   Console (command line interpreter)
#	   Plots
#    Help
#    Packages
#
# Download R studio from:  
#  http://www.rstudio.com/ide/download/desktop

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# Use the R console like a calculator
3 + 5
exp(-1^2/2)/sqrt(2*pi)

# assignment
x <- 2
y <- exp(-x^2/2)/sqrt(2*pi)

# Get values of variables
x
y

# Hello World
# Most language introductions start off by demonstrating a Hello World program
# The simplest Hello World is the string "Hello World" typed into the console:
"Hello World" # The console will respond with "Hello World" if you run this string
hw <- "Hi World"
hw

# How is code persisted?
# Scripts and Functions are persisted in R files like this one.
# These files have an R suffix (*.R)

# The most common data structure in R:  the vector
# here x is a scalar:
x <- -1
y <- exp(-x^2/2)/sqrt(2*pi)
x
y

# Ask R:  What is x?
is(x)
length(x)

# Some math with vectors
x <- -1:2
y <- 1:4
x
y
2*x^3
x+y
x*y

# here x is a vector:
x <- c(-2, -1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5, 2)
length(x)
# y is a vector because x is a vector
y <- exp(-x^2/2)/sqrt(2*pi)
length(y)
x
y
plot(x, y); lines(x, y) # plot points and add a line to the plot

# Use seq to create a sequence in a vector.
# Note how function arguments (from, to, by) can be specified by their name.
x <- seq(from = -2.5, to = 2.5, by = 0.1)
y <- exp(-x^2/2)/sqrt(2*pi)
plot(x, y); lines(x, y) # plot points and add a line to the plot

###################################################################
# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

# Sharing Data

# assign a url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)
head(ILPD)


# Abstract: This data set contains 10 variables that are age, gender, total Bilirubin, direct Bilirubin, total proteins, albumin, A/G ratio, SGPT, SGOT and Alkphos.

headers <- c("age", "gender", "total Bilirubin", "direct Bilirubin", "total proteins", "albumin", "A/G ratio", "SGPT", "SGOT", "Alkphos")

# Associate names with the dataframe using this pattern:  names(<dataframe>) <- headers
names(ILPD) <- headers
head(ILPD)

# determine the mean, median, and standard deviation (sd) of each column
is(ILPD)
sapply(ILPD, is)

sapply(ILPD, length)

summary(ILPD)

# determine the standard deviation, mean, and median for each vector
sapply(ILPD, sd, na.rm = TRUE)
sapply(ILPD, mean, na.rm = TRUE)
sapply(ILPD, median, na.rm = TRUE)

##### RESUME HERE #####

# Get a profile of each column
hist(ILPD$age, col=rgb(0,1,0,.5)) 
hist(ILPD$"total Bilirubin",  col=rgb(0,1,0,.5)) 
hist(ILPD$"direct Bilirubin", col=rgb(0,1,0,.5)) 
hist(ILPD$"total proteins",   col=rgb(0,1,0,.5)) 
hist(ILPD$"albumin",    col=rgb(0,1,0,.5)) 
hist(ILPD$"A/G ratio",  col=rgb(0,1,0,.5))  
hist(ILPD$"SGPT",       col=rgb(0,1,0,.5)) 
hist(ILPD$"SGOT",     col=rgb(0,1,0,.5)) 
hist(ILPD$ "Alkphos", col=rgb(0,1,0,.5)) 


# Correlate columns
plot(ILPD)

############################################################################
# A glimpse into what we will do in future lessons
# Predictive Analytics:

# Names are difficult.  They are too long.  
names(DATAFRAME.obj) <- c("Recency", "Frequency", "Monetary", "Time", "whether")
head(DATAFRAME.obj)
# Create a schema
formula <- whether ~ Recency + Frequency + Monetary + Time
# Create a model using the data and an algorithm (logistic regression)
model <- glm(formula=formula, data=DATAFRAME.obj, family="binomial")
# Predict probabilities based on model and data
predictedProbabilities <- predict(model, newdata=DATAFRAME.obj[-5], type="response")

# Compare predictions to actual outcomes
threshold <- 0.5
predictedValues <- as.numeric(predictedProbabilities > threshold)
"Confusion Matrix "
table(predictedValues, DATAFRAME.obj$whether)
############################################################################
