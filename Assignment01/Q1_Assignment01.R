# UW PCE Data Science Autumn 2017 Assignment 1
# Based on DataScience01a.R, Copyright 2017 by Ernst Henle
####################################################

# CLEAR WORKSPACE & CONSOLE
rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console


# STEP (3) assign a url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)


# STEP (4) Gather info on column headers, assoicate to data frame
# Get info from http://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset)
# Abstract: This data set contains 10 variables that are age, gender, total Bilirubin, direct Bilirubin, total proteins, albumin, A/G ratio, SGPT, SGOT and Alkphos.

headers <- c("age", "gender", "total Bilirubin", "direct Bilirubin", "total proteins", "albumin", "A/G ratio", "SGPT", "SGOT", "Alkphos", "Selector")

# Associate names with the dataframe using this pattern:  names(<dataframe>) <- headers
names(ILPD) <- headers


# STEP (5) View first 6 rows
head(ILPD)

#Binarize gender to become male (1=male; 0=female)
x <- ILPD$"gender"
isMale <- x == "Male"

x
isMale

ILPD$"gender" <- as.numeric(isMale)
head(ILPD)


# STEP (6) Determine mean, median, standard deviation for all columns.
# Gender mean, median, sd will have no meaning, but they'll "work" mathematically.
sapply(ILPD, mean, na.rm = TRUE)
sapply(ILPD, median, na.rm = TRUE)
sapply(ILPD, sd, na.rm = TRUE)


# STEP (7) Create Histograms
hist(ILPD$"age", col=rgb(0,1,0,.5)) 
hist(ILPD$"gender", col=rgb(0,1,0,.5)) 
hist(ILPD$"total Bilirubin",  col=rgb(0,1,0,.5)) 
hist(ILPD$"direct Bilirubin", col=rgb(0,1,0,.5)) 
hist(ILPD$"total proteins",   col=rgb(0,1,0,.5)) 
hist(ILPD$"albumin",    col=rgb(0,1,0,.5)) 
hist(ILPD$"A/G ratio",  col=rgb(0,1,0,.5))  
hist(ILPD$"SGPT",       col=rgb(0,1,0,.5)) 
hist(ILPD$"SGOT",     col=rgb(0,1,0,.5)) 
hist(ILPD$ "Alkphos", col=rgb(0,1,0,.5)) 

# STEP (8) Matrix Plots
# Gender was turned into a numeric back on step (5)
plot(ILPD)

# STEP (9) Observing Plots
# - You can tell a vector contains binary data when the plots take the form 
#   of two parallel lines, either vertical or horizontal. Examples are gender
#   (column 2) and Selector (column 11). Continuous data takes on the form of 
#   classic point cloud exhibiting some degree of "fuzziness"
#
# - Vectors "Total Bilirubin" (column 4) and "direct Bilirubin" are most 
#   strongly correlated, to the surprise of almost no one.
#
# - Vector age (column 1) has very little correlation with vectors SGPT,
#   SGOT, and Alkphos (columns 8-10).


# STEP (10) Removing outliers
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
goodFlag <- (x < highLimit) & (x > lowLimit)
x[goodFlag]


# STEP (11) Relabel a vector to use shortest strings.
# Relabel
Education <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'MS', 'BS', 'MS')
unique(Education)
Education[Education == 'Bachelors'] <- 'BS'
Education[Education == 'Masters'] <- 'MS'
Education[Education == 'High School'] <- 'HS'
Education
  

# STEP (12) Min-Max normalization
vec12 <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)

vec12min <- min(vec12)
vec12range <- max(vec12) - min(vec12)
vec12normalized <- (vec12 - vec12min) / vec12range
vec12normalized


# STEP (13) Z-score normalization
vec13 <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)

# z-Score Normalization
vec13mean <- mean(vec13)
vec13sd <- sd(vec13)
vec13normalized <- (vec13 - vec13mean) / vec13sd
vec13normalized

# STEP (14) Binarization
vec14 <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Red', 'Blue', 'Green', 'Blue')

isRed <- vec14 == 'Red'
isGreen <- vec14 == 'Green'
isBlue <- vec14 == 'Blue'

# You can cast T/F into 1/0
isRed <- as.numeric(isRed)
isGreen <- as.numeric(isGreen)
isBlue <- as.numeric(isBlue)

# Better Presentation:
isRed; isGreen; isBlue

# Or, as a data frame
data.frame(isRed, isGreen, isBlue)


# STEP (15) discretize into 3 equal bins.
vec15 <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)

# Discretization into 4 bins
range <- max(vec15) - min(vec15)
binWidth <- range / 3

bin1Min <- -Inf
bin1Max <- min(vec15) + binWidth
bin2Max <- min(vec15) + 2*binWidth
bin3Max <- Inf

vec15Discretized <- rep(NA, length(vec15))
vec15Discretized

vec15Discretized[bin1Min < vec15 & vec15 <= bin1Max] <- "Low"
vec15Discretized

vec15Discretized[bin1Max < vec15 & vec15 <= bin2Max] <- "Middle"
vec15Discretized

vec15Discretized[bin2Max < vec15 & vec15 <= bin3Max] <- "High"
vec15Discretized


# STEP (16) discretzing into 3 equal-sized (not equal-ranged) bins
# vec16 <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
# 
# bins: 
# 3,  3,  4,  4, 5,  5,  5,  5,  5,  5,  5 (11 items)
# 6,  6,  6,  6,  6,  7,  7,  7,  7 (9 items)
# 8,  8,  9, 12, 24, 24, 25, 81 (8 items)
# bins <- c("High", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", 
#          "Middle", "Middle", "Middle", "Middle", "Middle", "Middle", "Middle", "Middle", "Middle", 
#           "High", "High", "High", "High", "High" 2"High", "High")






