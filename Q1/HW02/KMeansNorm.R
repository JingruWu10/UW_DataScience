# UW PCE Data Science Autumn 2017 Assignment 2
# Leo Salemann 10/13/17
# Based on KMeansNorm_skeleton.R, Copyright 2017 by Ernst Henle
###############################################################

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

source("KMeansHelper.R") # ClusterPlot. Samples for observations, clusterCenters, and labels.
source("KMeans.R")

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  # initialize some vars, because I'm not sure what R's scoping rules are.
  observationMean <- NULL
  observationStdev <- NULL
  
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    observationMean1 <- mean((observations[,1]))
    observationStdev1 <- sd((observations[,1]))
    
    # normalize 1st dimension of observations
    observations[,1] <- ((observations[,1]) - observationMean1)/observationStdev1

    # normalize 1st dimension of clusterCenters based on mean & stdev OF OBSERVATIONS
    clusterCenters[,1] <- ((clusterCenters[,1]) - observationMean1)/observationStdev1
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    observationMean2 <- mean((observations[,2]))
    observationStdev2 <- sd((observations[,2]))
    
    # normalize 2nd dimension of observations
    observations[,2] <- ((observations[,2]) - observationMean2)/observationStdev2
    
    # normalize 2nd dimension of clusterCenters
    clusterCenters[,2] <- ((clusterCenters[,2]) - observationMean2)/observationStdev2
  }
  
  KMeans(observations, clusterCenters, mainTitle=paste("NormD1=", normD1, "; NormD2=", normD2, sep=" "))

  # KMeansResult <- kmeans(x=observations, centers=clusterCenters)
  # clusterCenters <- KMeansResult$centers
  # ClusterPlot(observations=observations, centers=clusterCenters, labels=KMeansResult$cluster, 
  #             paste("NormD1=", normD1, "NormD2=", normD2, sep=" "))
  if (normD1)
  {
    # denormalize in first dimension
    clusterCenters[,1] <- ((clusterCenters[,1])*observationMean1 + observationStdev1)
  } 
  if (normD2)
  {
    # denormalize in second dimension
    clusterCenters[,2] <- ((clusterCenters[,2])*observationMean2 + observationStdev2)
  } 
  return(clusterCenters)
}

# Q4a
# The most obvious difference in the two dimension are their range.
# The first dimension ranges from about -25 5o 15; while the range of the 2nd is about +/- 250

# Q4b
# The separation of Test 1 occurs along the y (second) dimension, because the data is much more 
# spread out along y then x.  This becomes very clear if you plot with an aspect ration of 1 ;)
# 

# Q4c
# The separation of Test 1 occurs along the y (second) dimension, for similar reasons as Test 1.
# All we did was normalize the x-axis.  The Y axis is still un-normalized, so the spread is massive.
# I discovered the abas=1 argument for plot() aver finishing up step 3 but before I moved on to step 4.
# 

# Q4d
# The separation for Test 3 occurs along the x axis, because y has been normalized, and has a 
# range much smaller than X now.
# 

# Q4e
# The sparation for Test 4 occurs along both the x and y axes, beacuse both dimensions have been
# normalized, so now the range for both is about +/- 2
# 

# Q5
# Normalization is important in K-means clustering because it forces every dimension to the same scale.  
# K-means works on euclidean distance, which only "makes sense" when every dimension is on the same scale.

# Q6
# In order to encode categorical data for K-means, you have to convert your categories to numerics with 
# one-hot encoding, and then have a uniqe dimension for each value. So, if you start with gender (m, f)
# and favoriteIceCream(chocolate, strawberry, vanilla) you'd convert them to isFemale, likesChocolate, 
# likesStrawberry, and likesVanilla. Each of these would be its own dimension, where the values just-so-happen 
# to only be 0 or 1.
# 

# Q7
# Clustering is un-supervised learning because the cluster centers we start with can be completely 
# random. We never need to "train" the algoirthim with data showing what the centers are "supposed to be."
# 
